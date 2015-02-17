/*
 * Clutter.
 *
 * An OpenGL based 'interactive canvas' library.
 *
 * Authored By: Lionel Landwerlin <lionel.g.landwerlin@linux.intel.com>
 *
 * Copyright (C) 2015  Intel Corporation.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * SECTION:clutter-master-clock-gdk
 * @short_description: The GDK master clock for all animations
 *
 * The #ClutterMasterClockDefault class is the GdkFrameClock based implementation
 * of #ClutterMasterClock.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <gdk/gdk.h>

#include "clutter-master-clock.h"
#include "clutter-master-clock-gdk.h"
#include "clutter-stage-gdk.h"
#include "clutter-debug.h"
#include "clutter-private.h"
#include "clutter-profile.h"
#include "clutter-stage-manager-private.h"
#include "clutter-stage-private.h"

#ifdef CLUTTER_ENABLE_DEBUG
#define clutter_warn_if_over_budget(master_clock,start_time,section)    G_STMT_START  { \
  gint64 __delta = g_get_monotonic_time () - start_time;                                \
  gint64 __budget = master_clock->remaining_budget;                                     \
  if (__budget > 0 && __delta >= __budget) {                                            \
    _clutter_diagnostic_message ("%s took %" G_GINT64_FORMAT " microseconds "           \
                                 "more than the remaining budget of %" G_GINT64_FORMAT  \
                                 " microseconds",                                       \
                                 section, __delta - __budget, __budget);                \
  }                                                                     } G_STMT_END
#else
#define clutter_warn_if_over_budget(master_clock,start_time,section)
#endif

typedef struct _ClutterClockSource              ClutterClockSource;

struct _ClutterMasterClockGdk
{
  GObject parent_instance;

  /* the list of timelines handled by the clock */
  GSList *timelines;

  /* mapping between ClutterStages and GdkFrameClocks */
  GHashTable *clock_to_stage;
  GHashTable *stage_to_clock;

  /* the current state of the clock, in usecs */
  gint64 cur_tick;

  /* the previous state of the clock, in usecs, used to compute the delta */
  gint64 prev_tick;

#ifdef CLUTTER_ENABLE_DEBUG
  gint64 frame_budget;
  gint64 remaining_budget;
#endif
};

struct _ClutterClockSource
{
  GSource source;

  ClutterMasterClock *master_clock;
};

static void clutter_master_clock_iface_init (ClutterMasterClockIface *iface);

G_DEFINE_TYPE_WITH_CODE (ClutterMasterClockGdk,
                         clutter_master_clock_gdk,
                         G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (CLUTTER_TYPE_MASTER_CLOCK,
                                                clutter_master_clock_iface_init));

static void
master_clock_schedule_stages_updates (ClutterMasterClockGdk *master_clock)
{
  ClutterStageManager *stage_manager = clutter_stage_manager_get_default ();
  const GSList *stages, *l;

  stages = clutter_stage_manager_peek_stages (stage_manager);

  for (l = stages; l != NULL; l = l->next)
    _clutter_stage_schedule_update (l->data);
}

static void
master_clock_reschedule_stage_update (ClutterMasterClockGdk *master_clock,
                                      ClutterStage          *stage)
{
  /* Clear the old update time */
  _clutter_stage_clear_update_time (stage);

  /* And if there is still work to be done, schedule a new one */
  if (_clutter_stage_has_queued_events (stage) ||
      _clutter_stage_needs_update (stage))
    _clutter_stage_schedule_update (stage);
}

static void
master_clock_process_stage_events (ClutterMasterClockGdk *master_clock,
                                   ClutterStage          *stage)
{
#ifdef CLUTTER_ENABLE_DEBUG
  gint64 start = g_get_monotonic_time ();
#endif

  CLUTTER_STATIC_TIMER (master_event_process,
                        "Master Clock",
                        "Event Processing",
                        "The time spent processing events on all stages",
                        0);

  CLUTTER_TIMER_START (_clutter_uprof_context, master_event_process);

  /* Process queued events */
  _clutter_stage_process_queued_events (stage);

  CLUTTER_TIMER_STOP (_clutter_uprof_context, master_event_process);

#ifdef CLUTTER_ENABLE_DEBUG
  if (_clutter_diagnostic_enabled ())
    clutter_warn_if_over_budget (master_clock, start, "Event processing");

  master_clock->remaining_budget -= (g_get_monotonic_time () - start);
#endif
}

/*
 * master_clock_advance_timelines:
 * @master_clock: a #ClutterMasterClock
 *
 * Advances all the timelines held by the master clock. This function
 * should be called before calling _clutter_stage_do_update() to
 * make sure that all the timelines are advanced and the scene is updated.
 */
static void
master_clock_advance_timelines (ClutterMasterClockGdk *master_clock)
{
  GSList *timelines, *l;
#ifdef CLUTTER_ENABLE_DEBUG
  gint64 start = g_get_monotonic_time ();
#endif

  CLUTTER_STATIC_TIMER (master_timeline_advance,
                        "Master Clock",
                        "Timelines Advancement",
                        "The time spent advancing all timelines",
                        0);

  /* we protect ourselves from timelines being removed during
   * the advancement by other timelines by copying the list of
   * timelines, taking a reference on them, iterating over the
   * copied list and then releasing the reference.
   *
   * we cannot simply take a reference on the timelines and still
   * use the list held by the master clock because the do_tick()
   * might result in the creation of a new timeline, which gets
   * added at the end of the list with no reference increase and
   * thus gets disposed at the end of the iteration.
   *
   * this implies that a newly added timeline will not be advanced
   * by this clock iteration, which is perfectly fine since we're
   * in its first cycle.
   *
   * we also cannot steal the master clock timelines list because
   * a timeline might be removed as the direct result of do_tick()
   * and remove_timeline() would not find the timeline, failing
   * and leaving a dangling pointer behind.
   */
  timelines = g_slist_copy (master_clock->timelines);
  g_slist_foreach (timelines, (GFunc) g_object_ref, NULL);

  CLUTTER_TIMER_START (_clutter_uprof_context, master_timeline_advance);

  for (l = timelines; l != NULL; l = l->next)
    _clutter_timeline_do_tick (l->data, master_clock->cur_tick / 1000);

  CLUTTER_TIMER_STOP (_clutter_uprof_context, master_timeline_advance);

  g_slist_foreach (timelines, (GFunc) g_object_unref, NULL);
  g_slist_free (timelines);

#ifdef CLUTTER_ENABLE_DEBUG
  if (_clutter_diagnostic_enabled ())
    clutter_warn_if_over_budget (master_clock, start, "Animations");

  master_clock->remaining_budget -= (g_get_monotonic_time () - start);
#endif
}

static gboolean
master_clock_update_stage (ClutterMasterClockGdk *master_clock,
                           ClutterStage          *stage)
{
  gboolean stage_updated = FALSE;
#ifdef CLUTTER_ENABLE_DEBUG
  gint64 start = g_get_monotonic_time ();
#endif

  _clutter_run_repaint_functions (CLUTTER_REPAINT_FLAGS_PRE_PAINT);

  /* Update any stage that needs redraw/relayout after the clock
   * is advanced.
   */
  stage_updated |= _clutter_stage_do_update (stage);

  _clutter_run_repaint_functions (CLUTTER_REPAINT_FLAGS_POST_PAINT);

#ifdef CLUTTER_ENABLE_DEBUG
  if (_clutter_diagnostic_enabled ())
    clutter_warn_if_over_budget (master_clock, start, "Updating the stage");

  master_clock->remaining_budget -= (g_get_monotonic_time () - start);
#endif

  return stage_updated;
}

/* static gboolean */
/* clutter_clock_prepare (GSource *source, */
/*                        gint    *timeout) */
/* { */
/*   ClutterClockSource *clock_source = (ClutterClockSource *) source; */
/*   ClutterMasterClock *master_clock = clock_source->master_clock; */
/*   int delay; */

/*   _clutter_threads_acquire_lock (); */

/*   if (G_UNLIKELY (clutter_paint_debug_flags & */
/*                   CLUTTER_DEBUG_CONTINUOUS_REDRAW)) */
/*     { */
/*       ClutterStageManager *stage_manager = clutter_stage_manager_get_default (); */
/*       const GSList *stages, *l; */

/*       stages = clutter_stage_manager_peek_stages (stage_manager); */

/*       /\* Queue a full redraw on all of the stages *\/ */
/*       for (l = stages; l != NULL; l = l->next) */
/*         clutter_actor_queue_redraw (l->data); */
/*     } */

/*   delay = master_clock_next_frame_delay (master_clock); */

/*   _clutter_threads_release_lock (); */

/*   *timeout = delay; */

/*   return delay == 0; */
/* } */

static void
clutter_master_clock_gdk_update (GdkFrameClock         *frame_clock,
                                 ClutterMasterClockGdk *master_clock)
{
  ClutterStage *stage;

  CLUTTER_STATIC_TIMER (master_dispatch_timer,
                        "Mainloop",
                        "Master Clock",
                        "Master clock dispatch",
                        0);

  CLUTTER_TIMER_START (_clutter_uprof_context, master_dispatch_timer);

  CLUTTER_NOTE (SCHEDULER, "Master clock [tick]");

  _clutter_threads_acquire_lock ();

  stage = g_hash_table_lookup (master_clock->clock_to_stage, frame_clock);

  /* Get the time to use for this frame */
  master_clock->cur_tick = g_get_monotonic_time ();

#ifdef CLUTTER_ENABLE_DEBUG
  master_clock->remaining_budget = master_clock->frame_budget;
#endif

  /* Each frame is split into three separate phases: */

  /* 1. process all the events; goes through the stage's event queue
   *    and processes each event according to its type, then emits the
   *    various signals that are associated with the event
   */
  master_clock_process_stage_events (master_clock, stage);

  /* 2. advance the timelines */
  master_clock_advance_timelines (master_clock);

  /* 3. relayout and redraw the stage; the stage might have been
   *    destroyed in 1. when processing events, check whether it's
   *    still alive. */
  if (g_hash_table_lookup (master_clock->clock_to_stage, frame_clock) != NULL)
    {
      master_clock_update_stage (master_clock, stage);
      master_clock_reschedule_stage_update (master_clock, stage);
    }

  master_clock->prev_tick = master_clock->cur_tick;

  _clutter_threads_release_lock ();

  CLUTTER_TIMER_STOP (_clutter_uprof_context, master_dispatch_timer);
}

static void
clutter_master_clock_gdk_remove_stage_clock (ClutterMasterClockGdk *master_clock,
                                             ClutterStage          *stage)
{
  gpointer frame_clock = g_hash_table_lookup (master_clock->stage_to_clock, stage);
  if (frame_clock == NULL)
      return;

  g_signal_handlers_disconnect_by_func (frame_clock,
                                        clutter_master_clock_gdk_update,
                                        master_clock);

  g_hash_table_remove (master_clock->stage_to_clock, stage);
  g_hash_table_remove (master_clock->clock_to_stage, frame_clock);
}

static void
clutter_master_clock_gdk_add_stage_clock (ClutterMasterClockGdk *master_clock,
                                          ClutterStage          *stage,
                                          GdkFrameClock         *frame_clock)
{
  clutter_master_clock_gdk_remove_stage_clock (master_clock, stage);

  g_hash_table_insert (master_clock->stage_to_clock, stage, g_object_ref (frame_clock));
  g_hash_table_insert (master_clock->clock_to_stage, g_object_ref (frame_clock), stage);

  g_signal_connect (frame_clock, "update",
                    G_CALLBACK (clutter_master_clock_gdk_update),
                    master_clock);
}

static void
clutter_master_clock_gdk_listen_to_stage (ClutterMasterClockGdk *master_clock,
                                          ClutterStage          *stage)
{
  ClutterStageWindow *stage_window;
  ClutterStageGdk *stage_window_gdk;
  GdkFrameClock *frame_clock;

  stage_window = _clutter_stage_get_window (stage);
  if (stage_window == NULL)
    {
      clutter_master_clock_gdk_remove_stage_clock (master_clock, stage);
      return;
    }

  stage_window_gdk = CLUTTER_STAGE_GDK (stage_window);
  if (stage_window_gdk->window == NULL)
    {
      clutter_master_clock_gdk_remove_stage_clock (master_clock, stage);
      return;
    }

  frame_clock = gdk_window_get_frame_clock (stage_window_gdk->window);
  if (frame_clock == NULL)
    {
      clutter_master_clock_gdk_remove_stage_clock (master_clock, stage);
      return;
    }

  clutter_master_clock_gdk_add_stage_clock (master_clock, stage, frame_clock);
}

static void
clutter_master_clock_gdk_stage_realized (ClutterStage          *stage,
                                         GParamSpec            *spec,
                                         ClutterMasterClockGdk *master_clock)
{
  if (CLUTTER_ACTOR_IS_REALIZED (stage))
    clutter_master_clock_gdk_listen_to_stage (master_clock, stage);
  else
    clutter_master_clock_gdk_remove_stage_clock (master_clock, stage);
}

static void
clutter_master_clock_gdk_stage_added (ClutterStageManager   *manager,
                                      ClutterStage          *stage,
                                      ClutterMasterClockGdk *master_clock)
{
  g_signal_connect (stage, "notify::realized",
                    G_CALLBACK (clutter_master_clock_gdk_stage_realized),
                    master_clock);

  clutter_master_clock_gdk_listen_to_stage (master_clock, stage);
}

static void
clutter_master_clock_gdk_stage_removed (ClutterStageManager   *manager,
                                        ClutterStage          *stage,
                                        ClutterMasterClockGdk *master_clock)
{
  clutter_master_clock_gdk_remove_stage_clock (master_clock, stage);

  g_signal_handlers_disconnect_by_func (stage,
                                        clutter_master_clock_gdk_stage_realized,
                                        master_clock);
}

static void
clutter_master_clock_gdk_dispose (GObject *gobject)
{
  ClutterStageManager *manager = clutter_stage_manager_get_default ();

  g_signal_handlers_disconnect_by_func (manager,
                                        clutter_master_clock_gdk_stage_added,
                                        gobject);
  g_signal_handlers_disconnect_by_func (manager,
                                        clutter_master_clock_gdk_stage_removed,
                                        gobject);

  G_OBJECT_CLASS (clutter_master_clock_gdk_parent_class)->dispose (gobject);
}

static void
clutter_master_clock_gdk_finalize (GObject *gobject)
{
  ClutterMasterClockGdk *master_clock = CLUTTER_MASTER_CLOCK_GDK (gobject);

  g_hash_table_unref (master_clock->clock_to_stage);
  g_hash_table_unref (master_clock->stage_to_clock);
  g_slist_free (master_clock->timelines);

  G_OBJECT_CLASS (clutter_master_clock_gdk_parent_class)->finalize (gobject);
}

static void
clutter_master_clock_gdk_class_init (ClutterMasterClockGdkClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->dispose = clutter_master_clock_gdk_dispose;
  gobject_class->finalize = clutter_master_clock_gdk_finalize;
}

static void
clutter_master_clock_gdk_init (ClutterMasterClockGdk *self)
{
  ClutterStageManager *manager;
  const GSList *stages, *l;

  self->clock_to_stage = g_hash_table_new_full (g_direct_hash, g_direct_equal,
                                                g_object_unref, NULL);
  self->stage_to_clock = g_hash_table_new_full (g_direct_hash, g_direct_equal,
                                                NULL, g_object_unref);

  manager = clutter_stage_manager_get_default ();
  g_signal_connect (manager, "stage-added",
                    G_CALLBACK (clutter_master_clock_gdk_stage_added), self);
  g_signal_connect (manager, "stage-removed",
                    G_CALLBACK (clutter_master_clock_gdk_stage_removed), self);

  stages = clutter_stage_manager_peek_stages (manager);
  for (l = stages; l; l = l->next)
    clutter_master_clock_gdk_stage_added (manager, l->data, self);
}

static void
clutter_master_clock_gdk_add_timeline (ClutterMasterClock *clock,
                                       ClutterTimeline    *timeline)
{
  ClutterMasterClockGdk *master_clock = (ClutterMasterClockGdk *) clock;
  gboolean is_first;

  if (g_slist_find (master_clock->timelines, timeline))
    return;

  is_first = master_clock->timelines == NULL;

  master_clock->timelines = g_slist_prepend (master_clock->timelines,
                                             timeline);

  if (is_first)
    {
      master_clock_schedule_stages_updates (master_clock);
      _clutter_master_clock_start_running (clock);
    }
}

static void
clutter_master_clock_gdk_remove_timeline (ClutterMasterClock *clock,
                                          ClutterTimeline    *timeline)
{
  ClutterMasterClockGdk *master_clock = (ClutterMasterClockGdk *) clock;

  master_clock->timelines = g_slist_remove (master_clock->timelines,
                                            timeline);
}

static void
clutter_master_clock_gdk_start_running (ClutterMasterClock *clock)
{
  master_clock_schedule_stages_updates ((ClutterMasterClockGdk *) clock);
}

static void
clutter_master_clock_gdk_ensure_next_iteration (ClutterMasterClock *clock)
{
  master_clock_schedule_stages_updates ((ClutterMasterClockGdk *) clock);
}

static void
clutter_master_clock_gdk_set_paused (ClutterMasterClock *clock,
                                     gboolean            paused)
{
  /* GdkFrameClock runs the show here. We do not decide whether the
     clock is paused or not. */
}

static void
clutter_master_clock_iface_init (ClutterMasterClockIface *iface)
{
  iface->add_timeline = clutter_master_clock_gdk_add_timeline;
  iface->remove_timeline = clutter_master_clock_gdk_remove_timeline;
  iface->start_running = clutter_master_clock_gdk_start_running;
  iface->ensure_next_iteration = clutter_master_clock_gdk_ensure_next_iteration;
  iface->set_paused = clutter_master_clock_gdk_set_paused;
}

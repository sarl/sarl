/**
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2016 the original authors or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.core;

import io.sarl.core.AgentTask;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Capacity;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

/**
 * Schedules actions in time.
 */
@SuppressWarnings("all")
public interface Schedules extends Capacity {
  /**
   * Creates an anonymous task to execute the procedure that will be triggered after the specified delay.
   * @param delay time in milliseconds to delay the procedure execution
   * @param procedure the closure to execute.
   * @return the generated task
   */
  public abstract AgentTask in(final long delay, final Procedure1<? super Agent> procedure);
  
  /**
   * Schedule a given task to be executed after the specified delay.
   */
  public abstract AgentTask in(final AgentTask task, final long delay, final Procedure1<? super Agent> procedure);
  
  /**
   * Create a named task that can be retrieved and schedule later.
   */
  public abstract AgentTask task(final String name);
  
  /**
   * Attempts to cancel execution of this task.  This attempt will
   * fail if the task has already completed, has already been cancelled,
   * or could not be cancelled for some other reason. If successful,
   * and this task has not started when <tt>cancel</tt> is called,
   * this task should never run.  If the task has already started,
   * then the <tt>mayInterruptIfRunning</tt> parameter determines
   * whether the thread executing this task should be interrupted in
   * an attempt to stop the task.
   * <p>
   * This function interrupts ongoing tasks. So, it is
   * equivalent to passing <code>true</code> as the
   * value for the parameter <tt>mayInterruptIfRunning</tt>
   * to the function {@link #cancel(AgentTask, boolean)}.
   * 
   * @return <tt>false</tt> if the task could not be cancelled,
   * typically because it has already completed normally;
   * <tt>true</tt> otherwise
   */
  public abstract boolean cancel(final AgentTask task);
  
  /**
   * Attempts to cancel execution of this task.  This attempt will
   * fail if the task has already completed, has already been cancelled,
   * or could not be cancelled for some other reason. If successful,
   * and this task has not started when <tt>cancel</tt> is called,
   * this task should never run.  If the task has already started,
   * then the <tt>mayInterruptIfRunning</tt> parameter determines
   * whether the thread executing this task should be interrupted in
   * an attempt to stop the task.
   * 
   * @param mayInterruptIfRunning <tt>true</tt> if the thread executing this
   * task should be interrupted; otherwise, in-progress tasks are allowed
   * to complete
   * @return <tt>false</tt> if the task could not be cancelled,
   * typically because it has already completed normally;
   * <tt>true</tt> otherwise
   */
  public abstract boolean cancel(final AgentTask task, final boolean mayInterruptIfRunning);
  
  /**
   * Create an anonymous task and schedules a periodic execution of its behavior.
   * <p>
   * If the duration of the task is greater to the given period length, then
   * multiple task's instances will be run in parallel.
   * For example, consider the following code:
   * <pre><code>
   * every(500) [ sleep(2000) ]
   * </code></pre>
   * At a given time, 4 instances (A, B, C, D) of the task may be run in parallel:
   * <pre><code>
   * t=0   0500   1000   1500   2000   2500   3000   3500   4000   4500
   *   |    |      |      |      |      |      |      |      |      |
   *   [-A-----------------------]
   *        [-B-------------------------]
   *               [-C-------------------------]
   *                      [-D-------------------------]
   *                             [-E-------------------------]
   *                                    [-F-------------------------]
   * </code></pre>
   */
  public abstract AgentTask every(final long period, final Procedure1<? super Agent> procedure);
  
  /**
   * Schedule a periodic execution of the given task.
   * <p>
   * If the duration of the task is greater to the given period length, then
   * multiple task's instances will be run in parallel.
   * For example, consider the following code:
   * <pre><code>
   * every(500) [ sleep(2000) ]
   * </code></pre>
   * At a given time, 4 instances (A, B, C, D) of the task may be run in parallel:
   * <pre><code>
   * t=0   0500   1000   1500   2000   2500   3000   3500   4000   4500
   *   |    |      |      |      |      |      |      |      |      |
   *   [-A-----------------------]
   *        [-B-------------------------]
   *               [-C-------------------------]
   *                      [-D-------------------------]
   *                             [-E-------------------------]
   *                                    [-F-------------------------]
   * </code></pre>
   */
  public abstract AgentTask every(final AgentTask task, final long period, final Procedure1<? super Agent> procedure);
}

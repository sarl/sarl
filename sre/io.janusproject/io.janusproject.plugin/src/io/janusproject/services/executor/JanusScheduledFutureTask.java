/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.services.executor;

import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableScheduledFuture;
import java.util.concurrent.ScheduledFuture;

/**
 * A {@link ScheduledFuture} that is {@link Runnable}. Successful execution of the <tt>run</tt> method causes completion of the
 * <tt>Future</tt> and allows access to its results.
 *
 * @param <V> - type of the values supported by the task.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see FutureTask
 */
public interface JanusScheduledFutureTask<V> extends RunnableScheduledFuture<V> {

	/**
	 * Replies the thread that is running the task associated to this future.
	 *
	 * @return the thread, never <code>null</code>.
	 */
	Thread getThread();

	/**
	 * Replies the task associated to this future is running on the calling thread.
	 *
	 * @return <code>true</code> if the current thread is running the associated task, <code>false</code> otherwie.
	 */
	boolean isCurrentThread();

}

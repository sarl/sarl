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

package io.janusproject.modules.executors;

import java.lang.Thread.UncaughtExceptionHandler;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ThreadFactory;

import com.google.inject.AbstractModule;
import com.google.inject.Singleton;
import io.janusproject.kernel.services.jdk.executors.JdkExecutorService;
import io.janusproject.kernel.services.jdk.executors.JdkScheduledThreadPoolExecutor;
import io.janusproject.kernel.services.jdk.executors.JdkThreadFactory;
import io.janusproject.kernel.services.jdk.executors.JdkThreadPoolExecutor;
import io.janusproject.kernel.services.jdk.executors.JdkUncaughtExceptionHandler;
import io.janusproject.services.executor.ExecutorService;

/**
 * Configure the module for the {@code ExecutorService} based on the JDF.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JdkExecutorModule extends AbstractModule {

	@Override
	protected void configure() {
		// Thread catchers
		bind(UncaughtExceptionHandler.class).to(JdkUncaughtExceptionHandler.class).in(Singleton.class);

		// Bind the background objects
		bind(ThreadFactory.class).to(JdkThreadFactory.class).in(Singleton.class);
		bind(java.util.concurrent.ExecutorService.class).to(JdkThreadPoolExecutor.class).in(Singleton.class);
		bind(ScheduledExecutorService.class).to(JdkScheduledThreadPoolExecutor.class).in(Singleton.class);

		// Bind the service
		bind(ExecutorService.class).to(JdkExecutorService.class).in(Singleton.class);
	}

}

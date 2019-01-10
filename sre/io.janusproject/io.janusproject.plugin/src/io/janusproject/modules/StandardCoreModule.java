/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.janusproject.modules;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.logging.Logger;

import com.google.common.util.concurrent.Service;
import com.google.inject.AbstractModule;
import com.google.inject.Inject;
import com.google.inject.MembersInjector;
import com.google.inject.Singleton;
import com.google.inject.TypeLiteral;
import com.google.inject.matcher.Matchers;
import com.google.inject.multibindings.Multibinder;
import com.google.inject.spi.TypeEncounter;
import com.google.inject.spi.TypeListener;

import io.janusproject.kernel.services.jdk.contextspace.StandardContextSpaceService;
import io.janusproject.kernel.services.jdk.logging.StandardLogService;
import io.janusproject.kernel.services.jdk.spawn.StandardSpawnService;
import io.janusproject.modules.executors.JdkExecutorModule;
import io.janusproject.modules.kernel.MandatoryKernelModule;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.NetworkService;
import io.janusproject.services.spawn.SpawnService;
import io.janusproject.util.LoggerCreator;

/**
 * The Core Janus Module configures the minimum requirements for Janus to run properly. The network-based modules are skipped in
 * this StandardCoreModule. See {@link StandardJanusPlatformModule} for the configuration of the network-based modules
 *
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class StandardCoreModule extends AbstractModule {

	@Override
	protected void configure() {
		// Custom logger
		LoggerCreator.useJanusMessageFormat();
		bindListener(Matchers.any(), new LoggerMemberListener());

		bind(LogService.class).to(StandardLogService.class).in(Singleton.class);

		bind(ContextSpaceService.class).to(StandardContextSpaceService.class).in(Singleton.class);
		bind(SpawnService.class).to(StandardSpawnService.class).in(Singleton.class);

		install(new JdkExecutorModule());

		// Install the elements for the Janus kernel
		install(new MandatoryKernelModule());

		// Check if all the services are binded
		requireBinding(DistributedDataStructureService.class);
		requireBinding(KernelDiscoveryService.class);
		requireBinding(ExecutorService.class);
		requireBinding(ContextSpaceService.class);
		requireBinding(Logger.class);
		requireBinding(LogService.class);
		requireBinding(NetworkService.class);
		requireBinding(SpawnService.class);

		// Create a binder for: Set<Service>
		// (This set is given to the service manager to launch the services).
		final Multibinder<Service> serviceSetBinder = Multibinder.newSetBinder(binder(), Service.class);
		serviceSetBinder.addBinding().to(LogService.class);
		serviceSetBinder.addBinding().to(ExecutorService.class);
		serviceSetBinder.addBinding().to(ContextSpaceService.class);
		serviceSetBinder.addBinding().to(KernelDiscoveryService.class);
		serviceSetBinder.addBinding().to(SpawnService.class);
		serviceSetBinder.addBinding().to(DistributedDataStructureService.class);
	}

	/**
	 * Provider of logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static final class LoggerMemberListener implements TypeListener {

		private final Logger rootLogger;

		/**
		 * Construct.
		 */
		LoggerMemberListener() {
			this.rootLogger = LoggerCreator.createPlatformLogger();
		}

		@Override
		public <I> void hear(TypeLiteral<I> type, TypeEncounter<I> encounter) {
			for (final Field field : type.getRawType().getDeclaredFields()) {
				if (field.getType() == Logger.class
					&& (field.isAnnotationPresent(Inject.class) || field.isAnnotationPresent(javax.inject.Inject.class))) {
					encounter.register(new LoggerMemberInjector<I>(this.rootLogger, field));
				}
			}
			for (final Method method : type.getRawType().getDeclaredMethods()) {
				if (Modifier.isPublic(method.getModifiers())
					&& method.getParameterCount() == 1
					&& method.getParameterTypes()[0].equals(Logger.class)
					&& (method.isAnnotationPresent(Inject.class) || method.isAnnotationPresent(javax.inject.Inject.class))) {
					encounter.register(new LoggerMemberInjector<I>(this.rootLogger, method));
				}
			}
		}

	}

	/**
	 * Provider of logger.
	 *
	 * @param <T>
	 *            the type of the type of the field.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static final class LoggerMemberInjector<T> implements MembersInjector<T> {

		private final Logger rootLogger;

		private final Field field;

		private final Method method;

		/**
		 * Construct.
		 *
		 * @param rootLogger the root logger.
		 * @param field
		 *            the field to inject.
		 */
		LoggerMemberInjector(Logger rootLogger, Field field) {
			this.rootLogger = rootLogger;
			this.field = field;
			this.method = null;
		}

		/**
		 * Construct.
		 *
		 * @param rootLogger the root logger.
		 * @param method
		 *            the method to inject.
		 */
		LoggerMemberInjector(Logger rootLogger, Method method) {
			this.rootLogger = rootLogger;
			this.field = null;
			this.method = method;
		}

		@Override
		public void injectMembers(T instance) {
			if (this.field != null) {
				final boolean accessible = this.field.isAccessible();
				try {
					this.field.setAccessible(true);
					this.field.set(instance, this.rootLogger);
				} catch (IllegalArgumentException | IllegalAccessException e) {
					throw new RuntimeException(e);
				} finally {
					this.field.setAccessible(accessible);
				}
			}
			if (this.method != null) {
				final boolean accessible = this.method.isAccessible();
				try {
					this.method.setAccessible(true);
					this.method.invoke(instance, this.rootLogger);
				} catch (IllegalArgumentException | IllegalAccessException | InvocationTargetException e) {
					throw new RuntimeException(e);
				} finally {
					this.method.setAccessible(accessible);
				}
			}
		}

	}

}

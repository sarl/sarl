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

package io.janusproject.modules.kernel;

import java.util.Set;
import java.util.UUID;
import java.util.logging.Logger;

import com.google.common.util.concurrent.Service;
import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import com.google.inject.name.Named;
import com.google.inject.name.Names;

import io.janusproject.JanusConfig;
import io.janusproject.kernel.bic.StandardBuiltinCapacitiesProvider;
import io.janusproject.kernel.bic.internaleventdispatching.AgentInternalEventsDispatcher;
import io.janusproject.kernel.space.EventSpaceSpecificationImpl;
import io.janusproject.kernel.space.OpenEventSpaceSpecificationImpl;
import io.janusproject.kernel.space.RestrictedAccessEventSpaceSpecificationImpl;
import io.janusproject.services.GoogleServiceManager;
import io.janusproject.services.IServiceManager;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.executor.ExecutorService;

import io.sarl.core.OpenEventSpaceSpecification;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.sarlspecification.SarlSpecificationChecker;
import io.sarl.sarlspecification.StandardSarlSpecificationChecker;
import io.sarl.util.RestrictedAccessEventSpaceSpecification;

/**
 * Configure the mandatory elements of the Janus kernel.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class MandatoryKernelModule extends AbstractModule {

	@Override
	protected void configure() {
		requireBinding(Key.get(UUID.class, Names.named(JanusConfig.DEFAULT_CONTEXT_ID_NAME)));
		requireBinding(Key.get(UUID.class, Names.named(JanusConfig.DEFAULT_SPACE_ID_NAME)));
		requireBinding(Logger.class);

		// Sarl specification checker
		bind(SarlSpecificationChecker.class).to(StandardSarlSpecificationChecker.class).in(Singleton.class);

		// Built-in Capacities
		bind(BuiltinCapacitiesProvider.class).to(StandardBuiltinCapacitiesProvider.class).in(Singleton.class);

		// Space specifications
		bind(EventSpaceSpecification.class).to(EventSpaceSpecificationImpl.class).in(Singleton.class);
		bind(OpenEventSpaceSpecification.class).to(OpenEventSpaceSpecificationImpl.class).in(Singleton.class);
		bind(RestrictedAccessEventSpaceSpecification.class).to(RestrictedAccessEventSpaceSpecificationImpl.class)
				.in(Singleton.class);
	}

	/** Construct the root agent context within the Janus platform.
	 *
	 * @param contextService the service for managing the contexts.
	 * @param janusContextID the root context's id.
	 * @param defaultJanusSpaceId the id of the space within the root context.
	 * @return the root context.
	 */
	@Provides
	@io.janusproject.kernel.annotations.Kernel
	@Singleton
	public static AgentContext getKernel(ContextSpaceService contextService,
			@Named(JanusConfig.DEFAULT_CONTEXT_ID_NAME) UUID janusContextID,
			@Named(JanusConfig.DEFAULT_SPACE_ID_NAME) UUID defaultJanusSpaceId) {
		return contextService.createContext(janusContextID, defaultJanusSpaceId);
	}

	/** Create an instance of the event dispatcher for an agent.
	 *
	 * @param injector the injector.
	 * @return the event dispatcher.
	 */
	@Provides
	public static AgentInternalEventsDispatcher createAgentInternalEventsDispatcher(Injector injector) {
		final AgentInternalEventsDispatcher aeb = new AgentInternalEventsDispatcher(injector.getInstance(ExecutorService.class));
		// to be able to inject the ExecutorService and SubscriberFindingStrategy
		injector.injectMembers(aeb);
		return aeb;
	}

	/** Create the service manager for the Janus platform.
	 *
	 * @param services the services to be managed.
	 * @return the service manager.
	 */
	@Provides
	@Singleton
	public static IServiceManager createServiceManager(Set<Service> services) {
		return new GoogleServiceManager(services);
	}

}

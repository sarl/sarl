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
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.util.OpenEventSpaceSpecification;
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

		// Built-in Capacities
		bind(BuiltinCapacitiesProvider.class).to(StandardBuiltinCapacitiesProvider.class).in(Singleton.class);

		// Space specifications
		bind(EventSpaceSpecification.class).to(EventSpaceSpecificationImpl.class).in(Singleton.class);
		bind(OpenEventSpaceSpecification.class).to(OpenEventSpaceSpecificationImpl.class).in(Singleton.class);
		bind(RestrictedAccessEventSpaceSpecification.class).to(RestrictedAccessEventSpaceSpecificationImpl.class)
				.in(Singleton.class);
	}

	@Provides
	@io.janusproject.kernel.annotations.Kernel
	@Singleton
	private static AgentContext getKernel(ContextSpaceService contextService,
			@Named(JanusConfig.DEFAULT_CONTEXT_ID_NAME) UUID janusContextID,
			@Named(JanusConfig.DEFAULT_SPACE_ID_NAME) UUID defaultJanusSpaceId) {
		return contextService.createContext(janusContextID, defaultJanusSpaceId);
	}

	@Provides
	private static AgentInternalEventsDispatcher createAgentInternalEventsDispatcher(Injector injector,
			java.util.concurrent.ExecutorService service) {
		AgentInternalEventsDispatcher aeb = new AgentInternalEventsDispatcher(service, PerceptGuardEvaluator.class);
		// to be able to inject the SubscriberFindingStrategy
		injector.injectMembers(aeb);
		return aeb;
	}

	@Provides
	@Singleton
	private static IServiceManager createServiceManager(Set<Service> services) {
		return new GoogleServiceManager(services);
	}

}

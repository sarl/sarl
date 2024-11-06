/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package org.arakhne.afc.services;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.arakhne.afc.vmutil.locale.Locale;

import com.google.common.collect.LinkedListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;

/** 
 * Abstract implementation of a service manager for the SRE platform.
 * 
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version afc.extension 0.14.0 20241106-161406
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid afc.extension
 */
public abstract class AbstractServiceManager implements IServiceManager {

	private Logger logger;

	private final Multimap<ServiceState, IService> servicesByState = LinkedListMultimap.create();

	private final Map<Class<? extends IService>, IService> servicesByType = new HashMap<>();

	/** Constructor.
	 *
	 * @param services the services to be managed by this manager.
	 */
	public AbstractServiceManager(Iterable<? extends IService> services) {
		for (final var service : services) {
			this.servicesByState.put(ServiceState.NEW, service);
			this.servicesByType.put(service.getReferenceType(), service);
		}
	}

	/** Replies the logger.
	 *
	 * @return the logger.
	 */
	protected Logger getLogger() {
		return this.logger;
	}

	/** Change the logger.
	 * 
	 * @param logger the new logger.
	 */
	protected void setLogger(Logger logger) {
		this.logger = logger;
	}

	@Override
	public Multimap<ServiceState, IService> getServicesByState() {
		return Multimaps.unmodifiableMultimap(this.servicesByState);
	}
	
	/** Replies the list of services with the given state.
	 *
	 * @param state the expected state of the services to reply.
	 * @return the list of services.
	 */
	protected Collection<IService> getServicesByState(ServiceState state) {
		var list = this.servicesByState.get(state);
		if (list == null) {
			return Collections.emptyList();
		}
		return Collections.unmodifiableCollection(list);
	}

	@Override
	public <T extends IService> T getService(Class<T> type) {
		return type.cast(this.servicesByType.get(type));
	}

	/** Start the given service.
	 *
	 * @param service the service to start.
	 */
	protected void startService(IService service) {
		if (service.getState() == ServiceState.NEW) {
			this.servicesByState.remove(ServiceState.NEW, service);
			service.setState(ServiceState.STARTING);
			this.servicesByState.put(ServiceState.STARTING, service);
			service.onStart();
			this.servicesByState.remove(ServiceState.STARTING, service);
			service.setState(ServiceState.RUNNING);
			this.servicesByState.put(ServiceState.RUNNING, service);
		}
	}

	/** Stop the given service.
	 *
	 * @param service the service to stop.
	 */
	protected void stopService(IService service) {
		if (service.getState() == ServiceState.RUNNING) {
			this.servicesByState.remove(ServiceState.RUNNING, service);
			service.setState(ServiceState.STOPPING);
			this.servicesByState.put(ServiceState.STOPPING, service);
			service.onStop();
			this.servicesByState.remove(ServiceState.STOPPING, service);
			service.setState(ServiceState.STOPPED);
			this.servicesByState.put(ServiceState.STOPPED, service);
		}
	}

	@Override
	public void startServices(Logger logger) {
		setLogger(logger);
		internalStartAllServices();
	}

	@Override
	public void stopServices(Logger logger) {
		preStop(logger);
		logger.fine(Locale.getString("AbstractServiceManager_0")); //$NON-NLS-1$
		internalStopAllServices();
	}

	/** Pre-stop all the services.
	 *
	 * @param logger the logger to use.
	 */
	protected void preStop(Logger logger) {
		logger.fine(Locale.getString("AbstractServiceManager_1")); //$NON-NLS-1$
		for (final var service : getServicesByState(ServiceState.RUNNING)) {
			if (service instanceof IPreReleasableService iservice) {
				iservice.onPreStop();
			}
		}
	}

	/** Start all the services whatever the dependencies between the services.
	 */
	protected abstract void internalStartAllServices();

	/** Stop all the services whatever the dependencies between the services.
	 */
	protected abstract void internalStopAllServices();

}

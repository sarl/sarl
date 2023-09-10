/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

import java.util.logging.Logger;

import com.google.common.collect.Multimap;

/**
 * Manager of services for the SRE platform.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface IServiceManager {

	/** 
	 * Replies the services by state.
	 * 
	 * @return the services.
	 */
	Multimap<ServiceState, IService> getServicesByState();

	/**
	 * Replies the service of the given type.
	 * 
	 * @param type the type of the service to search for.
	 * @return the service
	 */
	<T extends IService> T getService(Class<T> type);

	/** 
	 * Start the services associated to the service manager.
	 * 
	 * <p>This starting function supports the prioritized services.
	 * 
	 * @param logger the logger to use for any information message.
	 */
	void startServices(Logger logger);

	/** 
	 * Stop the services associated to the service manager.
	 * 
	 * <p>This stopping function supports the prioritized services.
	 *
	 * @param logger the logger to use for any information message.
	 */
	void stopServices(Logger logger);

}

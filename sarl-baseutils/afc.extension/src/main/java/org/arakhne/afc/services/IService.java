/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

/** 
 * Service in a SRE implementation. 
 * 
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface IService {

	/** Replies the state of the service.
	 *
	 * @return the state of the service.
	 */
	ServiceState getState();

	/** Change the state of the service.
	 *
	 * @param state the new state of the service.
	 */
	void setState(ServiceState state);

	/** Replies the type that must be used as implemented reference for this service.
	 *
	 * @return the implemented service type.
	 */
	Class<? extends IService> getReferenceType();

	/** Invoked for starting the service.
	 */
	void onStart();

	/** Invoked for stopping the service.
	 */
	void onStop();

}

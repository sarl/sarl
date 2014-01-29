/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.core;

import java.util.Collection;
import java.util.UUID;

/**
 * An AgentContext defines the boundary of a sub-system, and gathers a collection of Spaces.
 * Each context has a default context that provides a basic interaction context
 * 
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface AgentContext {
	
	public UUID getID();

	public EventSpace getDefaultSpace();

	public Collection<Space> getSpaces();

	public <S extends Space> S createSpace(Class<? extends SpaceSpecification> spec, UUID spaceUUID,
			Object... creationParams);

	public <S extends Space> Collection<S> getSpaces(Class<? extends SpaceSpecification> spec);

	public <S extends Space> S getOrCreateSpace(Class<? extends SpaceSpecification> spec, UUID spaceUUID,
			Object... creationParams);

}

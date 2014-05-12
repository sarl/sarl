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
	
	/** Replies the identifier of the context.
	 * 
	 * @return the identifier of the context.
	 */
	public UUID getID();

	/** Replies the default space of the context.
	 * The default space is assumed to be always an {@link EventSpace event-based space}.
	 * 
	 * @return the default space.
	 */
	public EventSpace getDefaultSpace();

	/** Replies all the spaces defined in this context.
	 * 
	 * @return the spaces.
	 */
	public Collection<? extends Space> getSpaces();

	/** Create an instance of space following the given specification.
	 * This function always creates a new instance of space.
	 * If you want to find an existing space prior to the creation of
	 * a new one, you should use {@link #getOrCreateSpace(Class, UUID, Object...)}.
	 * 
	 * @param spec - specification of the space to create.
	 * @param spaceUUID - identifier of the new space.
	 * @param creationParams - parameters to pass to the space constructor.
	 * @return the new space.
	 * @see #getOrCreateSpace(Class, UUID, Object...)
	 * @see #getSpace(UUID)
	 */
	public <S extends Space> S createSpace(Class<? extends SpaceSpecification<S>> spec, UUID spaceUUID,
			Object... creationParams);

	/** Replies all the spaces that are implementing the given specification.
	 * 
	 * @param spec
	 * @return the spaces associated to the given space specification.
	 */
	public <S extends Space> Collection<S> getSpaces(Class<? extends SpaceSpecification<S>> spec);

	/** Retreive or create an instance of space following the given specification.
	 * This function tries to find a space that fits the given specification.
	 * If none wis found, this function create a new space in the same way as
	 * {@link #createSpace(Class, UUID, Object...)}.
	 * 
	 * @param spec - specification of the space to create.
	 * @param spaceUUID - identifier of the new space.
	 * @param creationParams - parameters to pass to the space constructor.
	 * @return the new space.
	 * @see #createSpace(Class, UUID, Object...)
	 * @see #getSpace(UUID)
	 */
	public <S extends Space> S getOrCreateSpace(Class<? extends SpaceSpecification<S>> spec, UUID spaceUUID,
			Object... creationParams);

	/** Retreive, but do not create, an instance of space following the given ID.
	 * This function tries to find a space that fits the given specification.
	 * If none was found, this function replies <code>null</code>.
	 * 
	 * @param spaceUUID - identifier of the space.
	 * @return the space, or <code>null</code> if there is no space found.
	 * @see #createSpace(Class, UUID, Object...)
	 * @see #getOrCreateSpace(Class, UUID, Object...)
	 */
	public <S extends Space> S getSpace(UUID spaceUUID);

}

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

package io.janusproject.services.contextspace;

import java.util.Collection;
import java.util.UUID;
import java.util.concurrent.locks.ReadWriteLock;

import io.janusproject.services.DependentService;

import io.sarl.lang.core.AgentContext;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.lang.util.SynchronizedSet;

/**
 * This service enables to store the contexts and to manage the spaces in the janus platform.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ContextSpaceService extends DependentService {

	/**
	 * Replies the lock that is used to synchronized the access to the service.
	 *
	 * @return the lock.
	 */
	ReadWriteLock getLock();

	/**
	 * Does this repository contain some context.
	 *
	 * @return <code>true</code> if this repository contains no context, <code>false</code> otherwise
	 */
	boolean isEmptyContextRepository();

	/**
	 * Returns the number of context registered in this repository.
	 *
	 * @return the number of context registered in this repository
	 */
	int getNumberOfContexts();

	/**
	 * Check if this repository contains a context with the specified id.
	 *
	 * @param contextID the id to test
	 * @return <code>true</code> if this repository contains a context with the specified id, <code>false</code> otherwise
	 */
	boolean containsContext(UUID contextID);

	/**
	 * Create a new context and add it to this repository.
	 *
	 * @param contextID the id of the context to create
	 * @param defaultSpaceID the id of the default space of the context to create
	 * @return the context.
	 */
	AgentContext createContext(UUID contextID, UUID defaultSpaceID);

	/**
	 * Remove the specified context from this repository.
	 *
	 * @param context the context to remove
	 */
	void removeContext(AgentContext context);

	/**
	 * Remove the context with the specified id from this repository.
	 *
	 * @param contextID the id of the context to remove
	 */
	void removeContext(UUID contextID);

	/**
	 * Returns the collection of all agent's contexts stored in this repository.
	 *
	 * @return the collection of all agent's contexts stored in this repository
	 */
	SynchronizedCollection<AgentContext> getContexts();

	/**
	 * Returns the collection of {@link AgentContext} with the given IDs.
	 *
	 * <p>The replies collection is synchronized and any iteration on it must be synchronized on the mutex replies by {@link #getLock()}.
	 *
	 * @param contextIDs the identifiers of the contexts to retreive.
	 * @return the collection of {@link AgentContext} with the given IDs
	 */
	SynchronizedCollection<AgentContext> getContexts(Collection<UUID> contextIDs);

	/**
	 * Returns the set of all agent context IDs stored in this repository.
	 *
	 * <p>The replies collection is synchronized and any iteration on it must be synchronized on the mutex replies by {@link #getLock()}.
	 *
	 * @return the set of all agent context IDs stored in this repository
	 */
	SynchronizedSet<UUID> getContextIDs();

	/**
	 * Returns the {@link AgentContext} with the given ID.
	 *
	 * @param contextID the identifier of the context to retreive.
	 * @return the {@link AgentContext} with the given ID
	 */
	AgentContext getContext(UUID contextID);

	/**
	 * Returns the {@link AgentContext} that is the root of all the contexts.
	 *
	 * @return the root {@link AgentContext}.
	 */
	AgentContext getUniverseContext();

	/**
	 * Add a listener on the context repository events.
	 *
	 * @param listener the listener on the context repository events.
	 */
	void addContextRepositoryListener(ContextRepositoryListener listener);

	/**
	 * Remove a listener on the context repository events.
	 *
	 * @param listener the listener on the context repository events.
	 */
	void removeContextRepositoryListener(ContextRepositoryListener listener);

	/**
	 * Add a listener on the space repository events.
	 *
	 * @param listener the listener on the space repository events.
	 */
	void addSpaceRepositoryListener(SpaceRepositoryListener listener);

	/**
	 * Remove a listener on the space repository events.
	 *
	 * @param listener the listener on the space repository events.
	 */
	void removeSpaceRepositoryListener(SpaceRepositoryListener listener);

}

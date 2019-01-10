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

package io.sarl.bootstrap;

import java.util.UUID;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;

/**
 * Represents an access point to the SARL run-time environment (SRE).
 * This access point may be used for accessing the underlying SRE independently of its implementation.
 *
 * <p>Depending on the implementation of the SRE, an instance of this SRE access point could be injected.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public interface SREBootstrap {

	/**
	 * Start the SRE without an agent.
	 This function prepare the default context.
	 *
	 * @return the context that is created by the bootstrap. If {@code null} there is no context created.
	 * @since 0.7
	 */
	AgentContext startWithoutAgent();

	/**
	 * Launch the SRE and the first agent in the kernel.
	 *
	 * <p>The function {@link #getBootAgentIdentifier()} permits to retrieve the identifier of the launched agent.
	 *
	 * @param agentCls type of the first agent to launch.
	 * @param params parameters to pass to the agent as its initialization parameters.
	 * @return the identifier of the created agent.
	 * @throws Exception - if it is impossible to start the platform.
	 * @see #getBootAgentIdentifier()
	 */
	UUID startAgent(Class<? extends Agent> agentCls, Object... params) throws Exception;

	/**
	 * Launch the SRE and the first agent in the kernel.
	 *
	 * <p>The function {@link #getBootAgentIdentifier()} permits to retrieve the identifier of the launched agent.
	 *
	 * @param nbAgents the number of agents to be launched.
	 * @param agentCls type of the first agent to launch.
	 * @param params parameters to pass to the agent as its initialization parameters.
	 * @return the identifiers of the created agents.
	 * @throws Exception - if it is impossible to start the platform.
	 * @see #getBootAgentIdentifier()
	 */
	Iterable<UUID> startAgent(int nbAgents, Class<? extends Agent> agentCls, Object... params) throws Exception;

	/**
	 * Replies the identifier of the boot agent from the system's properties. The boot agent is launched with
	 * {@link #startAgent(int, Class, Object...)}.
	 *
	 * @return the identifier of the boot agent, or <code>null</code> if it is unknown.
	 * @see #startAgent(int, Class, Object...)
	 */
	UUID getBootAgentIdentifier();

	/** Replies if the bootstrap could be used.
	 *
	 * <p>If the bootstrap cannot be used, it cannot launch agent.
	 *
	 * @return {@code true} if the bootstrap could be used. {@code false} if it cannot be used.
	 */
	default boolean isActive() {
		return true;
	}

	/**
	 * Set offline flag of the agent platform before it is launched.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @param isOffline the offline flag.
	 * @since 0.7
	 */
	default void setOffline(boolean isOffline) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Force the SRE platform to use a random identifier for its default context.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @since 0.7
	 */
	default void setRandomContextUUID() {
		throw new UnsupportedOperationException();
	}

	/**
	 * Force the SRE platform to use a default context identifier that tis build upon the classname of the boot agent. It means
	 * that the UUID is always the same for a given classname.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @since 0.7
	 */
	default void setBootAgentTypeContextUUID() {
		throw new UnsupportedOperationException();
	}

	/**
	 * Force the SRE platform to use the identifier hard-coded in the source code for its default context.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @since 0.7
	 * @see #setUniverseContextUUID(UUID)
	 * @see #getUniverseContextUUID()
	 * @see #setUniverseSpaceUUID(UUID)
	 * @see #getUniverseSpaceUUID()
	 * @deprecated see {@link #setSpecificContextUUID()}
	 */
	@Deprecated
	default void setDefaultContextUUID() {
		setSpecificContextUUID();
	}

	/**
	 * Force the SRE platform to use the identifier hard-coded in the source code for its default context.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @since 0.9
	 * @see #setUniverseContextUUID(UUID)
	 * @see #getUniverseContextUUID()
	 * @see #setUniverseSpaceUUID(UUID)
	 * @see #getUniverseSpaceUUID()
	 */
	default void setSpecificContextUUID() {
		throw new UnsupportedOperationException();
	}

	/**
	 * Force the identifier of the root/universe context in case the SRE is run when the {@link #setDefaultContextUUID() default
	 * identifier mode is enabled}.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @param id the identifier, or {@code null} if the default identifier should be used.
	 * @since 0.9
	 */
	default void setUniverseContextUUID(UUID id) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Replies the UUID of the root/universe context that is given by the user of the bootstrap.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @return the identifier, or {@code null} if no identifier is provided by the user and the default identifier should be used.
	 * @since 0.9
	 */
	default UUID getUniverseContextUUID() {
		throw new UnsupportedOperationException();
	}

	/**
	 * Force the identifier of the root/universe default space in case the SRE is run when the {@link #setDefaultContextUUID() default
	 * identifier mode is enabled}.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @param id the identifier, or {@code null} if the default identifier should be used.
	 * @since 0.9
	 */
	default void setUniverseSpaceUUID(UUID id) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Replies the UUID of the root/universe default space that is given by the user of the bootstrap.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @return the identifier, or {@code null} if no identifier is provided by the user and the default identifier should be used.
	 * @since 0.9
	 */
	default UUID getUniverseSpaceUUID() {
		throw new UnsupportedOperationException();
	}

	/**
	 * Force the verbosity level.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @param level the verbosity level.
	 * @since 0.7
	 */
	default void setVerboseLevel(int level) {
		throw new UnsupportedOperationException();
	}

}

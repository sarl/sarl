/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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
import java.util.logging.Logger;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

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
	 * Specify arguments that should be used by the SRE as command-line arguments.
	 * These arguments are usually used as start-up of the SRE.
	 *
	 * @param arguments the command-line arguments.
	 * @since 0.12
	 */
	void setCommandLineArguments(String[] arguments);

	/**
	 * Start the SRE without an agent.
	 * This function prepare the default context.
	 *
	 * @param asCommandLineApp indicates if the application should be started as a commend-line application.
	 *     If {@code true}, the SRE is started and run directly. Then, the "start agent" functions becomes
	 *     unusable because the SRE has started everything as it is called from the shell command line.
	 *     If {@code false}, the SRE is started in is normal way and all the functions are available according
	 *     to their own specifications.
	 *     If it not recommended to invoke this function with this argument evaluated to {@code true}.
	 * @return the context that is created by the bootstrap. If {@code null} there is no context created.
	 * @since 0.12
	 * @see #startWithoutAgent()
	 */
	AgentContext startWithoutAgent(boolean asCommandLineApp);

	/**
	 * Start the SRE without an agent.
	 * This function prepare the default context.
	 *
	 * @return the context that is created by the bootstrap. If {@code null} there is no context created.
	 * @since 0.7
	 */
	default AgentContext startWithoutAgent() {
		return startWithoutAgent(false);
	}

	/**
	 * Launch the SRE and the first agent in the kernel.
	 *
	 * <p>The function {@link #getBootAgentIdentifier()} permits to retrieve the identifier of the first launched agent.
	 *
	 * @param agentCls type of the first agent to launch.
	 * @param params parameters to pass to the agent as its initialization parameters.
	 * @throws Exception - if it is impossible to start the platform.
	 * @see #getBootAgentIdentifier()
	 */
	void startAgent(Class<? extends Agent> agentCls, Object... params) throws Exception;

	/**
	 * Launch the SRE and the first agent in the kernel.
	 *
	 * <p>The function {@link #getBootAgentIdentifier()} permits to retrieve the identifier of the first launched agent.
	 *
	 * @param nbAgents the number of agents to be launched.
	 * @param agentCls type of the first agent to launch.
	 * @param params parameters to pass to the agent as its initialization parameters.
	 * @throws Exception - if it is impossible to start the platform.
	 * @see #getBootAgentIdentifier()
	 */
	void startAgent(int nbAgents, Class<? extends Agent> agentCls, Object... params) throws Exception;

	/**
	 * Launch the SRE and the first agent in the kernel and assigning a specific UUID.
	 *
	 * @param agentCls type of the first agent to launch.
	 * @param agentID the identifier of the agent to be created.
	 * @param params parameters to pass to the agent as its initialization parameters.
	 * @throws Exception - if it is impossible to start the platform.
	 * @since 0.10
	 */
	void startAgentWithID(Class<? extends Agent> agentCls, UUID agentID, Object... params) throws Exception;

	/** Replies if the bootstrap could be used.
	 *
	 * <p>If the bootstrap cannot be used, it cannot launch agent.
	 *
	 * @return {@code true} if the bootstrap could be used. {@code false} if it cannot be used.
	 */
	@Pure
	default boolean isActive() {
		return true;
	}

	/** Replies if the kernel is running.
	 *
	 * @return {@code true} if the kernel is running. {@code false} is replied
	 *     if the kernel was stopped.
	 * @since 0.10
	 */
	@Pure
	default boolean isRunning() {
		return true;
	}

	/**
	 * Set offline flag of the agent platform before it is launched.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @param isOffline the offline flag.
	 * @throws UnsupportedOperationException a runtime exception
	 * @since 0.7
	 * @deprecated since 0.12, no direct replacement
	 */
	@Deprecated
	default void setOffline(boolean isOffline) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Force the SRE platform to use a random identifier for its default context.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @throws UnsupportedOperationException a runtime exception
	 * @since 0.7
	 */
	default void setRandomContextUUID() {
		throw new UnsupportedOperationException();
	}

	/**
	 * Force the SRE platform to use a default context identifier that is build upon the classname of the boot agent. It means
	 * that the UUID is always the same for a given classname.
	 *
	 * <p>This function has no effect if the agent framework is already launched.
	 *
	 * @throws UnsupportedOperationException a runtime exception
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
	 * @throws UnsupportedOperationException a runtime exception
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
	 * @throws UnsupportedOperationException a runtime exception
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
	 * @throws UnsupportedOperationException a runtime exception
	 * @since 0.9
	 */
	@Pure
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
	 * @throws UnsupportedOperationException a runtime exception
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
	 * @throws UnsupportedOperationException a runtime exception
	 * @since 0.9
	 */
	@Pure
	default UUID getUniverseSpaceUUID() {
		throw new UnsupportedOperationException();
	}

	/**
	 * Force the verbosity level.
	 *
	 * <p>This function may have no effect if the agent framework is already launched. This depends on the implementation
	 * of the SRE.
	 *
	 * <p>The value of the {@code level} argument does not depend on a specific logging infrastructure.
	 * Indeed, JUL, Log4J and SLF4J attach different numeric values to the different logging levels.
	 * Consequently, the numerical values that are assummed by this function are: <ul>
	 * <li>{@code &lt;= 0}: off, no logging.</li>
	 * <li>{@code = 1}: errors only.</li>
	 * <li>{@code = 2}: errors and warnings.</li>
	 * <li>{@code = 3}: errors, warnings and info.</li>
	 * <li>{@code = 4}: errors, warnings, info, debug 1.</li>
	 * <li>{@code = 5}: errors, warnings, info, debug 1 to 2.</li>
	 * <li>{@code = 6}: errors, warnings, info, debug 1 to 3.</li>
	 * <li>{@code = 7}: all messages.</li>
	 * </ul>
	 *
	 * @param level the verbosity level. See explanation in the documentation of this function.
	 * @throws UnsupportedOperationException a runtime exception
	 * @since 0.7
	 */
	default void setVerboseLevel(int level) {
		throw new UnsupportedOperationException();
	}

	/** Replies the logger used by the SRE.
	 *
	 * <p>This function replies a not {@code null} value only if the kernel was launched.
	 *
	 * @return the logger, or {@code null} if the kernel was not launched or has no logger.
	 * @since 0.10
	 */
	@Pure
	default Logger getKernelLogger() {
		return null;
	}

	/**
	 * Stop the SRE.
	 * This function may cause the agents to stop during the run of a behavior.
	 * This function has no timeout.
	 * This function returns when the kernel and all its services are stopped.
	 *
	 * @throws InterruptedException the shutdown process is interrupted.
	 * @since 0.10
	 * @see #shutdown(boolean)
	 * @see #shutdown(int)
	 * @see #shutdown(boolean, int)
	 */
	@Inline("shutdown(-1)")
	default void shutdown() throws InterruptedException {
		shutdown(-1);
	}

	/**
	 * Stop the SRE.
	 * This function may cause the agents to stop during the run of a behavior.
	 * This function has no timeout.
	 *
	 * <p>If the argument's value is evaluated to {@code true}, the {@link #shutdown(int)}
	 * functions is invoked with negative timeout; Otherwise the {@link #shutdown(int)}
	 * functions is invoked with {@code 0} value.
	 *
	 * @param blocking indicates if the functions is blocked until the shutdown task is finished.
	 *     If it is {@code true}, the function returns when the kernel is down. If it is
	 *     {@code false}, the function could return before the kernel is down.
	 * @throws InterruptedException the shutdown process is interrupted.
	 * @since 0.10
	 * @see #shutdown()
	 * @see #shutdown(int)
	 * @see #shutdown(boolean, int)
	 */
	@Inline("shutdown(($1) ? -1 : 0)")
	default void shutdown(boolean blocking) throws InterruptedException {
		shutdown(blocking ? -1 : 0);
	}

	/**
	 * Stop the SRE.
	 * This function may cause the agents to stop during the run of a behavior.
	 * This function returns when the kernel and all its services are stopped.
	 *
	 * @param timeout the maximum amount of milliseconds to wait for the shutdown.
	 *      If the provided value is strictly positive, it is the number of milliseconds
	 *      to wait for the termination of the shutdown. If the provided value is equal
	 *      to {@code 0}, the function returns as soon as the shutdown process is launch
	 *      (no waiting for the termination of the shutdown). If the value is strictly
	 *      negative, the function waits forever for the termination of the shutdown
	 *      process. The default value is {@code -1}.
	 * @throws InterruptedException the shutdown process is interrupted.
	 * @since 0.11
	 * @see #shutdown()
	 * @see #shutdown(boolean)
	 * @see #shutdown(int)
	 */
	void shutdown(int timeout) throws InterruptedException;

	/** Replies the service of the given type that is implemented into the SRE.
	 *
	 * @param <T> the type of the service.
	 * @param serviceType the type of the service.
	 * @return the service instance, or {@code null} if the SRE does not have any instance of service of the given type.
	 * @since 0.10
	 */
	@Pure
	<T> T getService(Class<T> serviceType);

	/**
	 * Add an observer on the SRE.
	 *
	 * @param listener the observer to add.
	 * @since 0.11
	 */
	void addSREListener(SREListener listener);

	/**
	 * Remove the given observer on the SRE.
	 *
	 * @param listener the observer to remove.
	 * @since 0.11
	 */
	void removeSREListener(SREListener listener);

}

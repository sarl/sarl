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

package io.sarl.eclipse.launching.config;

import java.util.List;

import com.google.inject.ImplementedBy;
import org.eclipse.debug.core.ILaunchConfiguration;

/**
 * Accessor for a SARL launch configuration.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@ImplementedBy(LaunchConfigurationConfigurator.class)
public interface ILaunchConfigurationAccessor {

	/**
	 * Returns the launch configuration type for SARL agents.
	 *
	 * @return the launch configuration type, never null.
	 * @since 0.7
	 */
	String getAgentLaunchConfigurationType();

	/**
	 * Returns the launch configuration type for SARL applications.
	 *
	 * @return the launch configuration type, never null.
	 * @since 0.7
	 */
	String getApplicationLaunchConfigurationType();

	/** Replies the identifier of the SRE to be used by the launch configuration.
	 *
	 * @param configuration the configuration.
	 * @return the SRE identifier or null.
	 */
	String getSREId(ILaunchConfiguration configuration);

	/** Replies the name of the Main type of the SRE stored in this launch configuration.
	 *
	 * @param configuration the configuration.
	 * @return the fully qualified name of the class that contains the main function; or null.
	 */
	String getMain(ILaunchConfiguration configuration);

	/** Replies if the configuration uses the system-wide SRE.
	 *
	 * @param configuration the configuration.
	 * @return <code>true</code> if the system-wide SRE is used.
	 */
	boolean getUseSystemSREFlag(ILaunchConfiguration configuration);

	/** Replies if the configuration uses the project SRE.
	 *
	 * @param configuration the configuration.
	 * @return <code>true</code> if the project SRE is used.
	 */
	boolean getUseProjectSREFlag(ILaunchConfiguration configuration);

	/** Replies the name of the project attached to the configuration.
	 *
	 * @param configuration the configuration.
	 * @return the project name, or null.
	 */
	String getProjectName(ILaunchConfiguration configuration);

	/** Replies the name of the agent to launch.
	 *
	 * @param configuration the configuration.
	 * @return the fully qualified name of the agent, or null.
	 */
	String getAgent(ILaunchConfiguration configuration);

	/** Replies the type of identifier for the default agent context.
	 *
	 * @param configuration the configuration.
	 * @return the type of identifier, never null.
	 */
	RootContextIdentifierType getDefaultContextIdentifier(ILaunchConfiguration configuration);

	/** Replies the arguments to pass to the launched agent.
	 *
	 * @param configuration the configuration.
	 * @return the arguments, never null
	 */
	String getAgentLaunchingArguments(ILaunchConfiguration configuration);

	/** Replies the arguments to pass to the SRE.
	 * These arguments are used only when launching an SRE directly, not when launching a Java application.
	 * For passing arguments to the Java application also, see {@link #getJRELaunchingArguments(ILaunchConfiguration)}.
	 *
	 * @param configuration the configuration.
	 * @return the arguments, never null
	 * @see #getExtraSRELaunchingArguments(ILaunchConfiguration)
	 */
	String getSRELaunchingArguments(ILaunchConfiguration configuration);

	/** Replies the arguments to pass to the SRE from a given contributor.
	 *
	 * @param configuration the configuration.
	 * @param contributorId the identifier of the contributor.
	 * @return the arguments, never null
	 * @since 0.12
	 * @see #getSRELaunchingArguments(ILaunchConfiguration)
	 * @see #getExtraSRELaunchingArguments(ILaunchConfiguration)
	 */
	String getExtraSRELaunchingArguments(ILaunchConfiguration configuration, String contributorId);

	/** Replies the arguments to pass to the SRE by all the contributors.
	 * These arguments are used only when launching an SRE directly, not when launching a Java application.
	 * For passing arguments to the Java application also, see {@link #getExtraJRELaunchingArguments(ILaunchConfiguration)}.
	 *
	 * @param configuration the configuration to read.
	 * @return the arguments to give to the SRE.
	 * @since 0.12
	 * @see #getSRELaunchingArguments(ILaunchConfiguration)
	 * @see #getExtraSRELaunchingArguments(ILaunchConfiguration, String)
	 */
	String getExtraSRELaunchingArguments(ILaunchConfiguration configuration);

	/** Replies the arguments to pass to the JRE.
	 * These arguments are passed to all applications, either it is an agent-based launching application or
	 * a Java application.
	 * For passing arguments to only the agent-based application, see {@link #getSRELaunchingArguments(ILaunchConfiguration)}.
	 *
	 * @param configuration the configuration.
	 * @return the arguments, never null
	 */
	String getJRELaunchingArguments(ILaunchConfiguration configuration);

	/** Replies the arguments to pass to the JRE from a given contributor.
	 *
	 * @param configuration the configuration.
	 * @param contributorId the identifier of the contributor.
	 * @return the arguments, never null
	 * @since 0.12
	 * @see #getSRELaunchingArguments(ILaunchConfiguration)
	 * @see #getExtraSRELaunchingArguments(ILaunchConfiguration)
	 */
	String getExtraJRELaunchingArguments(ILaunchConfiguration configuration, String contributorId);

	/** Replies the arguments to pass to the SRE by all the contributors.
	 * These arguments are passed to all applications, either it is an agent-based launching application or
	 * a Java application.
	 * For passing arguments to only the agent-based application, see {@link #getExtraSRELaunchingArguments(ILaunchConfiguration)}.
	 *
	 * @param configuration the configuration to read.
	 * @return the arguments to give to the SRE.
	 * @since 0.12
	 * @see #getSRELaunchingArguments(ILaunchConfiguration)
	 * @see #getExtraSRELaunchingArguments(ILaunchConfiguration, String)
	 */
	String getExtraJRELaunchingArguments(ILaunchConfiguration configuration);

	/** Replies the agent should be launched in the current Eclipse VM.
	 *
	 * @param configuration the configuration.
	 * @return <code>true</code> if the current Eclipse VM must be used for running the agent.
	 */
	boolean isEmbeddedSRE(ILaunchConfiguration configuration);

	/** Replies if the assertions are automatically enabled in debug mode.
	 *
	 * <p>When assertions are enabled, the <code>-ea</code> command line option will be given to the virtual machine.
	 *
	 * @param configuration the configuration.
	 * @return <code>true</code> if the assertions are enabled.
	 * @since 0.5
	 */
	boolean isAssertionEnabledInDebugMode(ILaunchConfiguration configuration);

	/** Replies if the assertions are automatically enabled in run mode.
	 *
	 * <p>When assertions are enabled, the <code>-ea</code> command line option will be given to the virtual machine.
	 *
	 * @param configuration the configuration.
	 * @return <code>true</code> if the assertions are enabled.
	 * @since 0.5
	 */
	boolean isAssertionEnabledInRunMode(ILaunchConfiguration configuration);

	/** Replies if the launching parameters from the launch configuration should be printed out on
	 * the console.
	 *
	 * @param configuration the configuration.
	 * @return <code>true</code> if the launching parameters are printed out.
	 * @since 0.12
	 */
	boolean isLaunhcingParametersPrintedOut(ILaunchConfiguration configuration);

	/** Replies the identifier of the classpath provider  that is injected into the classpath by the given contributor.
	 *
	 * <p>A classpath provider has a specific definition of a set of libraries that is considered to be included into the
	 * application classpath.
	 *
	 * @param configuration the configuration to read.
	 * @param contributorId the identifier of the contributor.
	 * @return the identifier of the classpath provider.
	 * @since 0.12
	 */
	String getExtraClasspathProvider(ILaunchConfiguration configuration, String contributorId);

	/** Replies the identifiers of the classpath providers that are injected into the classpath by the contributors.
	 *
	 * <p>A classpath provider has a specific definition of a set of libraries that is considered to be included into the
	 * application classpath.
	 *
	 * @param configuration the configuration to read.
	 * @return the identifiers of the classpath providers.
	 * @since 0.12
	 */
	List<String> getExtraClasspathProviders(ILaunchConfiguration configuration);

	/** Change the name of argument to pass to the application for changing its log level.
	 *
	 * @param configuration the configuration to read.
	 * @return the name of the option.
	 * @since 0.12
	 */
	String getLogArgumentName(ILaunchConfiguration configuration);

	/** Change the value of argument to pass to the application for changing its log level.
	 *
	 * @param configuration the configuration to read.
	 * @return the value of the option.
	 * @since 0.12
	 */
	String getLogArgumentValue(ILaunchConfiguration configuration);

}

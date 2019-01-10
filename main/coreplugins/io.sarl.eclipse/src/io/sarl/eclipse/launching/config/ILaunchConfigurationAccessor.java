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

package io.sarl.eclipse.launching.config;

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

	/** Replies if the SRE should show its logo.
	 *
	 * @param configuration the configuration.
	 * @return <code>true</code> if the logo should be shown.
	 */
	boolean getShowLogoFlag(ILaunchConfiguration configuration);

	/** Replies if the SRE should log information messages, or error messages.
	 *
	 * @param configuration the configuration.
	 * @return <code>true</code> if the SRE logs information messages.
	 */
	boolean getShowLogInfoFlag(ILaunchConfiguration configuration);

	/** Replies if the SRE should be run offline.
	 *
	 * @param configuration the configuration.
	 * @return <code>true</code> if the SRE should be run offline.
	 */
	boolean getOfflineFlag(ILaunchConfiguration configuration);

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
	 *
	 * @param configuration the configuration.
	 * @return the arguments, never null
	 */
	String getSRELaunchingArguments(ILaunchConfiguration configuration);

	/** Replies the arguments to pass to the JRE.
	 *
	 * @param configuration the configuration.
	 * @return the arguments, never null
	 */
	String getJRELaunchingArguments(ILaunchConfiguration configuration);

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

}

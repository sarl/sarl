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
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.jdt.launching.IRuntimeClasspathProvider;
import org.eclipse.xtext.xbase.lib.Inline;

import io.sarl.eclipse.runtime.ISREInstall;

/**
 * Configurator for a SARL launch configuration.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@ImplementedBy(LaunchConfigurationConfigurator.class)
public interface ILaunchConfigurationConfigurator {

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

	/** Create a default launch configuration for SARL agents.
	 *
	 * <p>The launch configuration is attached to the given project.
	 *
	 * @param projectName the name of the project that contains the main class.
	 * @param fullyQualifiedNameOfAgent the fully qualified name of the agent to be launched.
	 * @return the launch configuration.
	 * @throws CoreException if something is going wrong.
	 */
	@Inline("newAgentLaunchConfiguration($1, null, $2)")
	default ILaunchConfiguration newAgentLaunchConfiguration(String projectName, String fullyQualifiedNameOfAgent)
			throws CoreException {
		return newAgentLaunchConfiguration(projectName, null, fullyQualifiedNameOfAgent);
	}

	/** Create a default launch configuration for SARL agents.
	 *
	 * <p>The launch configuration is attached to the given project.
	 *
	 * @param projectName the name of the project that contains the main class.
	 * @param launchConfigurationName a proposal of name for the launch configuration.
	 * @param fullyQualifiedNameOfAgent the fully qualified name of the agent to be launched.
	 * @return the launch configuration.
	 * @throws CoreException if something is going wrong.
	 * @since 0.10
	 */
	ILaunchConfiguration newAgentLaunchConfiguration(String projectName, String launchConfigurationName,
			String fullyQualifiedNameOfAgent) throws CoreException;

	/** Create a default launch configuration for SARL agents.
	 *
	 * <p>The launch configuration is attached to the given project.
	 *
	 * @param projectName the name of the project that contains the main class.
	 * @param fullyQualifiedNameOfClass the fully qualified name of the class that contains the main function.
	 * @param classPathProvider type of the class path provider to use for launching the application.
	 * @return the launch configuration.
	 * @throws CoreException if something is going wrong.
	 */
	@Inline("newApplicationLaunchConfiguration($1, null, $2, $3)")
	default ILaunchConfiguration newApplicationLaunchConfiguration(String projectName, String fullyQualifiedNameOfClass,
			Class<? extends IRuntimeClasspathProvider> classPathProvider) throws CoreException {
		return newApplicationLaunchConfiguration(projectName, null, fullyQualifiedNameOfClass, classPathProvider);
	}

	/** Create a default launch configuration for SARL agents.
	 *
	 * <p>The launch configuration is attached to the given project.
	 *
	 * @param projectName the name of the project that contains the main class.
	 * @param launchConfigurationName a proposal of name for the launch configuration.
	 * @param fullyQualifiedNameOfClass the fully qualified name of the class that contains the main function.
	 * @param classPathProvider type of the class path provider to use for launching the application.
	 * @return the launch configuration.
	 * @throws CoreException if something is going wrong.
	 * @since 0.10
	 */
	ILaunchConfiguration newApplicationLaunchConfiguration(String projectName, String launchConfigurationName,
			String fullyQualifiedNameOfClass, Class<? extends IRuntimeClasspathProvider> classPathProvider)
			throws CoreException;

	/** Change the runtime configuration of the given launch configuration.
	 *
	 * <p>Only one of {@code useSystemSre} and {@code useProjectSre} could be <code>true</code> at the same time.
	 * If both are <code>true</code>, {@code useSystemSre} will be unchanged and {@code useProjectSre} sets to
	 * <code>false</code>.
	 *
	 * @param configuration the configuration to change.
	 * @param sre the SRE to use; or {@code null} for removing the SRE configuration entries.
	 * @param useSystemSre indicates if the system-wide SRE must be used. If null, the default is used.
	 * @param useProjectSre indicates if the SRE in the project's classpath must be used. If null, the default is used.
	 * @param resetJavaMainClass indicates if the Java main class must be reset or not within the configuration.
	 */
	void setRuntimeConfiguration(ILaunchConfigurationWorkingCopy configuration, ISREInstall sre,
			Boolean useSystemSre, Boolean useProjectSre, boolean resetJavaMainClass);

	/** Attach the given resources to the launch configuration.
	 *
	 * @param configuration the configuration to change.
	 * @param resources the resources to attach.
	 * @throws CoreException if something is going wrong.
	 */
	void attachResources(ILaunchConfigurationWorkingCopy configuration, IResource... resources)
			throws CoreException;

	/** Detach the given resources to the launch configuration.
	 *
	 * @param configuration the configuration to change.
	 * @param resources the resources to detach.
	 * @throws CoreException if something is going wrong.
	 */
	void detachResources(ILaunchConfigurationWorkingCopy configuration, IResource... resources)
			throws CoreException;

	/** Change the name of the project associated to the launch configuration.
	 *
	 * @param configuration the configuration to change.
	 * @param projectName the name of the project.
	 */
	void setProjectName(ILaunchConfigurationWorkingCopy configuration, String projectName);

	/** Change the name of the agent to be launched.
	 *
	 * @param configuration the configuration to change.
	 * @param agentFullyQualifiedName the fully qualified name of the agent to launch.
	 */
	void setAgent(ILaunchConfigurationWorkingCopy configuration, String agentFullyQualifiedName);

	/** Change the identifier of the default agent context.
	 *
	 * @param configuration the configuration to change.
	 * @param contextID the type of identifier to use for the default context.
	 */
	void setDefaultContextIdentifier(ILaunchConfigurationWorkingCopy configuration, RootContextIdentifierType contextID);

	/** Change the command-line arguments to give to the launched agents.
	 *
	 * @param configuration the configuration to change.
	 * @param arguments the arguments to give to the launched agent.
	 */
	void setAgentLaunchingArguments(ILaunchConfigurationWorkingCopy configuration, String arguments);

	/** Change the command-line arguments to give to the SRE.
	 *
	 * @param configuration the configuration to change.
	 * @param arguments the arguments to give to the SRE.
	 * @see #setExtraSRELaunchingArguments(ILaunchConfigurationWorkingCopy, String, List)
	 */
	void setSRELaunchingArguments(ILaunchConfigurationWorkingCopy configuration, String arguments);

	/** Change the command-line arguments to give to the SRE and for a specific contributor ID.
	 *
	 * <p>This function differs from {@link #setSRELaunchingArguments(ILaunchConfigurationWorkingCopy, String)} in
	 * the fact that the given arguments are associated to the given contributor in this function.
	 * In {@link #setSRELaunchingArguments(ILaunchConfigurationWorkingCopy, String)}, the arguments are not
	 * associated to a specific contributor.
	 *
	 * @param configuration the configuration to change.
	 * @param contributorId the identifier of the contributor.
	 * @param arguments the arguments to give to the SRE.
	 * @since 0.12
	 * @see #setSRELaunchingArguments(ILaunchConfigurationWorkingCopy, String)
	 */
	void setExtraSRELaunchingArguments(ILaunchConfigurationWorkingCopy configuration, String contributorId, String arguments);

	/** Change the command-line arguments to give to the JRE.
	 *
	 * @param configuration the configuration to change.
	 * @param arguments the arguments to give to the JRE.
	 */
	void setJRELaunchingArguments(ILaunchConfigurationWorkingCopy configuration, String arguments);

	/** Change the command-line arguments to give to the JRE and for a specific contributor ID.
	 *
	 * <p>This function differs from {@link #setJRELaunchingArguments(ILaunchConfigurationWorkingCopy, String)} in
	 * the fact that the given arguments are associated to the given contributor in this function.
	 * In {@link #setJRELaunchingArguments(ILaunchConfigurationWorkingCopy, String)}, the arguments are not
	 * associated to a specific contributor.
	 *
	 * @param configuration the configuration to change.
	 * @param contributorId the identifier of the contributor.
	 * @param arguments the arguments to give to the JRE.
	 * @since 0.12
	 * @see #setSRELaunchingArguments(ILaunchConfigurationWorkingCopy, String)
	 */
	void setExtraJRELaunchingArguments(ILaunchConfigurationWorkingCopy configuration, String contributorId, String arguments);

	/** Set if the agent should be launched in the current Eclipse VM.
	 *
	 * @param configuration the configuration to change.
	 * @param embedded <code>true</code> if the current Eclipse VM must be used for running the agent.
	 */
	void setEmbeddedSRE(ILaunchConfigurationWorkingCopy configuration, boolean embedded);

	/** Set if the assertions are automatically enabled in debug mode.
	 *
	 * <p>When assertions are enabled, the <code>-ea</code> command line option will be given to the virtual machine.
	 *
	 * @param configuration the configuration.
	 * @param enable <code>true</code> if the assertions are enabled.
	 * @since 0.5
	 */
	void setAssertionEnabledInDebugMode(ILaunchConfigurationWorkingCopy configuration, boolean enable);

	/** Set if the assertions are automatically enabled in run mode.
	 *
	 * <p>When assertions are enabled, the <code>-ea</code> command line option will be given to the virtual machine.
	 *
	 * @param configuration the configuration.
	 * @param enable <code>true</code> if the assertions are enabled.
	 * @since 0.5
	 */
	void setAssertionEnabledInRunMode(ILaunchConfigurationWorkingCopy configuration, boolean enable);

	/** Set if the launching parameters from the launch configuration should be printed out on
	 * the console.
	 *
	 * @param configuration the configuration.
	 * @param enable is <code>true</code> if the launching parameters are printed out.
	 * @since 0.12
	 */
	void setLaunhcingParametersPrintedOut(ILaunchConfigurationWorkingCopy configuration, boolean enable);

	/** Change the identifier of the classpath provider that is injected into the classpath by the given contributor.
	 *
	 * <p>A classpath provider has a specific definition of a set of libraries that is considered to be included into the
	 * application classpath.
	 *
	 * @param configuration the configuration to change.
	 * @param contributorId the identifier of the contributor.
	 * @param classpathProviderId the identifier of the classpath provider.
	 * @since 0.12
	 */
	void setExtraClasspathProvider(ILaunchConfigurationWorkingCopy configuration, String contributorId, String classpathProviderId);

	/** Change the argument to pass to the application for changing its log level.
	 *
	 * @param configuration the configuration to change.
	 * @param optionName is the name of the option.
	 * @param optionValue is the value to pass with the option.
	 * @since 0.12
	 */
	void setLogArgument(ILaunchConfigurationWorkingCopy configuration, String optionName, String optionValue);

}

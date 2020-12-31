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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.inject.Singleton;

import com.google.common.base.Strings;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jdt.internal.launching.JavaMigrationDelegate;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IRuntimeClasspathProvider;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.SARLRuntime;

/**
 * Configurator for a SARL launch configuration.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class LaunchConfigurationConfigurator implements ILaunchConfigurationConfigurator, ILaunchConfigurationAccessor {

	/**
	 * Launch configuration attribute key. The value is a fully qualified name
	 * of the agent to launch.
	 */
	public static final String ATTR_AGENT_NAME = SARLEclipsePlugin.PLUGIN_ID + ".AGENT_NAME"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates the type of the root context id.
	 */
	public static final String ATTR_ROOT_CONTEXT_ID_TYPE = SARLEclipsePlugin.PLUGIN_ID + ".ROOT_CONTEXT_ID_TYPE"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is the identifier of the SRE;
	 */
	public static final String ATTR_SARL_RUNTIME_ENVIRONMENT = SARLEclipsePlugin.PLUGIN_ID
			+ ".SARL_RUNTIME_ENVIRONMENT"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is a boolean that indicates if the system-wide SRE should be used.
	 */
	public static final String ATTR_USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT = SARLEclipsePlugin.PLUGIN_ID
			+ ".USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is a boolean that indicates if the project SRE should be used.
	 */
	public static final String ATTR_USE_PROJECT_SARL_RUNTIME_ENVIRONMENT = SARLEclipsePlugin.PLUGIN_ID
			+ ".USE_PROJECT_SARL_RUNTIME_ENVIRONMENT"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is the arguments for the SRE.
	 *
	 * @since 0.12
	 */
	public static final String ATTR_CONTRIB_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS_POSTFIX = ".CONTRIB_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is the arguments for the SRE.
	 */
	public static final String ATTR_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS = SARLEclipsePlugin.PLUGIN_ID
			+ ".SARL_RUNTIME_ENVIRONMENT_ARGUMENTS"; //$NON-NLS-1$

	/**
	 * Extra launch configuration attribute key. The value is the extra arguments for the SRE.
	 * @since 0.12
	 */
	public static final String ATTR_EXTRA_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS = ".EXTRA_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS"; //$NON-NLS-1$

	/**
	 * Extra launch configuration attribute key. The value is the extra arguments for the JRE.
	 * @since 0.12
	 */
	public static final String ATTR_EXTRA_VM_ARGUMENTS = ".EXTRA_VM_ARGUMENTS"; //$NON-NLS-1$

	/**
	 * Extra classpath provider attribute key. The value is the identifier of an extra classpath provider.
	 * @since 0.12
	 */
	public static final String ATTR_EXTRA_CLASSPATH_PROVIDER = ".EXTRA_CLASSPATH_PROVIDER"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates if the agents are run in the same VM as
	 * the Eclipse product.
	 */
	public static final String ATTR_EMBEDDED_SRE = SARLEclipsePlugin.PLUGIN_ID + ".EMBEDDED_SRE"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates if the launching parameters are
	 * printed out on the console.
	 * @since 0.12
	 */
	public static final String ATTR_ENABLE_LAUNCHING_PARAMETERS_PRINT_OUT = SARLEclipsePlugin.PLUGIN_ID + ".ENABLE_LAUNCHING_PARAMETERS_PRINT_OUT"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates if the assertions are enabled in the virtual machine
	 * when it is launched in debug mode.
	 */
	public static final String ATTR_ENABLE_ASSERTIONS_IN_DEBUG_MODE = SARLEclipsePlugin.PLUGIN_ID + ".ENABLE_ASSERTIONS_DEBUG"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates if the assertions are enabled in the virtual machine
	 * when it is launched in run mode.
	 */
	public static final String ATTR_ENABLE_ASSERTIONS_IN_RUN_MODE = SARLEclipsePlugin.PLUGIN_ID + ".ENABLE_ASSERTIONS_RUN"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates the name of the command-line option for changing
	 * the log level of the application.
	 */
	public static final String ATTR_LOG_OPTION_NAME = SARLEclipsePlugin.PLUGIN_ID + ".LOG_OPTION_NAME"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates the value of the command-line option for changing
	 * the log level of the application.
	 */
	public static final String ATTR_LOG_OPTION_VALUE = SARLEclipsePlugin.PLUGIN_ID + ".LOG_OPTION_VALUE"; //$NON-NLS-1$

	/** Identifier of the type of launch configuration dedicated to SARL agents
	 * (value <code>io.sarl.eclipse.debug.LaunchConfigType</code>).
	 */
	public static final String SARL_AGENT_LAUNCH_CONFIG_TYPE = "io.sarl.eclipse.debug.AgentLaunchConfigType"; //$NON-NLS-1$

	/** Identifier of the type of launch configuration dedicated to SARL applications
	 * (value <code>io.sarl.eclipse.debug.LaunchConfigType</code>).
	 */
	public static final String SARL_APPLICATION_LAUNCH_CONFIG_TYPE = "io.sarl.eclipse.debug.ApplicationLaunchConfigType"; //$NON-NLS-1$

	private static final boolean DEFAULT_USE_SYSTEM_SRE = true;

	private static final boolean DEFAULT_USE_PROJECT_SRE = false;

	private static final boolean DEFAULT_EMBEDDED_SRE = false;

	private static final boolean DEFAULT_ENABLE_LAUNCHING_PARAMETERS_PRINT_OUT = false;

	private static final boolean DEFAULT_ENABLE_ASSERTIONS_IN_DEBUG_MODE = true;

	private static final boolean DEFAULT_ENABLE_ASSERTIONS_IN_RUN_MODE = false;

	@Override
	public String getAgentLaunchConfigurationType() {
		return SARL_AGENT_LAUNCH_CONFIG_TYPE;
	}

	@Override
	public String getApplicationLaunchConfigurationType() {
		return SARL_APPLICATION_LAUNCH_CONFIG_TYPE;
	}

	private static String simpleName(String fullName) {
		if (!Strings.isNullOrEmpty(fullName)) {
			final int index = fullName.lastIndexOf('.');
			if (index >= 0 && index < fullName.length() - 1) {
				return fullName.substring(index + 1);
			}
		}
		return fullName;
	}

	@Override
	public ILaunchConfiguration newAgentLaunchConfiguration(String projectName, String launchConfigurationName,
			String fullyQualifiedNameOfAgent) throws CoreException {
		final String name = Strings.isNullOrEmpty(launchConfigurationName)
				? simpleName(fullyQualifiedNameOfAgent) : launchConfigurationName;
		final ILaunchConfigurationWorkingCopy wc = initLaunchConfiguration(getAgentLaunchConfigurationType(), projectName,
				name, true);
		setAgent(wc, fullyQualifiedNameOfAgent);
		return wc.doSave();
	}

	@Override
	public ILaunchConfiguration newApplicationLaunchConfiguration(String projectName, String launchConfigurationName,
			String fullyQualifiedNameOfClass, Class<? extends IRuntimeClasspathProvider> classPathProvider) throws CoreException {
		final String name = Strings.isNullOrEmpty(launchConfigurationName)
				? simpleName(fullyQualifiedNameOfClass) : launchConfigurationName;
		final ILaunchConfigurationWorkingCopy wc = initLaunchConfiguration(getApplicationLaunchConfigurationType(), projectName,
				name, false);
		setMainJavaClass(wc, fullyQualifiedNameOfClass);
		if (classPathProvider != null) {
			wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_CLASSPATH_PROVIDER, classPathProvider.getName());
		}
		return wc.doSave();
	}

	/** Change the main java class within the given configuration.
	 *
	 * @param wc the configuration to change.
	 * @param name the qualified name of the main Java class.
	 * @since 0.7
	 */
	protected static void setMainJavaClass(ILaunchConfigurationWorkingCopy wc, String name) {
		wc.setAttribute(
				IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
				name);
	}

	/** initialize the launch configuration.
	 *
	 * @param configurationType the name of the configuration type to create.
	 * @param projectName the name of the project.
	 * @param id the identifier of the launch configuration.
	 * @param resetJavaMainClass indicates if the JAva main class should be reset from the SRE configuration.
	 * @return the created launch configuration.
	 * @throws CoreException if the configuration cannot be created.
	 * @since 0.7
	 */
	protected ILaunchConfigurationWorkingCopy initLaunchConfiguration(String configurationType, String projectName,
			String id, boolean resetJavaMainClass) throws CoreException {
		final ILaunchManager launchManager = DebugPlugin.getDefault().getLaunchManager();
		final ILaunchConfigurationType configType = launchManager.getLaunchConfigurationType(configurationType);
		final ILaunchConfigurationWorkingCopy wc = configType.newInstance(null, launchManager.generateLaunchConfigurationName(id));
		setProjectName(wc, projectName);
		setDefaultContextIdentifier(wc, null);
		setRuntimeConfiguration(wc, SARLRuntime.getDefaultSREInstall(), DEFAULT_USE_SYSTEM_SRE, DEFAULT_USE_PROJECT_SRE, resetJavaMainClass);
		JavaMigrationDelegate.updateResourceMapping(wc);
		return wc;
	}

	@Override
	public void attachResources(ILaunchConfigurationWorkingCopy configuration, IResource... resources)
			throws CoreException {
		final IResource[] oldTab = configuration.getMappedResources();
		final IResource[] newTab;
		if (oldTab == null) {
			newTab = Arrays.copyOf(resources, resources.length);
		} else {
			newTab = Arrays.copyOf(oldTab, oldTab.length + resources.length);
			System.arraycopy(resources, 0, newTab, oldTab.length, resources.length);
		}
		configuration.setMappedResources(newTab);
	}

	@Override
	public void detachResources(ILaunchConfigurationWorkingCopy configuration, IResource... resources)
			throws CoreException {
		final List<IResource> res = new ArrayList<>();
		final IResource[] oldTab = configuration.getMappedResources();
		if (oldTab != null) {
			res.addAll(Arrays.asList(oldTab));
		}
		if (res.removeAll(Arrays.asList(resources))) {
			configuration.setMappedResources(res.toArray(new IResource[res.size()]));
		}
	}

	@Override
	public void setRuntimeConfiguration(ILaunchConfigurationWorkingCopy configuration, ISREInstall sre,
			Boolean useSystemSre, Boolean useProjectSre, boolean resetJavaMainClass) {
		boolean system = useSystemSre == null ? DEFAULT_USE_SYSTEM_SRE : useSystemSre.booleanValue();
		boolean project = useProjectSre == null ? DEFAULT_USE_PROJECT_SRE : useProjectSre.booleanValue();
		if (system && project) {
			system = true;
			project = false;
		}
		// Save the SRE specific parameters
		if (sre != null) {
			configuration.setAttribute(
					ATTR_SARL_RUNTIME_ENVIRONMENT,
					sre.getId());
			final String mainClass = sre.getMainClass();
			if (resetJavaMainClass) {
				if (Strings.isNullOrEmpty(mainClass)) {
					configuration.removeAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME);
				} else {
					setMainJavaClass(configuration, mainClass);
				}
			}
		} else {
			configuration.removeAttribute(ATTR_SARL_RUNTIME_ENVIRONMENT);
			if (resetJavaMainClass) {
				configuration.removeAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME);
			}
		}
		// Save the boolean configuration flags
		configuration.setAttribute(ATTR_USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT, system);
		configuration.setAttribute(ATTR_USE_PROJECT_SARL_RUNTIME_ENVIRONMENT, project);
		// Use the default JRE
		configuration.setAttribute(IJavaLaunchConfigurationConstants.ATTR_JRE_CONTAINER_PATH, (String) null);
	}

	@Override
	public void setProjectName(ILaunchConfigurationWorkingCopy configuration, String projectName) {
		configuration.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, Strings.emptyToNull(projectName));
	}

	@Override
	public void setAgent(ILaunchConfigurationWorkingCopy configuration, String agentFullyQualifiedName) {
		configuration.setAttribute(ATTR_AGENT_NAME, Strings.emptyToNull(agentFullyQualifiedName));
	}

	@Override
	public void setDefaultContextIdentifier(ILaunchConfigurationWorkingCopy configuration, RootContextIdentifierType contextID) {
		if (contextID == null) {
			configuration.setAttribute(ATTR_ROOT_CONTEXT_ID_TYPE,
					RootContextIdentifierType.DEFAULT_CONTEXT_ID.name());
		} else {
			configuration.setAttribute(ATTR_ROOT_CONTEXT_ID_TYPE, contextID.name());
		}
	}

	@Override
	public void setAgentLaunchingArguments(ILaunchConfigurationWorkingCopy configuration, String arguments) {
		configuration.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROGRAM_ARGUMENTS, arguments);
	}

	@Override
	public void setSRELaunchingArguments(ILaunchConfigurationWorkingCopy configuration, String arguments) {
		configuration.setAttribute(ATTR_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS, Strings.emptyToNull(arguments));
	}

	@Override
	public void setExtraSRELaunchingArguments(ILaunchConfigurationWorkingCopy configuration, String contributorId,
			String arguments) {
		final String attrName = contributorId + ATTR_EXTRA_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS;
		configuration.setAttribute(attrName, Strings.emptyToNull(arguments));
	}

	@Override
	public void setJRELaunchingArguments(ILaunchConfigurationWorkingCopy configuration, String arguments) {
		configuration.setAttribute(IJavaLaunchConfigurationConstants.ATTR_VM_ARGUMENTS, Strings.emptyToNull(arguments));
	}

	@Override
	public void setExtraJRELaunchingArguments(ILaunchConfigurationWorkingCopy configuration, String contributorId,
			String arguments) {
		final String attrName = contributorId + ATTR_EXTRA_VM_ARGUMENTS;
		configuration.setAttribute(attrName, Strings.emptyToNull(arguments));
	}

	@Override
	public String getSREId(ILaunchConfiguration configuration) {
		try {
			return Strings.emptyToNull(configuration.getAttribute(ATTR_SARL_RUNTIME_ENVIRONMENT, (String) null));
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public String getMain(ILaunchConfiguration configuration) {
		try {
			return Strings.emptyToNull(configuration.getAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, (String) null));
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public boolean getUseSystemSREFlag(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(ATTR_USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT, DEFAULT_USE_SYSTEM_SRE);
		} catch (CoreException e) {
			// For backward compatibility
			try {
				final String value = configuration.getAttribute(ATTR_USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT,
						Boolean.toString(DEFAULT_USE_SYSTEM_SRE));
				return Boolean.valueOf(value);
			} catch (Throwable e2) {
				return DEFAULT_USE_SYSTEM_SRE;
			}
		}
	}

	@Override
	public boolean getUseProjectSREFlag(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(ATTR_USE_PROJECT_SARL_RUNTIME_ENVIRONMENT, DEFAULT_USE_PROJECT_SRE);
		} catch (CoreException e) {
			// For backward compatibility
			try {
				final String value = configuration.getAttribute(ATTR_USE_PROJECT_SARL_RUNTIME_ENVIRONMENT,
						Boolean.toString(DEFAULT_USE_PROJECT_SRE));
				return Boolean.valueOf(value);
			} catch (Throwable e2) {
				return DEFAULT_USE_PROJECT_SRE;
			}
		}
	}

	@Override
	public String getProjectName(ILaunchConfiguration configuration) {
		try {
			return Strings.emptyToNull(configuration.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, (String) null));
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public String getAgent(ILaunchConfiguration configuration) {
		try {
			return Strings.emptyToNull(configuration.getAttribute(ATTR_AGENT_NAME, (String) null));
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public RootContextIdentifierType getDefaultContextIdentifier(ILaunchConfiguration configuration) {
		String name;
		try {
			name = configuration.getAttribute(ATTR_ROOT_CONTEXT_ID_TYPE, (String) null);
		} catch (CoreException e) {
			name = null;
		}
		if (Strings.isNullOrEmpty(name)) {
			try {
				final RootContextIdentifierType type = RootContextIdentifierType.valueOf(name);
				return type;
			} catch (Throwable exception) {
				//
			}
		}
		return RootContextIdentifierType.DEFAULT_CONTEXT_ID;
	}

	@Override
	public String getAgentLaunchingArguments(ILaunchConfiguration configuration) {
		try {
			return Strings.nullToEmpty(configuration.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROGRAM_ARGUMENTS,
					(String) null));
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public String getSRELaunchingArguments(ILaunchConfiguration configuration) {
		try {
			return Strings.nullToEmpty(configuration.getAttribute(ATTR_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS, (String) null));
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public String getExtraSRELaunchingArguments(ILaunchConfiguration configuration, String contributorId) {
		try {
			final String attrName = contributorId + ATTR_EXTRA_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS;
			return Strings.nullToEmpty(configuration.getAttribute(attrName, (String) null));
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public String getExtraSRELaunchingArguments(ILaunchConfiguration configuration) {
		final StringBuilder builder = new StringBuilder();
		try {
			for (final String key : configuration.getAttributes().keySet()) {
				if (key.endsWith(ATTR_EXTRA_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS)) {
					final String value = Strings.nullToEmpty(configuration.getAttribute(key, (String) null));
					if (!Strings.isNullOrEmpty(value)) {
						if (builder.length() > 0) {
							builder.append(" "); //$NON-NLS-1$
						}
						builder.append(value);
					}
				}
			}
			return builder.toString();
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public String getJRELaunchingArguments(ILaunchConfiguration configuration) {
		try {
			return Strings.nullToEmpty(configuration.getAttribute(IJavaLaunchConfigurationConstants.ATTR_VM_ARGUMENTS, (String) null));
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public String getExtraJRELaunchingArguments(ILaunchConfiguration configuration, String contributorId) {
		try {
			final String attrName = contributorId + ATTR_EXTRA_VM_ARGUMENTS;
			return Strings.nullToEmpty(configuration.getAttribute(attrName, (String) null));
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public String getExtraJRELaunchingArguments(ILaunchConfiguration configuration) {
		final StringBuilder builder = new StringBuilder();
		try {
			for (final String key : configuration.getAttributes().keySet()) {
				if (key.endsWith(ATTR_EXTRA_VM_ARGUMENTS)) {
					final String value = Strings.nullToEmpty(configuration.getAttribute(key, (String) null));
					if (!Strings.isNullOrEmpty(value)) {
						if (builder.length() > 0) {
							builder.append(" "); //$NON-NLS-1$
						}
						builder.append(value);
					}
				}
			}
			return builder.toString();
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public boolean isEmbeddedSRE(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(ATTR_EMBEDDED_SRE, DEFAULT_EMBEDDED_SRE);
		} catch (CoreException e) {
			return DEFAULT_EMBEDDED_SRE;
		}
	}

	@Override
	public void setEmbeddedSRE(ILaunchConfigurationWorkingCopy configuration, boolean embedded) {
		configuration.setAttribute(ATTR_EMBEDDED_SRE, embedded);
	}

	@Override
	public boolean isAssertionEnabledInDebugMode(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(ATTR_ENABLE_ASSERTIONS_IN_DEBUG_MODE, DEFAULT_ENABLE_ASSERTIONS_IN_DEBUG_MODE);
		} catch (CoreException e) {
			return DEFAULT_ENABLE_ASSERTIONS_IN_DEBUG_MODE;
		}
	}

	@Override
	public void setAssertionEnabledInDebugMode(ILaunchConfigurationWorkingCopy configuration, boolean enable) {
		configuration.setAttribute(ATTR_ENABLE_ASSERTIONS_IN_DEBUG_MODE, enable);
	}

	@Override
	public boolean isAssertionEnabledInRunMode(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(ATTR_ENABLE_ASSERTIONS_IN_RUN_MODE, DEFAULT_ENABLE_ASSERTIONS_IN_RUN_MODE);
		} catch (CoreException e) {
			return DEFAULT_ENABLE_ASSERTIONS_IN_RUN_MODE;
		}
	}

	@Override
	public void setAssertionEnabledInRunMode(ILaunchConfigurationWorkingCopy configuration, boolean enable) {
		configuration.setAttribute(ATTR_ENABLE_ASSERTIONS_IN_RUN_MODE, enable);
	}

	@Override
	public String getExtraClasspathProvider(ILaunchConfiguration configuration, String contributorId) {
		try {
			final String attrName = contributorId + ATTR_EXTRA_CLASSPATH_PROVIDER;
			return Strings.nullToEmpty(configuration.getAttribute(attrName, (String) null));
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public void setExtraClasspathProvider(ILaunchConfigurationWorkingCopy configuration, String contributorId, String classpathContainerId) {
		final String attrName = contributorId + ATTR_EXTRA_CLASSPATH_PROVIDER;
		configuration.setAttribute(attrName, Strings.emptyToNull(classpathContainerId));
	}

	@Override
	public List<String> getExtraClasspathProviders(ILaunchConfiguration configuration) {
		final List<String> identifiers = new ArrayList<>();
		try {
			for (final String key : configuration.getAttributes().keySet()) {
				if (key.endsWith(ATTR_EXTRA_CLASSPATH_PROVIDER)) {
					final String value = configuration.getAttribute(key, (String) null);
					if (!Strings.isNullOrEmpty(value)) {
						identifiers.add(value);
					}
				}
			}
		} catch (CoreException e) {
			//
		}
		return identifiers;
	}

	@Override
	public String getLogArgumentName(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(ATTR_LOG_OPTION_NAME, (String) null);
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public String getLogArgumentValue(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(ATTR_LOG_OPTION_VALUE, (String) null);
		} catch (CoreException e) {
			return null;
		}
	}

	@Override
	public void setLogArgument(ILaunchConfigurationWorkingCopy configuration, String optionName, String optionValue) {
		String name = Strings.emptyToNull(optionName);
		String value = Strings.emptyToNull(optionValue);
		if (name == null || value == null) {
			value = null;
			name = null;
		}
		configuration.setAttribute(ATTR_LOG_OPTION_NAME, name);
		configuration.setAttribute(ATTR_LOG_OPTION_VALUE, value);
	}

	@Override
	public boolean isLaunhcingParametersPrintedOut(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(ATTR_ENABLE_LAUNCHING_PARAMETERS_PRINT_OUT, DEFAULT_ENABLE_LAUNCHING_PARAMETERS_PRINT_OUT);
		} catch (CoreException e) {
			return DEFAULT_ENABLE_LAUNCHING_PARAMETERS_PRINT_OUT;
		}
	}

	@Override
	public void setLaunhcingParametersPrintedOut(ILaunchConfigurationWorkingCopy configuration, boolean enable) {
		configuration.setAttribute(ATTR_ENABLE_LAUNCHING_PARAMETERS_PRINT_OUT, enable);
	}

}

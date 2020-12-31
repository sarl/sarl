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

package io.sarl.eclipse.launching.runner.general;

import static io.sarl.eclipse.launching.config.LaunchConfigurationUtils.join;

import java.io.File;
import java.lang.ref.WeakReference;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IModuleDescription;
import org.eclipse.jdt.internal.launching.LaunchingMessages;
import org.eclipse.jdt.launching.ExecutionArguments;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMRunner;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.launching.VMRunnerConfiguration;
import org.eclipse.xtext.util.Strings;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.runtime.ISREInstall;

/** Implementation of a launching process, split in separated steps for
 * making easier the cancellation.
 *
 * @param <T> the type of the owner.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public abstract class AbstractLaunchProcess<T extends AbstractSARLLaunchConfiguration> implements ILaunchProcess {

	private static final String OPTION_ENABLEASSERTIONS = "-ea"; //$NON-NLS-1$

	/** Launch configuration to be used.
	 */
	protected final ILaunchConfiguration configuration;

	/** Running mode (running or debugging).
	 */
	protected final String mode;

	/** Launching infrastructure.
	 */
	protected final ILaunch launch;

	/** The launch configuration that owns this process.
	 */
	private final WeakReference<T> owner;

	private PreparationProcessState preparationState = PreparationProcessState.STEP_0_PREPARE_PARAMETERS;

	private String mainTypeName;

	private IVMRunner runner;

	private String workingDirName;

	private String[] envp;

	private String[] classpath;

	private String[] modulepath;

	private ExecutionArguments execArgs;

	private Map<String, Object> vmAttributesMap;

	private VMRunnerConfiguration runConfig;

	private RunProcessState runState = RunProcessState.STEP_0_CONFIGURE_SOURCE_LOCATOR;

	/** Constructor.
	 *
	 * @param owner the launch configuration that is owning the process.
	 * @param configuration the launch configuration.
	 * @param mode the launching mode.
	 * @param launch the launching
	 */
	protected AbstractLaunchProcess(T owner, ILaunchConfiguration configuration,
			String mode, ILaunch launch) {
		this.owner = new WeakReference<>(owner);
		this.configuration = configuration;
		this.mode = mode;
		this.launch = launch;
	}

	/** Replies the owner of the process.
	 *
	 * @return the owner of the process.
	 */
	protected T getOwner() {
		return this.owner.get();
	}

	@Override
	public VMRunnerConfiguration getVirtualMachineRunnerConfiguration() {
		return this.runConfig;
	}

	/** Change the configuration of the virtual machine runner.
	 *
	 * @param configuration the configuration of the virtual machine runner.
	 */
	protected void setVirtualMachineRunnerConfiguration(VMRunnerConfiguration configuration) {
		this.runConfig = configuration;
	}

	/** Replies the attributes of the virtual machine.
	 *
	 * @return the virtual machine attributes.
	 */
	protected Map<String, Object> getVirtualMachineAttributes() {
		return this.vmAttributesMap;
	}

	/** Change the attributes of the virtual machine.
	 *
	 * @param attributes the attributes of the virtual machine.
	 */
	protected void setVirtualMachineAttributes(Map<String, Object> attributes) {
		this.vmAttributesMap = attributes;
	}

	/** Replies the arguments to pass to the application.
	 *
	 * @return the execution arguments.
	 */
	protected ExecutionArguments getExecutionArguments() {
		return this.execArgs;
	}

	/** Change the arguments to pass to the application.
	 *
	 * @param arguments the execution arguments.
	 */
	protected void setExecutionArguments(ExecutionArguments arguments) {
		this.execArgs = arguments;
	}

	/** Replies the qualified name of the main class.
	 *
	 * @return the main class name.
	 */
	protected String getMainTypeName() {
		return this.mainTypeName;
	}

	/** Change the qualified name of the main class.
	 *
	 * @param name the main class name.
	 */
	protected void setMainTypeName(String name) {
		this.mainTypeName = name;
	}

	/** Replies the virtual machine runner.
	 *
	 * @return the runner.
	 */
	protected IVMRunner getRunner() {
		return this.runner;
	}

	/** Change the virtual machine runner.
	 *
	 * @param runner the runner.
	 */
	protected void setRunner(IVMRunner runner) {
		this.runner = runner;
	}

	/** Replies the working folder name.
	 *
	 * @return the name of the working folder.
	 */
	protected String getWorkingDirectory() {
		return this.workingDirName;
	}

	/** Change the working folder name.
	 *
	 * @param name the name of the working folder.
	 */
	protected void setWorkingDirectory(String name) {
		this.workingDirName = name;
	}

	/** Replies the environment.
	 *
	 * @return the environment.
	 */
	protected String[] getEnvironment() {
		return this.envp;
	}

	/** Change the environment.
	 *
	 * @param environment the environment.
	 */
	protected void setEnvironment(String[] environment) {
		this.envp = environment;
	}

	/** Replies the class path.
	 *
	 * @return the class path.
	 */
	protected String[] getClasspath() {
		return this.classpath;
	}

	/** Change the class path.
	 *
	 * @param classpath the class path.
	 */
	protected void setClasspath(String[] classpath) {
		this.classpath = classpath;
	}

	/** Replies the module path.
	 *
	 * @return the module path.
	 * @since 0.12
	 */
	protected String[] getModulepath() {
		return this.modulepath;
	}

	/** Change the module path.
	 *
	 * @param modulepath the module path.
	 * @since 0.12
	 */
	protected void setModulepath(String[] modulepath) {
		this.modulepath = modulepath;
	}

	@Override
	public int getStepNumber() {
		return PreparationProcessState.values().length + RunProcessState.values().length;
	}

	@Override
	public boolean prepare(IProgressMonitor monitor) throws CoreException {
		switch (this.preparationState) {
		case STEP_0_PREPARE_PARAMETERS:
			readConfigurationParameters(monitor);
			break;
		case STEP_1_BUILD_CLASSPATH:
			buildClasspathAndModulePath(monitor);
			break;
		case STEP_2_PREPARE_LAUNCHING:
			readLaunchingArguments(monitor);
			break;
		case STEP_3_POST_VALIDATION:
			postValidation(monitor);
			break;
		case STEP_4_CREATE_RUN_CONFIGURATION:
			createRunConfiguration(monitor);
			break;
		case STEP_5_CONFIGURE_STOP_IN_MAIN:
		default:
			configureStopInMain(monitor);
			return false;
		}
		this.preparationState = this.preparationState.next();
		return true;
	}

	/** Extract the general parameters from the configuration.
	 *
	 * @param monitor the progress monitor.
	 * @throws CoreException if a parameter cannot be extracted.
	 */
	protected void readConfigurationParameters(IProgressMonitor monitor) throws CoreException {
		monitor.subTask(
				LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_Verifying_launch_attributes____1);

		final AbstractSARLLaunchConfiguration own = getOwner();

		// Clear cached entries
		own.clearBuffers();

		setMainTypeName(own.verifyMainTypeName(this.configuration));
		setRunner(own.getVMRunner(this.configuration, this.mode));

		final File workingDir = own.verifyWorkingDirectory(this.configuration);
		setWorkingDirectory(null);
		if (workingDir != null) {
			setWorkingDirectory(workingDir.getAbsolutePath());
		}

		// Environment variables
		setEnvironment(own.getEnvironment(this.configuration));
	}

	/** Extract the class path from the configuration.
	 *
	 * @param monitor the progress monitor.
	 * @throws CoreException if a parameter cannot be extracted.
	 */
	protected void buildClasspathAndModulePath(IProgressMonitor monitor) throws CoreException {
		monitor.subTask(
				Messages.AbstractLaunchProcess_0);
		final String[][] paths = getOwner().getClasspathAndModulepath(this.configuration);

		final AbstractSARLLaunchConfiguration own = getOwner();
		if (own.getConfigurationAccessor().isLaunhcingParametersPrintedOut(this.configuration)) {
			SARLEclipsePlugin.getDefault().getLog().info(
					MessageFormat.format(Messages.AbstractLaunchProcess_9, Arrays.toString(paths[0])));
			SARLEclipsePlugin.getDefault().getLog().info(
					MessageFormat.format(Messages.AbstractLaunchProcess_10, Arrays.toString(paths[1])));
		}

		setClasspath(paths[0]);
		setModulepath(paths[1]);
	}

	/** Read the arguments to pass to the launched application.
	 *
	 * @param monitor the progress monitor.
	 * @throws CoreException if a parameter cannot be extracted.
	 */
	protected final void readLaunchingArguments(IProgressMonitor monitor) throws CoreException {
		monitor.subTask(
				Messages.AbstractLaunchProcess_1);

		final AbstractSARLLaunchConfiguration own = getOwner();

		// Program & VM arguments
		final String pgmArgs = getProgramArguments(own);
		final String vmArgs = getVMArguments(own);

		if (own.getConfigurationAccessor().isLaunhcingParametersPrintedOut(this.configuration)) {
			SARLEclipsePlugin.getDefault().getLog().info(
					MessageFormat.format(Messages.AbstractLaunchProcess_6, pgmArgs));
			SARLEclipsePlugin.getDefault().getLog().info(
					MessageFormat.format(Messages.AbstractLaunchProcess_7, vmArgs));
		}

		setExecutionArguments(new ExecutionArguments(vmArgs, pgmArgs));

		// VM-specific attributes
		final Map<String, Object> vmAttrs = own.getVMSpecificAttributesMap(this.configuration);

		if (own.getConfigurationAccessor().isLaunhcingParametersPrintedOut(this.configuration)) {
			SARLEclipsePlugin.getDefault().getLog().info(
					MessageFormat.format(Messages.AbstractLaunchProcess_8, vmAttrs.toString()));
		}

		setVirtualMachineAttributes(vmAttrs);
	}

	/** Build the program arguments.
	 *
	 * @param own the configuration.
	 * @return the program arguments.
	 * @throws CoreException if a parameter cannot be extracted.
	 * @since 0.12
	 */
	protected String getProgramArguments(AbstractSARLLaunchConfiguration own) throws CoreException {
		return own.getProgramArguments(this.configuration);
	}

	/** Build the VM arguments.
	 *
	 * @param own the configuration.
	 * @return the VM arguments.
	 * @throws CoreException if a parameter cannot be extracted.
	 * @since 0.12
	 */
	protected String getVMArguments(AbstractSARLLaunchConfiguration own) throws CoreException {
		final String vmArgs = own.getVMArguments(this.configuration);

		final String modeArgs = own.getVMArguments(this.configuration, this.mode);

		// Add -ea option if in debug mode
		final String eaArg;
		if ((Objects.equals(this.mode,  ILaunchManager.RUN_MODE)
				&& own.getConfigurationAccessor().isAssertionEnabledInRunMode(this.configuration))
				|| (Objects.equals(this.mode,  ILaunchManager.DEBUG_MODE)
						&& own.getConfigurationAccessor().isAssertionEnabledInDebugMode(this.configuration))) {
			eaArg = OPTION_ENABLEASSERTIONS;
		} else {
			eaArg = null;
		}

		// Add special arguments that are provided by contributors
		final String extraJreArgs = own.getConfigurationAccessor().getExtraJRELaunchingArguments(this.configuration);

		return join(vmArgs, modeArgs, eaArg, extraJreArgs);
	}

	/** Validate the extracted values from the launch configuration.
	 *
	 * @param monitor the progress monitor.
	 * @throws CoreException if a parameter cannot be extracted.
	 */
	@SuppressWarnings("synthetic-access")
	private void postValidation(IProgressMonitor monitor) throws CoreException {
		monitor.subTask(
				Messages.AbstractLaunchProcess_2);
		if (Strings.isEmpty(getMainTypeName())) {
			final AbstractSARLLaunchConfiguration own = getOwner();
			// This case occurs when the launch configuration is using
			// a SRE that is inside the class path.
			// The name of the main class is then no saved in the launch configuration properties.
			final ISREInstall sre = SrePathUtils.getSREInstallFor(this.configuration,
					own.getConfigurationAccessor(), cfg -> own.getJavaProject(cfg));
			if (sre != null) {
				setMainTypeName(sre.getMainClass());
			}
		}
	}

	/** Create the configuration for the virtual machine.
	 *
	 * @param monitor the progress monitor.
	 * @throws CoreException if a parameter cannot be extracted.
	 */
	@SuppressWarnings({"deprecation", "checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity"})
	protected void createRunConfiguration(IProgressMonitor monitor) throws CoreException {
		monitor.subTask(Messages.AbstractLaunchProcess_3);

		final AbstractSARLLaunchConfiguration owner = getOwner();

		final String[] classpath;
		final String[] modulepath;
		if (owner.supportsModularProjectAndLauncher(this.configuration)) {
			final String[][] paths = owner.getClasspathAndModulepath(this.configuration);
			if (paths != null && paths[0] != null) {
				classpath = paths[0];
			} else {
				classpath = owner.getClasspath(this.configuration);
			}
			if (paths != null && paths.length > 1) {
				modulepath = paths[1];
			} else {
				modulepath = null;
			}
		} else {
			classpath = owner.getClasspath(this.configuration);
			modulepath = null;
		}

		final VMRunnerConfiguration cfg = new VMRunnerConfiguration(getMainTypeName(), classpath);

		cfg.setProgramArguments(getExecutionArguments().getProgramArgumentsArray());
		cfg.setEnvironment(getEnvironment());
		cfg.setVMArguments(getExecutionArguments().getVMArgumentsArray());
		cfg.setWorkingDirectory(getWorkingDirectory());
		cfg.setVMSpecificAttributesMap(getVirtualMachineAttributes());
		cfg.setPreviewEnabled(owner.supportsPreviewFeatures(this.configuration));

		// Module name not required for Scrapbook page
		if (owner.supportsModule()
				&& !getMainTypeName().equals("org.eclipse.jdt.internal.debug.ui.snippeteditor.ScrapbookMain")) { //$NON-NLS-1$
			// Module name need not be the same as project name
			try {
				final IJavaProject proj = JavaRuntime.getJavaProject(this.configuration);
				if (proj != null) {
					final IModuleDescription module = proj == null ? null : proj.getModuleDescription();
					final String modName = module == null ? null : module.getElementName();
					if (modName != null && modName.length() > 0) {
						final String moduleName = this.configuration.getAttribute(
								IJavaLaunchConfigurationConstants.ATTR_MODULE_NAME, (String) null);
						if (moduleName != null) {
							cfg.setModuleDescription(moduleName);
						} else {
							cfg.setModuleDescription(modName);
						}
					}
				}
			} catch (CoreException e) {
				// Not a java Project so no need to set module description
			}
		}

		// Launch Configuration should be launched by Java 9 or above for modulepath setting
		if (!JavaRuntime.isModularConfiguration(this.configuration)) {
			// Bootpath
			cfg.setBootClassPath(getOwner().getBootpath(this.configuration));
		} else if (owner.supportsModule()) {
			// module path
			if (modulepath != null) {
				cfg.setModulepath(modulepath);
			}
			if (!this.configuration.getAttribute(IJavaLaunchConfigurationConstants.ATTR_DEFAULT_MODULE_CLI_OPTIONS, true)) {
				cfg.setOverrideDependencies(this.configuration.getAttribute(IJavaLaunchConfigurationConstants.ATTR_MODULE_CLI_OPTIONS, "")); //$NON-NLS-1$
			} else {
				cfg.setOverrideDependencies(owner.getModuleCLIOptions(this.configuration));
			}
		}
		cfg.setMergeOutput(this.configuration.getAttribute(DebugPlugin.ATTR_MERGE_OUTPUT, false));

		setVirtualMachineRunnerConfiguration(cfg);
	}

	/** Configure the "stop in main" feature.
	 *
	 * @param monitor the progress monitor.
	 * @throws CoreException if a parameter cannot be extracted.
	 */
	@SuppressWarnings("synthetic-access")
	protected void configureStopInMain(IProgressMonitor monitor) throws CoreException {
		monitor.subTask(
				Messages.AbstractLaunchProcess_4);
		getOwner().prepareStopInMain(this.configuration);
	}

	@Override
	public boolean launch(IProgressMonitor monitor) throws CoreException {
		switch (this.runState) {
		case STEP_0_CONFIGURE_SOURCE_LOCATOR:
			configureSourceLocator(monitor);
			break;
		case STEP_1_RUN:
		default:
			launchRunner(monitor);
			return false;
		}
		this.runState = this.runState.next();
		return true;
	}

	/** Configure the source locator for run-time.
	 *
	 * @param monitor the progress monitor.
	 * @throws CoreException if a parameter cannot be extracted.
	 */
	@SuppressWarnings("synthetic-access")
	private void configureSourceLocator(IProgressMonitor monitor) throws CoreException {
		monitor.subTask(LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_Creating_source_locator____2);
		getOwner().setDefaultSourceLocator(this.launch, this.configuration);
	}

	/** Run.
	 *
	 * @param monitor the progress monitor.
	 * @throws CoreException if a parameter cannot be extracted.
	 */
	protected void launchRunner(IProgressMonitor monitor) throws CoreException {
		monitor.subTask(
				MessageFormat.format(Messages.AbstractLaunchProcess_5, this.configuration.getName()));
		getRunner().run(getVirtualMachineRunnerConfiguration(), this.launch, monitor);
	}

}

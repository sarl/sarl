/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.launching;

import io.sarl.eclipse.SARLConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.ProjectSREProvider;
import io.sarl.eclipse.runtime.ProjectSREProviderFactory;
import io.sarl.eclipse.runtime.SARLRuntime;

import java.io.File;
import java.lang.ref.SoftReference;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.variables.VariablesPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.launching.JRERuntimeClasspathEntryResolver;
import org.eclipse.jdt.internal.launching.LaunchingMessages;
import org.eclipse.jdt.internal.launching.RuntimeClasspathEntry;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.ExecutionArguments;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMRunner;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.launching.LibraryLocation;
import org.eclipse.jdt.launching.VMRunnerConfiguration;
import org.eclipse.xtext.xbase.lib.Pair;

import com.google.common.base.Strings;

/**
 * Implementation of an eclipse LauncConfigurationDelegate to launch SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLLaunchConfigurationDelegate extends AbstractJavaLaunchConfigurationDelegate {

	private SoftReference<IRuntimeClasspathEntry[]> unresolvedClasspathEntries;
	private SoftReference<String[]> classpathEntries;

	/**
	 */
	public SARLLaunchConfigurationDelegate() {
		//
	}

	private synchronized void clearBuffers() {
		this.unresolvedClasspathEntries = null;
		this.classpathEntries = null;
	}

	@Override
	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		try {
			LaunchProcess process = new LaunchProcess(configuration, mode, launch);
			// Preparation
			SubMonitor progressMonitor = SubMonitor.convert(
					monitor,
					MessageFormat.format(Messages.SARLLaunchConfigurationDelegate_1,
							configuration.getName()),
					process.getStepNumber());
			while (process.prepare(progressMonitor.newChild(1))) {
				if (progressMonitor.isCanceled()) {
					return;
				}
			}

			// Launching
			while (process.launch(progressMonitor.newChild(1))) {
				if (progressMonitor.isCanceled()) {
					return;
				}
			}
		} finally {
			// Clear cached entries
			clearBuffers();
		}
	}

	/**
	 * Returns the main type name specified by the given launch configuration,
	 * or <code>null</code> if none.
	 *
	 * @param configuration - launch configuration
	 * @return the main type name specified by the given launch configuration,
	 *         or <code>null</code> if none
	 * @throws CoreException if unable to retrieve the attribute
	 */
	@SuppressWarnings("static-method")
	protected String getAgentName(ILaunchConfiguration configuration) throws CoreException {
		String agentName = configuration.getAttribute(
				SARLConfig.ATTR_AGENT_NAME,
				(String) null);
		if (agentName == null) {
			return null;
		}
		return VariablesPlugin.getDefault().getStringVariableManager()
				.performStringSubstitution(agentName);
	}

	/**
	 * Verifies a main type name is specified by the given launch configuration,
	 * and returns the main type name.
	 *
	 * @param configuration - launch configuration
	 * @return the main type name specified by the given launch configuration
	 * @throws CoreException if unable to retrieve the attribute or the attribute is
	 * unspecified
	 */
	protected String verifyAgentName(ILaunchConfiguration configuration) throws CoreException {
		String name = getAgentName(configuration);
		if (name == null) {
			abort(
					Messages.MainLaunchConfigurationTab_2,
					null,
					SARLConfig.ERR_UNSPECIFIED_AGENT_NAME);
		}
		return VariablesPlugin.getDefault().getStringVariableManager()
				.performStringSubstitution(name);
	}

	/** Copied from JDT's super class, and patched for invoking
	 * {@link #computeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration)}.
	 * {@inheritDoc}
	 */
	@Override
	public String[] getClasspath(ILaunchConfiguration configuration) throws CoreException {
		String[] userEntries = null;
		synchronized (this) {
			if (this.classpathEntries != null) {
				userEntries = this.classpathEntries.get();
			}
		}
		if (userEntries != null) {
			return userEntries;
		}

		IRuntimeClasspathEntry[] entries = computeUnresolvedSARLRuntimeClasspath(configuration);
		entries = JavaRuntime.resolveRuntimeClasspath(entries, configuration);

		boolean isMavenProject = getJavaProject(configuration).getProject().hasNature(SARLConfig.MAVEN_NATURE_ID);
		boolean needSREEntry = isMavenProject;

		// Store in a list for preserving the order of the entries.
		List<String> userEntryList = new ArrayList<>(entries.length + 1);
		Set<String> set = new TreeSet<>();
		for (int i = 0; i < entries.length; i++) {
			if (entries[i].getClasspathProperty() == IRuntimeClasspathEntry.USER_CLASSES) {
				String location = entries[i].getLocation();
				if (location != null && !set.contains(location)) {
					userEntryList.add(location);
					set.add(location);
					if (needSREEntry) {
						needSREEntry = isNotSREEntry(entries[i]);
					}
				}
			}
		}

		if (needSREEntry) {
			int insertIndex = 0;
			for (IRuntimeClasspathEntry entry : getSREClasspathEntries(configuration)) {
				if (entry.getClasspathProperty() == IRuntimeClasspathEntry.USER_CLASSES) {
					String location = entry.getLocation();
					if (location != null && !set.contains(location)) {
						userEntryList.add(insertIndex, location);
						set.add(location);
						++insertIndex;
					}
				}
			}
		}

		userEntries = userEntryList.toArray(new String[userEntryList.size()]);
		synchronized (this) {
			this.classpathEntries = new SoftReference<>(userEntries);
		}
		return userEntries;
	}

	private static Pair<IRuntimeClasspathEntry, Integer>
	getJreEntry(IRuntimeClasspathEntry[] entries,
			List<IRuntimeClasspathEntry> bootEntriesPrepend) {
		int index = 0;
		IRuntimeClasspathEntry jreEntry = null;
		while (jreEntry == null && index < entries.length) {
			IRuntimeClasspathEntry entry = entries[index++];
			if (entry.getClasspathProperty() == IRuntimeClasspathEntry.BOOTSTRAP_CLASSES
					|| entry.getClasspathProperty() == IRuntimeClasspathEntry.STANDARD_CLASSES) {
				if (JavaRuntime.isVMInstallReference(entry)) {
					jreEntry = entry;
				} else {
					bootEntriesPrepend.add(entry);
				}
			}
		}
		return Pair.of(jreEntry, index);
	}

	private void getBootpathExtForJRE(ILaunchConfiguration configuration,
			IRuntimeClasspathEntry[] entries, IRuntimeClasspathEntry jreEntry, int idx,
			String[] entriesPrep, IRuntimeClasspathEntry[] bootEntriesPrep, String[][] bootpathInfo)
					throws CoreException {
		int index = idx;
		List<IRuntimeClasspathEntry> bootEntriesAppend = new ArrayList<>();
		for (; index < entries.length; index++) {
			IRuntimeClasspathEntry entry = entries[index];
			if (entry.getClasspathProperty() == IRuntimeClasspathEntry.BOOTSTRAP_CLASSES) {
				bootEntriesAppend.add(entry);
			}
		}
		bootpathInfo[0] = entriesPrep;
		IRuntimeClasspathEntry[] bootEntriesApp = JavaRuntime
				.resolveRuntimeClasspath(
						bootEntriesAppend
						.toArray(new IRuntimeClasspathEntry[bootEntriesAppend
						                                    .size()]), configuration);
		if (bootEntriesApp.length > 0) {
			bootpathInfo[2] = new String[bootEntriesApp.length];
			for (int i = 0; i < bootEntriesApp.length; i++) {
				bootpathInfo[2][i] = bootEntriesApp[i].getLocation();
			}
		}
		IVMInstall install = getVMInstall(configuration);
		LibraryLocation[] libraryLocations = install.getLibraryLocations();
		if (libraryLocations != null) {
			// determine if explicit bootpath should be used
			if (!JRERuntimeClasspathEntryResolver.isSameArchives(libraryLocations,
					install.getVMInstallType().getDefaultLibraryLocations(install.getInstallLocation()))) {
				// resolve bootpath entries in JRE entry
				IRuntimeClasspathEntry[] bootEntries = null;
				if (jreEntry.getType() == IRuntimeClasspathEntry.CONTAINER) {
					IRuntimeClasspathEntry bootEntry = JavaRuntime.newRuntimeContainerClasspathEntry(
							jreEntry.getPath(),
							IRuntimeClasspathEntry.BOOTSTRAP_CLASSES,
							getJavaProject(configuration));
					bootEntries = JavaRuntime.resolveRuntimeClasspathEntry(bootEntry, configuration);
				} else {
					bootEntries = JavaRuntime.resolveRuntimeClasspathEntry(jreEntry, configuration);
				}

				// non-default JRE libraries - use explicit bootpath only
				String[] bootpath = new String[bootEntriesPrep.length
				                               + bootEntries.length + bootEntriesApp.length];
				if (bootEntriesPrep.length > 0) {
					System.arraycopy(bootpathInfo[0], 0, bootpath, 0,
							bootEntriesPrep.length);
				}
				int dest = bootEntriesPrep.length;
				for (int i = 0; i < bootEntries.length; i++) {
					bootpath[dest] = bootEntries[i].getLocation();
					dest++;
				}
				if (bootEntriesApp.length > 0) {
					System.arraycopy(bootpathInfo[2], 0, bootpath, dest,
							bootEntriesApp.length);
				}
				bootpathInfo[0] = null;
				bootpathInfo[1] = bootpath;
				bootpathInfo[2] = null;
			}
		}
	}

	/** Copied from JDT's super class, and patched for invoking
	 * {@link #computeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration)}.
	 * {@inheritDoc}
	 */
	@Override
	public String[][] getBootpathExt(ILaunchConfiguration configuration)
			throws CoreException {
		String[][] bootpathInfo = new String[3][];
		IRuntimeClasspathEntry[] entries = computeUnresolvedSARLRuntimeClasspath(configuration);
		List<IRuntimeClasspathEntry> bootEntriesPrepend = new ArrayList<>();
		IRuntimeClasspathEntry jreEntry;
		int index;
		Pair<IRuntimeClasspathEntry, Integer> pair = getJreEntry(entries, bootEntriesPrepend);
		jreEntry = pair.getKey();
		index = pair.getValue();
		IRuntimeClasspathEntry[] bootEntriesPrep = JavaRuntime
				.resolveRuntimeClasspath(
						bootEntriesPrepend
						.toArray(new IRuntimeClasspathEntry[bootEntriesPrepend
						                                    .size()]), configuration);
		String[] entriesPrep = null;
		if (bootEntriesPrep.length > 0) {
			entriesPrep = new String[bootEntriesPrep.length];
			for (int i = 0; i < bootEntriesPrep.length; i++) {
				entriesPrep[i] = bootEntriesPrep[i].getLocation();
			}
		}
		if (jreEntry != null) {
			getBootpathExtForJRE(configuration, entries, jreEntry, index, entriesPrep, bootEntriesPrep, bootpathInfo);
		} else {
			if (entriesPrep == null) {
				bootpathInfo[1] = new String[0];
			} else {
				bootpathInfo[1] = entriesPrep;
			}
		}
		return bootpathInfo;
	}

	/** Copied from JDT's super class, and patched for invoking
	 * {@link #computeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration)}.
	 * {@inheritDoc}
	 */
	@Override
	public String[] getBootpath(ILaunchConfiguration configuration) throws CoreException {
		String[][] paths = getBootpathExt(configuration);
		String[] pre = paths[0];
		String[] main = paths[1];
		String[] app = paths[2];
		if (pre == null && main == null && app == null) {
			// default
			return null;
		}
		IRuntimeClasspathEntry[] entries = computeUnresolvedSARLRuntimeClasspath(configuration);
		entries = JavaRuntime.resolveRuntimeClasspath(entries, configuration);
		List<String> bootEntries = new ArrayList<>(entries.length);
		boolean empty = true;
		boolean allStandard = true;
		for (int i = 0; i < entries.length; i++) {
			if (entries[i].getClasspathProperty() != IRuntimeClasspathEntry.USER_CLASSES) {
				String location = entries[i].getLocation();
				if (location != null) {
					empty = false;
					bootEntries.add(location);
					allStandard = allStandard
							&& entries[i].getClasspathProperty() == IRuntimeClasspathEntry.STANDARD_CLASSES;
				}
			}
		}
		if (empty) {
			return new String[0];
		} else if (allStandard) {
			return null;
		} else {
			return bootEntries.toArray(new String[bootEntries.size()]);
		}
	}

	private static ISREInstall getSREFromExtension(IProject project, boolean verify) {
		IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(
				SARLEclipsePlugin.PLUGIN_ID,
				SARLConfig.EXTENSION_POINT_PROJECT_SRE_PROVIDER_FACTORY);
		if (extensionPoint != null) {
			for (IConfigurationElement element : extensionPoint.getConfigurationElements()) {
				try {
					Object obj = element.createExecutableExtension("class"); //$NON-NLS-1$
					assert (obj instanceof ProjectSREProviderFactory);
					ProjectSREProviderFactory factory = (ProjectSREProviderFactory) obj;
					ProjectSREProvider provider = factory.getProjectSREProvider(project);
					if (provider != null) {
						ISREInstall sre = provider.getProjectSREInstall();
						if (sre == null) {
							return null;
						}
						if (verify) {
							verifySREValidity(sre, sre.getId(), false);
						}
						return sre;
					}
				} catch (CoreException e) {
					SARLEclipsePlugin.log(e);
				}
			}
		}
		return null;
	}

	/** Replies the project SRE from the given configuration.
	 *
	 * @param configuration - the configuration to read.
	 * @return the project SRE or <code>null</code>.
	 */
	private ISREInstall getProjectSpecificSRE(ILaunchConfiguration configuration, boolean verify) throws CoreException {
		IJavaProject jprj = getJavaProject(configuration);
		if (jprj != null) {
			IProject prj = jprj.getProject();
			assert (prj != null);

			// Get the SRE from the extension point
			ISREInstall sre = getSREFromExtension(prj, verify);
			if (sre != null) {
				return null;
			}

			// Get the SRE from the default project configuration
			ProjectSREProvider provider = new StandardProjectSREProvider(prj);
			sre = provider.getProjectSREInstall();
			if (sre != null) {
				if (verify) {
					verifySREValidity(sre, sre.getId(), true);
				}
				return sre;
			}
		}
		ISREInstall sre = SARLRuntime.getDefaultSREInstall();
		if (verify) {
			verifySREValidity(sre, (sre == null) ? Messages.SARLLaunchConfigurationDelegate_8 : sre.getId(), true);
		}
		return sre;
	}

	/** Replies the SRE installation to be used for the given configuration.
	 *
	 * @param configuration - the configuration to check.
	 * @return the SRE install.
	 * @throws CoreException if impossible to get the SRE.
	 */
	private ISREInstall getSREInstallFor(ILaunchConfiguration configuration) throws CoreException {
		String useSystemSRE = configuration.getAttribute(
				SARLConfig.ATTR_USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT,
				Boolean.TRUE.toString());
		String useProjectSRE = configuration.getAttribute(
				SARLConfig.ATTR_USE_PROJECT_SARL_RUNTIME_ENVIRONMENT,
				Boolean.FALSE.toString());
		ISREInstall sre = null;
		if (Boolean.parseBoolean(useSystemSRE)) {
			sre = SARLRuntime.getDefaultSREInstall();
			verifySREValidity(sre, sre.getId(), true);
		} else if (Boolean.parseBoolean(useProjectSRE)) {
			sre = getProjectSpecificSRE(configuration, true);
		} else  {
			String runtime = configuration.getAttribute(SARLConfig.ATTR_SARL_RUNTIME_ENVIRONMENT, (String) null);
			sre = SARLRuntime.getSREFromId(runtime);
			verifySREValidity(sre, runtime, true);
		}

		if (sre == null) {
			throw new CoreException(SARLEclipsePlugin.createStatus(IStatus.ERROR,
					Messages.SARLLaunchConfigurationDelegate_0));
		}

		return sre;
	}

	/** Replies the classpath entries associated to the SRE of the given configuration.
	 *
	 * @param configuration - the configuration to read.
	 * @return the classpath entries for the SRE associated to the configuration.
	 * @throws CoreException if impossible to determine the classpath entries.
	 */
	private List<IRuntimeClasspathEntry> getSREClasspathEntries(
			ILaunchConfiguration configuration) throws CoreException {
		ISREInstall sre = getSREInstallFor(configuration);
		LibraryLocation[] locations = sre.getLibraryLocations();
		List<IRuntimeClasspathEntry> sreClasspathEntries = new ArrayList<>(locations.length);
		for (int i = 0; i < locations.length; ++i) {
			LibraryLocation location = locations[i];
			IClasspathEntry cpEntry = JavaCore.newLibraryEntry(
					location.getSystemLibraryPath(),
					location.getSystemLibrarySourcePath(),
					location.getPackageRootPath());
			IRuntimeClasspathEntry rtcpEntry = new RuntimeClasspathEntry(cpEntry);
			// No more a bootstrap library for enabling it to be in the classpath (not the JVM bootstrap).
			rtcpEntry.setClasspathProperty(IRuntimeClasspathEntry.USER_CLASSES);
			sreClasspathEntries.add(rtcpEntry);
		}
		return sreClasspathEntries;
	}

	/** Replies if the given classpath entry is a SRE.
	 *
	 * @param entry - the entry.
	 * @return <code>true</code> if the entry points to a SRE;
	 * <code>false</code> otherwise.
	 */
	private static boolean isNotSREEntry(IRuntimeClasspathEntry entry) {
		try {
			File file = new File(entry.getLocation());
			if (file.isDirectory()) {
				return !SARLRuntime.isUnpackedSRE(file);
			} else if (file.canRead()) {
				return !SARLRuntime.isPackedSRE(file);
			}
		} catch (Throwable e) {
			SARLEclipsePlugin.log(e);
		}
		return true;
	}

	/** Replies the class path for the SARL application.
	 *
	 * @param configuration - the configuration that provides the classpath.
	 * @return the filtered entries.
	 * @throws CoreException if impossible to get the classpath.
	 */
	private IRuntimeClasspathEntry[] computeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration configuration)
			throws CoreException {
		// Get the buffered entries
		IRuntimeClasspathEntry[] entries = null;
		synchronized (this) {
			if (this.unresolvedClasspathEntries != null) {
				entries = this.unresolvedClasspathEntries.get();
			}
		}
		if (entries != null) {
			return entries;
		}
		// Get the classpath from the configuration.
		entries = JavaRuntime.computeUnresolvedRuntimeClasspath(configuration);
		//
		List<IRuntimeClasspathEntry> filteredEntries = new ArrayList<>();
		List<IRuntimeClasspathEntry> sreClasspathEntries = null;
		// Filtering the entries by replacing the "SARL Libraries" with the SARL runtime environment.
		for (IRuntimeClasspathEntry entry : entries) {
			if (entry.getPath().equals(SARLClasspathContainerInitializer.CONTAINER_ID)) {
				if (sreClasspathEntries == null) {
					sreClasspathEntries = getSREClasspathEntries(configuration);
				}
				filteredEntries.addAll(sreClasspathEntries);
			} else {
				filteredEntries.add(entry);
			}
		}
		entries = filteredEntries.toArray(new IRuntimeClasspathEntry[filteredEntries.size()]);
		//
		synchronized (this) {
			this.unresolvedClasspathEntries = new SoftReference<>(entries);
		}
		return entries;
	}

	private static void verifySREValidity(ISREInstall sre, String runtime, boolean onlyStandalone) throws CoreException {
		if (sre == null) {
			throw new CoreException(SARLEclipsePlugin.createStatus(IStatus.ERROR,
					MessageFormat.format(Messages.RuntimeEnvironmentTab_6, runtime)));
		}
		int ignoreCode = 0;
		if (!onlyStandalone) {
			ignoreCode = ISREInstall.CODE_STANDALONE_SRE;
		}
		if (!sre.getValidity(ignoreCode).isOK()) {
			throw new CoreException(SARLEclipsePlugin.createStatus(IStatus.ERROR, MessageFormat.format(
					Messages.RuntimeEnvironmentTab_5,
					sre.getName())));
		}
	}

	/** Definition of the launching process splitted in separated steps for
	 * making easier the cancellation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class LaunchProcess {

		private final ILaunchConfiguration configuration;
		private final String mode;
		private final ILaunch launch;

		private PreparationProcessState preparationState = PreparationProcessState.STEP_0;
		private RunProcessState runState = RunProcessState.STEP_0;

		private String mainTypeName;
		private String agentName;
		private IVMRunner runner;
		private String workingDirName;
		private String[] envp;
		private ExecutionArguments execArgs;
		private Map<String, Object> vmAttributesMap;
		private String[] classpath;
		private VMRunnerConfiguration runConfig;

		/**
		 * @param configuration - the launch configuration.
		 * @param mode - the launching mode.
		 * @param launch - the launching
		 */
		public LaunchProcess(ILaunchConfiguration configuration, String mode, ILaunch launch) {
			this.configuration = configuration;
			this.mode = mode;
			this.launch = launch;
		}

		@SuppressWarnings("synthetic-access")
		private void readConfigurationParameters(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_Verifying_launch_attributes____1);

			// Clear cached entries
			clearBuffers();

			this.mainTypeName = verifyMainTypeName(this.configuration);
			this.agentName = verifyAgentName(this.configuration);
			this.runner = getVMRunner(this.configuration, this.mode);

			File workingDir = verifyWorkingDirectory(this.configuration);
			this.workingDirName = null;
			if (workingDir != null) {
				this.workingDirName = workingDir.getAbsolutePath();
			}

			// Environment variables
			this.envp = getEnvironment(this.configuration);
		}

		private void readLaunchingArguments(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					Messages.SARLLaunchConfigurationDelegate_2);

			// Program & VM arguments
			String pgmArgs = getProgramArguments(this.configuration);
			if (!Strings.isNullOrEmpty(pgmArgs)) {
				pgmArgs = this.agentName + " " + pgmArgs; //$NON-NLS-1$
			} else {
				pgmArgs = this.agentName;
			}
			String vmArgs = getVMArguments(this.configuration);
			this.execArgs = new ExecutionArguments(vmArgs, pgmArgs);

			// VM-specific attributes
			this.vmAttributesMap = getVMSpecificAttributesMap(this.configuration);
		}

		private void buildClasspath(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					Messages.SARLLaunchConfigurationDelegate_3);
			this.classpath = getClasspath(this.configuration);
		}

		private void createRunConfiguration(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					Messages.SARLLaunchConfigurationDelegate_4);
			this.runConfig = new VMRunnerConfiguration(this.mainTypeName, this.classpath);
			this.runConfig.setProgramArguments(this.execArgs.getProgramArgumentsArray());
			this.runConfig.setEnvironment(this.envp);
			this.runConfig.setVMArguments(this.execArgs.getVMArgumentsArray());
			this.runConfig.setWorkingDirectory(this.workingDirName);
			this.runConfig.setVMSpecificAttributesMap(this.vmAttributesMap);
			this.runConfig.setBootClassPath(getBootpath(this.configuration));
		}

		@SuppressWarnings("synthetic-access")
		private void configureStopInMain(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					Messages.SARLLaunchConfigurationDelegate_5);
			prepareStopInMain(this.configuration);
		}

		/** Replies the total number of steps.
		 *
		 * @return the total number of steps.
		 */
		public int getStepNumber() {
			return PreparationProcessState.values().length + RunProcessState.values().length;
		}

		/** Run a preparation step of the launching process.
		 *
		 * @param monitor - the progression monitor.
		 * @return <code>true</code> if something more must be done; otherwise <code>false</code>.
		 * @throws CoreException if something cannot be done.
		 */
		public boolean prepare(IProgressMonitor monitor) throws CoreException {
			switch (this.preparationState) {
			case STEP_0:
				readConfigurationParameters(monitor);
				break;
			case STEP_1:
				readLaunchingArguments(monitor);
				break;
			case STEP_2:
				buildClasspath(monitor);
				break;
			case STEP_3:
				postValidation(monitor);
				break;
			case STEP_4:
				createRunConfiguration(monitor);
				break;
			case STEP_5:
			default:
				configureStopInMain(monitor);
				return false;
			}
			this.preparationState = this.preparationState.next();
			return true;
		}

		@SuppressWarnings("synthetic-access")
		private void postValidation(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					Messages.SARLLaunchConfigurationDelegate_7);
			if (Strings.isNullOrEmpty(this.mainTypeName)) {
				// This case occurs when the launch configuration is using
				// a SRE that is inside the classpath.
				// The name of the main class is then no saved in the launch configuration properties.
				ISREInstall sre = getSREInstallFor(this.configuration);
				if (sre != null) {
					this.mainTypeName = sre.getMainClass();
				}
			}
		}

		@SuppressWarnings("synthetic-access")
		private void configureSourceLocator(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_Creating_source_locator____2);
			setDefaultSourceLocator(this.launch, this.configuration);
		}

		private void launchRunner(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					MessageFormat.format(Messages.SARLLaunchConfigurationDelegate_6, this.configuration.getName()));
			this.runner.run(this.runConfig, this.launch, monitor);
		}

		/** Run a launching step of the launching process.
		 *
		 * @param monitor - the progression monitor.
		 * @return <code>true</code> if something more must be done; otherwise <code>false</code>.
		 * @throws CoreException if something cannot be done.
		 */
		public boolean launch(IProgressMonitor monitor) throws CoreException {
			switch (this.runState) {
			case STEP_0:
				configureSourceLocator(monitor);
				break;
			case STEP_1:
			default:
				launchRunner(monitor);
				return false;
			}
			this.runState = this.runState.next();
			return true;
		}

	}

	/** Steps of preparation in the launching process.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static enum PreparationProcessState {
		STEP_0, STEP_1, STEP_2, STEP_3, STEP_4, STEP_5;

		public PreparationProcessState next() {
			int index = ordinal() + 1;
			PreparationProcessState[] vals = values();
			if (index < vals.length) {
				return vals[index];
			}
			return this;
		}

	}

	/** Steps of run in the launching process.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static enum RunProcessState {
		STEP_0, STEP_1;

		public RunProcessState next() {
			int index = ordinal() + 1;
			RunProcessState[] vals = values();
			if (index < vals.length) {
				return vals[index];
			}
			return this;
		}

	}

}

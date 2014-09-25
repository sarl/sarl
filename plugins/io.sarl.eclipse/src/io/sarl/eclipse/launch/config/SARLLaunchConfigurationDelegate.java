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
package io.sarl.eclipse.launch.config;

import io.sarl.eclipse.builder.SARLClasspathContainer;
import io.sarl.eclipse.launch.sre.ISREInstall;
import io.sarl.eclipse.launch.sre.SARLRuntime;
import io.sarl.eclipse.util.PluginUtil;

import java.io.File;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.variables.VariablesPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jdt.core.IClasspathEntry;
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
import org.eclipse.osgi.util.NLS;
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

	/**
	 */
	public SARLLaunchConfigurationDelegate() {
		//
	}

	@Override
	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		IProgressMonitor progressMonitor;
		if (monitor == null) {
			progressMonitor = new NullProgressMonitor();
		} else {
			progressMonitor = monitor;
		}

		progressMonitor.beginTask(NLS.bind("{0}...", //$NON-NLS-1$
				new String[] {configuration.getName()}), 3);
		// check for cancellation
		if (progressMonitor.isCanceled()) {
			return;
		}
		try {
			progressMonitor.subTask(
					LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_Verifying_launch_attributes____1);

			String mainTypeName = verifyMainTypeName(configuration);
			String agentName = verifyAgentName(configuration);
			IVMRunner runner = getVMRunner(configuration, mode);

			File workingDir = verifyWorkingDirectory(configuration);
			String workingDirName = null;
			if (workingDir != null) {
				workingDirName = workingDir.getAbsolutePath();
			}

			// Environment variables
			String[] envp = getEnvironment(configuration);

			// Program & VM arguments
			String pgmArgs = getProgramArguments(configuration);
			if (!Strings.isNullOrEmpty(pgmArgs)) {
				pgmArgs = agentName + " " + pgmArgs; //$NON-NLS-1$
			} else {
				pgmArgs = agentName;
			}
			String vmArgs = getVMArguments(configuration);
			ExecutionArguments execArgs = new ExecutionArguments(vmArgs, pgmArgs);

			// VM-specific attributes
			Map<String, Object> vmAttributesMap = getVMSpecificAttributesMap(configuration);

			// Classpath
			String[] classpath = getClasspath(configuration);

			// Create VM config
			VMRunnerConfiguration runConfig = new VMRunnerConfiguration(mainTypeName, classpath);
			runConfig.setProgramArguments(execArgs.getProgramArgumentsArray());
			runConfig.setEnvironment(envp);
			runConfig.setVMArguments(execArgs.getVMArgumentsArray());
			runConfig.setWorkingDirectory(workingDirName);
			runConfig.setVMSpecificAttributesMap(vmAttributesMap);

			// Bootpath
			runConfig.setBootClassPath(getBootpath(configuration));

			// check for cancellation
			if (progressMonitor.isCanceled()) {
				return;
			}

			// stop in main
			prepareStopInMain(configuration);

			// done the verification phase
			progressMonitor.worked(1);

			progressMonitor.subTask(
					LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_Creating_source_locator____2);
			// set the default source locator if required
			setDefaultSourceLocator(launch, configuration);
			progressMonitor.worked(1);

			// Launch the configuration - 1 unit of work
			runner.run(runConfig, launch, monitor);

			// check for cancellation
			if (progressMonitor.isCanceled()) {
				return;
			}
		} finally {
			progressMonitor.done();
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
	public String getAgentName(ILaunchConfiguration configuration) throws CoreException {
		String agentName = configuration.getAttribute(
				LaunchConfigurationConstants.ATTR_AGENT_NAME,
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
	public String verifyAgentName(ILaunchConfiguration configuration) throws CoreException {
		String name = getAgentName(configuration);
		if (name == null) {
			abort(
					Messages.MainLaunchConfigurationTab_2,
					null,
					LaunchConfigurationConstants.ERR_UNSPECIFIED_AGENT_NAME);
		}
		return name;
	}

	/** Copied from JDT's super class, and patched for invoking
	 * {@link #computeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration)}.
	 * {@inheritDoc}
	 */
	@Override
	public String[] getClasspath(ILaunchConfiguration configuration) throws CoreException {
		// Specific to SARL launch configuration
		IRuntimeClasspathEntry[] entries = computeUnresolvedSARLRuntimeClasspath(configuration);

		entries = JavaRuntime.resolveRuntimeClasspath(entries, configuration);
		List<String> userEntries = new ArrayList<>(entries.length);
		Set<String> set = new HashSet<>(entries.length);
		for (int i = 0; i < entries.length; i++) {
			if (entries[i].getClasspathProperty() == IRuntimeClasspathEntry.USER_CLASSES) {
				String location = entries[i].getLocation();
				if (location != null) {
					if (!set.contains(location)) {
						userEntries.add(location);
						set.add(location);
					}
				}
			}
		}
		return userEntries.toArray(new String[userEntries.size()]);
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
		// Specific to SARL launch configuration
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
		// Specific to SARL launch configuration
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
			return bootEntries
					.toArray(new String[bootEntries.size()]);
		}
	}

	/** Replies the class path for the SARL application.
	 *
	 * @param configuration - the configuration that provides the classpath.
	 * @return the filtered entries.
	 * @throws CoreException if impossible to get the classpath.
	 */
	protected IRuntimeClasspathEntry[] computeUnresolvedSARLRuntimeClasspath(
			ILaunchConfiguration configuration) throws CoreException {
		// Retrieve the SARL runtime environment jar file.
		String useDefaultSRE = configuration.getAttribute(
				LaunchConfigurationConstants.ATTR_USE_SARL_RUNTIME_ENVIRONMENT,
				Boolean.TRUE.toString());
		String runtime = null;
		if (Boolean.parseBoolean(useDefaultSRE)) {
			ISREInstall sre = SARLRuntime.getDefaultSREInstall();
			if (sre != null) {
				runtime = sre.getId();
			}
		} else  {
			runtime = configuration.getAttribute(LaunchConfigurationConstants.ATTR_SARL_RUNTIME_ENVIRONMENT, (String) null);
		}

		if (Strings.isNullOrEmpty(runtime)) {
			throw new CoreException(PluginUtil.createStatus(IStatus.ERROR,
					Messages.SARLLaunchConfigurationDelegate_0));
		}

		ISREInstall sre = SARLRuntime.getSREFromId(runtime);
		if (sre == null) {
			throw new CoreException(PluginUtil.createStatus(IStatus.ERROR,
					MessageFormat.format(Messages.RuntimeEnvironmentTab_6, runtime)));
		}

		verifySREValidity(sre);

		LibraryLocation[] locations = sre.getLibraryLocations();
		List<IRuntimeClasspathEntry> cpEntries = new ArrayList<>(locations.length);
		for (int i = 0; i < locations.length; ++i) {
			LibraryLocation location = locations[i];
			IClasspathEntry cpEntry = JavaCore.newLibraryEntry(
					location.getSystemLibraryPath(),
					location.getSystemLibrarySourcePath(),
					location.getPackageRootPath());
			IRuntimeClasspathEntry rtcpEntry = new RuntimeClasspathEntry(cpEntry);
			// No more a bootstrap library for enabling it to be in the classpath (not the JVM bootstrap).
			rtcpEntry.setClasspathProperty(IRuntimeClasspathEntry.USER_CLASSES);
			cpEntries.add(rtcpEntry);
		}

		// Get the classpath from the configuration.
		IRuntimeClasspathEntry[] entries = JavaRuntime.computeUnresolvedRuntimeClasspath(configuration);
		// Filtering the entries by replacing the "SARL Libraries" with the SARL runtime environment.
		List<IRuntimeClasspathEntry> filteredEntries = new ArrayList<>();
		for (IRuntimeClasspathEntry entry : entries) {
			if (entry.getPath().equals(SARLClasspathContainer.CONTAINER_ID)) {
				filteredEntries.addAll(cpEntries);
			} else {
				filteredEntries.add(entry);
			}
		}
		entries = filteredEntries.toArray(new IRuntimeClasspathEntry[filteredEntries.size()]);
		return entries;
	}

	@SuppressWarnings("static-method")
	private void verifySREValidity(ISREInstall sre) throws CoreException {
		if (!sre.getValidity().isOK()) {
			throw new CoreException(PluginUtil.createStatus(IStatus.ERROR, MessageFormat.format(
					Messages.RuntimeEnvironmentTab_5,
					sre.getName())));
		}
	}

}

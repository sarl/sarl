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
import java.lang.ref.SoftReference;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import javax.inject.Inject;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.variables.IStringVariableManager;
import org.eclipse.core.variables.VariablesPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jdt.internal.launching.JRERuntimeClasspathEntryResolver;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMRunner;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.launching.LibraryLocation;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.runner.general.SrePathUtils.ExtraClassPathProviders;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.SARLRuntime;

/**
 * Abstract Implementation of an eclipse LauncConfigurationDelegate to launch SARL agent or application.
 *
 * <p>This delegate is in charge of running a SARL agent/application with the specific
 * SRE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public abstract class AbstractSARLLaunchConfiguration extends AbstractJavaLaunchConfigurationDelegate {

	@Inject
	private ILaunchConfigurationAccessor configAccessor;

	private SoftReference<IRuntimeClasspathEntry[]> unresolvedClasspath;

	private SoftReference<IRuntimeClasspathEntry[]> resolvedClasspath;

	@Deprecated
	private SoftReference<String[]> bufferedClasspath8;

	private SoftReference<String[]> bufferedClasspath;

	private SoftReference<String[]> bufferedModulepath;

	private SoftReference<ExtraClassPathProviders> bufferedClasspathProviders;

	/** Replies the configuration accessor.
	 *
	 * @return the configuration accessor.
	 * @since 0.12
	 */
	public ILaunchConfigurationAccessor getConfigurationAccessor() {
		return this.configAccessor;
	}

	@Override
	public void prepareStopInMain(ILaunchConfiguration configuration) throws CoreException {
		// Increase the visibility of the function
		super.prepareStopInMain(configuration);
	}

	@Override
	public void setDefaultSourceLocator(ILaunch launch, ILaunchConfiguration configuration) throws CoreException {
		// Increase the visibility of the function
		super.setDefaultSourceLocator(launch, configuration);
	}

	@Override
	public boolean supportsPreviewFeatures(ILaunchConfiguration configuration) {
		// Increase the visibility of the function
		return super.supportsPreviewFeatures(configuration);
	}

	@Override
	public boolean supportsModule() {
		// Increase the visibility of the function
		return super.supportsModule();
	}

	/** Replies if both the current launch configuration and the provided configuration
	 * supports modules.
	 *
	 * @param configuration the configuration to test.
	 * @return {@code true} if both the configuration (i.e. the project and the concrete launch configuration.
	 */
	public boolean supportsModularProjectAndLauncher(ILaunchConfiguration configuration) {
		return JavaRuntime.isModularConfiguration(configuration) && supportsModule();
	}

	/** Clear any buffered value.
	 */
	protected synchronized void clearBuffers() {
		if (this.unresolvedClasspath != null) {
			this.unresolvedClasspath.clear();
			this.unresolvedClasspath = null;
		}
		if (this.resolvedClasspath != null) {
			this.resolvedClasspath.clear();
			this.resolvedClasspath = null;
		}
		if (this.bufferedClasspath8 != null) {
			this.bufferedClasspath8.clear();
			this.bufferedClasspath8 = null;
		}
		if (this.bufferedClasspath != null) {
			this.bufferedClasspath.clear();
			this.bufferedClasspath = null;
		}
		if (this.bufferedModulepath != null) {
			this.bufferedModulepath.clear();
			this.bufferedModulepath = null;
		}
		if (this.bufferedClasspathProviders != null) {
			this.bufferedClasspathProviders.clear();
			this.bufferedClasspathProviders = null;
		}
	}

	private ExtraClassPathProviders ensureClasspathProvidersBuffer() {
		ExtraClassPathProviders providers = this.bufferedClasspathProviders == null ? null : this.bufferedClasspathProviders.get();
		if (providers == null) {
			providers = new ExtraClassPathProviders();
			this.bufferedClasspathProviders = new SoftReference<>(providers);
		}
		return providers;
	}

	/** Replies the raw (unresolved) class path for the SARL application.
	 *
	 * @param configuration the configuration that provides the classpath.
	 * @return the filtered entries.
	 * @throws CoreException if impossible to get the classpath.
	 */
	private IRuntimeClasspathEntry[] getOrComputeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration configuration)
			throws CoreException {
		synchronized (this) {
			final IRuntimeClasspathEntry[] entries = this.unresolvedClasspath == null ? null : this.unresolvedClasspath.get();
			if (entries != null) {
				return entries;
			}
		}
		final IRuntimeClasspathEntry[] entries = SrePathUtils.computeUnresolvedSARLRuntimeClasspath(configuration, this.configAccessor,
			cfg -> getJavaProject(cfg), ensureClasspathProvidersBuffer());
		if (entries == null) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
					"Unable to computer the raw classpath from the launch configuration")); //$NON-NLS-1$
		}
		synchronized (this) {
			this.unresolvedClasspath = new SoftReference<>(entries);
		}
		return entries;
	}

	/** Replies the resolved class path for the SARL application.
	 *
	 * @param configuration the configuration that provides the classpath.
	 * @return the filtered entries.
	 * @throws CoreException if impossible to get the classpath.
	 */
	private IRuntimeClasspathEntry[] getOrComputeResolvedSARLRuntimeClasspath(ILaunchConfiguration configuration)
			throws CoreException {
		synchronized (this) {
			final IRuntimeClasspathEntry[] entries = this.resolvedClasspath == null ? null : this.resolvedClasspath.get();
			if (entries != null) {
				return entries;
			}
		}
		final IRuntimeClasspathEntry[] entries = getOrComputeUnresolvedSARLRuntimeClasspath(configuration);
		final IRuntimeClasspathEntry[] entries2 = JavaRuntime.resolveRuntimeClasspath(entries, configuration);
		if (entries2 == null) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
					"Unable to computer the resolved classpath from the launch configuration")); //$NON-NLS-1$
		}
		synchronized (this) {
			this.resolvedClasspath = new SoftReference<>(entries2);
		}
		return entries2;
	}

	/** Replies if the given classpath entry is NOT a  SRE.
	 *
	 * @param entry the entry.
	 * @return <code>false</code> if the entry points to a SRE;
	 * <code>true</code> otherwise.
	 */
	private static boolean isNotSREEntry(IRuntimeClasspathEntry entry) {
		try {
			final File file = new File(entry.getLocation());
			if (file.isDirectory()) {
				return !SARLRuntime.isUnpackedSRE(file);
			} else if (file.canRead()) {
				return !SARLRuntime.isPackedSRE(file);
			}
		} catch (Throwable e) {
			SARLEclipsePlugin.getDefault().log(e);
		}
		return true;
	}

	/** Copied from JDT's super class, and patched for invoking
	 * {@link #getOrComputeResolvedSARLRuntimeClasspath(ILaunchConfiguration)}.
	 * {@inheritDoc}
	 *
	 * @deprecated since 0.12, use {@link #getClasspathAndModulepath(ILaunchConfiguration)}.
	 */
	@Override
	@Deprecated
	public String[] getClasspath(ILaunchConfiguration configuration) throws CoreException {
		synchronized (this) {
			final String[] classpathEntries = this.bufferedClasspath8 == null ? null : this.bufferedClasspath8.get();
			if (classpathEntries != null) {
				return classpathEntries;
			}
		}
		final IRuntimeClasspathEntry[] entries = getOrComputeResolvedSARLRuntimeClasspath(configuration);

		final boolean isMavenProject = getJavaProject(configuration).getProject().hasNature(SARLEclipseConfig.MAVEN_NATURE_ID);
		boolean needSREEntry = isMavenProject;

		// Store in a list for preserving the order of the entries.
		final List<String> userEntryList = new ArrayList<>(entries.length + 1);
		final Set<String> set = new TreeSet<>();
		for (int i = 0; i < entries.length; i++) {
			if (entries[i].getClasspathProperty() == IRuntimeClasspathEntry.USER_CLASSES
					|| entries[i].getClasspathProperty() == IRuntimeClasspathEntry.CLASS_PATH) {
				final String location = entries[i].getLocation();
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
			for (final IRuntimeClasspathEntry entry : SrePathUtils.getSREClasspathEntries(configuration,
					this.configAccessor, cfg -> getJavaProject(cfg))) {
				if (entry.getClasspathProperty() == IRuntimeClasspathEntry.USER_CLASSES
						|| entry.getClasspathProperty() == IRuntimeClasspathEntry.CLASS_PATH) {
					final String location = entry.getLocation();
					if (location != null && !set.contains(location)) {
						userEntryList.add(insertIndex, location);
						set.add(location);
						++insertIndex;
					}
				}
			}
		}

		final String[] classpathEntries = userEntryList.toArray(new String[userEntryList.size()]);
		synchronized (this) {
			this.bufferedClasspath8 = new SoftReference<>(classpathEntries);
		}
		return classpathEntries;
	}

	/** Copied from JDT's super class, and patched for invoking
	 * {@link #getOrComputeResolvedSARLRuntimeClasspath(ILaunchConfiguration)}.
	 * {@inheritDoc}
	 *
	 * @since 0.12
	 */
	@Override
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	public String[][] getClasspathAndModulepath(ILaunchConfiguration configuration) throws CoreException {
		synchronized (this) {
			final String[] classpathEntries = this.bufferedClasspath == null ? null : this.bufferedClasspath.get();
			final String[] modulepathEntries = this.bufferedModulepath == null ? null : this.bufferedModulepath.get();
			if (classpathEntries != null && modulepathEntries != null) {
				return new String[][] {classpathEntries, modulepathEntries};
			}
		}
		final IRuntimeClasspathEntry[] entries = getOrComputeResolvedSARLRuntimeClasspath(configuration);

		final boolean isMavenProject = getJavaProject(configuration).getProject().hasNature(SARLEclipseConfig.MAVEN_NATURE_ID);
		boolean needSREEntry = isMavenProject;

		// Store in a list for preserving the order of the entries.
		final List<String> classpathEntries = new ArrayList<>(entries.length);
		final List<String> modulepathEntries = new ArrayList<>(entries.length);

		final Set<String> classpathSet = new TreeSet<>();
		final Set<String> modulepathSet = new TreeSet<>();

		for (final IRuntimeClasspathEntry entry : entries) {
			final String location = entry.getLocation();
			if (location != null) {
				switch (entry.getClasspathProperty()) {
				case IRuntimeClasspathEntry.USER_CLASSES:
					if (!classpathSet.contains(location)) {
						classpathEntries.add(location);
						classpathSet.add(location);
						if (needSREEntry) {
							needSREEntry = isNotSREEntry(entry);
						}
					}
					break;
				case IRuntimeClasspathEntry.CLASS_PATH:
					if (!classpathSet.contains(location)) {
						classpathEntries.add(location);
						classpathSet.add(location);
						if (needSREEntry) {
							needSREEntry = isNotSREEntry(entry);
						}
					}
					break;
				case IRuntimeClasspathEntry.MODULE_PATH:
					if (!modulepathSet.contains(location)) {
						modulepathEntries.add(location);
						modulepathSet.add(location);
						if (needSREEntry) {
							needSREEntry = isNotSREEntry(entry);
						}
					}
					break;
				default:
					break;
				}
			}
		}

		if (needSREEntry) {
			int insertIndex = 0;
			for (final IRuntimeClasspathEntry entry : SrePathUtils.getSREClasspathEntries(configuration,
					this.configAccessor, cfg -> getJavaProject(cfg))) {
				final String location = entry.getLocation();
				if (location != null) {
					switch (entry.getClasspathProperty()) {
					case IRuntimeClasspathEntry.USER_CLASSES:
						if (!classpathSet.contains(location)) {
							classpathEntries.add(insertIndex, location);
							classpathSet.add(location);
							++insertIndex;
						}
						break;
					case IRuntimeClasspathEntry.CLASS_PATH:
						if (!classpathSet.contains(location)) {
							classpathEntries.add(insertIndex, location);
							classpathSet.add(location);
							++insertIndex;
						}
						break;
					case IRuntimeClasspathEntry.MODULE_PATH:
						if (!modulepathSet.contains(location)) {
							modulepathEntries.add(insertIndex, location);
							modulepathSet.add(location);
							++insertIndex;
						}
						break;
					default:
						break;
					}
				}
			}
		}

		final String[] classpath = classpathEntries.toArray(new String[classpathEntries.size()]);
		final String[] modulepath = modulepathEntries.toArray(new String[modulepathEntries.size()]);

		synchronized (this) {
			this.bufferedClasspath = new SoftReference<>(classpath);
			this.bufferedModulepath = new SoftReference<>(modulepath);
		}

		return new String[][] {classpath, modulepath};
	}

	/** Copied from JDT's super class, and patched for invoking
	 * {@link #getOrComputeResolvedSARLRuntimeClasspath(ILaunchConfiguration)}.
	 * {@inheritDoc}
	 */
	@Override
	public String[] getBootpath(ILaunchConfiguration configuration) throws CoreException {
		if (JavaRuntime.isModularConfiguration(configuration)) {
			return null;
		}
		final String[][] paths = getBootpathExt(configuration);
		final String[] pre = paths[0];
		final String[] main = paths[1];
		final String[] app = paths[2];
		if (pre == null && main == null && app == null) {
			return null;
		}
		final IRuntimeClasspathEntry[] entries = getOrComputeResolvedSARLRuntimeClasspath(configuration);
		final List<String> bootEntries = new ArrayList<>(entries.length);
		boolean empty = true;
		boolean allStandard = true;
		for (int i = 0; i < entries.length; i++) {
			if (entries[i].getClasspathProperty() != IRuntimeClasspathEntry.USER_CLASSES) {
				final String location = entries[i].getLocation();
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

	/** Copied from JDT's super class, and patched for invoking
	 * {@link #getOrComputeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration)}.
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings("checkstyle:npathcomplexity")
	public String[][] getBootpathExt(ILaunchConfiguration configuration)
			throws CoreException {
		final String[][] bootpathInfo = new String[3][];
		final IRuntimeClasspathEntry[] entries = getOrComputeUnresolvedSARLRuntimeClasspath(configuration);
		final List<IRuntimeClasspathEntry> bootEntriesPrepend = new ArrayList<>();
		int index = 0;
		IRuntimeClasspathEntry jreEntry = null;
		while (jreEntry == null && index < entries.length) {
			final IRuntimeClasspathEntry entry = entries[index];
			if (JavaRuntime.isVMInstallReference(entry)) {
				jreEntry = entry;
			} else if (entry.getClasspathProperty() == IRuntimeClasspathEntry.BOOTSTRAP_CLASSES) {
				bootEntriesPrepend.add(entry);
			}
			++index;
		}
		final IRuntimeClasspathEntry[] bootEntriesPrep = JavaRuntime
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
			final List<IRuntimeClasspathEntry> bootEntriesAppend = new ArrayList<>();
			for (; index < entries.length; index++) {
				final IRuntimeClasspathEntry entry = entries[index];
				if (entry.getClasspathProperty() == IRuntimeClasspathEntry.BOOTSTRAP_CLASSES) {
					bootEntriesAppend.add(entry);
				}
			}
			bootpathInfo[0] = entriesPrep;
			final IRuntimeClasspathEntry[] bootEntriesApp = JavaRuntime
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
			final IVMInstall install = getVMInstall(configuration);
			final LibraryLocation[] libraryLocations = install.getLibraryLocations();
			if (libraryLocations != null) {
				// determine if explicit bootpath should be used
				// TODO: this test does not tell us if the bootpath entries are different (could still be
				// the same, as a non-bootpath entry on the JRE may have been removed/added)
				// We really need a way to ask a VM type for its default bootpath library locations and
				// compare that to the resolved entries for the "jreEntry" to see if they
				// are different (requires explicit bootpath)
				if (!JRERuntimeClasspathEntryResolver.isSameArchives(libraryLocations, install.getVMInstallType().getDefaultLibraryLocations(install.getInstallLocation()))) {
					// resolve bootpath entries in JRE entry
					IRuntimeClasspathEntry[] bootEntries = null;
					if (jreEntry.getType() == IRuntimeClasspathEntry.CONTAINER) {
						final IRuntimeClasspathEntry bootEntry = JavaRuntime.newRuntimeContainerClasspathEntry(
								jreEntry.getPath(),
								IRuntimeClasspathEntry.BOOTSTRAP_CLASSES,
								getJavaProject(configuration));
						bootEntries = JavaRuntime.resolveRuntimeClasspathEntry(bootEntry, configuration);
					} else {
						bootEntries = JavaRuntime.resolveRuntimeClasspathEntry(jreEntry, configuration);
					}

					// non-default JRE libraries - use explicit bootpath only
					final String[] bootpath = new String[bootEntriesPrep.length
					                                     + bootEntries.length
					                                     + bootEntriesApp.length];
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
		} else {
			if (entriesPrep == null) {
				bootpathInfo[1] = new String[0];
			} else {
				bootpathInfo[1] = entriesPrep;
			}
		}
		return bootpathInfo;
	}

	/** Compute the arguments that are specific to the launch configuration.
	 *
	 * @param configuration the launch configuration.
	 * @param sre the selected SARL run-time environment.
	 * @param standardProgramArguments the standard program arguments.
	 * @return the actual program arguments, or {@code null} for using the default program arguments.
	 * @throws CoreException if some argument cannot be computed.
	 */
	protected abstract String getProgramArguments(ILaunchConfiguration configuration,
			ISREInstall sre, String standardProgramArguments) throws CoreException;

	/** Replies the arguments of the program including the boot agent name.
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings("checkstyle:variabledeclarationusagedistance")
	public final String getProgramArguments(ILaunchConfiguration configuration) throws CoreException {
		// The following line get the standard arguments
		final String standardProgramArguments = super.getProgramArguments(configuration);

		// Get the specific SRE arguments
		final ISREInstall sre = SrePathUtils.getSREInstallFor(configuration, getConfigurationAccessor(), cfg -> getJavaProject(cfg));
		assert sre != null;

		return getProgramArguments(configuration, sre, standardProgramArguments);
	}

	@Override
	public IVMRunner getVMRunner(ILaunchConfiguration configuration, String mode) throws CoreException {
		if (getConfigurationAccessor().isEmbeddedSRE(configuration)) {
			if (!supportsModularProjectAndLauncher(configuration)) {
				return new EmbeddedNotModularVMRunner();
			}
			throw new IllegalStateException();
		}
		return super.getVMRunner(configuration, mode);
	}

	@Override
	public String getVMArguments(ILaunchConfiguration configuration) throws CoreException {
		final String launchConfigArgs = super.getVMArguments(configuration);
		final ISREInstall sre = SrePathUtils.getSREInstallFor(configuration, getConfigurationAccessor(), cfg -> getJavaProject(cfg));
		assert sre != null;
		final IStringVariableManager substitutor = VariablesPlugin.getDefault().getStringVariableManager();
		final String sreArgs = substitutor.performStringSubstitution(sre.getJVMArguments());
		return join(sreArgs, launchConfigArgs);
	}

	@Override
	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		try {
			final ILaunchProcess process = createLaunchingProcess(configuration, mode, launch);
			// Preparation
			final SubMonitor progressMonitor = SubMonitor.convert(
					monitor,
					MessageFormat.format(Messages.AbstractSARLLaunchConfiguration_0,
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

	@Override
	public String showCommandLine(ILaunchConfiguration configuration, String mode, ILaunch launch, IProgressMonitor monitor) throws CoreException {
		try {
			final ILaunchProcess process = createLaunchingProcess(configuration, mode, launch);
			// Preparation
			final SubMonitor progressMonitor = SubMonitor.convert(
					monitor,
					MessageFormat.format(Messages.AbstractSARLLaunchConfiguration_0,
							configuration.getName()),
					process.getStepNumber());
			while (process.prepare(progressMonitor.newChild(1))) {
				if (progressMonitor.isCanceled()) {
					return ""; //$NON-NLS-1$
				}
			}

			// Get command line
			final IVMRunner runner = getVMRunner(configuration, mode);
			final String cmdLine = runner.showCommandLine(process.getVirtualMachineRunnerConfiguration(), launch, monitor);
			return cmdLine;
		} finally {
			// Clear cached entries
			clearBuffers();
		}
	}

	/** Create the object that manage the launching process.
	 *
	 * @param configuration the configuration to be launched.
	 * @param mode the launching mode (debugging or running).
	 * @param launch the launching infrastructure.
	 * @return the launching process manager.
	 */
	protected abstract ILaunchProcess createLaunchingProcess(ILaunchConfiguration configuration, String mode,
			ILaunch launch);

}

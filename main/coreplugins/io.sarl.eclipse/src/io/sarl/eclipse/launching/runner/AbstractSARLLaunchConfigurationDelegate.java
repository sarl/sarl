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

package io.sarl.eclipse.launching.runner;

import java.io.File;
import java.lang.ref.SoftReference;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;

import javax.inject.Inject;

import com.google.common.base.Strings;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.variables.IStringVariableManager;
import org.eclipse.core.variables.VariablesPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.internal.launching.JRERuntimeClasspathEntryResolver;
import org.eclipse.jdt.internal.launching.LaunchingMessages;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;
import org.eclipse.jdt.launching.ExecutionArguments;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMRunner;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.launching.LibraryLocation;
import org.eclipse.jdt.launching.VMRunnerConfiguration;
import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.sreproviding.EclipseIDEProjectSREProvider;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.ProjectSREProvider;
import io.sarl.eclipse.runtime.ProjectSREProviderFactory;
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
public abstract class AbstractSARLLaunchConfigurationDelegate extends AbstractJavaLaunchConfigurationDelegate {

	private static final String OPTION_ENABLEASSERTIONS = "-ea"; //$NON-NLS-1$

	private SoftReference<IRuntimeClasspathEntry[]> unresolvedClasspathEntries;

	private SoftReference<String[]> classpathEntries;

	@Inject
	private ILaunchConfigurationAccessor configAccessor;

	@Override
	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		try {
			final ILaunchProcess process = createLaunchingProcess(configuration, mode, launch);
			// Preparation
			final SubMonitor progressMonitor = SubMonitor.convert(
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

	/** Clear any buffered value.
	 */
	protected void clearBuffers() {
		this.unresolvedClasspathEntries = null;
		this.classpathEntries = null;
	}

	/** Replies the configuration accessor.
	 *
	 * @return the configuration accessor.
	 */
	protected ILaunchConfigurationAccessor getConfigurationAccessor() {
		return this.configAccessor;
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

	/** Replies the SRE installation to be used for the given configuration.
	 *
	 * @param configuration the configuration to check.
	 * @param configAccessor the accessor to the SRE configuration.
	 * @param projectAccessor the accessor to the Java project.
	 * @return the SRE install.
	 * @throws CoreException if impossible to get the SRE.
	 */
	protected static ISREInstall getSREInstallFor(ILaunchConfiguration configuration,
			ILaunchConfigurationAccessor configAccessor,
			IJavaProjectAccessor projectAccessor) throws CoreException {
		assert configAccessor != null;
		assert projectAccessor != null;
		final ISREInstall sre;
		if (configAccessor.getUseProjectSREFlag(configuration)) {
			sre = getProjectSpecificSRE(configuration, true, projectAccessor);
		} else if (configAccessor.getUseSystemSREFlag(configuration)) {
			sre = SARLRuntime.getDefaultSREInstall();
			verifySREValidity(sre, sre.getId());
		} else  {
			final String runtime = configAccessor.getSREId(configuration);
			sre = SARLRuntime.getSREFromId(runtime);
			verifySREValidity(sre, runtime);
		}

		if (sre == null) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
					Messages.SARLLaunchConfigurationDelegate_0));
		}

		return sre;
	}

	/** Replies the project SRE from the given configuration.
	 *
	 * @param configuration the configuration to read.
	 * @param verify  if true verify the SRE validity, do nothing otherwise
	 * @param projectAccessor the accessor to the Java project.
	 * @return the project SRE or <code>null</code>.
	 * @throws CoreException Some error occurs when accessing to the ecore elements.
	 */
	private static ISREInstall getProjectSpecificSRE(ILaunchConfiguration configuration, boolean verify,
			IJavaProjectAccessor projectAccessor) throws CoreException {
		assert projectAccessor != null;
		final IJavaProject jprj = projectAccessor.get(configuration);
		if (jprj != null) {
			final IProject prj = jprj.getProject();
			assert prj != null;

			// Get the SRE from the extension point
			ISREInstall sre = getSREFromExtension(prj, verify);
			if (sre != null) {
				return sre;
			}

			// Get the SRE from the default project configuration
			final ProjectSREProvider provider = new EclipseIDEProjectSREProvider(prj);
			sre = provider.getProjectSREInstall();
			if (sre != null) {
				if (verify) {
					verifySREValidity(sre, sre.getId());
				}
				return sre;
			}
		}
		final ISREInstall sre = SARLRuntime.getDefaultSREInstall();
		if (verify) {
			verifySREValidity(sre, (sre == null) ? Messages.SARLLaunchConfigurationDelegate_8 : sre.getId());
		}
		return sre;
	}

	private static ISREInstall getSREFromExtension(IProject project, boolean verify) {
		final IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(
				SARLEclipsePlugin.PLUGIN_ID,
				SARLEclipseConfig.EXTENSION_POINT_PROJECT_SRE_PROVIDER_FACTORY);
		if (extensionPoint != null) {
			for (final IConfigurationElement element : extensionPoint.getConfigurationElements()) {
				try {
					final Object obj = element.createExecutableExtension("class"); //$NON-NLS-1$
					assert obj instanceof ProjectSREProviderFactory;
					final ProjectSREProviderFactory factory = (ProjectSREProviderFactory) obj;
					final ProjectSREProvider provider = factory.getProjectSREProvider(project);
					if (provider != null) {
						final ISREInstall sre = provider.getProjectSREInstall();
						if (sre == null) {
							return null;
						}
						if (verify) {
							verifySREValidity(sre, sre.getId());
						}
						return sre;
					}
				} catch (CoreException e) {
					SARLEclipsePlugin.getDefault().log(e);
				}
			}
		}
		return null;
	}

	private static void verifySREValidity(ISREInstall sre, String runtime) throws CoreException {
		if (sre == null) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
					MessageFormat.format(io.sarl.eclipse.launching.dialog.Messages.RuntimeEnvironmentTab_6, runtime)));
		}
		final int ignoreCode = 0;
		if (!sre.getValidity(ignoreCode).isOK()) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, MessageFormat.format(
					io.sarl.eclipse.launching.dialog.Messages.RuntimeEnvironmentTab_5,
					sre.getName())));
		}
	}

	@Override
	public IVMRunner getVMRunner(ILaunchConfiguration configuration, String mode) throws CoreException {
		if (getConfigurationAccessor().isEmbeddedSRE(configuration)) {
			return new EmbeddedVMRunner();
		}
		return super.getVMRunner(configuration, mode);
	}

	@Override
	public String getVMArguments(ILaunchConfiguration configuration) throws CoreException {
		final String launchConfigArgs = super.getVMArguments(configuration);
		final ISREInstall sre = getSREInstallFor(configuration, this.configAccessor, cfg -> getJavaProject(cfg));
		assert sre != null;
		final IStringVariableManager substitutor = VariablesPlugin.getDefault().getStringVariableManager();
		final String sreArgs = substitutor.performStringSubstitution(sre.getJVMArguments());
		return join(sreArgs, launchConfigArgs);
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
		final ISREInstall sre = getSREInstallFor(configuration, this.configAccessor, cfg -> getJavaProject(cfg));
		assert sre != null;

		return getProgramArguments(configuration, sre, standardProgramArguments);
	}

	/** Replies a string that is the concatenation of the given values.
	 *
	 * @param values the values to merge.
	 * @return the concatenation result.
	 */
	protected static String join(String... values) {
		final StringBuilder buffer = new StringBuilder();
		for (final String value : values) {
			if (!Strings.isNullOrEmpty(value)) {
				if (buffer.length() > 0) {
					buffer.append(" "); //$NON-NLS-1$
				}
				buffer.append(value);
			}
		}
		return buffer.toString();
	}

	/** Copied from JDT's super class, and patched for invoking
	 * {@link #getOrComputeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration)}.
	 * {@inheritDoc}
	 */
	@Override
	public String[][] getBootpathExt(ILaunchConfiguration configuration)
			throws CoreException {
		final String[][] bootpathInfo = new String[3][];
		final IRuntimeClasspathEntry[] entries = getOrComputeUnresolvedSARLRuntimeClasspath(configuration);
		final List<IRuntimeClasspathEntry> bootEntriesPrepend = new ArrayList<>();
		final IRuntimeClasspathEntry jreEntry;
		final int index;
		final Pair<IRuntimeClasspathEntry, Integer> pair = getJreEntry(entries, bootEntriesPrepend);
		jreEntry = pair.getKey();
		index = pair.getValue().intValue();
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
	 * {@link #getOrComputeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration)}.
	 * {@inheritDoc}
	 */
	@Override
	public String[] getBootpath(ILaunchConfiguration configuration) throws CoreException {
		final String[][] paths = getBootpathExt(configuration);
		final String[] pre = paths[0];
		final String[] main = paths[1];
		final String[] app = paths[2];
		if (pre == null && main == null && app == null) {
			// default
			return null;
		}
		IRuntimeClasspathEntry[] entries = getOrComputeUnresolvedSARLRuntimeClasspath(configuration);
		entries = JavaRuntime.resolveRuntimeClasspath(entries, configuration);
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

		IRuntimeClasspathEntry[] entries = getOrComputeUnresolvedSARLRuntimeClasspath(configuration);
		entries = JavaRuntime.resolveRuntimeClasspath(entries, configuration);

		final boolean isMavenProject = getJavaProject(configuration).getProject().hasNature(SARLEclipseConfig.MAVEN_NATURE_ID);
		boolean needSREEntry = isMavenProject;

		// Store in a list for preserving the order of the entries.
		final List<String> userEntryList = new ArrayList<>(entries.length + 1);
		final Set<String> set = new TreeSet<>();
		for (int i = 0; i < entries.length; i++) {
			if (entries[i].getClasspathProperty() == IRuntimeClasspathEntry.USER_CLASSES) {
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
			for (final IRuntimeClasspathEntry entry : getSREClasspathEntries(configuration,
					this.configAccessor, cfg -> getJavaProject(cfg))) {
				if (entry.getClasspathProperty() == IRuntimeClasspathEntry.USER_CLASSES) {
					final String location = entry.getLocation();
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

	/** Replies the class path for the SARL application.
	 *
	 * @param configuration the configuration that provides the classpath.
	 * @return the filtered entries.
	 * @throws CoreException if impossible to get the classpath.
	 */
	private IRuntimeClasspathEntry[] getOrComputeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration configuration)
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
		entries = computeUnresolvedSARLRuntimeClasspath(configuration, this.configAccessor, cfg -> getJavaProject(cfg));
		//
		synchronized (this) {
			this.unresolvedClasspathEntries = new SoftReference<>(entries);
		}
		return entries;
	}

	/** Compute the class path for the given launch configuration.
	 *
	 * @param configuration the configuration that provides the classpath.
	 * @param configAccessor the accessor to the SRE configuration.
	 * @param projectAccessor the accessor to the Java project.
	 * @return the filtered entries.
	 * @throws CoreException if impossible to get the classpath.
	 */
	public static IRuntimeClasspathEntry[] computeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration configuration,
			ILaunchConfigurationAccessor configAccessor,
			IJavaProjectAccessor projectAccessor) throws CoreException {
		// Get the classpath from the configuration.
		final IRuntimeClasspathEntry[] entries = JavaRuntime.computeUnresolvedRuntimeClasspath(configuration);
		//
		final List<IRuntimeClasspathEntry> filteredEntries = new ArrayList<>();
		List<IRuntimeClasspathEntry> sreClasspathEntries = null;
		// Filtering the entries by replacing the "SARL Libraries" with the SARL runtime environment.
		for (final IRuntimeClasspathEntry entry : entries) {
			if (entry.getPath().equals(SARLClasspathContainerInitializer.CONTAINER_ID)) {
				if (sreClasspathEntries == null) {
					sreClasspathEntries = getSREClasspathEntries(configuration, configAccessor, projectAccessor);
				}
				filteredEntries.addAll(sreClasspathEntries);
			} else {
				filteredEntries.add(entry);
			}
		}
		return filteredEntries.toArray(new IRuntimeClasspathEntry[filteredEntries.size()]);
	}

	/** Replies the classpath entries associated to the SRE of the given configuration.
	 *
	 * @param configuration the configuration to read.
	 * @param configAccessor the accessor to the SRE configuration.
	 * @param projectAccessor the accessor to the Java project.
	 * @return the classpath entries for the SRE associated to the configuration.
	 * @throws CoreException if impossible to determine the classpath entries.
	 */
	private static List<IRuntimeClasspathEntry> getSREClasspathEntries(
			ILaunchConfiguration configuration,
			ILaunchConfigurationAccessor configAccessor,
			IJavaProjectAccessor projectAccessor) throws CoreException {
		final ISREInstall sre = getSREInstallFor(configuration, configAccessor, projectAccessor);
		return sre.getClassPathEntries();
	}

	private static Pair<IRuntimeClasspathEntry, Integer> getJreEntry(IRuntimeClasspathEntry[] entries,
			List<IRuntimeClasspathEntry> bootEntriesPrepend) {
		int index = 0;
		IRuntimeClasspathEntry jreEntry = null;
		while (jreEntry == null && index < entries.length) {
			final IRuntimeClasspathEntry entry = entries[index++];
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

	/** Replies if the given classpath entry is a SRE.
	 *
	 * @param entry the entry.
	 * @return <code>true</code> if the entry points to a SRE;
	 * <code>false</code> otherwise.
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

	@SuppressWarnings("checkstyle:npathcomplexity")
	private void getBootpathExtForJRE(ILaunchConfiguration configuration,
			IRuntimeClasspathEntry[] entries, IRuntimeClasspathEntry jreEntry, int idx,
			String[] entriesPrep, IRuntimeClasspathEntry[] bootEntriesPrep, String[][] bootpathInfo)
					throws CoreException {
		int index = idx;
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
			if (!JRERuntimeClasspathEntryResolver.isSameArchives(libraryLocations,
					install.getVMInstallType().getDefaultLibraryLocations(install.getInstallLocation()))) {
				// resolve bootpath entries in JRE entry
				final IRuntimeClasspathEntry[] bootEntries;
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

	/** Implementation of a launching process.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	protected interface ILaunchProcess {

		/** Replies the number of steps within the launching process.
		 *
		 * @return the number of steps.
		 */
		int getStepNumber();

		/** Run the next step for preparing the launching of the application.
		 *
		 * @param monitor the progress monitor.
		 * @return {@code true} if another preparation step is required. {@code false} if no more preparation step should
		 *     be run.
		 * @throws CoreException if something cannot be done.
		 */
		boolean prepare(IProgressMonitor monitor) throws CoreException;

		/** Run the next step for launching the application.
		 *
		 * @param monitor the progress monitor.
		 * @return {@code true} if another launching step is required. {@code false} if no more launching step should
		 *     be run.
		 * @throws CoreException if something cannot be done.
		 */
		boolean launch(IProgressMonitor monitor) throws CoreException;

	}

	/** Implementation of a launching process, split in separated steps for
	 * making easier the cancellation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	protected abstract class AbstractLaunchProcess implements ILaunchProcess {

		/** Launch configuration to be used.
		 */
		protected final ILaunchConfiguration configuration;

		/** Running mode (running or debugging).
		 */
		protected final String mode;

		/** Launching infrastructure.
		 */
		protected final ILaunch launch;

		private PreparationProcessState preparationState = PreparationProcessState.STEP_0_PREPARE_PARAMETERS;

		private String mainTypeName;

		private IVMRunner runner;

		private String workingDirName;

		private String[] envp;

		private String[] classpath;

		private ExecutionArguments execArgs;

		private Map<String, Object> vmAttributesMap;

		private VMRunnerConfiguration runConfig;

		private RunProcessState runState = RunProcessState.STEP_0_CONFIGURE_SOURCE_LOCATOR;

		/** Constructor.
		 * @param configuration the launch configuration.
		 * @param mode the launching mode.
		 * @param launch the launching
		 */
		protected AbstractLaunchProcess(ILaunchConfiguration configuration, String mode, ILaunch launch) {
			this.configuration = configuration;
			this.mode = mode;
			this.launch = launch;
		}

		/** Replies the configuration of the virtual machine runner.
		 *
		 * @return the configuration of the virtual machine runner.
		 */
		protected VMRunnerConfiguration getVirtualMachineRunnerConfiguration() {
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
				buildClasspath(monitor);
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

			// Clear cached entries
			clearBuffers();

			setMainTypeName(verifyMainTypeName(this.configuration));
			setRunner(getVMRunner(this.configuration, this.mode));

			final File workingDir = verifyWorkingDirectory(this.configuration);
			setWorkingDirectory(null);
			if (workingDir != null) {
				setWorkingDirectory(workingDir.getAbsolutePath());
			}

			// Environment variables
			setEnvironment(AbstractSARLLaunchConfigurationDelegate.this.getEnvironment(this.configuration));
		}

		/** Extract the class path from the configuration.
		 *
		 * @param monitor the progress monitor.
		 * @throws CoreException if a parameter cannot be extracted.
		 */
		protected void buildClasspath(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					Messages.SARLLaunchConfigurationDelegate_3);
			setClasspath(AbstractSARLLaunchConfigurationDelegate.this.getClasspath(this.configuration));
		}

		/** Read the arguments to pass to the launched application.
		 *
		 * @param monitor the progress monitor.
		 * @throws CoreException if a parameter cannot be extracted.
		 */
		protected void readLaunchingArguments(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					Messages.SARLLaunchConfigurationDelegate_2);

			// Program & VM arguments
			final String pgmArgs = getProgramArguments(this.configuration);
			final StringBuilder vmArgs = new StringBuilder(getVMArguments(this.configuration));

			// Add -ea option if in debug mode
			if ((Objects.equals(this.mode,  ILaunchManager.RUN_MODE)
					&& getConfigurationAccessor().isAssertionEnabledInRunMode(this.configuration))
				|| (Objects.equals(this.mode,  ILaunchManager.DEBUG_MODE)
					&& getConfigurationAccessor().isAssertionEnabledInDebugMode(this.configuration))) {
				if (vmArgs.length() > 0) {
					vmArgs.append(" "); //$NON-NLS-1$
				}
				vmArgs.append(OPTION_ENABLEASSERTIONS);
			}

			setExecutionArguments(new ExecutionArguments(vmArgs.toString(), pgmArgs));

			// VM-specific attributes
			setVirtualMachineAttributes(getVMSpecificAttributesMap(this.configuration));
		}

		/** Validate the extracted values from the launch configuration.
		 *
		 * @param monitor the progress monitor.
		 * @throws CoreException if a parameter cannot be extracted.
		 */
		@SuppressWarnings("synthetic-access")
		private void postValidation(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					Messages.SARLLaunchConfigurationDelegate_7);
			if (Strings.isNullOrEmpty(getMainTypeName())) {
				// This case occurs when the launch configuration is using
				// a SRE that is inside the class path.
				// The name of the main class is then no saved in the launch configuration properties.
				final ISREInstall sre = getSREInstallFor(this.configuration,
						AbstractSARLLaunchConfigurationDelegate.this.configAccessor,
						cfg -> getJavaProject(cfg));
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
		protected void createRunConfiguration(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					Messages.SARLLaunchConfigurationDelegate_4);
			final VMRunnerConfiguration cfg = new VMRunnerConfiguration(getMainTypeName(), getClasspath());
			cfg.setProgramArguments(getExecutionArguments().getProgramArgumentsArray());
			cfg.setEnvironment(getEnvironment());
			cfg.setVMArguments(getExecutionArguments().getVMArgumentsArray());
			cfg.setWorkingDirectory(getWorkingDirectory());
			cfg.setVMSpecificAttributesMap(getVirtualMachineAttributes());
			cfg.setBootClassPath(getBootpath(this.configuration));
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
					Messages.SARLLaunchConfigurationDelegate_5);
			prepareStopInMain(this.configuration);
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
			monitor.subTask(
					LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_Creating_source_locator____2);
			setDefaultSourceLocator(this.launch, this.configuration);
		}

		/** Run.
		 *
		 * @param monitor the progress monitor.
		 * @throws CoreException if a parameter cannot be extracted.
		 */
		protected void launchRunner(IProgressMonitor monitor) throws CoreException {
			monitor.subTask(
					MessageFormat.format(Messages.SARLLaunchConfigurationDelegate_6, this.configuration.getName()));
			getRunner().run(getVirtualMachineRunnerConfiguration(), this.launch, monitor);
		}

	}

	/** Accessor to a Java project.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	@FunctionalInterface
	public interface IJavaProjectAccessor {
		/** Replies the Java project associated to the configuration.
		 *
		 * @param configuration the launch configuration.
		 * @return the java project.
		 * @throws CoreException  if the java project cannot be retrieved.
		 */
		IJavaProject get(ILaunchConfiguration configuration) throws CoreException;
	}

	/** Steps of preparation in the launching process.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected enum PreparationProcessState {
		/** Get launch configuration parameters.
		 */
		STEP_0_PREPARE_PARAMETERS,

		/** Build the run class path.
		 */
		STEP_1_BUILD_CLASSPATH,

		/** Prepare launching.
		 */
		STEP_2_PREPARE_LAUNCHING,

		/** Validate launching arguments.
		 */
		STEP_3_POST_VALIDATION,

		/** Create the concrete run configuration.
		 */
		STEP_4_CREATE_RUN_CONFIGURATION,

		/** Configure "stop in main".
		 */
		STEP_5_CONFIGURE_STOP_IN_MAIN;

		/** Replies the next step.
		 *
		 * @return the next step.
		 */
		public PreparationProcessState next() {
			final int index = ordinal() + 1;
			final PreparationProcessState[] vals = values();
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
	protected enum RunProcessState {
		/** Configure the source locator for run-time.
		 */
		STEP_0_CONFIGURE_SOURCE_LOCATOR,

		/** Run.
		 */
		STEP_1_RUN;

		/** Replies the next step.
		 *
		 * @return the next step.
		 */
		public RunProcessState next() {
			final int index = ordinal() + 1;
			final RunProcessState[] vals = values();
			if (index < vals.length) {
				return vals[index];
			}
			return this;
		}

	}
}

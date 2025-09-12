/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.stream.Collectors;

import com.google.common.base.Strings;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jdt.internal.launching.LaunchingPlugin;
import org.eclipse.jdt.internal.launching.RuntimeClasspathProvider;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.JavaRuntime;

import io.sarl.apputils.eclipseextensions.Extensions;
import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstall;
import io.sarl.apputils.eclipseextensions.sreprovider.ProjectSREProviderFactories;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.sreproviding.EclipseIDEProjectSREProvider;
import io.sarl.eclipse.runtime.SARLRuntime;

/**
 * Utilities for building the run [module|class]path of the SRE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@SuppressWarnings("restriction")
public final class SrePathUtils {

	private SrePathUtils() {
		//
	}

	/** Compute the class path for the given launch configuration.
	 *
	 * @param configuration the configuration that provides the classpath.
	 * @param configAccessor the accessor to the SRE configuration.
	 * @param projectAccessor the accessor to the Java project.
	 * @param classpathProviders the accessor of classpath providers.
	 * @return the filtered entries.
	 * @throws CoreException if impossible to get the classpath.
	 */
	public static IRuntimeClasspathEntry[] computeUnresolvedSARLRuntimeClasspath(ILaunchConfiguration configuration,
			ILaunchConfigurationAccessor configAccessor,
			IJavaProjectAccessor projectAccessor,
			ExtraClassPathProviders classpathProviders) throws CoreException {
		// Get the classpath from the configuration (Java classpath).
		final var entries = JavaRuntime.computeUnresolvedRuntimeClasspath(configuration);
		//
		final var addedEntries = new TreeSet<String>();
		final var filteredEntries = new ArrayList<IRuntimeClasspathEntry>();
		List<IRuntimeClasspathEntry> sreClasspathEntries = null;
		// Filtering the entries by replacing the "SARL Libraries" with the SARL runtime environment.
		for (final var entry : entries) {
			if (entry.getPath().equals(SARLClasspathContainerInitializer.CONTAINER_ID)) {
				if (sreClasspathEntries == null) {
					sreClasspathEntries = getSREClasspathEntries(configuration, configAccessor, projectAccessor);
				}
				for (final var containerEntry : sreClasspathEntries) {
					final var location = containerEntry.getLocation();
					if (Strings.isNullOrEmpty(location)) {
						filteredEntries.add(containerEntry);
					} else if (addedEntries.add(location)) {
						filteredEntries.add(containerEntry);
					}
				}
			} else {
				final var location = entry.getLocation();
				if (Strings.isNullOrEmpty(location)) {
					filteredEntries.add(entry);
				} else if (addedEntries.add(location)) {
					filteredEntries.add(entry);
				}
			}
		}
		// Get classpath from the extra contributors
		for (final var containerId : configAccessor.getExtraClasspathProviders(configuration)) {
			final var provider = classpathProviders.getProvider(containerId);
			if (provider != null) {
				final var extraEntries = provider.computeUnresolvedClasspath(configuration);
				if (extraEntries != null) {
					for (final var extraEntry : extraEntries) {
						final var location = extraEntry.getLocation();
						if (Strings.isNullOrEmpty(location)) {
							filteredEntries.add(extraEntry);
						} else if (addedEntries.add(location)) {
							filteredEntries.add(extraEntry);
						}
					}
				}
			} else {
				throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
						"Classpath provider not found: " + containerId)); //$NON-NLS-1$
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
	public static List<IRuntimeClasspathEntry> getSREClasspathEntries(
			ILaunchConfiguration configuration,
			ILaunchConfigurationAccessor configAccessor,
			IJavaProjectAccessor projectAccessor) throws CoreException {
		final var sre = getSREInstallFor(configuration, configAccessor, projectAccessor);
		return sre.getClassPathEntries();
	}

	/** Replies the SRE installation to be used for the given configuration.
	 *
	 * @param configuration the configuration to check.
	 * @param configAccessor the accessor to the SRE configuration.
	 * @param projectAccessor the accessor to the Java project.
	 * @return the SRE install.
	 * @throws CoreException if impossible to get the SRE.
	 */
	public static ISREInstall getSREInstallFor(ILaunchConfiguration configuration,
			ILaunchConfigurationAccessor configAccessor,
			IJavaProjectAccessor projectAccessor) throws CoreException {
		assert configAccessor != null;
		assert projectAccessor != null;
		final ISREInstall sre;
		if (configAccessor.getUseProjectSREFlag(configuration)) {
			sre = getProjectSpecificSRE(configuration, true, projectAccessor);
		} else if (configAccessor.getUseSystemSREFlag(configuration)) {
			sre = SARLRuntime.getDefaultSREInstall();
			if (sre != null) {
				verifySREValidity(sre, sre.getId());
			}
		} else  {
			final var runtime = configAccessor.getSREId(configuration);
			sre = SARLRuntime.getSREFromId(runtime);
			if (sre != null) {
				verifySREValidity(sre, runtime);
			}
		}

		if (sre == null) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
					Messages.SrepathUtils_0));
		}

		return sre;
	}

	/** Replies the project SRE from the given configuration.
	 *
	 * @param configuration the configuration to read.
	 * @param verify  if true verify the SRE validity, do nothing otherwise
	 * @param projectAccessor the accessor to the Java project.
	 * @return the project SRE or {@code null}.
	 * @throws CoreException Some error occurs when accessing to the ecore elements.
	 */
	private static ISREInstall getProjectSpecificSRE(ILaunchConfiguration configuration, boolean verify,
			IJavaProjectAccessor projectAccessor) throws CoreException {
		assert projectAccessor != null;
		final var jprj = projectAccessor.get(configuration);
		if (jprj != null) {
			final var prj = jprj.getProject();
			assert prj != null;

			// Get the SRE from the extension point
			var sre = getSREFromExtension(prj, verify);
			if (sre != null) {
				return sre;
			}

			// Get the SRE from the default project configuration
			final var provider = new EclipseIDEProjectSREProvider(prj);
			sre = provider.getProjectSREInstall();
			if (sre != null) {
				if (verify) {
					verifySREValidity(sre, sre.getId());
				}
				return sre;
			}
		}
		final var sre = SARLRuntime.getDefaultSREInstall();
		if (verify) {
			verifySREValidity(sre, (sre == null) ? Messages.SrepathUtils_1 : sre.getId());
		}
		return sre;
	}

	private static ISREInstall getSREFromExtension(IProject project, boolean verify) {
		final var sreInstall = ProjectSREProviderFactories.getSREProviderFactoryStreamFromExtension()
				.map(it -> {
					final var provider = it.getProjectSREProvider(project);
					if (provider != null) {
						final var sre = provider.getProjectSREInstall();
						if (sre != null) {
							return sre;
						}
					}
					return null;
				})
				.filter(it -> it != null)
				.findFirst();
		if (sreInstall.isPresent()) {
			final var sre = sreInstall.get();
			try {
				if (verify) {
					verifySREValidity(sre, sre.getId());
				}
				return sre;
			} catch (CoreException e) {
				SARLEclipsePlugin.getDefault().log(e);
			}
		}
		return null;
	}

	private static void verifySREValidity(ISREInstall sre, String runtime) throws CoreException {
		if (sre == null) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
					MessageFormat.format(io.sarl.eclipse.launching.dialog.Messages.RuntimeEnvironmentTab_6, runtime)));
		}
		final var ignoreCode = 0;
		if (!sre.getValidity(ignoreCode).isOK()) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, MessageFormat.format(
					io.sarl.eclipse.launching.dialog.Messages.RuntimeEnvironmentTab_5,
					sre.getName())));
		}
	}

	/** Accessor to the extra classpath providers.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	public static class ExtraClassPathProviders {

		private Map<String, RuntimeClasspathProvider> cpProviders;

		/** Replies the class path provider.
		 *
		 * @param identifier the identifier of the classpath provider.
		 * @return the provider or {@code null} if none.
		 */
		public RuntimeClasspathProvider getProvider(String identifier) {
			ensureProviders();
			return this.cpProviders.get(identifier);
		}

		/** Ensure that there are the providers of class paths.
		 */
		protected void ensureProviders() {
			if (this.cpProviders == null) {
				this.cpProviders = Extensions.getExtensions(
						LaunchingPlugin.ID_PLUGIN, JavaRuntime.EXTENSION_POINT_RUNTIME_CLASSPATH_PROVIDERS)
						.map(it -> new RuntimeClasspathProvider(it))
						.collect(Collectors.toMap(
								it -> it.getIdentifier(),
								it -> it,
								(oldValue, newValue) -> newValue,
								() -> new HashMap<>()));
			}
		}
	}

}

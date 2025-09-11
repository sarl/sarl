/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.eclipse.wizards.sarlapp;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.inject.Inject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.jarpackagerfat.FatJarPackagerMessages;
import org.eclipse.jdt.internal.ui.wizards.JavaProjectWizard;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jdt.ui.jarpackager.JarPackageData;
import org.eclipse.jface.viewers.IStructuredSelection;

import io.sarl.eclipse.launching.LaunchConfigurationConstants;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.runner.general.SrePathUtils;
import io.sarl.eclipse.launching.runner.general.SrePathUtils.ExtraClassPathProviders;

/**
 * SARL wizard page for exporting a SARL application into a single Jar file.
 * Most part of the code of this class comes from {@link JavaProjectWizard}.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.8
 */
@SuppressWarnings("restriction")
public class ExportSarlApplicationPage extends FixedFatJarExportPage  {

	@Inject
	private ILaunchConfigurationAccessor configAccessor;

	private final Set<String> selectedJavaProjects;

	private final ExtraClassPathProviders extraClasspathProviders = new ExtraClassPathProviders();

	/** Construct a wizard page for exporting a SARL application within a Jar file.
	 *
	 * @param jarPackage description of the package.
	 * @param selection the current selection.
	 */
	public ExportSarlApplicationPage(JarPackageData jarPackage, IStructuredSelection selection) {
		super(jarPackage, selection);
		this.selectedJavaProjects = getSelectedJavaProjects(selection);
		setTitle(Messages.ExportSarlApplicationPage_0);
		setDescription(Messages.ExportSarlApplicationPage_1);
	}

	/** Extract the names of the selected projects in order to use them for filtering.
	 *
	 * @param selection the current Eclipse selection.
	 * @return the names of the projects.
	 * @since 0.11
	 */
	private static Set<String> getSelectedJavaProjects(IStructuredSelection selection) {
		final var selectedProjects = new HashSet<String>();
		final var iter = selection.iterator();
		while (iter.hasNext()) {
			final var selectedElement = iter.next();
			if (selectedElement instanceof Iterable<?> cvalue) {
				final var subiter = cvalue.iterator();
				while (subiter.hasNext()) {
					final var selectedSubElement = subiter.next();
					if (selectedSubElement instanceof IJavaProject javaProject) {
						selectedProjects.add(javaProject.getElementName());
					}
				}
			} else if (selectedElement instanceof IJavaProject javaProject) {
				selectedProjects.add(javaProject.getElementName());
			}
		}
		return selectedProjects;
	}

	@Override
	protected LaunchConfigurationElement[] getLaunchConfigurations() {
		final var result = new ArrayList<ExistingLaunchConfigurationElement>();

		try {
			final var manager = DebugPlugin.getDefault().getLaunchManager();
			final var type = manager.getLaunchConfigurationType(LaunchConfigurationConstants.ID_APPLICATION_LAUNCH_CONFIGURATION);
			getLaunchConfiguration(result, type, manager, this.selectedJavaProjects);
		} catch (CoreException e) {
			JavaPlugin.log(e);
		}

		return result.toArray(new LaunchConfigurationElement[result.size()]);
	}

	private static void getLaunchConfiguration(List<ExistingLaunchConfigurationElement> result, ILaunchConfigurationType type,
			ILaunchManager manager, Set<String> selectedProjects) throws CoreException {
		final var launchconfigs = manager.getLaunchConfigurations(type);
		for (var i = 0; i < launchconfigs.length; ++i) {
			final var launchconfig = launchconfigs[i];
			if (!launchconfig.getAttribute(IDebugUIConstants.ATTR_PRIVATE, false)) {
				final var projectName = launchconfig.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""); //$NON-NLS-1$
				if (selectedProjects.isEmpty() || selectedProjects.contains(projectName)) {
					result.add(new ExistingLaunchConfigurationElement(launchconfig, projectName));
				}
			}
		}
	}

	@Override
	protected IPath[] getClasspath(ILaunchConfiguration configuration) throws CoreException {
		var entries = SrePathUtils.computeUnresolvedSARLRuntimeClasspath(
				configuration, this.configAccessor, cfg -> getJavaProject(cfg), this.extraClasspathProviders);

		entries = JavaRuntime.resolveRuntimeClasspath(entries, configuration);

		final var userEntries = new ArrayList<IPath>(entries.length);
		for (var i = 0; i < entries.length; i++) {
			final var classPathProperty = entries[i].getClasspathProperty();
			if (classPathProperty == IRuntimeClasspathEntry.USER_CLASSES
					|| classPathProperty == IRuntimeClasspathEntry.CLASS_PATH
					|| classPathProperty == IRuntimeClasspathEntry.MODULE_PATH) {

				final var location = entries[i].getLocation();
				if (location != null) {
					final var entry = Path.fromOSString(location);
					if (!userEntries.contains(entry)) {
						userEntries.add(entry);
					}
				}
			}
		}
		return userEntries.toArray(new IPath[userEntries.size()]);
	}

	/**
	 * Returns the Java project name specified by the given launch
	 * configuration, or {@code null} if none.
	 *
	 * @param configuration
	 *            launch configuration
	 * @return the Java project name specified by the given launch
	 *         configuration, or {@code null} if none
	 * @exception CoreException
	 *                if unable to retrieve the attribute
	 */
	protected static String getJavaProjectName(ILaunchConfiguration configuration)
			throws CoreException {
		return configuration.getAttribute(
				IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
				(String) null);
	}

	/**
	 * Returns the Java project specified by the given launch configuration, or
	 * {@code null} if none.
	 *
	 * @param configuration
	 *            launch configuration
	 * @return the Java project specified by the given launch configuration, or
	 *         {@code null} if none
	 * @exception CoreException
	 *                if unable to retrieve the attribute
	 */
	protected static IJavaProject getJavaProject(ILaunchConfiguration configuration)
			throws CoreException {
		var projectName = getJavaProjectName(configuration);
		if (projectName != null) {
			projectName = projectName.trim();
			if (projectName.length() > 0) {
				final var project = ResourcesPlugin.getWorkspace().getRoot()
						.getProject(projectName);
				final var javaProject = JavaCore.create(project);
				if (javaProject != null && javaProject.exists()) {
					return javaProject;
				}
			}
		}
		return null;
	}

	@Override
	protected IPackageFragmentRoot[] getRequiredPackageFragmentRoots(IPath[] classpathEntries, String projectName, MultiStatus status) {
		final var result = new ArrayList<IPackageFragmentRoot>();

		final var searchOrder = getProjectSearchOrder(projectName);
		final var project = getJavaProject(projectName);

		for (var i = 0; i < classpathEntries.length; ++i) {
			final var entry = classpathEntries[i];
			final var elements = findRootsForClasspath(entry, searchOrder);
			if (elements == null) {
				final IPackageFragmentRoot element;
				final var file = entry.toFile();
				if (file.exists()) {
					element = project.getPackageFragmentRoot(file.getAbsolutePath());
				} else {
					element = null;
				}
				if (element != null) {
					result.add(element);
				} else {
					status.add(new Status(IStatus.WARNING, JavaUI.ID_PLUGIN,
							org.eclipse.jdt.internal.corext.util.Messages.format(
									FatJarPackagerMessages.FatJarPackageWizardPage_error_missingClassFile,
									getPathLabel(entry, false))));
				}
			} else {
				for (int j = 0; j < elements.length; ++j) {
					result.add(elements[j]);
				}
			}
		}

		return result.toArray(new IPackageFragmentRoot[result.size()]);
	}

}

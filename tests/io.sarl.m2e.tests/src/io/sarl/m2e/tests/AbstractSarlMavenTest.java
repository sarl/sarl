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
package io.sarl.m2e.tests;

import static org.eclipse.xtext.junit4.ui.util.IResourcesSetupUtil.reallyWaitForAutoBuild;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.inject.Inject;

import com.google.inject.Binder;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Provider;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.m2e.core.internal.MavenPluginActivator;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.xtext.junit4.ui.util.JavaProjectSetupUtil;
import org.eclipse.xtext.ui.XtextProjectHelper;
import org.eclipse.xtext.ui.util.PluginProjectFactory;
import org.eclipse.xtext.util.JavaVersion;
import org.junit.Assume;
import org.junit.ComparisonFailure;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.lang.SARLConfig;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.WorkbenchTestHelper;
import io.sarl.tests.api.WorkbenchTestHelper.ProjectCreator;


/** Abstract implementation of a test for SARL Maven projects.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractSarlMavenTest extends AbstractSarlUiTest {

	protected String[] MAVEN_RUN_ERROR_MARKER_IDS = new String[] {
		"org.eclipse.m2e.core.maven2Problem.pomloading",
		"org.eclipse.m2e.core.maven2Problem.dependency",
	};

	/** Create and compile a maven project from the given content of the POM file.
	 *
	 * @param pomFileContent the content of he POM file.
	 * @return the markers on the project.
	 * @throws Exception if any exception.
	 */
	protected IMarker[] createMavenProject(String pomFileContent) throws Exception {
		IFile pomFile = helper().createFileImpl(helper().getFullFileNameInProject("pom.xml"), pomFileContent);
		IProject project = helper().getProject();
		IProjectDescription description = project.getDescription();
		String[] natures = description.getNatureIds();
		String[] newNatures = new String[natures.length];
		System.arraycopy(natures, 0, newNatures, 0, natures.length);
		newNatures[newNatures.length - 1] = SARLEclipseConfig.MAVEN_NATURE_ID;
		description.setNatureIds(newNatures);
		project.setDescription(description, new NullProgressMonitor());

		helper().awaitAutoBuild();

		MavenPluginActivator.getDefault().getProjectConfigurationManager().updateProjectConfiguration(
				project, new NullProgressMonitor());

		helper().awaitAutoBuild();

		return project.findMarkers(null, true, IResource.DEPTH_INFINITE); 
	}

	/** Check if one of the markers has the given message part.
	 *
	 * @param messagePart
	 * @param markers
	 */
	protected void assertContainsMarker(String messagePart, IMarker... markers) {
		StringBuilder markerText = new StringBuilder();
		for (IMarker marker : markers) {
			try {
				String markerMsg = marker.getAttribute(IMarker.MESSAGE).toString();
				if (markerMsg.contains(messagePart)) {
					return;
				}
				markerText.append(markerMsg);
				markerText.append("\n");
			} catch (CoreException exception) {
				//
			}
		}
		throw new ComparisonFailure("Missed marker: " + messagePart,
				messagePart,
				markerText.toString());
	}

	/** Check if none of the markers has the given message part.
	 * 
	 * @param messagePart
	 * @param markers
	 */
	protected void assertNotContainsMarker(String messagePart, IMarker... markers) {
		boolean found = false;
		StringBuilder markerText = new StringBuilder();
		for (IMarker marker : markers) {
			try {
				String markerMsg = marker.getAttribute(IMarker.MESSAGE).toString();
				if (markerMsg.contains(messagePart)) {
					found = true;
				}
				markerText.append(markerMsg);
				markerText.append("\n");
			} catch (CoreException exception) {
				//
			}
		}
		if (found) {
			throw new ComparisonFailure("Unexpected marker: " + messagePart,
					messagePart,
					markerText.toString());
		}
	}
	
	private boolean containsMavenErrorMarkers(IMarker[] markers) {
		List<String> ids = Arrays.asList(MAVEN_RUN_ERROR_MARKER_IDS);
		for (IMarker marker : markers) {
			try {
				if (ids.contains(marker.getType())) {
					return true;
				}
			} catch (CoreException exception) {
				//
			}
		}
		return false;
	}

	public void assumeMavenRunning(IMarker[] markers) {
		Assume.assumeFalse(containsMavenErrorMarkers(markers));
	}

	@Override
	public Injector getInjector() {
		return getInjectedInjector().createChildInjector(new Module() {
			@Override
			public void configure(Binder binder) {
				binder.bind(ProjectCreator.class).to(MavenProjectCreator.class);
				binder.bind(JavaVersion.class).toProvider(new Provider<JavaVersion>() {
					@Override
					public JavaVersion get() {
						return JavaVersion.JAVA8;
					}
				});
			}
		});
	}

	/** Factory of a Java project.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class MavenPluginProjectFactory extends PluginProjectFactory {

		@Override
		protected void createFolders(IProject project, SubMonitor subMonitor,
				Shell shell) throws CoreException {
			if (this.folders != null) {
				for (final String folderName : this.folders) {
					IPath path = Path.fromPortableString(folderName);
					IPath tmpPath = Path.EMPTY;
					for (String segment : path.segments()) {
						tmpPath = tmpPath.append(segment);
						IFolder folder = project.getFolder(tmpPath.toPortableString());
						if (!folder.exists()) {
							folder.create(false, true, subMonitor.newChild(1));
						}
					}
				}
			}
		}

	}

	/** Factory of a Java project.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class MavenProjectCreator implements ProjectCreator {

		@Inject
		private Injector injector;

		@Inject
		@NonNullByDefault
		private JavaVersion javaVersion;

		@Override
		public Injector getInjector() {
			return this.injector;
		}

		@Override
		public PluginProjectFactory getProjectFactory() {
			return getInjector().getInstance(MavenPluginProjectFactory.class);
		}

		@Override
		public JavaVersion getJavaVersion() {
			return this.javaVersion;
		}

		@Override
		public List<String> getSourceFolders() {
			return Arrays.asList(
					SARLConfig.FOLDER_SOURCE_JAVA,
					SARLConfig.FOLDER_SOURCE_SARL,
					SARLConfig.FOLDER_SOURCE_GENERATED_XTEXT);
		}

		@Override
		public String getGenerationFolder() {
			return SARLConfig.FOLDER_SOURCE_GENERATED;
		}

		@Override
		public String[] getBuilderIds() {
			return new String[] {
					XtextProjectHelper.BUILDER_ID,
					JavaCore.BUILDER_ID
			};
		}

		@Override
		public String[] getNatures() {
			return new String[] {
					XtextProjectHelper.NATURE_ID,
					JavaCore.NATURE_ID,
					WorkbenchTestHelper.NATURE_ID,
					SARLEclipseConfig.MAVEN_NATURE_ID
			};
		}

		@Override
		public void addJreClasspathEntry(IJavaProject javaProject) throws JavaModelException {
			JavaProjectSetupUtil.addJreClasspathEntry(javaProject);
		}

		@Override
		public void addToClasspath(IJavaProject javaProject, IClasspathEntry newClassPathEntry)throws JavaModelException {
			JavaProjectSetupUtil.addToClasspath(javaProject, newClassPathEntry);
		}

		@Override
		public void addToClasspath(IJavaProject javaProject, boolean autobuild,
				Iterable<IClasspathEntry> newClassPathEntries) throws JavaModelException {
			List<IClasspathEntry> newClassPath = new ArrayList<>(Arrays.asList(javaProject.getRawClasspath()));
			for (IClasspathEntry classPathEntry : newClassPathEntries) {
				if (!newClassPath.contains(classPathEntry)) {
					newClassPath.add(classPathEntry);
				}
			}
			IClasspathEntry[] classPath = new IClasspathEntry[newClassPath.size()];
			newClassPath.toArray(classPath);
			javaProject.setRawClasspath(classPath, null);
			if (autobuild) {
				reallyWaitForAutoBuild();
			}			
		}

	}

}
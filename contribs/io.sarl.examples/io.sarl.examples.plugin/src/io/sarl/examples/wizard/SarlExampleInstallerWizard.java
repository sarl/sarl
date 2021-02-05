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

package io.sarl.examples.wizard;

import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.LAUNCH_PROPERTY_FILE;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.readLaunchConfigurationFromXml;
import static io.sarl.examples.wizard.XmlUtils.readXmlContent;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import javax.inject.Inject;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.emf.common.ui.wizard.ExampleInstallerWizard;
import org.eclipse.jdt.launching.AbstractVMInstall;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.intro.IIntroManager;
import org.eclipse.ui.intro.IIntroPart;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.RuntimeIOException;
import org.eclipse.xtext.util.StringInputStream;
import org.osgi.framework.Version;
import org.w3c.dom.Document;

import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;
import io.sarl.eclipse.launching.shortcuts.SarlStandardClasspathProvider;
import io.sarl.eclipse.natures.SARLProjectConfigurator;
import io.sarl.examples.SARLExamplePlugin;
import io.sarl.lang.SARLConfig;
import io.sarl.lang.SARLVersion;
import io.sarl.m2e.wizards.importproject.MavenImportUtils;

/** Wizard for importing SARL samples.
 *
 * <p>This wizard extends the EMF wizard with the initialization of the SARL nature on the project,
 * the creation of launch configurations, and the closing of the welcome page of Eclipse.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SarlExampleInstallerWizard extends ExampleInstallerWizard {

	private static final int MIN_JDK_VERSION = 9;

	private static final String USER_JAVA_VERSION_KEY = "@USER_JAVA_VERSION@"; //$NON-NLS-1$

	private static final String START_JDK11_KEY = "<!-- @IF JDK11"; //$NON-NLS-1$

	private static final String START_JDK11_COMMENT = "<!-- @IF JDK11 -->"; //$NON-NLS-1$

	private static final String END_JDK11_KEY = "@ENDIF JDK11 -->"; //$NON-NLS-1$

	private static final String END_JDK11_COMMENT = "<!-- @ENDIF JDK11 -->"; //$NON-NLS-1$

	private ConfigurationPage configurationPage;

	private List<ConfigurationToLaunch> configurationsToLaunch;

	private ILaunchConfigurationConfigurator launchConfigConfigurator;

	/** Change the launch configuration configurator.
	 *
	 * @param launchConfigConfigurator the new configurator.
	 * @since 0.11
	 */
	@Inject
	public void setLaunchConfigurationConfigurator(ILaunchConfigurationConfigurator launchConfigConfigurator) {
		this.launchConfigConfigurator = launchConfigConfigurator;
	}

	/** Replies the launch configuration configurator.
	 *
	 * @return configurator.
	 * @since 0.11
	 */
	public ILaunchConfigurationConfigurator getLaunchConfigurationConfigurator() {
		assert this.launchConfigConfigurator != null : "Extension factory is missed for the example in the plugin.xml file";
		return this.launchConfigConfigurator;
	}

	@Override
	public void addPages() {
		// Override the default "addPages" in order to create a configuration page.
		super.addPages();
		this.configurationPage = new ConfigurationPage("configurationPage", Messages.SarlExampleInstallerWizard_0); //$NON-NLS-1$
		this.configurationPage.setDescription(Messages.SarlExampleInstallerWizard_1);
		addPage(this.configurationPage);
	}

	@Override
	public void dispose() {
		this.configurationPage.dispose();
		this.configurationPage = null;
		super.dispose();
	}

	private void fixFilesToOpen(IProject project) {
		if (this.filesToOpen != null) {
			final List<FileToOpen> fixedList = new ArrayList<>();
			for (final FileToOpen file : this.filesToOpen) {
				if (!(file instanceof ProjectFileToOpen)) {
					final ProjectFileToOpen projectFile = new ProjectFileToOpen(project, file);
					fixedList.add(projectFile);
				} else {
					fixedList.add(file);
				}
			}
			this.filesToOpen = fixedList;
		}
	}

	@Override
	protected void installProject(ProjectDescriptor projectDescriptor, IProgressMonitor progressMonitor)
			throws Exception {
		final SubMonitor mon = SubMonitor.convert(progressMonitor, 2);
		super.installProject(projectDescriptor, mon.newChild(1));
		postProjectInstallation(projectDescriptor, mon.newChild(1));
	}

	private JavaVersion parseVersion(AbstractVMInstall vmInstall, String minVersion) {
		final String vmVersion = vmInstall.getJavaVersion();
		final JavaVersion minJversion = JavaVersion.fromQualifier(minVersion);
		JavaVersion jversion = JavaVersion.fromQualifier(vmVersion);
		if (jversion == null) {
			final Version vers = Version.parseVersion(vmVersion);
			// This is a hard-coded support for the different version formats (1.x or x)
			// when using Java 9 or higher.
			if (vers.getMajor() >= MIN_JDK_VERSION) {
				jversion = JavaVersion.fromQualifier(Integer.toString(vers.getMajor()));
			} else {
				jversion = JavaVersion.fromQualifier(vers.getMajor() + "." + vers.getMinor());
			}
		}
		if (jversion != null && minJversion != null && !jversion.isAtLeast(minJversion)) {
			jversion = minJversion;
		}
		return jversion;
	}

	/** Post process the given project.
	 *
	 * @param projectDescriptor the descriptor of the project.
	 * @param progressMonitor the progression monitor.
	 * @throws Exception if the post process cannot be applied.
	 */
	protected void postProjectInstallation(ProjectDescriptor projectDescriptor, IProgressMonitor progressMonitor) throws Exception {
		final SubMonitor mon = SubMonitor.convert(progressMonitor, 3);

		// Force the natures of the project
		final IProject project = projectDescriptor.getProject();

		final IFile pomFile = project.getFile(Path.fromOSString("pom.xml")); //$NON-NLS-1$
		final boolean hasPomFile = pomFile.exists();
		if (hasPomFile) {
			// Search for specific keywords into the pom file, and replace them by the user configuration.
			String compliance = SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT;
			final IVMInstall vmInstall = JavaRuntime.getDefaultVMInstall();
			if (vmInstall instanceof AbstractVMInstall) {
				final AbstractVMInstall jvmInstall = (AbstractVMInstall) vmInstall;
				final JavaVersion jversion = parseVersion(jvmInstall, compliance);
				if (jversion != null) {
					compliance = jversion.getQualifier();
				}
			}
			updatePomContent(pomFile, compliance);
		}
		if (this.configurationPage.isMavenNatureEnabled() && pomFile.exists()) {
			// The project should be a Maven project.
			final IPath descriptionFilename = project.getFile(new Path(IProjectDescription.DESCRIPTION_FILE_NAME)).getLocation();
			final File projectDescriptionFile = descriptionFilename.toFile();
			// Project was open by the super class. Close it because Maven fails when a project already exists.
			project.close(mon.newChild(1));
			// Delete the Eclipse project definition because Maven fails when a project already exists.
			project.delete(false, true, mon.newChild(1));
			if (projectDescriptionFile.exists()) {
				projectDescriptionFile.delete();
			}
			// Import
			MavenImportUtils.importMavenProject(
					project.getWorkspace().getRoot(),
					projectDescriptor.getName(),
					true,
					mon.newChild(1));
		} else {
			// Force the project configuration to SARL.
			SARLProjectConfigurator.configureSARLProject(
					// Project to configure
					project,
					// Add SARL natures
					true,
					// Force java configuration
					true,
					// Create folders
					true,
					// Monitor
					mon.newChild(1));
		}

		// Fixing the names of the files to open because the project's names were not originally specified
		fixFilesToOpen(project);

		// Install the launch configuration(s)
		if (this.configurationPage.isLaunchConfigurationInstallable()) {
			try {
				final List<ConfigurationToLaunch> configs = getConfigurationsToLaunch(project);
				for (final ConfigurationToLaunch it : configs) {
					try {
						if (it.isAgentLaunch()) {
							createAgentLaunchConfiguration(project, it.getType(), it.getName());
						} else {
							createApplicationLaunchConfiguration(project, it.getType(), it.getName());
						}
					} catch (CoreException exception) {
						SARLExamplePlugin.getDefault().openError(getShell(),
								io.sarl.eclipse.util.Messages.AbstractSarlScriptInteractiveSelector_1,
								exception.getStatus().getMessage(), exception.getStatus().getMessage(), exception);
					}
				}
			} catch (CoreException exception) {
				SARLExamplePlugin.getDefault().openError(getShell(),
						io.sarl.eclipse.util.Messages.AbstractSarlScriptInteractiveSelector_1,
						exception.getStatus().getMessage(), null, exception);
			} catch (Exception exception) {
				SARLExamplePlugin.getDefault().openError(getShell(),
						io.sarl.eclipse.util.Messages.AbstractSarlScriptInteractiveSelector_1,
						exception.getMessage(), exception.getMessage(), exception);
			}
		}

		mon.done();
	}

	private void updatePomContent(IFile pomFile, String jdkCompliance) {
		JavaVersion jversion = JavaVersion.fromQualifier(jdkCompliance);
		if (jversion != null && !jversion.isAtLeast(JavaVersion.JAVA9)) {
			jversion = null;
		}
		// Read the pom
		final StringBuilder content = new StringBuilder();
		try (BufferedReader reader = new BufferedReader(new InputStreamReader(pomFile.getContents()))) {
			String line = reader.readLine();
			while (line != null) {
				line = line.replaceAll(Pattern.quote(USER_JAVA_VERSION_KEY), jdkCompliance);
				if (jversion != null) {
					line = line.replaceAll(Pattern.quote(START_JDK11_KEY), START_JDK11_COMMENT);
					line = line.replaceAll(Pattern.quote(END_JDK11_KEY), END_JDK11_COMMENT);
				}
				content.append(line).append("\n"); //$NON-NLS-1$
				line = reader.readLine();
			}
		} catch (Exception exception) {
			throw new RuntimeIOException(exception);
		}
		// Delete the pom
		try {
			pomFile.delete(true, false, new NullProgressMonitor());
		} catch (CoreException exception) {
			throw new RuntimeException(exception);
		}
		// Write the pom
		try (StringInputStream is = new StringInputStream(content.toString())) {
			pomFile.create(is, true, new NullProgressMonitor());
		} catch (CoreException exception) {
			throw new RuntimeException(exception);
		} catch (IOException exception) {
			throw new RuntimeIOException(exception);
		}
	}

	/** Replies the configurations to launch.
	 * These configurations are inside an xml file with the name {@link #LAUNCH_PROPERTY_FILE}.
	 *
	 * @param project the project to analyze.
	 * @return the configurations to launch.
	 * @throws CoreException if the launch configuration cannot be read.
	 * @since 0.10
	 */
	public List<ConfigurationToLaunch> getConfigurationsToLaunch(IProject project) throws CoreException {
		if (this.configurationsToLaunch == null) {
			this.configurationsToLaunch = new ArrayList<>();
			final IPath rootLocation = project.getWorkspace().getRoot().getLocation();
			final IPath configLocation = rootLocation.append(project.getName()).append(LAUNCH_PROPERTY_FILE);
			final File jFile = configLocation.toFile();
			if (jFile.canRead()) {
				final Document document = readXmlContent(jFile);
				if (document != null) {
					readLaunchConfigurationFromXml(document, null, (type, name, isAgent, rootFolder) -> {
						final ConfigurationToLaunch ctl = new ConfigurationToLaunch();
						ctl.setType(type);
						ctl.setName(name);
						ctl.setAgentLaunch(isAgent);
						this.configurationsToLaunch.add(ctl);
					});
				}
			}
		}
		return this.configurationsToLaunch;
	}

	/** Create the application launch configuration.
	 *
	 * @param project the associated project.
	 * @param mainClassfullyQualifedName the fully qualified name of the main class.
	 * @param configurationName the proposed name for the launch configuration.
	 * @throws CoreException if the launch configuration cannot be created.
	 * @since 0.10
	 */
	protected void createApplicationLaunchConfiguration(IProject project, String mainClassfullyQualifedName,
			String configurationName) throws CoreException {
		final ILaunchConfigurationConfigurator configurator = getLaunchConfigurationConfigurator();
		if (configurator != null) {
			final String projectName = project.getName();
			configurator.newApplicationLaunchConfiguration(projectName,
					configurationName, mainClassfullyQualifedName, SarlStandardClasspathProvider.class);
		}
	}

	/** Create the application launch configuration.
	 *
	 * @param project the associated project.
	 * @param agentFullyQualifiedName the fully qualified name of the agent to launch.
	 * @param configurationName the proposed name for the launch configuration.
	 * @throws CoreException if the launch configuration cannot be created.
	 * @since 0.10Agent
	 */
	protected void createAgentLaunchConfiguration(IProject project, String agentFullyQualifiedName,
			String configurationName) throws CoreException {
		final ILaunchConfigurationConfigurator configurator = getLaunchConfigurationConfigurator();
		if (configurator != null) {
			final String projectName = project.getName();
			configurator.newAgentLaunchConfiguration(projectName,
					configurationName, agentFullyQualifiedName);
		}
	}

	@Override
	public boolean performFinish() {
		if (super.performFinish()) {
			// Close the welcome page.
			closeWelcomePage();
			return true;
		}
		if (this.configurationPage != null && !this.configurationPage.getControl().isDisposed()) {
			this.configurationPage.refresh();
		}
		return false;
	}

	/** Close the welcome page.
	 */
	protected static void closeWelcomePage() {
		final IIntroManager introManager = PlatformUI.getWorkbench().getIntroManager();
		if (introManager != null) {
			final IIntroPart intro = introManager.getIntro();
			if (intro != null) {
				introManager.closeIntro(intro);
			}
		}
	}

	/** Configuration page.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	public class ConfigurationPage extends WizardPage {

		/** Indicates if the maven nature must be enabled by default.
		 */
		public static final boolean DEFAULT_MAVEN_NATURE = true;

		private Button mavenProjectButton;

		private Button installLaunchConfigButton;

		/** Constructor.
		 *
		 * @param pageName the name of the page.
		 * @param title the title.
		 */
		public ConfigurationPage(String pageName, String title) {
			super(pageName, title, null);
		}

		@Override
		public void createControl(Composite parent) {
			final Composite composite = new Composite(parent, SWT.NONE);
			final GridLayout layout = new GridLayout(2, false);
			final int margin = -5;
			final int spacing = 3;
			layout.marginTop = margin;
			layout.marginLeft = margin;
			layout.marginRight = margin;
			layout.marginBottom = margin;
			layout.horizontalSpacing = spacing;
			layout.verticalSpacing = spacing;
			composite.setLayout(layout);

			this.mavenProjectButton = SWTFactory.createCheckButton(composite,
					Messages.SarlExampleInstallerWizard_2, null, DEFAULT_MAVEN_NATURE, 2);

			this.installLaunchConfigButton = SWTFactory.createCheckButton(composite,
					Messages.SarlExampleInstallerWizard_3, null, true, 2);

			refresh();
			setControl(composite);
		}

		/** Refresh the configuration page.
		 */
		public void refresh() {
			setErrorMessage(null);
			setPageComplete(true);
		}

		/** Replies if the maven nature must be enabled.
		 *
		 * @return {@code true} for enabling the maven nature.
		 */
		public boolean isMavenNatureEnabled() {
			return this.mavenProjectButton.getSelection();
		}

		/** Replies if the launch configurations could be installed if they are available.
		 *
		 * @return {@code true} for installing the launch configurations.
		 */
		public boolean isLaunchConfigurationInstallable() {
			return this.installLaunchConfigButton.getSelection();
		}

	}

	/** A file to be opened that is associated to a project.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	public static class ProjectFileToOpen extends FileToOpen {

		private WeakReference<IProject> project;

		private IFile projectFile;

		/** Constructor.
		 *
		 * @param project the container of the file.
		 * @param source the source description.
		 */
		public ProjectFileToOpen(IProject project, FileToOpen source) {
			this.project = new WeakReference<>(project);
			setLocation(source.getLocation());
			setEditorID(source.getEditorID());
		}

		/** Replies the file into the project.
		 *
		 * @return the file or {@code null} if the file cannot be found.
		 */
		@SuppressWarnings("checkstyle:nestedifdepth")
		public IFile findProjectFile() {
			if (this.projectFile == null) {
				final IProject prj = this.project.get();
				if (prj != null) {
					// 3 cases for location:
					// * prj / src folder / source path
					// * src folder / source path
					// * source path
					final IPath path0 = Path.fromPortableString(getLocation());
					IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(path0);
					if (file != null && file.exists()) {
						this.projectFile = file;
					} else {
						file = prj.getFile(path0);
						if (file != null && file.exists()) {
							this.projectFile = file;
						} else {
							final IPath srcFolder = Path.fromOSString(SARLConfig.FOLDER_SOURCE_SARL);
							final IPath path1 = srcFolder.append(path0);
							file = prj.getFile(path1);
							if (file != null && file.exists()) {
								this.projectFile = file;
							}
						}
					}
				}
			}
			return this.projectFile;
		}

		@Override
		public IFile getWorkspaceFile() {
			if (this.workspaceFile == null) {
				final IFile projectFile = findProjectFile();
				if (projectFile != null) {
					this.workspaceFile = projectFile;
				} else {
					return super.getWorkspaceFile();
				}
			}
			return this.workspaceFile;
		}

	}

	/** Description of a launch configuration.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	public static class ConfigurationToLaunch {

		/** Name of the configuration.
		 */
		protected String name;

		/** Type to launch.
		 */
		protected String type;

		/** Indicates if the launch configuration is for an agent.
		 */
		protected boolean isAgentLaunch;

		/** Replies the name of the configuration.
		 *
		 * @return the name.
		 */
		public String getName() {
			return this.name;
		}

		/** Change the name of the configuration.
		 *
		 * @param name the name.
		 */
		public void setName(String name) {
			this.name = name;
		}

		/** Replies the type to launch.
		 *
		 * @return the type.
		 */
		public String getType() {
			return this.type;
		}

		/** Change the type to launch.
		 *
		 * @param type the type.
		 */
		public void setType(String type) {
			this.type = type;
		}

		/** Indicates if the launch configuration is for an agent.
		 *
		 * @return {@code true} if the configuration is for launching an agent;
		 *     {@code false} if it is for launching an application.
		 */
		public boolean isAgentLaunch() {
			return this.isAgentLaunch;
		}

		/** Change the flag that indicates if the launch configuration is for an agent.
		 *
		 * @param isAgent is {@code true} if the configuration is for launching an agent;
		 *     {@code false} if it is for launching an application.
		 */
		public void setAgentLaunch(boolean isAgent) {
			this.isAgentLaunch = isAgent;
		}

	}

}

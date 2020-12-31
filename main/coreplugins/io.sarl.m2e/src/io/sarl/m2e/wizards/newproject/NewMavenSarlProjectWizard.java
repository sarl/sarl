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

package io.sarl.m2e.wizards.newproject;

import java.nio.charset.Charset;

import com.google.common.collect.Iterables;
import org.apache.maven.model.Build;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.model.Plugin;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.m2e.core.MavenPlugin;
import org.eclipse.m2e.core.internal.IMavenConstants;
import org.eclipse.m2e.core.ui.internal.wizards.MavenProjectWizard;

import io.sarl.lang.SARLVersion;

/**
 * Wizard for creating a maven-based SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public final class NewMavenSarlProjectWizard extends MavenProjectWizard {

	private static final String SARL_VERSION_PROPERTY = "sarl.version"; //$NON-NLS-1$

	private static final String TARGET_JDK_VERSION_PROPERTY = "target.jdk.version"; //$NON-NLS-1$

	private static final String ENCODING_PROPERTY = "project.build.sourceEncoding"; //$NON-NLS-1$

	private static final String GROUP_ID = "io.sarl.maven"; //$NON-NLS-1$

	private static final String SDK_ARTIFACT_ID = "io.sarl.maven.sdk"; //$NON-NLS-1$

	private static final String VERSION = "${sarl.version}"; //$NON-NLS-1$

	private static final String PLUGIN_ARTIFACT_ID = "sarl-maven-plugin"; //$NON-NLS-1$

	private static final String CONFIGURATION_SOURCE_NAME = "source"; //$NON-NLS-1$

	private static final String CONFIGURATION_TARGET_NAME = "target"; //$NON-NLS-1$

	private static final String CONFIGURATION_ENCODING_NAME = "encoding"; //$NON-NLS-1$

	private static final String CONFIGURATION_KEY = "configuration"; //$NON-NLS-1$

	private static final String CONFIGURATION_LEVEL_VALUE = "${target.jdk.version}"; //$NON-NLS-1$

	private static final String CONFIGURATION_ENCODING_VALUE = "${project.build.sourceEncoding}"; //$NON-NLS-1$

	private static final String JAVA_GROUP_ID = "org.apache.maven.plugins"; //$NON-NLS-1$

	private static final String JAVA_ARTIFACT_ID = "maven-compiler-plugin"; //$NON-NLS-1$

	private Model lastModel;

	@Override
	public Model getModel() {
		final Model model = super.getModel();

		model.addProperty(SARL_VERSION_PROPERTY, SARLVersion.SARL_RELEASE_VERSION_MAVEN);
		model.addProperty(TARGET_JDK_VERSION_PROPERTY, SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);
		model.addProperty(ENCODING_PROPERTY, Charset.defaultCharset().displayName());

		final Dependency dep = new Dependency();
		dep.setGroupId(GROUP_ID);
		dep.setArtifactId(SDK_ARTIFACT_ID);
		dep.setVersion(VERSION);
		model.addDependency(dep);

		Build build = model.getBuild();
		if (build == null) {
			build = new Build();
			model.setBuild(build);
		}

		//We need to force the re-generation of the plugin map as it may be stale
		build.flushPluginMap();
		Plugin compilerPlugin = build.getPluginsAsMap().get(JAVA_GROUP_ID + ":" + JAVA_ARTIFACT_ID); //$NON-NLS-1$
		if (compilerPlugin == null) {
			compilerPlugin = build.getPluginsAsMap().get(JAVA_ARTIFACT_ID);
		}
		if (compilerPlugin == null) {
			compilerPlugin = new Plugin();
			compilerPlugin.setGroupId(JAVA_GROUP_ID);
			compilerPlugin.setArtifactId(JAVA_ARTIFACT_ID);
			final Xpp3Dom configuration = new Xpp3Dom(CONFIGURATION_KEY);
			final Xpp3Dom sourceDom = new Xpp3Dom(CONFIGURATION_SOURCE_NAME);
			sourceDom.setValue(CONFIGURATION_LEVEL_VALUE);
			configuration.addChild(sourceDom);
			final Xpp3Dom targetDom = new Xpp3Dom(CONFIGURATION_TARGET_NAME);
			targetDom.setValue(CONFIGURATION_LEVEL_VALUE);
			configuration.addChild(targetDom);
			final Xpp3Dom encodingDom = new Xpp3Dom(CONFIGURATION_ENCODING_NAME);
			encodingDom.setValue(CONFIGURATION_ENCODING_VALUE);
			configuration.addChild(encodingDom);
			compilerPlugin.setConfiguration(configuration);
			build.addPlugin(compilerPlugin);
		}

		//We need to force the re-generation of the plugin map as it may be stale
		build.flushPluginMap();
		Plugin sarlPlugin = build.getPluginsAsMap().get(GROUP_ID + ":" + PLUGIN_ARTIFACT_ID); //$NON-NLS-1$
		if (sarlPlugin == null) {
			sarlPlugin = build.getPluginsAsMap().get(PLUGIN_ARTIFACT_ID);
		}
		if (sarlPlugin == null) {
			sarlPlugin = new Plugin();
			sarlPlugin.setGroupId(GROUP_ID);
			sarlPlugin.setArtifactId(PLUGIN_ARTIFACT_ID);
			sarlPlugin.setVersion(VERSION);
			// Do not turn on the "extensions" feature because it cause an invalid initialization of
			// the Maven nature of the project.
			sarlPlugin.setExtensions(false);
			final Xpp3Dom configuration = new Xpp3Dom(CONFIGURATION_KEY);
			final Xpp3Dom sourceDom = new Xpp3Dom(CONFIGURATION_SOURCE_NAME);
			sourceDom.setValue(CONFIGURATION_LEVEL_VALUE);
			configuration.addChild(sourceDom);
			final Xpp3Dom targetDom = new Xpp3Dom(CONFIGURATION_TARGET_NAME);
			targetDom.setValue(CONFIGURATION_LEVEL_VALUE);
			configuration.addChild(targetDom);
			final Xpp3Dom encodingDom = new Xpp3Dom(CONFIGURATION_ENCODING_NAME);
			encodingDom.setValue(CONFIGURATION_ENCODING_VALUE);
			configuration.addChild(encodingDom);
			sarlPlugin.setConfiguration(configuration);
			build.addPlugin(sarlPlugin);
			build.flushPluginMap();
		}

		this.lastModel = model;

		return model;
	}

	@Override
	public boolean performFinish() {
		if (!super.performFinish()) {
			return false;
		}
		final Job job = new WorkspaceJob("Force the SARL nature") { //$NON-NLS-1$
			@SuppressWarnings({ "deprecation", "synthetic-access" })
			@Override
			public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
				final Model model = NewMavenSarlProjectWizard.this.lastModel;
				if (model != null) {
					final Plugin plugin = Iterables.find(model.getBuild().getPlugins(), it -> PLUGIN_ARTIFACT_ID.equals(it.getArtifactId()));
					plugin.setExtensions(true);
					final IWorkspace workspace = ResourcesPlugin.getWorkspace();
					final IWorkspaceRoot root = workspace.getRoot();
					final IProject project = NewMavenSarlProjectWizard.this.importConfiguration.getProject(root, model);
					// Fixing the "extensions" within the pom file
					final IFile pomFile = project.getFile(IMavenConstants.POM_FILE_NAME);
					pomFile.delete(true, new NullProgressMonitor());
					MavenPlugin.getMavenModelManager().createMavenModel(pomFile, model);
					// Update the project
					final SubMonitor submon = SubMonitor.convert(monitor);
					MavenPlugin.getProjectConfigurationManager().updateProjectConfiguration(project, submon.newChild(1));
					project.refreshLocal(IResource.DEPTH_ONE, submon.newChild(1));
				}
				return Status.OK_STATUS;
			}
		};
		job.setRule(MavenPlugin.getProjectConfigurationManager().getRule());
		job.schedule();
		return true;
	}

}

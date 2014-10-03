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
package io.sarl.maven.compiler;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;

/** Mojo for compiling SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @goal compile
 * @phase compile
 * @requiresDependencyResolution compile
 */
public class CompileMojo extends AbstractSarlMojo {

	private static final String SARL_SDK_GROUP_ID = "io.sarl.maven"; //$NON-NLS-1$
	private static final String SARL_SDK_ARTIFACT_ID = "io.sarl.maven.sdk"; //$NON-NLS-1$
	private static final String SARL_LANG_GROUP_ID = "io.sarl.lang"; //$NON-NLS-1$
	private static final String SARL_LANG_ARTIFACT_ID = "io.sarl.lang"; //$NON-NLS-1$
	private static final String XTEXT_GROUP_ID = "org.eclipse.xtext"; //$NON-NLS-1$
	private static final String XTEXT_ARTIFACT_ID = "xtext-maven-plugin"; //$NON-NLS-1$
	private static final String XTEXT_MOJO = "generate"; //$NON-NLS-1$
	private static final String JAVA_GROUP_ID = "org.apache.maven.plugins"; //$NON-NLS-1$
	private static final String JAVA_ARTIFACT_ID = "maven-compiler-plugin"; //$NON-NLS-1$
	private static final String JAVA_MOJO = "compile"; //$NON-NLS-1$

	/**
	 * @parameter property="source" default-value="1.7"
	 * @required
	 */
	protected String source;

	/**
	 * @parameter property="target"
	 * @required
	 */
	protected String target;

	/**
	 * @parameter property="encoding" default-value="${project.build.sourceEncoding}"
	 * @required
	 */
	protected String encoding;

	@Override
	public void execute() throws MojoExecutionException, MojoFailureException {
		if (this.target == null) {
			this.target = this.source;
		}
		compileSARL();
		compileJava();
	}

	private Dependency[] extractXtextDependencies() throws MojoExecutionException {
		List<Dependency> deps = new ArrayList<>();

		PluginDescriptor currentPlugin = (PluginDescriptor) getPluginContext().get("pluginDescriptor"); //$NON-NLS-1$
		Map<String, Artifact> artifacts = currentPlugin.getArtifactMap();

		String langKey = ArtifactUtils.versionlessKey(SARL_LANG_GROUP_ID, SARL_LANG_ARTIFACT_ID);
		Artifact langArtifact = artifacts.get(langKey);
		if (langArtifact == null) {
			throw new MojoExecutionException("Cannot find the artifact " + langKey); //$NON-NLS-1$
		}
		langArtifact = this.session.getLocalRepository().find(langArtifact);
		deps.add(toDependency(langArtifact));

		String sdkKey = ArtifactUtils.versionlessKey(SARL_SDK_GROUP_ID, SARL_SDK_ARTIFACT_ID);
		Artifact sdkArtifact = artifacts.get(sdkKey);
		if (sdkArtifact == null) {
			throw new MojoExecutionException("Cannot find the artifact " + sdkKey); //$NON-NLS-1$
		}
		sdkArtifact = this.session.getLocalRepository().find(sdkArtifact);

		File pomFile = sdkArtifact.getFile();
		if (pomFile != null && pomFile.canRead()) {
			try (FileInputStream is = new FileInputStream(pomFile)) {
				MavenXpp3Reader mavenReader = new MavenXpp3Reader();
				Model model = mavenReader.read(is);
				model.setPomFile(pomFile);
				// Get the dependency list from the POM file, but
				// the version numbers are from the resolved artifacts.
				for (Dependency mavenDep : model.getDependencies()) {
					Dependency newDep = mavenDep.clone();
					String key = ArtifactUtils.versionlessKey(
							mavenDep.getGroupId(), mavenDep.getArtifactId());
					Artifact a = artifacts.get(key);
					if (a == null) {
						throw new MojoExecutionException("Cannot find the artifact " + key); //$NON-NLS-1$
					}
					newDep.setVersion(a.getVersion());
					deps.add(newDep);
				}
			} catch (IOException | XmlPullParserException e) {
				throw new MojoExecutionException(
						"Cannot determine the dependencies for the artifact" + sdkKey, e); //$NON-NLS-1$
			}
		}

		Dependency[] depTab = new Dependency[deps.size()];
		deps.toArray(depTab);
		return depTab;
	}

	private void compileSARL() throws MojoExecutionException {
		getLog().info("Converting SARL to Java..."); //$NON-NLS-1$
		String version = getPluginVersionFromDependencies(XTEXT_GROUP_ID, XTEXT_ARTIFACT_ID);
		Dependency[] dependencies = extractXtextDependencies();
		executeDelegate(
				XTEXT_GROUP_ID, XTEXT_ARTIFACT_ID, version,
				XTEXT_MOJO,
				MessageFormat.format(
						getConfig(XTEXT_ARTIFACT_ID + ".configuration"), //$NON-NLS-1$
						this.source,
						this.target,
						this.encoding,
						this.output),
				dependencies);
	}

	private void compileJava() throws MojoExecutionException {
		getLog().info("Compiling Java files..."); //$NON-NLS-1$
		String version = getPluginVersionFromDependencies(JAVA_GROUP_ID, JAVA_ARTIFACT_ID);
		executeDelegate(
				JAVA_GROUP_ID, JAVA_ARTIFACT_ID, version,
				JAVA_MOJO,
				MessageFormat.format(
						getConfig(JAVA_ARTIFACT_ID + ".configuration"), //$NON-NLS-1$
						this.source,
						this.target,
						this.encoding));
	}

}

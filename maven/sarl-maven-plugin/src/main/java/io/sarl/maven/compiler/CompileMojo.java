/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import java.nio.charset.Charset;
import java.text.MessageFormat;

import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;

/** Mojo for compiling SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Mojo(name = "compile", defaultPhase = LifecyclePhase.COMPILE, requiresDependencyResolution = ResolutionScope.COMPILE)
public class CompileMojo extends AbstractSarlMojo {

	/** Version of the Java specification used for the source files.
	 */
	@Parameter(defaultValue = "1.7")
	protected String source;

	/** Version of the Java specification used for the output files.
	 */
	@Parameter
	protected String target;

	/** Encoding.
	 */
	@Parameter(defaultValue = "${project.build.sourceEncoding}")
	protected String encoding;

	@Override
	protected void buildPropertyString(StringBuilder buffer) {
		super.buildPropertyString(buffer);
		buffer.append("source = ").append(this.source).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("target = ").append(this.target).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("encoding = ").append(this.encoding).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
	}

	@Override
	protected void ensureDefaultParameterValues() {
		super.ensureDefaultParameterValues();
		if (this.encoding == null) {
			this.encoding = Charset.defaultCharset().name();
		}
		if (this.target == null) {
			this.target = this.source;
		}
	}

	@Override
	protected void executeMojo() throws MojoExecutionException, MojoFailureException {
		compileSARL();
		compileJava();
	}

	private void compileSARL() throws MojoExecutionException, MojoFailureException {
		getLog().info("Compiling SARL to Java..."); //$NON-NLS-1$
		String xtextGroupId = MavenHelper.getConfig("xtext-compiler.groupId"); //$NON-NLS-1$
		String xtextArtifactId = MavenHelper.getConfig("xtext-compiler.artifactId"); //$NON-NLS-1$
		String xtextVersion = this.mavenHelper.getPluginDependencyVersion(xtextGroupId, xtextArtifactId, "compile"); //$NON-NLS-1$
		String xtextMojo = MavenHelper.getConfig("xtext-compiler.mojo"); //$NON-NLS-1$
		Dependency[] dependencies = getDependenciesFor("xtext-compiler", "compile"); //$NON-NLS-1$ //$NON-NLS-2$
		executeMojo(
				xtextGroupId, xtextArtifactId, xtextVersion, xtextMojo,
				MessageFormat.format(
				MavenHelper.getConfig("xtext-compiler.configuration"), //$NON-NLS-1$
				this.source,
				this.target,
				this.encoding,
				getOutput().getAbsolutePath()),
				dependencies);
	}

	private void compileJava() throws MojoExecutionException, MojoFailureException {
		getLog().info("Compiling Java files..."); //$NON-NLS-1$
		String javaGroupId = MavenHelper.getConfig("java-compiler.groupId"); //$NON-NLS-1$
		String javaArtifactId = MavenHelper.getConfig("java-compiler.artifactId"); //$NON-NLS-1$
		String javaVersion = this.mavenHelper.getPluginDependencyVersion(
				javaGroupId, javaArtifactId, "compile"); //$NON-NLS-1$
		String javaMojo = MavenHelper.getConfig("java-compiler.mojo"); //$NON-NLS-1$
		executeMojo(
				javaGroupId, javaArtifactId, javaVersion, javaMojo,
				MessageFormat.format(
				MavenHelper.getConfig("java-compiler.configuration"), //$NON-NLS-1$
				this.source,
				this.target,
				this.encoding));
	}

}

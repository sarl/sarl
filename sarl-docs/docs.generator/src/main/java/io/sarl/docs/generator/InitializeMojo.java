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

package io.sarl.docs.generator;

import java.io.File;
import java.text.MessageFormat;
import java.util.List;

import com.google.common.base.Throwables;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.eclipse.xtext.util.Strings;

import io.sarl.docs.validator.DocumentationLogger;
import io.sarl.docs.validator.ShellCommandProvider;
import io.sarl.docs.validator.ShellExtensions;

/** Initialization mojo for the SARL Maven compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Mojo(name = "initialize", defaultPhase = LifecyclePhase.INITIALIZE,
	requiresDependencyResolution = ResolutionScope.COMPILE)
public class InitializeMojo extends AbstractDocumentationMojo {

	@Parameter
	private List<ShellCommand> shellCommands; 

	@Override
	protected String getSkippingMessage() {
		return null;
	}

	@Override
	protected String internalExecute() {
		try {
			DocumentationLogger.initializeLogger(new MavenJulHandler(getLog()));
			for (final var sourceFolder : this.inferredSourceDirectories) {
				addSourceFolder(sourceFolder);
			}
			addTestSourceFolder(this.testSourceDirectory);
			registerShellCommands();
		} catch (Throwable exception) {
			final var message = Throwables.getRootCause(exception).getLocalizedMessage();
			getLog().error(message);
			getLog().debug(exception);
			return message;
		}
		return null;
	}

	/** Registering the shell commands that are defined int the mojo configuration.
	 *
	 * @throws MojoExecutionException error when running the mojo.
	 */
	protected void registerShellCommands() throws MojoExecutionException {
		getLog().debug("Shell commands = " + this.shellCommands); //$NON-NLS-1$
		if (this.shellCommands != null) {
			final var provider = this.injector.getInstance(ShellCommandProvider.class);
			if (provider == null) {
				throw new MojoExecutionException("No shell command provider defined in the mojo source code"); //$NON-NLS-1$
			}
			for (final var command : this.shellCommands) {
				if (!registerShellCommand(command, provider)) {
					throw new MojoExecutionException("Artifact not found: " + command.toString()); //$NON-NLS-1$
				}
			}
			ShellExtensions.shellCommandProvider = provider;
		}
	}

	private boolean registerShellCommand(ShellCommand command, ShellCommandProvider provider) throws MojoExecutionException {
		getLog().debug("Register shell command: " + command); //$NON-NLS-1$
		final var dep = findDependency(command.getGroupId(), command.getArtifactId(), command.getType());
		getLog().debug("Associated dependency: " + dep); //$NON-NLS-1$
		if (dep != null) {
			final var artifacts = resolve(dep.getGroupId(), dep.getArtifactId(), dep.getVersion(), dep.getType());
			for (final var artifact : artifacts) {
				if (Strings.equal(command.getGroupId(), artifact.getGroupId())
						&& Strings.equal(command.getArtifactId(), artifact.getArtifactId())
						&& Strings.equal(command.getType(), artifact.getType())) {
					getLog().debug("Artifact candidate: " + artifact); //$NON-NLS-1$
					final var artifactFile = artifact.getFile();
					getLog().debug("Artifact file: " + artifactFile); //$NON-NLS-1$
					if (artifactFile != null) {
						getLog().info("Register shell command '" + command.getName() + "': " + artifactFile); //$NON-NLS-1$ //$NON-NLS-2$
						provider.register(command.getName(), artifactFile);
						if (!artifactFile.canExecute()) {
							getLog().info("Force execution flag for: " + artifactFile); //$NON-NLS-1$
							artifactFile.setExecutable(true);
						}
						return true;
					}
				}
			}
		}
		return false;
	}

	/** Find a dependency with the given information.
	 *
	 * @param groupId the identifier of the group.
	 * @param artifactId the identifier of the artifact.
	 * @param type the type of the artifact.
	 * @return the dependency.
	 */
	protected Dependency findDependency(String groupId, String artifactId, String type) {
		for (final var dep : this.session.getCurrentProject().getDependencies()) {
			if (Strings.equal(groupId, dep.getGroupId())
				&& Strings.equal(artifactId, dep.getArtifactId())
				&& Strings.equal(type, dep.getType())) {
				return dep;
			}
		}
		return null;
	}

	/** Add a source folder in the current projecT.
	 *
	 * @param path the source folder path.
	 */
	protected void addSourceFolder(String path) {
		final var existingFolders1 = this.project.getCompileSourceRoots();
		final var existingFolders2 = this.project.getTestCompileSourceRoots();
		if (!existingFolders1.contains(path) && !existingFolders2.contains(path)) {
			getLog().info(MessageFormat.format(Messages.InitializeMojo_0, path));
			this.session.getCurrentProject().addCompileSourceRoot(path);
		} else {
			getLog().info(MessageFormat.format(Messages.InitializeMojo_1, path));
		}
	}

	/** Add a source folder in the current projecT.
	 *
	 * @param path the source folder path.
	 */
	protected void addSourceFolder(File path) {
		addSourceFolder(path.getAbsolutePath());
	}

	/** Add a source folder in the current projecT.
	 *
	 * @param path the source folder path.
	 */
	protected void addTestSourceFolder(String path) {
		final var existingFolders1 = this.project.getCompileSourceRoots();
		final var existingFolders2 = this.project.getTestCompileSourceRoots();
		if (!existingFolders1.contains(path) && !existingFolders2.contains(path)) {
			getLog().info(MessageFormat.format(Messages.InitializeMojo_2, path));
			this.session.getCurrentProject().addTestCompileSourceRoot(path);
		} else {
			getLog().info(MessageFormat.format(Messages.InitializeMojo_3, path));
		}
	}

	/** Add a test source folder in the current projecT.
	 *
	 * @param path the source folder path.
	 */
	protected void addTestSourceFolder(File path) {
		addTestSourceFolder(path.getAbsolutePath());
	}

	/** Description of a shell command that is provided by a Maven artifact.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.13
	 */
	public static class ShellCommand {

		private String name;

		private String groupId;

		private String artifactId;

		private String type;

		/** Replies the name of the shell command.
		 *
		 * @return the name.
		 */
		public String getName() {
			return this.name;
		}

		/** Change the name of the shell command
		 *
		 * @param name the name.
		 */
		public void setName(String name) {
			this.name = name;
		}

		/** Replies the identifier of the group for the Maven artifact.
		 *
		 * @return the group id.
		 */
		public String getGroupId() {
			return this.groupId;
		}

		/** Change the identifier of the group for the Maven artifact.
		 *
		 * @param id the new group id.
		 */
		public void setGroupId(String id) {
			this.groupId = id;
		}

		/** Replies the identifier of the Maven artifact.
		 *
		 * @return the group id.
		 */
		public String getArtifactId() {
			return this.artifactId;
		}

		/** Change the identifier of the Maven artifact.
		 *
		 * @param id the new group id.
		 */
		public void setArtifactId(String id) {
			this.artifactId = id;
		}

		/** Replies the type of the Maven artifact. 
		 *
		 * @return the type, e.g {@code sh}, {@code exe}, or {@code dmg}.
		 */
		public String getType() {
			return this.type;
		}

		/** Change the type of the Maven artifact. 
		 *
		 * @param type the new type, e.g {@code sh}, {@code exe}, or {@code dmg}.
		 */
		public void setType(String type) {
			this.type = type;
		}

		@Override
		public String toString() {
			final var str = new StringBuilder();
			str.append(getGroupId()).append(":"); //$NON-NLS-1$
			str.append(getArtifactId()).append(":"); //$NON-NLS-1$
			str.append(getType()).append(":"); //$NON-NLS-1$
			str.append(getName());
			return str.toString();
		}

	}

}

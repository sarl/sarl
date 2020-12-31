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

package io.sarl.maven.docs;

import java.io.File;
import java.text.MessageFormat;
import java.util.List;

import com.google.common.base.Throwables;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

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

	@Override
	protected String getSkippingMessage() {
		return null;
	}

	@Override
	protected String internalExecute() {
		try {
			for (final String sourceFolder : this.inferredSourceDirectories) {
				addSourceFolder(sourceFolder);
			}
			addTestSourceFolder(this.testSourceDirectory);
		} catch (Throwable exception) {
			final String message = Throwables.getRootCause(exception).getLocalizedMessage();
			getLog().error(message);
			getLog().debug(exception);
			return message;
		}
		return null;
	}

	/** Add a source folder in the current projecT.
	 *
	 * @param path the source folder path.
	 */
	protected void addSourceFolder(String path) {
		final List<String> existingFolders1 = this.project.getCompileSourceRoots();
		final List<String> existingFolders2 = this.project.getTestCompileSourceRoots();
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
		final List<String> existingFolders1 = this.project.getCompileSourceRoots();
		final List<String> existingFolders2 = this.project.getTestCompileSourceRoots();
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

}

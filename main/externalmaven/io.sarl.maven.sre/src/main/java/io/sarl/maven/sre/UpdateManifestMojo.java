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

package io.sarl.maven.sre;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.arakhne.afc.vmutil.FileSystem;
import org.codehaus.plexus.util.IOUtil;

/** Update the project manifest with the SRE informations.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Mojo(name = "updatemanifest", requiresProject = true, defaultPhase = LifecyclePhase.PACKAGE,
		requiresDependencyResolution = ResolutionScope.COMPILE)
public class UpdateManifestMojo extends AbstractSREMojo {

	/** List of the final names that correspond to the archives to update.
	 */
	@Parameter(required = false)
	private List<String> archiveFinalNames;

	@Override
	protected void executeMojo() throws MojoExecutionException, MojoFailureException {
		final Build build = getMavenProject().getBuild();
		final String baseDir = build.getDirectory();

		final Manifest sreManifest = createSREManifest();

		if (this.archiveFinalNames == null || this.archiveFinalNames.isEmpty()) {
			this.archiveFinalNames = Collections.singletonList(build.getFinalName());
		}

		for (final String finalName : this.archiveFinalNames) {
			final File jarFile = new File(baseDir, finalName  + ".jar"); //$NON-NLS-1$
			getLog().info("Updating the manifest of " + jarFile.getName()); //$NON-NLS-1$
			final Manifest jarManifest = mergeManifests(jarFile, sreManifest);
			final File outputFile = createTempArchive(jarFile, jarManifest);
			try {
				FileSystem.delete(jarFile);
				FileSystem.copy(outputFile, jarFile);
				FileSystem.delete(outputFile);
			} catch (IOException exception) {
				throw new MojoFailureException(exception.getLocalizedMessage(), exception);
			}
		}
	}

	private static Manifest mergeManifests(File inputFile, Manifest sreManifest) throws MojoFailureException {
		// Merge the manifests
		final Manifest jarManifest;
		try (JarFile input = new JarFile(inputFile)) {
			jarManifest = input.getManifest();
			jarManifest.getMainAttributes().putAll(sreManifest.getMainAttributes());
			jarManifest.getEntries().putAll(sreManifest.getEntries());
		} catch (IOException exception) {
			throw new MojoFailureException(exception.getLocalizedMessage(), exception);
		}
		return jarManifest;
	}

	private File createTempArchive(File inputFile, Manifest newManifest) throws MojoFailureException {
		final Build build = getMavenProject().getBuild();
		final String finalName = build.getFinalName();
		final String baseDir = build.getDirectory();
		// Copy the jar
		try {
			final File target = new File(baseDir, finalName  + ".jar_tmp"); //$NON-NLS-1$
			try (ZipFile originalJar = new ZipFile(inputFile)) {
				try (JarOutputStream targetJar = new JarOutputStream(new FileOutputStream(target), newManifest)) {
					final Enumeration<? extends ZipEntry> entries = originalJar.entries();
					while (entries.hasMoreElements()) {
						final ZipEntry entry = entries.nextElement();

						// skip the original manifest
						if (JarFile.MANIFEST_NAME.equals(entry.getName())) {
							continue;
						}

						final ZipEntry newEntry = new ZipEntry(entry.getName());
						targetJar.putNextEntry(newEntry);

						// write content to stream if it is a file
						if (!entry.isDirectory()) {
							try (InputStream inputStream = originalJar.getInputStream(entry)) {
								IOUtil.copy(inputStream, targetJar);
								inputStream.close();
							}
						}
						targetJar.closeEntry();
					}
				}
			}
			return target;
		} catch (IOException exception) {
			throw new MojoFailureException(exception.getLocalizedMessage(), exception);
		}
	}
}

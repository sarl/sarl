/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.Arrays;
import java.util.Properties;

import org.apache.maven.it.Verifier;
import org.apache.maven.shared.utils.cli.CommandLineUtils;
import org.apache.maven.shared.utils.io.FileUtils;

/** Abstract test of Maven Mojo.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractMojoTest {

	/** Helper for writting a multiline string in unit tests.
	 *
	 * @param lines - the lines in the string.
	 * @return the complete multiline string.
	 */
	public static String multilineString(Object... lines) {
		final StringBuilder b = new StringBuilder();
		for (final Object line : lines) {
			if (line != null) {
				b.append(line);
			}
			b.append("\n"); //$NON-NLS-1$
		}
		return b.toString();
	}

	/** Recursive deletion of a folder when the VM is exiting.
	 *
	 * @param path the path of the folder to delete.
	 */
	protected static void recursiveDeleteOnShutdownHook(final File path) {
		Runtime.getRuntime().addShutdownHook(new Thread(
				new Runnable() {
					@Override
					public void run() {
						try {
							FileUtils.deleteDirectory(path);
						} catch (IOException e) {
							throw new RuntimeException("Failed to delete " + path, e); //$NON-NLS-1$
						}
					}}));
	}

	/** Execute a Mojo.
	 *
	 * @param projectName the name of the project to test for the unit test.
	 * @param goalName the goal to run.
	 * @return the verifier.
	 * @throws Exception any exception.
	 */
	protected Verifier executeMojo(String projectName, String goalName) throws Exception {
		String tempDirPath = System.getProperty("maven.test.tmpdir", //$NON-NLS-1$
				System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
		File tempDir = new File(tempDirPath);
		File baseDir = new File(tempDir, projectName);
		assertNotNull(baseDir);

		FileUtils.deleteDirectory(baseDir);

		URL url = getClass().getResource("/projects/" + projectName); //$NON-NLS-1$
		if (url == null) {
			throw new IllegalArgumentException("Resource not found: " + projectName); //$NON-NLS-1$
		}
		File resourceFile = new File(new URI(url.toExternalForm()));
		assertTrue(resourceFile.isDirectory());
		FileUtils.copyDirectoryStructure(resourceFile, baseDir);

		assertTrue(baseDir.exists());
		assertTrue(baseDir.isDirectory());

		recursiveDeleteOnShutdownHook(baseDir);

		Verifier verifier = new Verifier(baseDir.getAbsolutePath());
		verifier.setAutoclean(false);
		verifier.setDebug(false);
		final String m2home = findDefaultMavenHome();
		if (m2home != null && !m2home.isEmpty()) {
			verifier.setForkJvm(false);
			verifier.getSystemProperties().put("maven.multiModuleProjectDirectory", m2home); //$NON-NLS-1$
			verifier.getVerifierProperties().put("use.mavenRepoLocal", Boolean.FALSE.toString()); //$NON-NLS-1$
		}
		verifier.executeGoals(Arrays.asList("clean", goalName)); //$NON-NLS-1$
		return verifier;
	}

	private static String findDefaultMavenHome() {
		String defaultMavenHome = System.getProperty("maven.home"); //$NON-NLS-1$

		if ( defaultMavenHome == null ) {
			Properties envVars = CommandLineUtils.getSystemEnvVars();
			defaultMavenHome = envVars.getProperty("M2_HOME"); //$NON-NLS-1$
		}

		if ( defaultMavenHome == null )
		{
			File f = new File( System.getProperty( "user.home" ), "m2" ); //$NON-NLS-1$ //$NON-NLS-2$
			if ( new File( f, "bin/mvn" ).isFile() ) //$NON-NLS-1$
			{
				defaultMavenHome = f.getAbsolutePath();
			}
		}
		return defaultMavenHome;
	}

}

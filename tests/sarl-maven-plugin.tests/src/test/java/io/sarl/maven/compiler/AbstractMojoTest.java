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
package io.sarl.maven.compiler;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Properties;

import org.apache.maven.it.VerificationException;
import org.apache.maven.it.Verifier;
import org.apache.maven.shared.utils.cli.CommandLineUtils;
import org.apache.maven.shared.utils.io.FileUtils;
import org.junit.Assume;
import org.junit.internal.Throwables;

/** Abstract test of Maven Mojo.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractMojoTest {

	private static final int NETWORK_TIMEOUT = 5000;
	
	private static Boolean IS_SARL_WEBSITE_AVAILABLE = null;
	
	/** Test if the SARL website(s) are available on Internet.
	 *
	 * <p>The UTBM network infrastructure (hosting a part of the SARL servers)
	 * is not safe and is shut down several times per year.
	 *
	 * @throws Exception the error status.
	 */
	public static void touchSarlWebSites() throws Exception {
		Boolean bool = IS_SARL_WEBSITE_AVAILABLE;
		if (bool != null) {
			Assume.assumeTrue("SARL webiste are not available on Internet", bool.booleanValue()); //$NON-NLS-1$
			return;
		}
		bool = Boolean.FALSE;
		try {
			touchRemotePage("http://dependencies.sarl.io/", false); //$NON-NLS-1$
			bool = Boolean.TRUE;
		} finally {
			IS_SARL_WEBSITE_AVAILABLE = bool;
		}
	}

	/** Test if a remote page is accessible on Internet.
	 *
	 * @param url the url to test.
	 * @param failureIfNotAccessible if {@code true} the function fails with an error; otherwise
	 *     it fails with an false assumption.
	 * @throws Exception the error status.
	 */
	public static void touchRemotePage(String url, boolean failureIfNotAccessible) throws Exception {
		assert url != null;
		touchRemotePage(new URL(url), failureIfNotAccessible);
	}

	/** Test if a remote page is accessible on Internet.
	 *
	 * @param url the url to test.
	 * @param failureIfNotAccessible if {@code true} the function fails with an error; otherwise
	 *     it fails with an false assumption.
	 * @throws Exception the error status.
	 */
	public static void touchRemotePage(URL url, boolean failureIfNotAccessible) throws Exception {
		assert url != null;
		try {
			final URLConnection con = url.openConnection();
			HttpURLConnection httpCon = (HttpURLConnection) con;
			httpCon.setRequestMethod("HEAD"); //$NON-NLS-1$
			httpCon.setConnectTimeout(NETWORK_TIMEOUT);
			httpCon.setReadTimeout(NETWORK_TIMEOUT);
			httpCon.connect();
			final int code = httpCon.getResponseCode();
			httpCon.disconnect();
			if (code != HttpURLConnection.HTTP_OK) {
				throw new IOException("Remote server replies unexpected code " + code //$NON-NLS-1$
						+ " when accessing to " + url.toExternalForm()); //$NON-NLS-1$
			}
		} catch (Exception e) {
			if (failureIfNotAccessible) {
				Throwables.rethrowAsException(e);
			} else {
				Assume.assumeNoException(e);
			}
		}
	}

	/** Helper for writting a multiline string in unit tests.
	 *
	 * @param lines the lines in the string.
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

	/** Read the content of the given file.
	 *
	 * @param verifier the Maven verifier to use for obtaining the context.
	 * @param file the file to read, relatively to the context base dir.
	 * @return the file content.
	 * @throws VerificationException if the file cannot be loaded.
	 * @since 0.7
	 */
	protected static String readFile(Verifier verifier, Path file) throws VerificationException {
		verifier.assertFilePresent(file.toString());
		StringBuilder buffer = new StringBuilder();
		for (final String line : verifier.loadFile(verifier.getBasedir(), file.toString(), false)) {
			buffer.append(line).append("\n"); //$NON-NLS-1$
		}
		return buffer.toString();
	}

}

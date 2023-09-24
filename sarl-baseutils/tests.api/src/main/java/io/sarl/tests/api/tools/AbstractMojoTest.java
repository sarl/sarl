/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
package io.sarl.tests.api.tools;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.google.common.base.Strings;
import org.apache.commons.io.FileUtils;
import org.arakhne.afc.vmutil.FileSystem;
import org.opentest4j.TestAbortedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

	private boolean isDebug;

	/** Replies if the mojo is run in a debugging context.
	 *
	 * @return {@code true} if in debug mode.
	 * @since 0.14
	 */
	protected boolean isDebug() {
		return this.isDebug;
	}

	/** Change the flag that indicates if the mojo is run in a debugging context.
	 *
	 * @param debug {@code true} if in debug mode.
	 * @since 0.14
	 */
	protected void setDebug(boolean debug) {
		this.isDebug = debug;
	}

	/** Replies a logger that could be used for tests.
	 *
	 * @return a logger.
	 */
	protected Logger getLogger() {
		return LoggerFactory.getLogger(getClass());
	}
	
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
			assumeTrue(bool.booleanValue(), () -> "SARL webiste are not available on Internet"); //$NON-NLS-1$
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
				throw e;
			}
			throw new TestAbortedException(e.getLocalizedMessage(), e);
		}
	}

	/** Helper for writing a multiline string in unit tests.
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

	/** Assert that the given code generates an exception of the given type.
	 *
	 * @param expected the type of the expected exception.
	 * @param code the code to run.
	 * @throws Exception 
	 */
	public static void assertException(Class<? extends Throwable> expected, Code code) throws Exception {
		try {
			code.run();
			fail("Expecting exception of type " + expected.getName() + " but no exception was thrown"); //$NON-NLS-1$ //$NON-NLS-2$
		} catch (Throwable ex) {
			final Throwable cs = getCause(ex);
			if (!expected.isAssignableFrom(cs.getClass())) {
				fail("Expecting exception of type " + expected.getName() + " but catch exception of type " + cs.getClass().getName(), cs); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
	}

	private static Throwable getCause(Throwable ex) {
		if (ex != null) {
			Throwable cs = ex;
			while (cs.getCause() != null && cs.getCause() != cs && cs.getCause() != ex) {
				cs = cs.getCause();
			}
			return cs;
		}
		return ex;
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
						} catch (Throwable e) {
							// Silent error
						}
					}}));
	}

	/** Execute a Mojo.
	 *
	 * @param projectName the name of the project to test for the unit test.
	 * @param goalName the goal to run.
	 * @param outputOnConsole indicates if the standard output of Maven must be shown on the console.
	 * @return the verifier.
	 * @throws Exception any exception.
	 * @since 0.13
	 */
	protected Verifier executeMojo(String projectName, String goalName, boolean outputOnConsole) throws Exception {
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
		FileUtils.copyDirectory(resourceFile, baseDir);

		assertTrue(baseDir.exists());
		assertTrue(baseDir.isDirectory());

		recursiveDeleteOnShutdownHook(baseDir);

		final File mavenCmd = findDefaultMavenBin();
		assumeTrue(mavenCmd != null, "Maven command not found"); //$NON-NLS-1$

		@SuppressWarnings("null")
		final List<String> cmd = new ArrayList<>(Arrays.asList(mavenCmd.getAbsolutePath(), "-ff", "clean")); //$NON-NLS-1$ //$NON-NLS-2$
		if (!Strings.isNullOrEmpty(goalName)) {
			cmd.add(goalName);
		}
		return runMaven(baseDir, outputOnConsole, cmd);
	}

	/** Run the Maven command.
	 *
	 * @param baseDir the base directory
	 * @param outputOnConsole indicates if the standard output of Maven must be shown on the console.
	 * @param command the Maven command.
	 * @return the tool for verifying the mojo execution.
	 * @throws Exception if Maven fails
	 */
	protected abstract Verifier runMaven(File baseDir, boolean outputOnConsole, List<String> command) throws Exception;
	
	/** Execute a Mojo and expects an error.
	 *
	 * @param projectName the name of the project to test for the unit test.
	 * @param goalName the goal to run.
	 * @param outputOnConsole indicates if the standard output of Maven must be shown on the console.
	 * @param expectedErrorMessage the expected error message that must appear on the error console of Maven.
	 * @throws Exception any exception.
	 * @since 0.13
	 */
	protected void executeMojoWithError(String projectName, String goalName, boolean outputOnConsole, String expectedErrorMessage) throws Exception {
		final Verifier verifier = executeMojo(projectName, goalName, outputOnConsole);
		verifier.assertErrorLog(expectedErrorMessage);
	}

	private static File findDefaultMavenBin() {
		final File mavenHome = findDefaultMavenHome();
		if (mavenHome != null) {
			final File cmd = new File(new File(mavenHome, "bin"), "mvn"); //$NON-NLS-1$ //$NON-NLS-2$
			if (cmd.exists() && cmd.canExecute()) {
				return cmd;
			}
		}
		return null;
	}

	/** Find the Maven home.
	 *
	 * @return the Maven home directory.
	 */
	protected static File findDefaultMavenHome() {
		String defaultMavenHome = System.getProperty("maven.home"); //$NON-NLS-1$

		if ( defaultMavenHome == null ) {
			final Map<String, String> envVars = System.getenv();
			defaultMavenHome = envVars.get("M2_HOME"); //$NON-NLS-1$
		}

		if ( defaultMavenHome == null )
		{
			File f = new File( System.getProperty( "user.home" ), "m2" ); //$NON-NLS-1$ //$NON-NLS-2$
			if ( new File( f, "bin/mvn" ).isFile() ) //$NON-NLS-1$
			{
				defaultMavenHome = f.getAbsolutePath();
			}
		}
		if (!Strings.isNullOrEmpty(defaultMavenHome)) {
			return FileSystem.convertStringToFile(defaultMavenHome);
		}
		return null;
	}

	/** Code to be run.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.11
	 */
	@FunctionalInterface
	public interface Code {

		/** Code.
		 *
		 * @throws Exception any exception
		 */
		void run() throws Exception;

	}

}

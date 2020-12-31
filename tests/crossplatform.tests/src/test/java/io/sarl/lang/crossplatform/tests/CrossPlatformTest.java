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
package io.sarl.lang.crossplatform.tests;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.shared.utils.cli.CommandLineUtils;
import org.apache.maven.shared.utils.io.FileUtils;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure3;
import org.junit.ComparisonFailure;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.opentest4j.TestAbortedException;
import org.osgi.framework.Version;

import io.sarl.lang.SARLVersion;

/** Cross-platform tests.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@SuppressWarnings("all")
@DisplayName("Testing SARL compilation on different JDK")
@Tag("crossplatform")
public class CrossPlatformTest {

	private static final String ROOT_FOLDER_FOR_JDK = "/usr/lib/jvm"; // $NON-NLS-1$

	private static final String PATTERN = "^java-([^-]+)"; // $NON-NLS-1$

	private static final String SARLC_BIN_0 = "products/sarlc/target/sarlc";

	private static final String SARLC_BIN_1 = "../../" + SARLC_BIN_0;

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

	/** Recursive deletion of a folder when the VM is exiting.
	 *
	 * @param path the path of the folder to delete.
	 */
	protected static void recursiveDeleteOnShutdownHook(final File path, final File logFile) {
		Runtime.getRuntime().addShutdownHook(new Thread(
				new Runnable() {
					@Override
					public void run() {
						try {
							if (logFile != null) {
								FileUtils.delete(logFile);
							}
						} catch (IOException e) {
							//
						}
						try {
							FileUtils.deleteDirectory(path);
						} catch (IOException e) {
							throw new RuntimeException("Failed to delete " + path, e); //$NON-NLS-1$
						}
					}}));
	}

	private static String getJavaVersionLabel(Integer version) {
		return "Java " + version;
	}

	private static int parseJavaVersion(String value) {
		final int jversion;
		if (value != null) {
			final Version versionObj = Version.parseVersion(value);
			if (versionObj.getMajor() == 1) {
				jversion = versionObj.getMinor();
			} else {
				jversion = versionObj.getMajor();
			}
		} else {
			jversion = 0;
		}
		return jversion;
	}
	
	/** Replies the installed JDKs.
	 *
	 * @return the list of installed JDKs.
	 * @since 0.12
	 */
	protected static Map<Integer, File> getInstalledJdks() {
		final Map<Integer, File> jdks = new HashMap<>();
		final File current = new File(ROOT_FOLDER_FOR_JDK);
		final Pattern pattern = Pattern.compile(PATTERN);
		final int minJavaVersion = parseJavaVersion(JavaVersion.fromQualifier(SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH).getQualifier());
		for (final File child : current.listFiles()) {
			if (child.isDirectory()) {
				final Matcher matcher = pattern.matcher(child.getName());
				if (matcher.find()) {
					final String v = matcher.group(1);
					if (v != null) {
						final int jversion = parseJavaVersion(v);
						if (jversion >= minJavaVersion) {
							final File exec = new File(new File(child, "bin"), "java");
							if (exec.canExecute()) {
								jdks.putIfAbsent(jversion, child);
							}
						}
					}
				}
			}
		}
		return jdks;
	}

	/** Execute a Mojo.
	 *
	 * @param projectName the name of the project to test for the unit test.
	 * @param goalName the goal to run.
	 * @param jversion the java version to be used.
	 * @param jdk the JDK to be used.
	 * @throws Exception any exception.
	 */
	protected void executeMojo(String projectName, String goalName, int jversion, File jdk) throws Exception {
		String tempDirPath = System.getProperty("maven.test.tmpdir", //$NON-NLS-1$
				System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
		File tempDir = new File(tempDirPath);
		final String tmpName = projectName + "_" + jversion; // $NON-NLS-1$
		File baseDir = new File(tempDir, tmpName);
		final File logFile = new File(tempDir, tmpName + ".log");
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

		recursiveDeleteOnShutdownHook(baseDir, logFile);

		final List<String> command = new ArrayList<>();
		command.add("mvn"); // $NON-NLS-1$
		//command.add("-version");
		command.add("-o"); // $NON-NLS-1$
		command.add("clean"); // $NON-NLS-1$
		command.add(goalName);

		ProcessBuilder processBuilder = new ProcessBuilder(command);
		processBuilder.redirectErrorStream();
		processBuilder.redirectOutput(logFile);
		processBuilder.environment().put("JAVA_HOME", jdk.getAbsolutePath());
		processBuilder = processBuilder.directory(baseDir.getAbsoluteFile());

		final Process process = processBuilder.start();

		final int ret = process.waitFor();
		if (ret != 0) {
			final String logContent = readFile(logFile);
			throw new ComparisonFailure("Compilation error", "", logContent);
		}
	}

	/** Read the content of the given file.
	 *
	 * @param verifier the Maven verifier to use for obtaining the context.
	 * @param file the file to read, relatively to the context base dir.
	 * @return the file content.
	 * @throws IOException if the file cannot be loaded.
	 */
	protected static String readFile(File file) throws IOException {
		StringBuilder buffer = new StringBuilder();
		try (final BufferedReader fr = new BufferedReader(new FileReader(file))) {
			String line = fr.readLine();
			while (line != null) {
				buffer.append(line).append("\n"); //$NON-NLS-1$
				line = fr.readLine();
			}
		}
		return buffer.toString();
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
	
	/** Replies the list of test projects.
	 *
	 * @return the list of installed test projects.
	 */
	protected List<String> getInstalledProjects() {
		final List<String> projects = new ArrayList<>();
		int i = 1;
		URL url = null;
		do {
			final String name = "prj" + i;
			url = getClass().getResource("/projects/" + name + "/pom.xml"); //$NON-NLS-1$ //$NON-NLS-2$
			if (url != null) {
				projects.add(name);
			}
			++i;
		} while (url != null);
		return projects;
	}

	/** Run the test on all the platforms.
	 *
	 * @param testCode the test code.
	 * @return the list of dynamic tests.
	 */
	protected List<DynamicTest> buildTests(Procedure3<String, Integer, File> testCode) {
		final List<DynamicTest> tests = new ArrayList<>();
		final Map<Integer, File> jdks = getInstalledJdks();
		final List<String> projects = getInstalledProjects();
		for (final Entry<Integer, File> entry : jdks.entrySet()) {
			for (final String project : projects) {
				tests.add(DynamicTest.dynamicTest(getJavaVersionLabel(entry.getKey()) + " - " + project, () -> {
					testCode.apply(project, entry.getKey(), entry.getValue());
				}));
			}
		}
		if (tests.isEmpty()) {
			tests.add(DynamicTest.dynamicTest("Not a test", () -> {
				throw new TestAbortedException("Nothing to test. No installed JDK found into " + ROOT_FOLDER_FOR_JDK);
			}));
		}
		return tests;
	}

	/** Execute a SARLC.
	 *
	 * @param projectName the name of the project to test for the unit test.
	 * @param jversion the java version to be used.
	 * @param jdk the JDK to be used.
	 * @throws Exception any exception.
	 */
	protected void executeSarlc(String projectName, int jversion, File jdk) throws Exception {
		File sarlcExec = new File(SARLC_BIN_1);
		if (!sarlcExec.isFile()) {
			sarlcExec = new File(SARLC_BIN_0);
			if (!sarlcExec.isFile()) {
				final File currDir = new File("").getAbsoluteFile(); //$NON-NLS-1$
				throw new FileNotFoundException("Cannot find the sarlc binary file: " //$NON-NLS-1$
						+ SARLC_BIN_0 + " or "
						+ SARLC_BIN_1 + ". Rerun the maven compilation of the entire SARL project.\n" //$NON-NLS-1$
						+ "Current directory: " + currDir); //$NON-NLS-1$
			}
		}
		if (!sarlcExec.canExecute()) {
			final File currDir = new File("").getAbsoluteFile(); //$NON-NLS-1$
			throw new IllegalAccessException("Cannot execute the sarlc binary file: " //$NON-NLS-1$
					+ sarlcExec.getName() + ".\n" //$NON-NLS-1$
					+ "Current directory: " + currDir); //$NON-NLS-1$
		}
		
		String tempDirPath = System.getProperty("maven.test.tmpdir", //$NON-NLS-1$
				System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
		File tempDir = new File(tempDirPath);
		final String tmpName = projectName + "_" + jversion; // $NON-NLS-1$
		File baseDir = new File(tempDir, tmpName);
		final File sourceDir = new File(new File(new File(baseDir, "src"), "main"), "sarl");
		final File javaDir = new File(new File(new File(new File(baseDir, "src"), "main"), "generated-sources"), "sarl");
		final File binDir = new File(new File(baseDir, "target"), "classes");
		final File logFile = new File(tempDir, tmpName + ".log");
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

		recursiveDeleteOnShutdownHook(baseDir, logFile);

		final List<String> command = new ArrayList<>();
		command.add(sarlcExec.getAbsolutePath());
		command.add("-d");
		command.add(javaDir.getAbsolutePath());
		command.add("-o");
		command.add(binDir.getAbsolutePath());
		command.add(sourceDir.getAbsolutePath());

		ProcessBuilder processBuilder = new ProcessBuilder(command);
		processBuilder.redirectErrorStream();
		processBuilder.redirectOutput(logFile);
		processBuilder.environment().put("JAVA_HOME", jdk.getAbsolutePath());
		processBuilder = processBuilder.directory(baseDir.getAbsoluteFile());

		final Process process = processBuilder.start();

		final int ret = process.waitFor();
		if (ret != 0) {
			final String logContent = readFile(logFile);
			throw new ComparisonFailure("Compilation error", "", logContent);
		}
	}

	@TestFactory
	@DisplayName("maven")
	@EnabledOnOs(OS.LINUX)
	public List<DynamicTest> mavenCompilationOnLinux() throws Exception {
		return buildTests((project, jversion, jdk) -> {
			try {
				executeMojo(project, "compile", jversion, jdk);
			} catch (Exception exception) {
				throw new RuntimeException(exception);
			}
		});
	}

	@TestFactory
	@DisplayName("sarlc")
	@EnabledOnOs(OS.LINUX)
	public List<DynamicTest> sarlcCompilationOnLinux() throws Exception {
		return buildTests((project, jversion, jdk) -> {
			try {
				executeSarlc(project, jversion, jdk);
			} catch (Exception exception) {
				throw new RuntimeException(exception);
			}
		});
	}

}

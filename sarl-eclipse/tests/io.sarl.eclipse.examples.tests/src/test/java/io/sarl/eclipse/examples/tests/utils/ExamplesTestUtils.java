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

package io.sarl.eclipse.examples.tests.utils;

import static io.sarl.eclipse.examples.wizard.XmlUtils.readXmlAttribute;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.abort;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import com.google.inject.Injector;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.OperatingSystem;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.RuntimeIOException;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DynamicTest;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.compiler.batch.CleaningPolicy;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.core.SARLVersion;
import io.sarl.tests.api.tools.TestUtils;

/** Utilities for the example's tests.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public final class ExamplesTestUtils {

	private ExamplesTestUtils() {
		//
	}

	/** Name of the property that contains the root path for the project to test.
	 */
	public static final String ROOT_TEST_FOLDER_PROPERTY = "io.sarl.examples.test.rootDir"; //$NON-NLS-1$

	/** Relative path to the examples.
	 */
	private static final String DEFAULT_RELATIVE_PATH_BASE = "../../plugins/io.sarl.eclipse.examples"; //$NON-NLS-1$

	/** Relative path to the examples.
	 */
	public static final File DEFAULT_RELATIVE_PATH = FileSystem.convertURLToFile(
			FileSystem.convertStringToURL("file:" + DEFAULT_RELATIVE_PATH_BASE, false)); //$NON-NLS-1$

	/** Name of the folder that contains the examples.
	 */
	public static final String CONTENTS_FOLDER_NAME = "contents"; //$NON-NLS-1$

	/** Name of the folder that contains the examples' projects.
	 */
	public static final String PROJECTS_FOLDER_NAME = "projects"; //$NON-NLS-1$

	/** Name of maven command.
	 */
	public static final String MAVEN_COMMAND = "mvn"; //$NON-NLS-1$

	/** Current Java version.
	 */
	public static final String CURRENT_JAVA_VERSION = "17"; //$NON-NLS-1$

	private static final String[] WIN_EXTS = {".COM", ".EXE", ".BAT", ".CMD"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
	
	private volatile static List<ExampleDescription> ARCHIVE_BUFFER = null;

	/** Create the dynamic tests with the given function.
	 *
	 * @param testFunction the test code.
	 * @return the dynamic tests.
	 * @throws Exception if the example descriptions cannot be read.
	 */
	public static List<DynamicTest> dynamicTests(TestExecutable testFunction) throws Exception {
		return dynamicTests(true, testFunction);
	}
	
	/** Create the dynamic tests with the given function.
	 *
	 * @param testIfArchive indicates if the example archive should be test for creating the dynamic test.
	 * @param testFunction the test code.
	 * @return the dynamic tests.
	 * @throws Exception if the example descriptions cannot be read.
	 */
	public static List<DynamicTest> dynamicTests(boolean testIfArchive, TestExecutable testFunction) throws Exception {
		final List<ExampleDescription> descriptions = getExampleDescriptions();
		final List<DynamicTest> tests = new ArrayList<>();
		for (final ExampleDescription description : descriptions) {
			if (!testIfArchive || description.archive != null) {
				tests.add(DynamicTest.dynamicTest(description.name, () -> testFunction.execute(description)));
			}
		}
		return tests;
	}

	/** Replies the descriptions for the examples.
	 *
	 * @return the descriptions.
	 * @throws Exception in case of error.
	 */
	private static List<ExampleDescription> getExampleDescriptions() throws Exception {
		if (ARCHIVE_BUFFER == null) {
			final Set<String> names = new TreeSet<>();
			final Map<String, Pair<File, File>> rawSources = new TreeMap<>();
	
			File rootPath = null;
			final String projectdir = System.getProperty(ROOT_TEST_FOLDER_PROPERTY);
			if (!Strings.isEmpty(projectdir)) {
				rootPath = FileSystem.convertStringToFile(projectdir);
			}
			if (rootPath == null) {
				rootPath = DEFAULT_RELATIVE_PATH;
			}
	
			final File sourceFolder = new File(rootPath, PROJECTS_FOLDER_NAME);
			final File projectFolder = new File(rootPath, CONTENTS_FOLDER_NAME);
			if (projectFolder.isDirectory()) {
				for (File child : projectFolder.listFiles()) {
					if (child.isFile()) {
						final String basename = child.getName();
						final String sbasename = FileSystem.shortBasename(child);
						rawSources.putIfAbsent(basename, Pair.of(child,
								new File(sourceFolder, sbasename)));
						names.add(basename);
					}
				}
			}
	
			if (names.isEmpty()) {
				throw new Exception("no test found"); //$NON-NLS-1$
			}
	
			final List<ExampleDescription> list = new ArrayList<>();
	
			for (final String name : names) {
				final Pair<File, File> pair = rawSources.get(name);
				final File zipFile = pair.getKey();
				final File projectSourceFolder = pair.getValue();
				final ExampleDescription description = new ExampleDescription();
				description.name = FileSystem.shortBasename(name);
				description.archive = zipFile;
				description.sourceFolder = projectSourceFolder;
				list.add(description);
			}

			ARCHIVE_BUFFER = list;
		}
		return Collections.unmodifiableList(ARCHIVE_BUFFER);
	}

	/** Replies the SARL batch compiler.
	 *
	 * @return the SARL batch compiler.
	 */
	public static SarlBatchCompiler getSarlBatchCompiler() {
		Injector injector = SARLStandaloneSetup.doSetup();
		return injector.getInstance(SarlBatchCompiler.class);
	}

	/** Read the XML node.
	 *
	 * @param root the root.
	 * @param name the name of the node to read.
	 * @return the node with the given name.
	 */
	public static Node readXmlNode(Node root, String name) {
		NodeList nodes = root.getChildNodes();
		final int len = nodes.getLength();
		for (int i = 0; i < len; ++i) {
			Node node = nodes.item(i);
			if (node != null) {
				if (name.equals(node.getNodeName())) {
					return node;
				}
			}
		}
		return null;
	}
	
	/** Read the XML node that describes a file to open
	 *
	 * @param root the root.
	 * @param exampleZipFile the filename of the example's archive.
	 * @return the extension point node.
	 * @since 0.11
	 */
	public static Node readFileToOpenFromXml(Node root, File exampleZipFile) {
		NodeList nodes = root.getChildNodes();
		final int len = nodes.getLength();
		for (int i = 0; i < len; ++i) {
			Node node = nodes.item(i);
			if (node != null) {
				if ("extension".equals(node.getNodeName()) //$NON-NLS-1$
					&& "org.eclipse.emf.common.ui.examples".equals(readXmlAttribute(node, "point"))) { //$NON-NLS-1$ //$NON-NLS-2$
					node = readXmlNode(node, "example"); //$NON-NLS-1$
					if (node != null) {
						Node prjNode = readXmlNode(node, "projectDescriptor"); //$NON-NLS-1$
						if (prjNode != null) {
							String field = readXmlAttribute(prjNode, "contentURI"); //$NON-NLS-1$
							assertNotNull(field);
							File zip = FileSystem.convertStringToFile(field);
							if (exampleZipFile.getPath().endsWith(zip.getPath())) {
								return readXmlNode(node, "fileToOpen"); //$NON-NLS-1$
							}
						}
					}
				}
			}
		}
		return null;
	}

	/** Replies if the give source file is the folder of a maven project.
	 *
	 * @param exampleSourceFile the name of a folder to test.
	 * @return {@code true} if the folder contains a maven project.
	 */
	public static boolean isMavenProject(File exampleSourceFile) {
		if (exampleSourceFile != null) {
			final File pomFile = new File(exampleSourceFile, "pom.xml"); //$NON-NLS-1$
			return pomFile.exists();
		}
		return false;
	}

	/** Assert that the given list contains no issue.
	 *
	 * @param issues the list to test.
	 */
	public static void assertNoIssue(List<String> issues) {
		if (!issues.isEmpty()) {
			fail("Errors in the example code.\n" + Strings.concat("\n", issues)); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/** Build a string representation of a folder, recursively.
	 *
	 * @param root the root file.
	 * @return the string representation
	 * @since 0.12
	 */
	public static String buildFolderTree(File root) {
		final Deque<File> accumulator = new LinkedList<>();
		if (root != null && root.isDirectory()) {
			accumulator.add(root);
		}
		final StringBuilder str = new StringBuilder();
		if (root != null && root.isDirectory()) {
			str.append(root.getAbsolutePath()).append("\n"); //$NON-NLS-1$
		}
		while (!accumulator.isEmpty()) {
			final File folder = accumulator.removeFirst();
			for (final File file : folder.listFiles()) {
				if (!file.isHidden()) {
					str.append(file.getAbsolutePath()).append("\n"); //$NON-NLS-1$
					if (file.isDirectory()) {
						accumulator.add(file);
					}
				}
			}
		}
		return str.toString();
	}

	/** Assert file exists.
	 *
	 * @param file the file to test.
	 * @param root the root file.
	 )* @since 0.12
	 */
	public static void assertFile(File file, File root) {
		assertNotNull(file, "The filename cannot be null"); //$NON-NLS-1$
		if (!file.exists()) {
			File parent = file.getParentFile();
			final Deque<File> parents = new LinkedList<>();
			while (parent != null) {
				parents.addFirst(parent);
				parent = parent.getParentFile();
			}
			for (final File lparent : parents) {
				if (!lparent.exists()) {
					fail("Parent folder not found: " + lparent.getAbsolutePath() //$NON-NLS-1$
						+ "\nFor file: " + file.getAbsolutePath() //$NON-NLS-1$
						+ "\nTree:\n" + buildFolderTree(root)); //$NON-NLS-1$
					return;
				}
			}
			fail("File not found: " + file.getAbsolutePath() //$NON-NLS-1$
					+ "\nSibling files are: " //$NON-NLS-1$
					+ Strings.concat("\n", Arrays.asList(file.getParentFile().list())) //$NON-NLS-1$
					+ "\nTree:\n" + buildFolderTree(root)); //$NON-NLS-1$
		}
	}

	/** Prepare the pom file for a test.
	 *
	 * @param root the root directory where the pom file is located.
	 * @since 0.12
	 */
	public static void preparePomFileForTest(File root) {
		final File pomFile = new File(root, "pom.xml"); //$NON-NLS-1$
		if (pomFile.exists() && pomFile.canWrite()) {
			// Read the pom
			final StringBuilder content = new StringBuilder();
			try (BufferedReader reader = new BufferedReader(new FileReader(pomFile))) {
				String line = reader.readLine();
				while (line != null) {
					line = line.replaceAll(Pattern.quote("@USER_JAVA_VERSION@"), CURRENT_JAVA_VERSION); //$NON-NLS-1$
					content.append(line).append("\n"); //$NON-NLS-1$
					line = reader.readLine();
				}
			} catch (Exception exception) {
				throw new RuntimeIOException(exception);
			}
			// Write the pom
			try (FileWriter os = new FileWriter(pomFile)) {
				os.write(content.toString());
			} catch (IOException exception) {
				throw new RuntimeIOException(exception);
			}
		}
	}

	/** Search the given command on the operating system path.
	 *
	 * @param command the command to search for.
	 * @return the command path.
	 * @since 0.12
	 */
	public static String whichCommand(String command) {
		String paths = System.getenv("PATH"); //$NON-NLS-1$
		if (!Strings.isEmpty(paths)) {
			for (final String path : paths.split(Pattern.quote(File.pathSeparator))) {
				final File pathFile = FileSystem.convertStringToFile(path).getAbsoluteFile();
				if (pathFile.isDirectory()) {
					if (OperatingSystem.getCurrentOS() == OperatingSystem.WIN) {
						for (final String ext : WIN_EXTS) {
							final String cmd = testExecutable(command, pathFile, ext);
							if (cmd != null) {
								return cmd;
							}
						}
					} else {
						final String cmd = testExecutable(command, pathFile, null);
						if (cmd != null) {
							return cmd;
						}
					}
				}
			}
		}
		return command;
	}

	private static String testExecutable(String command, File path, String extension) {
		final File exec;
		if (Strings.isEmpty(extension)) {
			exec = FileSystem.join(path, command);
		} else {
			exec = FileSystem.join(path, command + extension);
		}
		if (exec != null && exec.canExecute()) {
			return exec.getAbsolutePath();
		}
		return null;
	}
	
	/** Compile the given project with the standard maven tool.
	 *
	 * @param compiler the SARL compiler to use.
	 * @param root the root folder of the project.
	 * @param installedFiles the installed files.
	 * @return the list of issues.
	 * @throws Exception if compilation cannot be proceeded.
	 * @see #MAVEN_COMMAND
	 */
	public static String compileMaven(File root) throws Exception {
		File compiledFile = new File(root, ".sarltestscompiled.tmp"); //$NON-NLS-1$
		if (compiledFile.exists()) {
			return null;
		}
		compiledFile.createNewFile();
		
		final String[] command = new String[] {
				whichCommand(MAVEN_COMMAND),
				// The options for maven always starts with "-" even on Windows OS
				"-q", //$NON-NLS-1$
				"clean", "package" //$NON-NLS-1$ //$NON-NLS-2$
		};
		final ProcessBuilder processBuilder = new ProcessBuilder(command);
		processBuilder.directory(root);
		final Process p = processBuilder.start();
		p.waitFor();
		/*final String[] environmentVariables = new String[] {
				"OPENJFX_LIB_PATH=" + DEFAULT_JAVAFX_PATH,
		};
		final Process p = Runtime.getRuntime().exec(command, environmentVariables, root);
		p.waitFor();*/
		final StringBuilder output = new StringBuilder();
		output.append("Exit code: ").append(p.exitValue()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		final BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
		String line = reader.readLine();
		while (line != null) {
			output.append(line + "\n"); //$NON-NLS-1$
			line = reader.readLine();
		}
		final BufferedReader readerErr = new BufferedReader(new InputStreamReader(p.getErrorStream()));
		line = readerErr.readLine();
		while (line != null) {
			output.append(line + "\n"); //$NON-NLS-1$
			line = readerErr.readLine();
		}
		if (p.exitValue() != 0) {
			return output.toString();
		}
		return null;
	}

	/** Compile the given files with the given batch compiler.
	 *
	 * @param root the root folder of the project.
	 * @param installedFiles the installed files.
	 * @return the list of issues.
	 * @throws Exception if compilation cannot be proceeded.
	 */
	public static List<String> compileFiles(File root, List<File> installedFiles) throws Exception {
		final SarlBatchCompiler compiler = getSarlBatchCompiler();
		final List<String> issues = new ArrayList<>();
		compiler.setBasePath(root.getAbsolutePath());
		compiler.setTempDirectory(getTempPath(root));
		compiler.addSourcePath(getSourcePath(root));
		compiler.setClassOutputPath(getBinPath(root));
		compiler.setOutputPath(getSourceGenPath(root));
		compiler.setGenerateGeneratedAnnotation(false);
		compiler.setGenerateInlineAnnotation(false);
		compiler.setGenerateSyntheticSuppressWarnings(true);
		compiler.setCleaningPolicy(CleaningPolicy.NO_CLEANING);
		compiler.setClassPath(getClassPath());
		compiler.setModulePath(getModulePath());
		compiler.setJavaSourceVersion(SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
		compiler.setAllWarningSeverities(Severity.IGNORE);
		compiler.setWarningSeverity(IssueCodes.DEPRECATED_MEMBER_REFERENCE, Severity.ERROR);
		compiler.setJavaCompilerVerbose(false);
		final Logger nopLogger = Logger.getAnonymousLogger();
		nopLogger.setLevel(Level.OFF);
		compiler.setLogger(nopLogger);
		compiler.addIssueMessageListener((severity, issue, uri, message) -> {
			if (issue.isSyntaxError() || severity.compareTo(Severity.ERROR) >= 0) {
				final Integer line = issue.getLineNumber();
				final int issueLine = (line == null ? 0 : line.intValue());
				issues.add(message + " (line " + issueLine + ")"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		});
		if (compiler.compile()) {
			return Collections.emptyList();
		}
		return issues;
	}

	private static List<File> getClassPath() throws Exception {
		final List<File> classpath = new ArrayList<>();
		final String cp = System.getProperty("java.class.path"); //$NON-NLS-1$
		if (!Strings.isEmpty(cp)) {
			for (final String filename : cp.split(Pattern.quote(File.pathSeparator))) {
				final File file = FileSystem.convertStringToFile(filename);
				if (file != null) {
					classpath.add(file);
				}
			}
		}
		return classpath;
	}

	private static List<File> getModulePath() throws Exception {
		return Collections.emptyList();
	}

	/** Unpack the given zip file into the root folder.
	 *
	 * @param root the root folder.
	 * @param exampleZipFile the zip file to unpack.
	 * @return the list of extracted files.
	 * @throws Exception if cannot unpack.
	 */
	public static List<File> unpackFiles(File root, File exampleZipFile) throws Exception {
		FileSystem.unzipFile(exampleZipFile, root);

		final List<File> installedFiles = new ArrayList<>();
		final List<File> folders = new ArrayList<>();
		folders.add(root);
		while (!folders.isEmpty()) {
			final File folder = folders.remove(0);
			for (final File file : folder.listFiles()) {
				if (file.isDirectory()) {
					folders.add(file);
				} else if (file.isFile()) {
					if (!isIgnorableFile(file)) {
						final File relPathFile = FileSystem.makeRelative(file, root);
						installedFiles.add(relPathFile);
					} else {
						file.delete();
					}
				}
			}
		}
		return installedFiles;
	}

	/** Copy the files from the given source folder into the root folder.
	 *
	 * @param root the root folder.
	 * @param sourceFolder the folder to copy.
	 * @return the list of extracted files.
	 * @throws Exception if cannot unpack.
	 * @since 0.11
	 */
	public static List<File> copySourceFiles(File root, File sourceFolder) throws Exception {
		List<File> folders = new ArrayList<>();
		if (sourceFolder.isDirectory()) {
			final File absSourceFolder = sourceFolder.getAbsoluteFile().getCanonicalFile();
			folders.add(sourceFolder);
			while (!folders.isEmpty()) {
				final File folder = folders.remove(0);
				for (final File file : folder.listFiles()) {
					if (file.isDirectory()) {
						folders.add(file);
					} else if (file.isFile()) {
						if (!isIgnorableFile(file)) {
							final File absFile = file.getAbsoluteFile().getCanonicalFile();
							final File relPathFile = FileSystem.makeRelative(absFile, absSourceFolder);
							final File targetFile = FileSystem.join(root, relPathFile);
							targetFile.getParentFile().mkdirs();
							FileSystem.copy(file, targetFile);
						}
					}
				}
			}
		}

		final List<File> installedFiles = new ArrayList<>();
		folders = new ArrayList<>();
		folders.add(root);
		while (!folders.isEmpty()) {
			final File folder = folders.remove(0);
			for (final File file : folder.listFiles()) {
				if (file.isDirectory()) {
					folders.add(file);
				} else if (file.isFile()) {
					if (!isIgnorableFile(file)) {
						final File relPathFile = FileSystem.makeRelative(file, root);
						installedFiles.add(relPathFile);
					} else {
						file.delete();
					}
				}
			}
		}
		return installedFiles;
	}

	private static boolean isIgnorableFile(File file) {
		final String name = file.getName();
		return ".classpath".equals(name) || ".project".equals(name); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Replies the path of the SARL source files in the example project.
	 *
	 * @param rootPath the root path.
	 * @return the source path.
	 */
	public static File getSourcePath(File rootPath) {
		return FileSystem.join(rootPath, "src", "main", "sarl"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}

	/** Replies the path in which the Java code is generated from the SARL code.
	 *
	 * @param rootPath the root path.
	 * @return the generation path.
	 */
	public static File getSourceGenPath(File rootPath) {
		return new File(rootPath, "src-gen"); //$NON-NLS-1$
	}

	/** Replies the path in which the byte code files is generated from the Java code.
	 *
	 * @param rootPath the root path.
	 * @return the byte code path.
	 */
	public static File getBinPath(File rootPath) {
		return new File(rootPath, "bin"); //$NON-NLS-1$
	}

	/** Replies the path of a temporary folder that is used for building the example's project.
	 *
	 * @param rootPath the root path.
	 * @return the temp path.
	 */
	public static File getTempPath(File rootPath) {
		return new File(rootPath, "build"); //$NON-NLS-1$
	}

	/** Create a compilable project.
	 *
	 * @return the folder that could contains the example's project.
	 * @throws Exception if the project's folder cannot be created.
	 */
	public static File createProject() throws Exception {
		final File rootPath = FileSystem.createTempDirectory("exampletests", ".tmp").getAbsoluteFile(); //$NON-NLS-1$ //$NON-NLS-2$
		getSourcePath(rootPath).mkdirs();
		getSourceGenPath(rootPath).mkdirs();
		getBinPath(rootPath).mkdirs();
		getTempPath(rootPath).mkdirs();
		FileSystem.deleteOnExit(rootPath);
		return rootPath;
	}

	/** Read the XML nodes that describes the wizard classes.
	 *
	 * @param root the root.
	 * @param exampleId the identifier of the example.
	 * @return the class names.
	 * @since 0.11
	 */
	public static List<String> readWizardClassesFromXml(Node root, String exampleId) {
		final List<String> classNames = new ArrayList<>();
		final Node pluginNode = readXmlNode(root, "plugin"); //$NON-NLS-1$
		if (pluginNode != null) {
			NodeList nodes = pluginNode.getChildNodes();
			final int len = nodes.getLength();
			for (int i = 0; i < len; ++i) {
				Node node = nodes.item(i);
				if (node != null) {
					if ("extension".equals(node.getNodeName()) //$NON-NLS-1$
						&& "org.eclipse.ui.newWizards".equals(readXmlAttribute(node, "point"))) { //$NON-NLS-1$ //$NON-NLS-2$
						final Node wizardNode = readXmlNode(node, "wizard"); //$NON-NLS-1$
						if (wizardNode != null) {
							final String id = readXmlAttribute(wizardNode, "id"); //$NON-NLS-1$
							if (Objects.equals(exampleId, id)) {
								final String className = readXmlAttribute(wizardNode, "class"); //$NON-NLS-1$
								classNames.add(className);
							}
						}
					}
				}
			}
		}
		return classNames;
	}

	/** Install the files of the example in order to be compiled.
	 * 
	 * @param example is the description of the example.
	 * @param projectRoot is the directory in which the files must be installed.
	 * @param skipIfInvalidPom is {@code true} to avoid to check the pom file.
	 * @return the list of the installed files.
	 * @throws Exception if an error occurs during the installation.
	 */
	public static List<File> installFiles(ExampleDescription example, File projectRoot, boolean skipIfInvalidPom) throws Exception {
		// The behavior is different in Eclipse Junit and Maven surefire.
		final List<File> installedFiles;
		if (TestUtils.isEclipseRuntimeEnvironment()) {
			if (skipIfInvalidPom && isMavenProject(example.sourceFolder)) {
				abort("Pom file is not valid because Maven has not yet applied the macro replacements into the file"); //$NON-NLS-1$
			}
			installedFiles = copySourceFiles(projectRoot, example.sourceFolder);
		} else {
			installedFiles = unpackFiles(projectRoot, example.archive);
		}
		preparePomFileForTest(projectRoot);
		return installedFiles;
	}

}
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

package io.sarl.examples.tests.utils;

import static io.sarl.examples.wizard.XmlUtils.readXmlAttribute;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.Iterator;
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
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.TrueFileFilter;
import org.arakhne.afc.vmutil.ClasspathUtil;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.OperatingSystem;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.RuntimeIOException;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DynamicTest;
import org.opentest4j.AssertionFailedError;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.compiler.batch.CleaningPolicy;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;

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
	public static final File DEFAULT_RELATIVE_PATH = FileSystem.convertStringToFile("file:../io.sarl.examples.plugin"); //$NON-NLS-1$

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
	public static final String CURRENT_JAVA_VERSION = "11"; //$NON-NLS-1$

	// TODO Remove this definition when moving to Java 9 or higher (because JavaFX is mavenized)
	public static final String DEFAULT_JAVAFX_PATH = "/home/sgalland/git/sarl.dsl/contribs/io.sarl.examples/io.sarl.examples.tests/../../../build-tools/libs/jfxrt.jar"; //$NON-NLS-1$

	private static final String[] WIN_EXTS = {".COM", ".EXE", ".BAT", ".CMD"};
	
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
				if ("extension".equals(node.getNodeName())
					&& "org.eclipse.emf.common.ui.examples".equals(readXmlAttribute(node, "point"))) {
					node = readXmlNode(node, "example");
					if (node != null) {
						Node prjNode = readXmlNode(node, "projectDescriptor");
						if (prjNode != null) {
							String field = readXmlAttribute(prjNode, "contentURI");
							assertNotNull(field);
							File zip = FileSystem.convertStringToFile(field);
							if (exampleZipFile.getPath().endsWith(zip.getPath())) {
								return readXmlNode(node, "fileToOpen");
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
			final File pomFile = new File(exampleSourceFile, "pom.xml");
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
			throw new AssertionFailedError("Errors in the example code", "", Strings.concat("\n", issues));
		}
	}

	/** Build a string representation of a folder, recursively.
	 *
	 * @param file the root file.
	 * @return the string representation
	 * @since 0.12
	 */
	public static String buildFolderTree(File root) {
		final Collection<File> files = FileUtils.listFilesAndDirs(root,
				TrueFileFilter.INSTANCE,
				TrueFileFilter.INSTANCE);
		final StringBuilder str = new StringBuilder();
		for (final File file : files) {
			str.append(file.getAbsolutePath());
			str.append("\n"); //$NON-NLS-2$
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
		assertNotNull(file, "The filename cannot be null");
		if (!file.exists()) {
			File parent = file.getParentFile();
			final Deque<File> parents = new LinkedList<>();
			while (parent != null) {
				parents.addFirst(parent);
				parent = parent.getParentFile();
			}
			for (final File lparent : parents) {
				if (!lparent.exists()) {
					fail("Parent folder not found: " + lparent.getAbsolutePath()
						+ "\nFor file: " + file.getAbsolutePath()
						+ "\nTree:\n" + buildFolderTree(root));
					return;
				}
			}
			fail("File not found: " + file.getAbsolutePath()
					+ "\nSibling files are: "
					+ Strings.concat("\n", Arrays.asList(file.getParentFile().list()))
					+ "\nTree:\n" + buildFolderTree(root));
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
		String paths = System.getenv("PATH");
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
		File compiledFile = new File(root, ".sarltestscompiled.tmp");
		if (compiledFile.exists()) {
			return null;
		}
		compiledFile.createNewFile();
		
		final String[] command = new String[] {
				whichCommand(MAVEN_COMMAND),
				// The options for maven always starts with "-" even on Windows OS
				"-q",
				"clean", "package"
		};
		final ProcessBuilder processBuilder = new ProcessBuilder(command);
		processBuilder.directory(root);
		// TODO Remove this definition when moving to Java 9 or higher (because JavaFX is mavenized)
		processBuilder.environment().put(
				"OPENJFX_LIB_PATH", DEFAULT_JAVAFX_PATH);
		final Process p = processBuilder.start();
		p.waitFor();
		/*final String[] environmentVariables = new String[] {
				"OPENJFX_LIB_PATH=" + DEFAULT_JAVAFX_PATH,
		};
		final Process p = Runtime.getRuntime().exec(command, environmentVariables, root);
		p.waitFor();*/
		final StringBuilder output = new StringBuilder();
		output.append("Exit code: ").append(p.exitValue()).append("\n");
		final BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
		String line = reader.readLine();
		while (line != null) {
			output.append(line + "\n");
			line = reader.readLine();
		}
		final BufferedReader readerErr = new BufferedReader(new InputStreamReader(p.getErrorStream()));
		line = readerErr.readLine();
		while (line != null) {
			output.append(line + "\n");
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
		compiler.addIssueMessageListener((issue, uri, message) -> {
			if (issue.isSyntaxError() || issue.getSeverity().compareTo(Severity.ERROR) >= 0) {
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
		final Iterator<URL> iterator = ClasspathUtil.getClasspath();
		while (iterator.hasNext()) {
			final URL url = iterator.next();
			try {
				final File file = FileSystem.convertURLToFile(url);
				classpath.add(file);
			} catch (IllegalArgumentException exception) {
				//
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
		return ".classpath".equals(name) || ".project".equals(name);
	}

	/** Replies the path of the SARL source files in the example project.
	 *
	 * @param rootPath the root path.
	 * @return the source path.
	 */
	public static File getSourcePath(File rootPath) {
		return FileSystem.join(rootPath, "src", "main", "sarl");
	}

	/** Replies the path in which the Java code is generated from the SARL code.
	 *
	 * @param rootPath the root path.
	 * @return the generation path.
	 */
	public static File getSourceGenPath(File rootPath) {
		return new File(rootPath, "src-gen");
	}

	/** Replies the path in which the byte code files is generated from the Java code.
	 *
	 * @param rootPath the root path.
	 * @return the byte code path.
	 */
	public static File getBinPath(File rootPath) {
		return new File(rootPath, "bin");
	}

	/** Replies the path of a temporary folder that is used for building the example's project.
	 *
	 * @param rootPath the root path.
	 * @return the temp path.
	 */
	public static File getTempPath(File rootPath) {
		return new File(rootPath, "build");
	}

	/** Create a compilable project.
	 *
	 * @return the folder that could contains the example's project.
	 * @throws Exception if the project's folder cannot be created.
	 */
	public static File createProject() throws Exception {
		final File rootPath = FileSystem.createTempDirectory("exampletests", ".tmp").getAbsoluteFile();
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
		final Node pluginNode = readXmlNode(root, "plugin");
		if (pluginNode != null) {
			NodeList nodes = pluginNode.getChildNodes();
			final int len = nodes.getLength();
			for (int i = 0; i < len; ++i) {
				Node node = nodes.item(i);
				if (node != null) {
					if ("extension".equals(node.getNodeName())
						&& "org.eclipse.ui.newWizards".equals(readXmlAttribute(node, "point"))) {
						final Node wizardNode = readXmlNode(node, "wizard");
						if (wizardNode != null) {
							final String id = readXmlAttribute(wizardNode, "id");
							if (Objects.equals(exampleId, id)) {
								final String className = readXmlAttribute(wizardNode, "class");
								classNames.add(className);
							}
						}
					}
				}
			}
		}
		return classNames;
	}

}
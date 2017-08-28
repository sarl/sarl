/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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


package io.sarl.examples.tests;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import com.google.inject.Injector;
import org.apache.log4j.Level;
import org.arakhne.afc.vmutil.ClasspathUtil;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;
import org.junit.Assume;
import org.junit.BeforeClass;
import org.junit.ComparisonFailure;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.compilation.compiler.batch.SarlBatchCompiler;
import io.sarl.tests.api.AbstractSarlTest;

/** Class for testing the examples.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Parameterized.class)
@SuppressWarnings("all")
public class ExamplesTest extends AbstractSarlTest {

	/** Name of the property that contains the root path for the project to test.
	 */
	public static final String ROOT_TEST_FOLDER_PROPERTY = "io.sarl.examples.test.rootDir"; //$NON-NLS-1$

	private static final File DEFAULT_RELATIVE_PATH = FileSystem.convertStringToFile("file:../io.sarl.examples.plugin"); //$NON-NLS-1$

	private static final String PROJECTS_FOLDER_NAME = "projects"; //$NON-NLS-1$

	/** Replies the archives for the examples.
	 *
	 * @return the archive locations.
	 * @throws Exception in case of error.
	 */
	@Parameters(name = "Example {1}")
	public static Collection<Object[]> getExampleArchives() throws Exception {
		final Set<String> names = new TreeSet<>();
		final Map<String, File> rawSources = new TreeMap<>();

		File rootPath = null;
		final String projectdir = System.getProperty(ROOT_TEST_FOLDER_PROPERTY);
		if (!Strings.isEmpty(projectdir)) {
			rootPath = FileSystem.convertStringToFile(projectdir);
		}
		if (rootPath == null) {
			rootPath = DEFAULT_RELATIVE_PATH;
		}

		final File projectFolder = new File(rootPath, PROJECTS_FOLDER_NAME);
		if (projectFolder.isDirectory()) {
			for (File child : projectFolder.listFiles()) {
				if (child.isDirectory()) {
					final String basename = child.getName();
					rawSources.putIfAbsent(basename, child);
					names.add(basename);
				}
			}
		}

		if (names.isEmpty()) {
			throw new Exception("no test found"); //$NON-NLS-1$
		}

		final List<Object[]> list = new ArrayList<>();

		for (final String name : names) {
			final File folder = rawSources.get(name);
			list.add(new Object[] {name, folder});
		}

		return list;
	}

	private final String name;

	private final File exampleFolder;

	static SarlBatchCompiler compiler;

	/** Constructor.
	 *
	 * @param name the name of the test.
	 * @param exampleFolder the folder to open.
	 */
	public ExamplesTest(String name, File exampleFolder) {
		this.name = name;
		this.exampleFolder = exampleFolder.getAbsoluteFile();
	}

	@BeforeClass
	public static void setupClass() throws Exception {
		Injector injector = SARLStandaloneSetup.doSetup();
		compiler = injector.getInstance(SarlBatchCompiler.class);
	}

	@Override
	public String toString() {
		return this.name;
	}

	@Test
	public void path() {
		assertNotNull(this.exampleFolder);
	}

	@Test
	public void compilation() throws Exception {
		Assume.assumeTrue(this.exampleFolder != null);
		final File projectRoot = createProject(); 
		final List<File> installedFiles = copyFiles(projectRoot);
		assertFalse("No installed file in " + projectRoot, installedFiles.isEmpty());
		List<String> issues = compileFiles(projectRoot, installedFiles);
		assertNoIssue(issues);
	}

	private void assertNoIssue(List<String> issues) {
		if (!issues.isEmpty()) {
			throw new ComparisonFailure("Errors in the example code", "", Strings.concat("\n", issues));
		}
	}

	private List<String> compileFiles(File root, List<File> installedFiles) throws Exception {
		final List<String> issues = new ArrayList<>();
		compiler.setBasePath(root.getAbsolutePath());
		compiler.setTempDirectory(getTempPath(root));
		compiler.addSourcePath(getSourcePath(root));
		compiler.setClassOutputPath(getBinPath(root));
		compiler.setOutputPath(getSourceGenPath(root));
		compiler.setGenerateGeneratedAnnotation(false);
		compiler.setGenerateInlineAnnotation(false);
		compiler.setGenerateSyntheticSuppressWarnings(true);
		compiler.setDeleteTempDirectory(false);
		compiler.setClassPath(getClasspath());
		compiler.setJavaSourceVersion(SARLVersion.MINIMAL_JDK_VERSION);
		compiler.setAllWarningSeverities(Severity.IGNORE);
		compiler.setJavaCompilerVerbose(false);
		compiler.getLogger().setLevel(Level.OFF);
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

	private List<File> getClasspath() throws Exception {
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

	private List<File> copyFiles(File root) throws Exception {
		final List<File> installedFiles = new ArrayList<>();
		final List<File> folders = new ArrayList<>();
		folders.add(this.exampleFolder);
		while (!folders.isEmpty()) {
			final File folder = folders.remove(0);
			for (final File file : folder.listFiles()) {
				if (file.isDirectory()) {
					folders.add(file);
				} else if (file.isFile()) {
					if (!isIgnorableFile(file)) {
						final File relPathFile = FileSystem.makeRelative(file, this.exampleFolder);
						final File targetFile = FileSystem.join(root, relPathFile);
						targetFile.getParentFile().mkdirs();
						FileSystem.copy(file, targetFile);
						installedFiles.add(relPathFile);
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

	private File getSourcePath(File rootPath) {
		return FileSystem.join(rootPath, "src", "main", "sarl");
	}

	private File getSourceGenPath(File rootPath) {
		return new File(rootPath, "src-gen");
	}

	private File getBinPath(File rootPath) {
		return new File(rootPath, "bin");
	}

	private File getTempPath(File rootPath) {
		return new File(rootPath, "build");
	}

	private File createProject() throws Exception {
		final File rootPath = FileSystem.createTempDirectory("exampletests", ".tmp").getAbsoluteFile();
		getSourcePath(rootPath).mkdirs();
		getSourceGenPath(rootPath).mkdirs();
		getBinPath(rootPath).mkdirs();
		getTempPath(rootPath).mkdirs();
		FileSystem.deleteOnExit(rootPath);
		return rootPath;
	}

}

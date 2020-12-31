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

package io.sarl.lang.sarlc.tests;

import static io.sarl.tests.api.tools.TestAssertions.assertEqualsExceptNewLines;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.util.Iterator;

import javax.annotation.Nullable;

import org.arakhne.afc.vmutil.ClasspathUtil;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.Resources;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarlc.Main;
import io.sarl.lang.sarlc.configs.subconfigs.JavaCompiler;
import io.sarl.maven.bootiqueapp.utils.SystemPath;

/** Tests for {@code Main}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
@SuppressWarnings("all")
@DisplayName("Entry point test")
@Tag("sarlc")
public class MainTest {

	private static final boolean CAPTURE_OUTPUTS = true;
	
	@Nullable
	private File rootFolder;

	@Nullable
	private File srcFolder;

	@Nullable
	private File genFolder;

	@Nullable
	private File tempFolder;

	@Nullable
	private File binFolder;

	@Nullable
	private File srcFile;

	@Nullable
	private PrintStream originalStdout;

	@Nullable
	private PrintStream originalStderr;

	@Nullable
	private ByteArrayOutputStream captureStdout;

	@Nullable
	private ByteArrayOutputStream captureStderr;

	private void restoreOutputs() throws IOException {
		System.setOut(this.originalStdout);
		System.setErr(this.originalStderr);
		if (this.captureStdout != null) {
			this.captureStdout.close();
			this.captureStdout = null;
		}
		if (this.captureStderr != null) {
			this.captureStderr.close();
			this.captureStderr = null;
		}
	}
	
	@BeforeEach
	public void setUp() {
		this.rootFolder = null;
		if (CAPTURE_OUTPUTS) {
			this.originalStdout = System.out;
			this.captureStdout = new ByteArrayOutputStream();
			System.setOut(new PrintStream(this.captureStdout));
			this.originalStderr = System.err;
			this.captureStderr = new ByteArrayOutputStream();
			System.setErr(new PrintStream(this.captureStderr));
		}
	}

	@AfterEach
	public void tearDown() throws IOException {
		if (this.rootFolder != null) {
			FileSystem.delete(this.rootFolder);
		}
		if (CAPTURE_OUTPUTS) {
			restoreOutputs();
		}
	}

	private void prepareProject(String name) throws IOException {
		this.rootFolder = FileSystem.createTempDirectory("sarlctests_" + name, "");
		this.srcFolder = FileSystem.join(this.rootFolder, "src", "main", "sarl");
		this.srcFolder.mkdirs();
		this.genFolder = FileSystem.join(this.rootFolder, "src", "main", "generated-sources", "sarl");
		this.genFolder.mkdirs();
		this.binFolder = FileSystem.join(this.rootFolder, "target", "classes");
		this.binFolder.mkdirs();
		this.tempFolder = FileSystem.join(this.rootFolder, "target", "sarl-build");
		this.tempFolder.mkdirs();

		final URL resource = Resources.getResource(MainTest.class, "resources/" + name + ".sarl");
		this.srcFile = FileSystem.join(this.srcFolder, name + ".sarl");
		FileSystem.copy(resource, this.srcFile);
	}

	private static final String EXPECTED_TEST1 = "package io.sarl.lang.sarlc.tests.resources.test1;\n" +
			"\n" +
			"import io.sarl.lang.annotation.DefaultValue;\n" +
			"import io.sarl.lang.annotation.DefaultValueSource;\n" +
			"import io.sarl.lang.annotation.DefaultValueUse;\n" +
			"import io.sarl.lang.annotation.SarlElementType;\n" +
			"import io.sarl.lang.annotation.SarlSourceCode;\n" +
			"import io.sarl.lang.annotation.SarlSpecification;\n" +
			"import io.sarl.lang.annotation.SyntheticMember;\n" +
			"import org.eclipse.xtend.lib.annotations.Accessors;\n" +
			"import org.eclipse.xtext.xbase.lib.Pure;\n" +
			"\n" +
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")\n" +
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")\n" +
			"@SuppressWarnings(\"all\")\n" +
			"public class Foo {\n" +
			"  @Accessors\n" +
			"  private int fooField;\n" +
			"  \n" +
			"  @DefaultValueSource\n" +
			"  public Foo(@DefaultValue(\"io.sarl.lang.sarlc.tests.resources.test1.Foo#NEW_0\") final int value) {\n" +
			"    this.fooField = value;\n" +
			"  }\n" +
			"  \n" +
			"  /**\n" +
			"   * Default value for the parameter value\n" +
			"   */\n" +
			"  @SyntheticMember\n" +
			"  @SarlSourceCode(\"0\")\n" +
			"  private static final int $DEFAULT_VALUE$NEW_0 = 0;\n" +
			"  \n" +
			"  @DefaultValueUse(\"int\")\n" +
			"  @SyntheticMember\n" +
			"  public Foo() {\n" +
			"    this($DEFAULT_VALUE$NEW_0);\n" +
			"  }\n" +
			"  \n" +
			"  @Override\n" +
			"  @Pure\n" +
			"  @SyntheticMember\n" +
			"  public boolean equals(final Object obj) {\n" +
			"    if (this == obj)\n" +
			"      return true;\n" +
			"    if (obj == null)\n" +
			"      return false;\n" +
			"    if (getClass() != obj.getClass())\n" +
			"      return false;\n" +
			"    Foo other = (Foo) obj;\n" +
			"    if (other.fooField != this.fooField)\n" +
			"      return false;\n" +
			"    return super.equals(obj);\n" +
			"  }\n" +
			"  \n" +
			"  @Override\n" +
			"  @Pure\n" +
			"  @SyntheticMember\n" +
			"  public int hashCode() {\n" +
			"    int result = super.hashCode();\n" +
			"    final int prime = 31;\n" +
			"    result = prime * result + Integer.hashCode(this.fooField);\n" +
			"    return result;\n" +
			"  }\n" +
			"  \n" +
			"  @Pure\n" +
			"  public int getFooField() {\n" +
			"    return this.fooField;\n" +
			"  }\n" +
			"  \n" +
			"  public void setFooField(final int fooField) {\n" +
			"    this.fooField = fooField;\n" +
			"  }\n" +
			"}\n";

	@Test
	public void compileTest1() throws IOException {
		prepareProject("test1");
		
		SystemPath classPath = new SystemPath();
		Iterator<URL> iterator = ClasspathUtil.getClasspath();
		while (iterator.hasNext()) {
			final URL classPathElement = iterator.next();
			final File localElement = FileSystem.convertURLToFile(classPathElement);
			classPath.add(localElement);
		}
		
		final int retcode = Main.run(
				"--encoding", "UTF-8",
				"--java-source", SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT,
				"--java-compiler", JavaCompiler.JAVAC.name(),
				"--tempdir", this.tempFolder.getAbsolutePath(),
				"--directory", this.genFolder.getAbsolutePath(),
				"--outputdir", this.binFolder.getAbsolutePath(),
				"--cp", classPath.toString(),
				this.srcFolder.getAbsolutePath());
		assertEquals(0, retcode);
		
		File javaFile = FileSystem.join(this.genFolder, "io", "sarl", "lang",
				"sarlc", "tests", "resources", "test1", "Foo.java");
		assertTrue(javaFile.exists(), () -> "The Java file " + javaFile.getAbsolutePath() + " was not found");
		String content = new String(Files.readAllBytes(javaFile.toPath()), Charset.defaultCharset());
		assertEqualsExceptNewLines(EXPECTED_TEST1, content);

		File classFile = FileSystem.join(this.binFolder, "io", "sarl", "lang",
				"sarlc", "tests", "resources", "test1", "Foo.class");
		assertTrue(classFile.exists(), () -> "The binary file " + classFile.getAbsolutePath() + " was not found");
	}

	@Test
	public void helpOption() throws IOException {
		//restoreOutputs();
		final int retcode = Main.run("--help");
		assertEquals(0, retcode);
	}

	@Test
	public void printConfigOption() throws IOException {
		//restoreOutputs();
		final int retcode = Main.run("-C");
		assertEquals(0, retcode);
	}

	@Test
	public void printConfigHelpOption() throws IOException {
		final int retcode = Main.run("-H");
		assertEquals(0, retcode);
	}

	@Test
	public void illegalOption() throws IOException {
		//restoreOutputs();
		final int retcode = Main.run("--thisisanillegaloption");
		assertEquals(255, retcode);
	}

}

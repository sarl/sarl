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
package io.sarl.maven.compiler;

import static io.sarl.tests.api.tools.TestAssertions.assertEqualsExceptNewLines;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.nio.file.FileSystems;
import java.nio.file.Path;

import org.apache.maven.it.Verifier;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Bug #799")
@Tag("maven")
@Tag("mvn-run")
public class Bug799Test extends AbstractMojoTest {

	private static final String EXPECTED_A = multilineString(
			"package io.sarl.maven.bug799;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.Skill;",
			"import io.sarl.maven.bug799.Cap;",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@SuppressWarnings(\"all\")",
			"public class A extends Skill implements Cap {",
			"public void act_connectToSimulator(final String address, final int port) {",
			"while (true) {",
			"}",
			"}",
			"@SyntheticMember",
			"public A() {",
			"super();",
			"}",
			"@SyntheticMember",
			"public A(final Agent arg0) {",
			"super(arg0);",
			"}",
			"}");
	
	@Test
	public void compile() throws Exception {
		Verifier verifier = executeMojo("bug799", "compile");
		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "bug799", "A.java");
		assertNotNull(path);
		verifier.assertFilePresent(path.toString());
		assertEqualsExceptNewLines(EXPECTED_A, readFile(verifier, path));
	}

}

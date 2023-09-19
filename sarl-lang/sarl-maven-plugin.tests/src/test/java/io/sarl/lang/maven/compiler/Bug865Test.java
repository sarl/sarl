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
package io.sarl.lang.maven.compiler;

import static io.sarl.tests.api.tools.TestAssertions.assertEqualsExceptNewLines;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.nio.file.FileSystems;
import java.nio.file.Path;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.maven.compiler.utils.MavenHelper;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.tools.AbstractEmbeddedMavenMojoTest;
import io.sarl.tests.api.tools.Verifier;

/** Test for issue #865: Maven complains on guava version in 0.8.2.
 * 
 * @author $Author: sgalland$
 * @version sarl-maven-plugin.tests 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid sarl-maven-plugin.tests
 * @see "https://github.com/sarl/sarl/issues/865"
 */
@SuppressWarnings("all")
@DisplayName("Bug #865")
@Tag("maven")
@Tag("mvn-run")
public class Bug865Test extends AbstractEmbeddedMavenMojoTest {

	@Override
	protected String getPluginGroupId() {
		return MavenHelper.getSarlMavenPluginGroupId(getLogger());
	}

	@Override
	protected String getPluginArtefactId() {
		return MavenHelper.getSarlMavenPluginArtifactId(getLogger());
	}

	@Override
	protected String getPluginVersion() {
		return MavenHelper.getSarlMavenPluginVersion(getLogger());
	}

	private static Verifier verifier = null;

	private synchronized Verifier doCompile() throws Exception {
		if (verifier == null) {
			verifier = executeMojo("bug865", "compile", false);
		}
		return verifier;
	}

	private static final String EXPECTED_MYAGENT = multilineString(
			"package io.sarl.maven.bug865;",
			"import com.google.common.primitives.Booleans;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.List;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class MyAgent extends Agent {",
			"@Pure",
			"protected List<Boolean> buildList(final boolean... values) {",
			"return Booleans.asList(values);",
			"}",
			"@SyntheticMember",
			"public MyAgent(final UUID arg0, final UUID arg1) {",
			"super(arg0, arg1);",
			"}",
			"@SyntheticMember",
			"@Inject",
			"public MyAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"}");

	@Test
	@DisplayName("Test MyAgent")
	public void compile01() throws Exception {
		Verifier verifier = doCompile();
		verifier.assertErrorFreeLog();

		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "bug865", "MyAgent.java");
		assertNotNull(path);
		verifier.assertFilePresent(path);
		assertEqualsExceptNewLines(EXPECTED_MYAGENT, verifier.readFile(path));
	}

}

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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.nio.file.FileSystems;
import java.nio.file.Path;

import org.apache.maven.it.Verifier;
import org.junit.BeforeClass;
import org.junit.Test;

import io.sarl.lang.SARLVersion;

/** Test for issue #850: CLI Compile error when using {@code occurrence.xxxx}.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/850"
 */
@SuppressWarnings("all")
public class Bug850Test extends AbstractMojoTest {

	private static Verifier verifier = null;

	private synchronized Verifier doCompile() throws Exception {
		if (verifier == null) {
			verifier = executeMojo("bug850", "compile");
		}
		return verifier;
	}

	private static final String EXPECTED_AGENT1 = multilineString(
			"package io.sarl.maven.bug850;",
			"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.maven.bug850.CarRequestPercept;",
			"import java.util.Collection;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(18)",
			"@SuppressWarnings(\"all\")",
			"public class Agent1 extends Agent {",
			"@Pure",
			"protected void reportMessage(final String a, final Object... b) {",
			"}",
			"private void $behaviorUnit$CarRequestPercept$0(final CarRequestPercept occurrence) {",
			"this.reportMessage(\"Car requested at floor :{0}, direction {1}\", Integer.valueOf(occurrence.floor), occurrence.direction);",
			"}",
			"@SyntheticMember",
			"@PerceptGuardEvaluator",
			"private void $guardEvaluator$CarRequestPercept(final CarRequestPercept occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"assert occurrence != null;",
			"assert ___SARLlocal_runnableCollection != null;",
			"___SARLlocal_runnableCollection.add(() -> $behaviorUnit$CarRequestPercept$0(occurrence));",
			"}",
			"@SyntheticMember",
			"public Agent1(final UUID arg0, final UUID arg1) {",
			"super(arg0, arg1);",
			"}",
			"@SyntheticMember",
			"@Deprecated",
			"@Inject",
			"public Agent1(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"@SyntheticMember",
			"@Inject",
			"public Agent1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"}");

	@Test
	public void compile01() throws Exception {
		Verifier verifier = doCompile();

		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "bug850", "Agent1.java");
		assertNotNull(path);
		verifier.assertFilePresent(path.toString());
		assertEquals(EXPECTED_AGENT1, readFile(verifier, path));
	}

	private static final String EXPECTED_AGENT2 = multilineString(
			"package io.sarl.maven.bug850;",
			"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.maven.bug850.CarRequestPercept;",
			"import io.sarl.maven.bug850.Direction;",
			"import java.util.Collection;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(18)",
			"@SuppressWarnings(\"all\")",
			"public class Agent2 extends Agent {",
			"@Pure",
			"protected void reportMessage(final String a, final Object... b) {",
			"}",
			"private void $behaviorUnit$CarRequestPercept$0(final CarRequestPercept occurrence) {",
			"final Direction dir = occurrence.direction;",
			"this.reportMessage(\"Car requested at floor :{0}, direction {1}\", Integer.valueOf(occurrence.floor), dir);",
			"}",
			"@SyntheticMember",
			"@PerceptGuardEvaluator",
			"private void $guardEvaluator$CarRequestPercept(final CarRequestPercept occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"assert occurrence != null;",
			"assert ___SARLlocal_runnableCollection != null;",
			"___SARLlocal_runnableCollection.add(() -> $behaviorUnit$CarRequestPercept$0(occurrence));",
			"}",
			"@SyntheticMember",
			"public Agent2(final UUID arg0, final UUID arg1) {",
			"super(arg0, arg1);",
			"}",
			"@SyntheticMember",
			"@Deprecated",
			"@Inject",
			"public Agent2(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"@SyntheticMember",
			"@Inject",
			"public Agent2(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"}");

	@Test
	public void compile02() throws Exception {
		Verifier verifier = doCompile();

		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "bug850", "Agent2.java");
		assertNotNull(path);
		verifier.assertFilePresent(path.toString());
		assertEquals(EXPECTED_AGENT2, readFile(verifier, path));
	}


	private static final String EXPECTED_AGENT3 = multilineString(
			"package io.sarl.maven.bug850;",
			"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.maven.bug850.CarRequestPercept;",
			"import java.util.Collection;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(18)",
			"@SuppressWarnings(\"all\")",
			"public class Agent3 extends Agent {",
			"@Pure",
			"protected void prologAssert(final String a, final int b, final String c) {",
			"}",
			"private void $behaviorUnit$CarRequestPercept$0(final CarRequestPercept occurrence) {",
			"this.prologAssert(\"request(@I, @S)\", occurrence.floor, occurrence.direction.toString().toLowerCase());",
			"}",
			"@SyntheticMember",
			"@PerceptGuardEvaluator",
			"private void $guardEvaluator$CarRequestPercept(final CarRequestPercept occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"assert occurrence != null;",
			"assert ___SARLlocal_runnableCollection != null;",
			"___SARLlocal_runnableCollection.add(() -> $behaviorUnit$CarRequestPercept$0(occurrence));",
			"}",
			"@SyntheticMember",
			"public Agent3(final UUID arg0, final UUID arg1) {",
			"super(arg0, arg1);",
			"}",
			"@SyntheticMember",
			"@Deprecated",
			"@Inject",
			"public Agent3(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"@SyntheticMember",
			"@Inject",
			"public Agent3(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"}");

	@Test
	public void compile03() throws Exception {
		Verifier verifier = doCompile();

		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "bug850", "Agent3.java");
		assertNotNull(path);
		verifier.assertFilePresent(path.toString());
		assertEquals(EXPECTED_AGENT3, readFile(verifier, path));
	}

	private static final String EXPECTED_AGENT4 = multilineString(
			"package io.sarl.maven.bug850;",
			"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.maven.bug850.CarRequestPercept;",
			"import java.util.Collection;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(18)",
			"@SuppressWarnings(\"all\")",
			"public class Agent4 extends Agent {",
			"@Pure",
			"protected void prologAssert(final String a, final int b, final String c) {",
			"}",
			"private void $behaviorUnit$CarRequestPercept$0(final CarRequestPercept occurrence) {",
			"final String x = occurrence.direction.toString().toLowerCase();",
			"this.prologAssert(\"request(@I, @S)\", occurrence.floor, x);",
			"}",
			"@SyntheticMember",
			"@PerceptGuardEvaluator",
			"private void $guardEvaluator$CarRequestPercept(final CarRequestPercept occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"assert occurrence != null;",
			"assert ___SARLlocal_runnableCollection != null;",
			"___SARLlocal_runnableCollection.add(() -> $behaviorUnit$CarRequestPercept$0(occurrence));",
			"}",
			"@SyntheticMember",
			"public Agent4(final UUID arg0, final UUID arg1) {",
			"super(arg0, arg1);",
			"}",
			"@SyntheticMember",
			"@Deprecated",
			"@Inject",
			"public Agent4(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"@SyntheticMember",
			"@Inject",
			"public Agent4(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"}");

	@Test
	public void compile04() throws Exception {
		Verifier verifier = doCompile();

		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "bug850", "Agent4.java");
		assertNotNull(path);
		verifier.assertFilePresent(path.toString());
		assertEquals(EXPECTED_AGENT4, readFile(verifier, path));
	}

}

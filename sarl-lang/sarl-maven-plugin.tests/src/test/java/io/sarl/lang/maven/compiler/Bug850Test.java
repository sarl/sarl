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

/** Test for issue #850: CLI Compile error when using {@code occurrence.xxxx}.
 * 
 * @author $Author: sgalland$
 * @version sarl-maven-plugin.tests 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid sarl-maven-plugin.tests
 * @see "https://github.com/sarl/sarl/issues/850"
 */
@SuppressWarnings("all")
@DisplayName("Bug #850")
@Tag("maven")
@Tag("mvn-run")
public class Bug850Test extends AbstractEmbeddedMavenMojoTest {

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
			verifier = executeMojo("bug850", "compile", false);
		}
		return verifier;
	}

	private static final String EXPECTED_AGENT1 = multilineString(
			"package io.sarl.maven.bug850;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.maven.bug850.CarRequestPercept;",
			"import java.util.Collection;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class Agent1 extends Agent {",
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
			"@Override",
			"public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"super.$getSupportedEvents(toBeFilled);",
			"toBeFilled.add(CarRequestPercept.class);",
			"}",
			"@SyntheticMember",
			"@Override",
			"public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
			"super.$evaluateBehaviorGuards(event, callbacks);",
			"if (event instanceof CarRequestPercept) {",
			"final CarRequestPercept occurrence = (CarRequestPercept) event;",
			"$guardEvaluator$CarRequestPercept(occurrence, callbacks);",
			"}",
			"}",
			"@SyntheticMember",
			"public Agent1(final UUID arg0, final UUID arg1) {",
			"super(arg0, arg1);",
			"}",
			"@SyntheticMember",
			"@Inject",
			"public Agent1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"}");

	@Test
	@DisplayName("Test Agent1")
	public void compile01() throws Exception {
		Verifier verifier = doCompile();
		verifier.assertErrorFreeLog();

		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "bug850", "Agent1.java");
		assertNotNull(path);
		verifier.assertFilePresent(path);
		assertEqualsExceptNewLines(EXPECTED_AGENT1, verifier.readFile(path));
	}

	private static final String EXPECTED_AGENT2 = multilineString(
			"package io.sarl.maven.bug850;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.maven.bug850.CarRequestPercept;",
			"import io.sarl.maven.bug850.Direction;",
			"import java.util.Collection;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class Agent2 extends Agent {",
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
			"@Override",
			"public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"super.$getSupportedEvents(toBeFilled);",
			"toBeFilled.add(CarRequestPercept.class);",
			"}",
			"@SyntheticMember",
			"@Override",
			"public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
			"super.$evaluateBehaviorGuards(event, callbacks);",
			"if (event instanceof CarRequestPercept) {",
			"final CarRequestPercept occurrence = (CarRequestPercept) event;",
			"$guardEvaluator$CarRequestPercept(occurrence, callbacks);",
			"}",
			"}",
			"@SyntheticMember",
			"public Agent2(final UUID arg0, final UUID arg1) {",
			"super(arg0, arg1);",
			"}",
			"@SyntheticMember",
			"@Inject",
			"public Agent2(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"}");

	@Test
	@DisplayName("Test Agent2")
	public void compile02() throws Exception {
		Verifier verifier = doCompile();
		verifier.assertErrorFreeLog();

		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "bug850", "Agent2.java");
		assertNotNull(path);
		verifier.assertFilePresent(path);
		assertEqualsExceptNewLines(EXPECTED_AGENT2, verifier.readFile(path));
	}


	private static final String EXPECTED_AGENT3 = multilineString(
			"package io.sarl.maven.bug850;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.maven.bug850.CarRequestPercept;",
			"import java.util.Collection;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class Agent3 extends Agent {",
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
			"@Override",
			"public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"super.$getSupportedEvents(toBeFilled);",
			"toBeFilled.add(CarRequestPercept.class);",
			"}",
			"@SyntheticMember",
			"@Override",
			"public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
			"super.$evaluateBehaviorGuards(event, callbacks);",
			"if (event instanceof CarRequestPercept) {",
			"final CarRequestPercept occurrence = (CarRequestPercept) event;",
			"$guardEvaluator$CarRequestPercept(occurrence, callbacks);",
			"}",
			"}",
			"@SyntheticMember",
			"public Agent3(final UUID arg0, final UUID arg1) {",
			"super(arg0, arg1);",
			"}",
			"@SyntheticMember",
			"@Inject",
			"public Agent3(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"}");

	@Test
	@DisplayName("Test Agent3")
	public void compile03() throws Exception {
		Verifier verifier = doCompile();
		verifier.assertErrorFreeLog();

		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "bug850", "Agent3.java");
		assertNotNull(path);
		verifier.assertFilePresent(path);
		assertEqualsExceptNewLines(EXPECTED_AGENT3, verifier.readFile(path));
	}

	private static final String EXPECTED_AGENT4 = multilineString(
			"package io.sarl.maven.bug850;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.maven.bug850.CarRequestPercept;",
			"import java.util.Collection;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class Agent4 extends Agent {",
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
			"@Override",
			"public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"super.$getSupportedEvents(toBeFilled);",
			"toBeFilled.add(CarRequestPercept.class);",
			"}",
			"@SyntheticMember",
			"@Override",
			"public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
			"super.$evaluateBehaviorGuards(event, callbacks);",
			"if (event instanceof CarRequestPercept) {",
			"final CarRequestPercept occurrence = (CarRequestPercept) event;",
			"$guardEvaluator$CarRequestPercept(occurrence, callbacks);",
			"}",
			"}",
			"@SyntheticMember",
			"public Agent4(final UUID arg0, final UUID arg1) {",
			"super(arg0, arg1);",
			"}",
			"@SyntheticMember",
			"@Inject",
			"public Agent4(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"super(arg0, arg1, arg2);",
			"}",
			"}");

	@Test
	@DisplayName("Test Agent4")
	public void compile04() throws Exception {
		Verifier verifier = doCompile();
		verifier.assertErrorFreeLog();

		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "bug850", "Agent4.java");
		assertNotNull(path);
		verifier.assertFilePresent(path);
		assertEqualsExceptNewLines(EXPECTED_AGENT4, verifier.readFile(path));
	}

}

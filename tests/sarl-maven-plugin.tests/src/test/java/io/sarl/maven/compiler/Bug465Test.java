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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static io.sarl.tests.api.tools.TestAssertions.*;

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
@DisplayName("Bug #465")
@Tag("maven")
@Tag("mvn-run")
public class Bug465Test extends AbstractMojoTest {

	@Test
	public void compile() throws Exception {
		Verifier verifier = executeMojo("bug465", "compile");
		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "compiler", "tests", "MyAgent.java");
		assertNotNull(path);
		verifier.assertFilePresent(path.toString());
		assertEqualsExceptNewLines(multilineString(
				"package io.sarl.maven.compiler.tests;",
				"import io.sarl.core.DefaultContextInteractions;",
				"import io.sarl.core.Initialize;",
				"import io.sarl.core.Schedules;",
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.AtomicSkillReference;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.Event;",
				"import io.sarl.maven.compiler.tests.Hello;",
				"import java.util.Collection;",
				"import java.util.Set;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgent extends Agent {",
				"private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
				"Schedules _$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER = this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER();",
				"Schedules _$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER_1 = this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER();",
				"final Procedure1<Agent> _function = (Agent it) -> {",
				"DefaultContextInteractions _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
				"Hello _hello = new Hello();",
				"_$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(_hello);",
				"};",
				"_$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER.every(_$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER_1.task(\"discovery-task\"), 1000, _function);",
				"}",
				"@Extension",
				"@ImportedCapacityFeature(DefaultContextInteractions.class)",
				"@SyntheticMember",
				"private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS;",
				"@SyntheticMember",
				"@Pure",
				"private DefaultContextInteractions $CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {",
				"if (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) {",
				"this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = $getSkill(DefaultContextInteractions.class);",
				"}",
				"return $castSkill(DefaultContextInteractions.class, this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS);",
				"}",
				"@Extension",
				"@ImportedCapacityFeature(Schedules.class)",
				"@SyntheticMember",
				"private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_CORE_SCHEDULES;",
				"@SyntheticMember",
				"@Pure",
				"private Schedules $CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER() {",
				"if (this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES == null || this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES.get() == null) {",
				"this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = $getSkill(Schedules.class);",
				"}",
				"return $castSkill(Schedules.class, this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES);",
				"}",
				"@SyntheticMember",
				"@PerceptGuardEvaluator",
				"private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"assert occurrence != null;",
				"assert ___SARLlocal_runnableCollection != null;",
				"___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
				"}",
				"@SyntheticMember",
				"@Override",
				"public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
				"super.$getSupportedEvents(toBeFilled);",
				"toBeFilled.add(Initialize.class);",
				"}",
				"@SyntheticMember",
				"@Override",
				"public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
				"super.$evaluateBehaviorGuards(event, callbacks);",
				"if (event instanceof Initialize) {",
				"final Initialize occurrence = (Initialize) event;",
				"$guardEvaluator$Initialize(occurrence, callbacks);",
				"}",
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
				"}"), readFile(verifier, path));
	}

}

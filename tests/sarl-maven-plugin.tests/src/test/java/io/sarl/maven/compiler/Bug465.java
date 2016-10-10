/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import static org.junit.Assert.*;

import java.io.File;
import java.nio.file.FileSystems;
import java.nio.file.Path;

import org.apache.maven.it.Verifier;
import org.apache.maven.shared.utils.io.FileUtils;
import org.junit.Test;

import io.sarl.lang.SARLVersion;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug465 extends AbstractMojoTest {

	@Test
	public void compile() throws Exception {
		Verifier verifier = executeMojo("bug465", "compile");
		Path path = FileSystems.getDefault().getPath(
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "maven", "compiler", "tests", "MyAgent.java");
		assertNotNull(path);
		verifier.assertFilePresent(path.toString());
		File file = new File(verifier.getBasedir(), path.toString());
		String fileContent = FileUtils.fileRead(file);
		assertEquals(multilineString(
				"package io.sarl.maven.compiler.tests;",
				"",
				"import io.sarl.core.AgentTask;",
				"import io.sarl.core.DefaultContextInteractions;",
				"import io.sarl.core.Initialize;",
				"import io.sarl.core.Schedules;",
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.core.Capacity;",
				"import io.sarl.lang.core.Skill;",
				"import io.sarl.maven.compiler.tests.Hello;",
				"import java.util.Collection;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgent extends Agent {",
				"  @SyntheticMember",
				"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
				"    Schedules _$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER = this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES == null ? (this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = getSkill(Schedules.class)) : this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES;",
				"    Schedules _$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER_1 = this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES == null ? (this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = getSkill(Schedules.class)) : this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES;",
				"    AgentTask _task = _$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER_1.task(\"discovery-task\");",
				"    final Procedure1<Agent> _function = (Agent it) -> {",
				"      DefaultContextInteractions _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null ? (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = getSkill(DefaultContextInteractions.class)) : this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS;",
				"      Hello _hello = new Hello();",
				"      _$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(_hello);",
				"    };",
				"    _$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER.every(_task, 1000, _function);",
				"  }",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(DefaultContextInteractions.class)",
				"  @SyntheticMember",
				"  private transient DefaultContextInteractions $CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS;",
				"  ",
				"  @Inline(value = \"$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null ? (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = getSkill(DefaultContextInteractions.class)) : this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS\")",
				"  @SyntheticMember",
				"  @Pure",
				"  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {",
				"    if (this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS == null) {",
				"      this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = getSkill(DefaultContextInteractions.class);",
				"    }",
				"    return this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS;",
				"  }",
				"  ",  
				"  @Extension",
				"  @ImportedCapacityFeature(Schedules.class)",
				"  @SyntheticMember",
				"  private transient Schedules $CAPACITY_USE$IO_SARL_CORE_SCHEDULES;",
				"  ",  
				"  @Inline(value = \"$CAPACITY_USE$IO_SARL_CORE_SCHEDULES == null ? (this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = getSkill(Schedules.class)) : this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES\")",
				"  @SyntheticMember",
				"  @Pure",
				"  private Schedules $CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER() {",
				"    if (this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES == null) {",
				"      this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = getSkill(Schedules.class);",
				"    }",
				"    return this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES;",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  @PerceptGuardEvaluator",
				"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
				"    assert occurrence != null;",
				"    assert ___SARLlocal_runnableCollection != null;",
				"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
				"  }",
				"  ",  
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @SyntheticMember",
				"  public MyAgent(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  @Override",
				"  protected <S extends Skill> S $setSkill(final S skill, final Class<? extends Capacity>... capacities) {",
				"    this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = null;",
				"    this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = null;",
				"    return super.$setSkill(skill, capacities);",
				"  }",
				"  ",  
				"  @SyntheticMember",
				"  @Override",
				"  protected <S extends Capacity> S clearSkill(final Class<S> capacity) {",
				"    this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = null;",
				"    this.$CAPACITY_USE$IO_SARL_CORE_DEFAULTCONTEXTINTERACTIONS = null;",
				"    return super.clearSkill(capacity);",
				"  }",
				"}"), fileContent);
	}

}

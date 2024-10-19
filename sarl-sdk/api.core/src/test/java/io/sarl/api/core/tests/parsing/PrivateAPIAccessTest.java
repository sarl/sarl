/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.api.core.tests.parsing;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: @PrivateAPIAccess")
@Tag("core")
public class PrivateAPIAccessTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void ambigousPrivateAPI_01() throws Exception {
		String source = multilineString(
				"import io.sarl.lang.core.annotation.PrivateAPI",
				"import io.sarl.api.core.AgentTask",
				"import io.sarl.api.core.Schedules",
				"",
				"agent Accessor {",
				"  uses Schedules",
				"  var t : AgentTask",
				"  def action : void {",
				"    this.t.name = \"hello\"",
				"  }",
				"}",
				"");
		final String expectedAccessor = multilineString(
				"import io.sarl.api.core.AgentTask;",
				"import io.sarl.api.core.Schedules;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.AtomicSkillReference;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import io.sarl.lang.core.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@XbaseGenerated",
				"@SuppressWarnings(\"all\")",
				"public class Accessor extends Agent {",
				"  private AgentTask t;",
				"  ",
				"  protected void action() {",
				"    Schedules _$CAPACITY_USE$IO_SARL_API_CORE_SCHEDULES$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_SCHEDULES$CALLER();",
				"    _$CAPACITY_USE$IO_SARL_API_CORE_SCHEDULES$CALLER.setName(this.t, \"hello\");",
				"  }",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(Schedules.class)",
				"  @SyntheticMember",
				"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_SCHEDULES;",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private Schedules $CAPACITY_USE$IO_SARL_API_CORE_SCHEDULES$CALLER() {",
				"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_SCHEDULES == null || this.$CAPACITY_USE$IO_SARL_API_CORE_SCHEDULES.get() == null) {",
				"      this.$CAPACITY_USE$IO_SARL_API_CORE_SCHEDULES = $getSkill(Schedules.class);",
				"    }",
				"    return $castSkill(Schedules.class, this.$CAPACITY_USE$IO_SARL_API_CORE_SCHEDULES);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public Accessor(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Inject",
				"  public Accessor(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
				"    super(arg0, arg1, arg2);",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedAccessor, r.getGeneratedCode("Accessor"));
		});
	}

	@Test
	@Tag("sarlValidation")
	public void ambigousPrivateAPI_02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.core.annotation.PrivateAPI",
				"import io.sarl.api.core.AgentTask",
				"import io.sarl.api.core.Schedules",
				"",
				"agent Accessor {",
				"  uses Schedules",
				"  var t : AgentTask",
				"  def action : void {",
				"    this.t.name = \"hello\"",
				"  }",
				"}",
				""));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

}

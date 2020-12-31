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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.bugs.to00399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #23")
@SuppressWarnings("all")
@Tag("core")
public class Bug23Test extends AbstractSarlTest {

	private String snippet = multilineString(
			"import java.util.UUID\n",
			"event AgentSpawned {",
			"  var agentID : UUID",
			"}",
			"event MyAgentSpawned extends AgentSpawned{",
			"  var titi : UUID",
			"}",
			"agent MyAgent {",
			"  on MyAgentSpawned {",
			"    System.out.println(occurrence.titi)",
			"    System.out.println(occurrence.agentID)",
			"  }",
			"}"
			);


	@Test
	@Tag("sarlValidation")
	public void bug23() throws Exception {
		SarlScript mas = file(getParseHelper(), snippet);
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void myAgentSpawnedCompile() throws Exception {
		final String expectedMyAgentSpawned = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Address;",
				"import java.util.Objects;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgentSpawned extends AgentSpawned {",
				"  public UUID titi;",
				"  ",
				"  @SyntheticMember",
				"  public MyAgentSpawned() {",
				"    super();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public MyAgentSpawned(final Address arg0) {",
				"    super(arg0);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    MyAgentSpawned other = (MyAgentSpawned) obj;",
				"    if (!Objects.equals(this.titi, other.titi))",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Objects.hashCode(this.titi);",
				"    return result;",
				"  }",
				"  ",
				"  /**",
				"   * Returns a String representation of the MyAgentSpawned event's attributes only.",
				"   */",
				"  @SyntheticMember",
				"  @Pure",
				"  protected void toString(final ToStringBuilder builder) {",
				"    super.toString(builder);",
				"    builder.add(\"titi\", this.titi);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  private static final long serialVersionUID = -201217093L;",
				"}",
				""
				);

		getCompileHelper().compile(snippet, (r) -> assertEquals(expectedMyAgentSpawned,r.getGeneratedCode("MyAgentSpawned")));
	}

}

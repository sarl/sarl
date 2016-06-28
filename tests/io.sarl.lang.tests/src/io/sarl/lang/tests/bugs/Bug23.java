/*
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.lang.tests.bugs;

import static org.junit.Assert.assertEquals;

import com.google.inject.Inject;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug23 extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

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
	public void bug23() throws Exception {
		SarlScript mas = file(snippet);
		validate(mas).assertNoErrors();
	}

	@Test
	public void myAgentSpawnedCompile() throws Exception {
		final String expectedMyAgentSpawned = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Address;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class MyAgentSpawned extends AgentSpawned {",
				"  public UUID titi;",
				"  ",
				"  /**",
				"   * Construct an event. The source of the event is unknown.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public MyAgentSpawned() {",
				"    super();",
				"  }",
				"  ",
				"  /**",
				"   * Construct an event.",
				"   * @param source - address of the agent that is emitting this event.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public MyAgentSpawned(final Address source) {",
				"    super(source);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    MyAgentSpawned other = (MyAgentSpawned) obj;",
				"    if (this.titi == null) {",
				"      if (other.titi != null)",
				"        return false;",
				"    } else if (!this.titi.equals(other.titi))",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public int hashCode() {",
				"    final int prime = 31;",
				"    int result = super.hashCode();",
				"    result = prime * result + ((this.titi== null) ? 0 : this.titi.hashCode());",
				"    return result;",
				"  }",
				"  ",
				"  /**",
				"   * Returns a String representation of the MyAgentSpawned event's attributes only.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  @Pure",
				"  protected String attributesToString() {",
				"    StringBuilder result = new StringBuilder(super.attributesToString());",
				"    result.append(\"titi  = \").append(this.titi);",
				"    return result.toString();",
				"  }",
				"  ",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  private final static long serialVersionUID = -201217093L;",
				"}",
				""
				);

		this.compiler.compile(snippet, (r) -> assertEquals(expectedMyAgentSpawned,r.getGeneratedCode("MyAgentSpawned")));
	}

}

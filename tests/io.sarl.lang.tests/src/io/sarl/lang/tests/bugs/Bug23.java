/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug23 extends AbstractSarlTest {

	@Inject
	private ParseHelper<XtendFile> parser;

	@Inject
	private ValidationTestHelper validator;

	@Inject
	private CompilationTestHelper compiler;

	private CharSequence snippet = multilineString(
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
		XtendFile mas = this.parser.parse(snippet);
		this.validator.assertNoErrors(mas);
	}

	@Test
	public void myAgentSpawnedCompile() throws Exception {
		final String expectedMyAgentSpawned = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Address;",
				"import java.util.UUID;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class MyAgentSpawned extends AgentSpawned {",
				"  public UUID titi;",
				"  ",
				"  /**",
				"   * Construct an event. The source of the event is unknown.",
				"   */",
				"  @Generated",
				"  public MyAgentSpawned() {",
				"    super();",
				"  }",
				"  ",
				"  /**",
				"   * Construct an event.",
				"   * @param source - address of the agent that is emitting this event.",
				"   */",
				"  @Generated",
				"  public MyAgentSpawned(final Address source) {",
				"    super(source);",
				"  }",
				"  ",
				"  @Override",
				"  @Generated",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    if (!super.equals(obj))",
				"      return false;",
				"    MyAgentSpawned other = (MyAgentSpawned) obj;",
				"    if (this.titi == null) {",
				"      if (other.titi != null)",
				"        return false;",
				"    } else if (!this.titi.equals(other.titi))",
				"      return false;",
				"    return true;",
				"  }",
				"  ",
				"  @Override",
				"  @Generated",
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
				"  @Generated",
				"  protected String attributesToString() {",
				"    StringBuilder result = new StringBuilder(super.attributesToString());",
				"    result.append(\"titi  = \").append(this.titi);",
				"    return result.toString();",
				"  }",
				"  ",
				"  @Generated",
				"  private final static long serialVersionUID = -267285920L;",
				"}",
				""
				);

		this.compiler.compile(snippet, new IAcceptor<CompilationTestHelper.Result>() {
			@Override
			public void accept(Result r) {
				assertEquals(expectedMyAgentSpawned,r.getGeneratedCode("MyAgentSpawned"));
			}
		});
	}

}

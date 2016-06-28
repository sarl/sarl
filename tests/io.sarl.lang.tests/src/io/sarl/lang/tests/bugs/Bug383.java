/*
 * $Id$
 * 
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
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	Bug383.ParserTest.class,
	Bug383.CompilerTest.class,
})
@SuppressWarnings("all")
public class Bug383 {

	protected static String snippet = AbstractSarlTest.multilineString(
			"agent A1 {",
			"  /** Testing documentation generator.",
			"   * @param type the type.",
			"   * @param defaultValue the default value.",
			"   * @return the value.",
			"   */",
			"  def getInstance(type : String, defaultValue : String = null) : String {",
			"    return null",
			"  }",
			"}");

	public static class ParserTest extends AbstractSarlTest {

		@Test
		public void parsing() throws Exception {
			SarlScript mas = file(snippet);
			validate(mas).assertNoErrors();
		}

	}

	public static class CompilerTest extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;
		
		@Test
		public void compilation() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
					"import java.util.UUID;",
					"import javax.annotation.Generated;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  /**",
					"   * Testing documentation generator.",
					"   * @param type the type.",
					"   * @param defaultValue the default value.",
					"   * @return the value.",
					"   */",
					"  @DefaultValueSource",
					"  @Pure",
					"  protected String getInstance(final String type, @DefaultValue(\"A1#GETINSTANCE_0\") final String defaultValue) {",
					"    return null;",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter defaultValue",
					"   */",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @SarlSourceCode(\" null\")",
					"  private final static String $DEFAULT_VALUE$GETINSTANCE_0 = null;",
					"  ",
					"  /**",
					"   * Testing documentation generator.",
					"   * @param type the type.",
					"   * @optionalparam defaultValue the default value.",
					"   * @return the value.",
					"   */",
					"  @DefaultValueUse(\"java.lang.String,java.lang.String\")",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @Pure",
					"  protected final String getInstance(final String type) {",
					"    return getInstance(type, $DEFAULT_VALUE$GETINSTANCE_0);",
					"  }",
					"  ",
					"  /**",
					"   * Construct an agent.",
					"   * @param builtinCapacityProvider - provider of the built-in capacities.",
					"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
					"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
					"   */",
					"  @Inject",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
					"    super(builtinCapacityProvider, parentID, agentID);",
					"  }",
					"}",
					"");

			this.compiler.compile(snippet, 
					(r) -> assertEquals(expected, r.getGeneratedCode("A1")));
		}

	}

}

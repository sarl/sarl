/*
 * Copyright (C) 2014-2015 the original authors or authors.
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
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.compilation.aop.SkillCompilerTest;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	Bug312.CapacityTest.class,
	//Bug312.ClassTest.class,
})
@SuppressWarnings("all")
public class Bug312 {

	public static class CapacityTest extends AbstractSarlTest {

		private String snippet = multilineString(
				"class Vector2f {}",
				"class Vector2i {}",
				"capacity C1 {",
				"    def move(direction1 : Vector2f, changeHeading1 : boolean = false)",
				"    def move(direction2 : Vector2i, changeHeading2 : boolean = false)",
				"}");

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void bug312() throws Exception {
			SarlScript mas = file(snippet);
			validate(mas).assertNoErrors();
		}

		@Test
		public void testCompiler() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.core.Capacity;",
					"import javax.annotation.Generated;",
					"",
					"@SuppressWarnings(\"all\")",
					"public interface C1 extends Capacity {",
					"  @DefaultValueSource",
					"  public abstract void move(final Vector2f direction1, @DefaultValue(\"C1#MOVE_0\") final boolean changeHeading1);",
					"  ",
					"  /**",
					"   * Default value for the parameter changeHeading1",
					"   */",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @SarlSourceCode(\" false\")",
					"  public final static boolean ___FORMAL_PARAMETER_DEFAULT_VALUE_MOVE_0 = false;",
					"  ",
					"  @DefaultValueSource",
					"  public abstract void move(final Vector2i direction2, @DefaultValue(\"C1#MOVE_1\") final boolean changeHeading2);",
					"  ",
					"  /**",
					"   * Default value for the parameter changeHeading2",
					"   */",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  @SarlSourceCode(\" false\")",
					"  public final static boolean ___FORMAL_PARAMETER_DEFAULT_VALUE_MOVE_1 = false;",
					"  ",
					"  @DefaultValueUse(\"Vector2f,boolean\")",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public abstract void move(final Vector2f direction1);",
					"  ",
					"  @DefaultValueUse(\"Vector2i,boolean\")",
					"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
					"  public abstract void move(final Vector2i direction2);",
					"}",
					"");
			this.compiler.compile(this.snippet, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expected, r.getGeneratedCode("C1"));
				}
			});
		}

	}

}

/*
 * Copyright (C) 2014-2018 the original authors or authors.
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
package io.sarl.lang.tests.general.compilation.oop;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.SyntaxIssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
@RunWith(Suite.class)
@SuiteClasses({
	CastOperatorOverriding.StringType.class,
})
@SuppressWarnings("all")
public class CastOperatorOverriding {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	public static class StringType extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		private static final String OBJECT_TOSTRING_SARL = multilineString(
				"package io.sarl.lang.tests.castoperatoroverriding",
				"class A1 {",
				"}",
				"class A2 {",
				"  def fct(x : A1) : String {",
				"    var y = fct(null)",
				"    return x as String",
				"  }",
				"}"
				);

		private static final String OBJECT_TOSTRING_JAVA = multilineString(
				"package io.sarl.lang.tests.castoperatoroverriding;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.tests.castoperatoroverriding.A1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Pure",
				"  public String fct(final A1 x) {",
				"    throw new Error(\"Unresolved compilation problems:\"",
				"      + \"\\nCannot cast from A1 to String\");",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@Test
		public void parseObjectToString() throws Exception {
			SarlScript mas = file(OBJECT_TOSTRING_SARL);
			validate(mas).assertNoErrors();
		}

		@Test
		public void compileObjectToString() throws Exception {
			this.compiler.compile(OBJECT_TOSTRING_SARL, (r) -> {
				assertEquals(OBJECT_TOSTRING_JAVA, r.getGeneratedCode("io.sarl.lang.tests.castoperatoroverriding.A2"));
			});
		}

		private static final String LOCAL_TOSTRING_SARL = multilineString(
				"package io.sarl.lang.tests.castoperatoroverriding",
				"class A1 {",
				"}",
				"class A2 {",
				"  def fct(x : A1) : String {",
				"    var y = fct(null)",
				"    return x as String",
				"  }",
				"  def toString : String {",
				"  }",
				"  def toString(x : int) : String {",
				"  }",
				"  def toString(x : int, y : float) : String {",
				"  }",
				"}"
				);

		private static final String LOCAL_TOSTRING_JAVA = multilineString(
				"package io.sarl.lang.tests.castoperatoroverriding;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.tests.castoperatoroverriding.A1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Pure",
				"  public String fct(final A1 x) {",
				"    throw new Error(\"Unresolved compilation problems:\"",
				"      + \"\\nCannot cast from A1 to String\");",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@Test
		public void parseLocalToString() throws Exception {
			SarlScript mas = file(LOCAL_TOSTRING_SARL);
			validate(mas).assertNoErrors();
		}

		@Test
		public void compileLocalToString() throws Exception {
			this.compiler.compile(LOCAL_TOSTRING_SARL, (r) -> {
				assertEquals(LOCAL_TOSTRING_JAVA, r.getGeneratedCode("io.sarl.lang.tests.castoperatoroverriding.A2"));
			});
		}

		private static final String A1_TOSTRING_SARL = multilineString(
				"package io.sarl.lang.tests.castoperatoroverriding",
				"class A1 {",
				"  def toString : String {",
				"  }",
				"}",
				"class A2 {",
				"  def fct(x : A1) : String {",
				"    var y = fct(null)",
				"    return x as String",
				"  }",
				"}"
				);

		private static final String A1_TOSTRING_JAVA = multilineString(
				"package io.sarl.lang.tests.castoperatoroverriding;",
				"",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.tests.castoperatoroverriding.A1;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class A2 {",
				"  @Pure",
				"  public String fct(final A1 x) {",
				"    throw new Error(\"Unresolved compilation problems:\"",
				"      + \"\\nCannot cast from A1 to String\");",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A2() {",
				"    super();",
				"  }",
				"}",
				"");

		@Test
		public void parseA1ToString() throws Exception {
			SarlScript mas = file(A1_TOSTRING_SARL);
			validate(mas).assertNoErrors();
		}

		@Test
		public void compileA1ToString() throws Exception {
			this.compiler.compile(A1_TOSTRING_SARL, (r) -> {
				assertEquals(A1_TOSTRING_JAVA, r.getGeneratedCode("io.sarl.lang.tests.castoperatoroverriding.A2"));
			});
		}
		
		// local
		// static local
		// extension field
		// static import
		// extension import
		// definition into super type

	}

}

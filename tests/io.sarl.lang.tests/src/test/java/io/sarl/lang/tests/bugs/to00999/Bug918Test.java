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

package io.sarl.lang.tests.bugs.to00999;

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
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Add test on casting operator overriding.
 *
 * <p>https://github.com/sarl/sarl/issues/918
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/918"
 */
@DisplayName("Bug #918")
@SuppressWarnings("all")
@Tag("core")
public class Bug918Test extends AbstractSarlTest {

	private static final String SARL_CODE = multilineString(
			"package io.sarl.lang.tests.bug918",
			"class A {}",
			"class B extends A {}",
			"class C {}",
			"class D extends C {}",
			"agent X {",
			"    def toA : A { null }",
			"}",
			"agent Y extends X {",
			"    def toB : B { null }",
			"    def toD : D { null }",
			"    def test0 : A {",
			"        this as A",
			"    }",
			"    def test1 : A {",
			"        this as B",
			"    }",
			"    def test2 : C {",
			"        this as C",
			"    }",
			"    def test3 : C {",
			"        this as D",
			"    }",
			"}");

	private static final String X_JAVA_CODE = multilineString(
			"package io.sarl.lang.tests.bug918;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class X extends Agent {",
			"  @Pure",
			"  protected A toA() {",
			"    return null;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public X(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	private static final String Y_JAVA_CODE = multilineString(
			"package io.sarl.lang.tests.bug918;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class Y extends X {",
			"  @Pure",
			"  protected B toB() {",
			"    return null;",
			"  }",
			"  ",
			"  @Pure",
			"  protected D toD() {",
			"    return null;",
			"  }",
			"  ",
			"  @Pure",
			"  protected A test0() {",
			"    return (this == null ? null : this.toA());",
			"  }",
			"  ",
			"  @Pure",
			"  protected A test1() {",
			"    return (this == null ? null : this.toB());",
			"  }",
			"  ",
			"  @Pure",
			"  protected C test2() {",
			"    return (this == null ? null : this.toD());",
			"  }",
			"  ",
			"  @Pure",
			"  protected C test3() {",
			"    return (this == null ? null : this.toD());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Y(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public Y(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compilingX() throws Exception {
		getCompileHelper().compile(SARL_CODE, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug918.X");
			assertEquals(X_JAVA_CODE, actual);
		});
	}

	@Test
	@Tag("compileToJava")
	public void compilingY() throws Exception {
		getCompileHelper().compile(SARL_CODE, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug918.Y");
			assertEquals(Y_JAVA_CODE, actual);
		});
	}

}

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
package io.sarl.lang.tests.general.compilation.general;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: @Injectable")
@Tag("core")
@Tag("compileToJava")
public class InjectableAnnotationTest extends AbstractSarlTest {

	@Test
	@DisplayName("@Inject to field (javax)")
	public void field_javax() throws Exception {
		String source = multilineString(
				"import javax.inject.Inject",
				"class C1 {",
				"  @Inject",
				"  var theField : Object",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.Injectable;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@Injectable",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inject",
				"  private Object theField;",
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
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	@DisplayName("@Inject to field (google)")
	public void field_google() throws Exception {
		String source = multilineString(
				"import com.google.inject.Inject",
				"class C1 {",
				"  @Inject",
				"  var theField : Object",
				"}"
				);
		final String expected = multilineString(
				"import com.google.inject.Inject;",
				"import io.sarl.lang.annotation.Injectable;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@Injectable",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inject",
				"  private Object theField;",
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
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	@DisplayName("@Inject to contructor (javax)")
	public void contructor_javax() throws Exception {
		String source = multilineString(
				"import javax.inject.Inject",
				"class C1 {",
				"  @Inject",
				"  new { }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.Injectable;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@Injectable",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inject",
				"  public C1() {",
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	@DisplayName("@Inject to contructor (google)")
	public void contructor_google() throws Exception {
		String source = multilineString(
				"import com.google.inject.Inject",
				"class C1 {",
				"  @Inject",
				"  new { }",
				"}"
				);
		final String expected = multilineString(
				"import com.google.inject.Inject;",
				"import io.sarl.lang.annotation.Injectable;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@Injectable",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inject",
				"  public C1() {",
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	@DisplayName("@Inject to method (javax)")
	public void method_javax() throws Exception {
		String source = multilineString(
				"import javax.inject.Inject",
				"class C1 {",
				"  @Inject",
				"  def mth { }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.Injectable;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@Injectable",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inject",
				"  public void mth() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	@DisplayName("@Inject to method (google)")
	public void method_google() throws Exception {
		String source = multilineString(
				"import com.google.inject.Inject",
				"class C1 {",
				"  @Inject",
				"  def mth { }",
				"}"
				);
		final String expected = multilineString(
				"import com.google.inject.Inject;",
				"import io.sarl.lang.annotation.Injectable;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@Injectable",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  @Inject",
				"  public void mth() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	@DisplayName("@Injectable to super type")
	public void supertype() throws Exception {
		String source = multilineString(
				"import javax.inject.Inject",
				"class C0 {",
				"  @Inject",
				"  def mth { }",
				"}",
				"class C1 extends C0{",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.Injectable;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@Injectable",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			final String actual = it.getGeneratedCode("C1");
			assertEquals(expected, actual);
		});
	}

}

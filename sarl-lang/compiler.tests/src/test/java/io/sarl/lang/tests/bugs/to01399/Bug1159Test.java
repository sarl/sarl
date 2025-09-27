/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.tests.bugs.to01399;

import static io.sarl.tests.api.tools.TestAssertions.assertEqualsExceptNewLines;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Automatic generation of constructors for the events.
 *
 * <p>https://github.com/sarl/sarl/issues/1159
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1159"
 */
@DisplayName("Bug #1159")
@SuppressWarnings("all")
@Tag("core")
public class Bug1159Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E {",
			"   var a : String",
			"   var b : int",
			"}");

	private static final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E extends Event {",
			"  public String a;",
			"",
			"  public int b;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(final int b) {",
			"    super();",
			"    this.b = b;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(final String a) {",
			"    super();",
			"    this.a = a;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(final String a, final int b) {",
			"    super();",
			"    this.a = a;",
			"    this.b = b;",
			"  }",
			"",
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
			"    E other = (E) obj;",
			"    if (!Objects.equals(this.a, other.a))",
			"      return false;",
			"    if (other.b != this.b)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.a);",
			"    result = prime * result + Integer.hashCode(this.b);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"a\", this.a);",
			"    builder.add(\"b\", this.b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 1783732581L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing var-s")
	@Tag("sarlParsing")
	public void parsingVars() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling var-s")
	@Tag("compileToJava")
	public void compilingVars() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E");
			assertEqualsExceptNewLines(JAVA_CODE_01, actual);
		});
	}

	private static final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E {",
			"   val a : String",
			"   val b : int",
			"}");

	private static final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructorMandatoryParameter;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E extends Event {",
			"  public final String a;",
			"",
			"  public final int b;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(@SyntheticEventConstructorMandatoryParameter final String a, @SyntheticEventConstructorMandatoryParameter final int b) {",
			"    super();",
			"    this.a = a;",
			"    this.b = b;",
			"  }",
			"",
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
			"    E other = (E) obj;",
			"    if (!Objects.equals(this.a, other.a))",
			"      return false;",
			"    if (other.b != this.b)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.a);",
			"    result = prime * result + Integer.hashCode(this.b);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"a\", this.a);",
			"    builder.add(\"b\", this.b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 1783732581L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing val-s")
	@Tag("sarlParsing")
	public void parsingVals() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling val-s")
	@Tag("compileToJava")
	public void compilingVals() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E");
			assertEqualsExceptNewLines(JAVA_CODE_02, actual);
		});
	}

	private static final String SARL_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"import java.util.Collection",
			"event E {",
			"   var a : Collection<String>",
			"   var b : int",
			"}");

	private static final String JAVA_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Collection;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E extends Event {",
			"  public Collection<String> a;",
			"",
			"  public int b;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(final int b) {",
			"    super();",
			"    this.b = b;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(final Collection<String> a) {",
			"    super();",
			"    this.a = a;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(final Collection<String> a, final int b) {",
			"    super();",
			"    this.a = a;",
			"    this.b = b;",
			"  }",
			"",
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
			"    E other = (E) obj;",
			"    if (other.b != this.b)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.b);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"a\", this.a);",
			"    builder.add(\"b\", this.b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 405005723L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing generic var-s")
	@Tag("sarlParsing")
	public void parsingGenericVars() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling generic var-s")
	@Tag("compileToJava")
	public void compilingGenericVars() throws Exception {
		getCompileHelper().compile(SARL_CODE_03, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E");
			assertEqualsExceptNewLines(JAVA_CODE_03, actual);
		});
	}

	private static final String SARL_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"import java.util.Collection",
			"event E {",
			"   val a : Collection<String>",
			"   val b : int",
			"}");

	private static final String JAVA_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructorMandatoryParameter;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Collection;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E extends Event {",
			"  public final Collection<String> a;",
			"",
			"  public final int b;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(@SyntheticEventConstructorMandatoryParameter final Collection<String> a, @SyntheticEventConstructorMandatoryParameter final int b) {",
			"    super();",
			"    this.a = a;",
			"    this.b = b;",
			"  }",
			"",
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
			"    E other = (E) obj;",
			"    if (other.b != this.b)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.b);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"a\", this.a);",
			"    builder.add(\"b\", this.b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 405005723L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing generic val-s")
	@Tag("sarlParsing")
	public void parsingGenericVals() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling generic val-s")
	@Tag("compileToJava")
	public void compilingGenericVals() throws Exception {
		getCompileHelper().compile(SARL_CODE_04, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E");
			assertEqualsExceptNewLines(JAVA_CODE_04, actual);
		});
	}

	private static final String SARL_CODE_05 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E {",
			"   var a : String",
			"   var b : int",
			"   new(x : int) {",
			"   }",
			"}");

	private static final String JAVA_CODE_05 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E extends Event {",
			"  public String a;",
			"",
			"  public int b;",
			"",
			"  public E(final int x) {",
			"  }",
			"",
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
			"    E other = (E) obj;",
			"    if (!Objects.equals(this.a, other.a))",
			"      return false;",
			"    if (other.b != this.b)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.a);",
			"    result = prime * result + Integer.hashCode(this.b);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"a\", this.a);",
			"    builder.add(\"b\", this.b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 1786973101L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing var-s w/ constructor")
	@Tag("sarlParsing")
	public void parsingVarsConstructor() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_05);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling var-s w/ constructor")
	@Tag("compileToJava")
	public void compilingVarsConstructor() throws Exception {
		getCompileHelper().compile(SARL_CODE_05, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E");
			assertEqualsExceptNewLines(JAVA_CODE_05, actual);
		});
	}

	private static final String SARL_CODE_06 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E {",
			"   val a : String",
			"   val b : int",
			"   new(x : int) {",
			"     this.b = x",
			"     this.a = \"abc\"",
			"   }",
			"}");

	private static final String JAVA_CODE_06 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E extends Event {",
			"  public final String a;",
			"",
			"  public final int b;",
			"",
			"  public E(final int x) {",
			"    this.b = x;",
			"    this.a = \"abc\";",
			"  }",
			"",
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
			"    E other = (E) obj;",
			"    if (!Objects.equals(this.a, other.a))",
			"      return false;",
			"    if (other.b != this.b)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.a);",
			"    result = prime * result + Integer.hashCode(this.b);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"a\", this.a);",
			"    builder.add(\"b\", this.b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 1786973101L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing val-s w/ constructor")
	@Tag("sarlParsing")
	public void parsingValsConstructor() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_06);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling val-s w/ constructor")
	@Tag("compileToJava")
	public void compilingValsConstructor() throws Exception {
		getCompileHelper().compile(SARL_CODE_06, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E");
			assertEqualsExceptNewLines(JAVA_CODE_06, actual);
		});
	}

	private static final String SARL_CODE_07 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E");

	private static final String JAVA_CODE_07 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E extends Event {",
			"  @SyntheticMember",
			"  public E() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  public E(final Address arg0) {",
			"    super(arg0);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 588368462L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing empty event")
	@Tag("sarlParsing")
	public void parsingEmptyEvent() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_07);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling empty event")
	@Tag("compileToJava")
	public void compilingEmptyEvent() throws Exception {
		getCompileHelper().compile(SARL_CODE_07, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E");
			assertEqualsExceptNewLines(JAVA_CODE_07, actual);
		});
	}

	private static final String SARL_CODE_08 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E<T1, T2 extends Number> {",
			"  var a : T1",
			"  var b : T2",
			"}");

	private static final String JAVA_CODE_08 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E<T1 extends Object, T2 extends Number> extends Event {",
			"  public T1 a;",
			"",
			"  public T2 b;",
			"",
			"  @SyntheticMember",
			"  @Pure",
			"  public static boolean $matchesTypeBounds(final E<?, ?> it, final Class<?>... bounds) {",
			"    if (bounds != null && bounds.length == 2) {",
			"      if ((it.a != null && !bounds[0].isInstance(it.a))) {",
			"        return false;",
			"      }",
			"      if ((it.b != null && !bounds[1].isInstance(it.b))) {",
			"        return false;",
			"      }",
			"      return true;",
			"    }",
			"    return false;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(final T2 b) {",
			"    super();",
			"    this.b = b;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(final T1 a) {",
			"    super();",
			"    this.a = a;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(final T1 a, final T2 b) {",
			"    super();",
			"    this.a = a;",
			"    this.b = b;",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"a\", this.a);",
			"    builder.add(\"b\", this.b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 588373964L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing event with generic")
	@Tag("sarlParsing")
	public void parsingGenericEvent() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_08);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling event with generic")
	@Tag("compileToJava")
	public void compilingGenericEvent() throws Exception {
		getCompileHelper().compile(SARL_CODE_08, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E");
			assertEqualsExceptNewLines(JAVA_CODE_08, actual);
		});
	}

	private static final String SARL_CODE_09 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E1",
			"event E2 extends E1");

	private static final String JAVA_CODE_09_E1 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E1 extends Event {",
			"  @SyntheticMember",
			"  public E1() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  public E1(final Address arg0) {",
			"    super(arg0);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 588368462L;",
			"}",
			"");

	private static final String JAVA_CODE_09_E2 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E2 extends E1 {",
			"  @SyntheticMember",
			"  public E2() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  public E2(final Address arg0) {",
			"    super(arg0);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 1108586578L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing inheritance w/o field")
	@Tag("sarlParsing")
	public void parsingInheritanceWoField() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_09);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling inheritance w/o field")
	@Tag("compileToJava")
	public void compilingInheritanceWoField() throws Exception {
		getCompileHelper().compile(SARL_CODE_09, (it) -> {
			final String actualE1 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E1");
			assertEqualsExceptNewLines(JAVA_CODE_09_E1, actualE1);
			final String actualE2 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E2");
			assertEqualsExceptNewLines(JAVA_CODE_09_E2, actualE2);
		});
	}

	private static final String SARL_CODE_10 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E1 {",
			"  var fieldE1 : String",
			"}",
			"event E2 extends E1");

	private static final String JAVA_CODE_10_E1 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E1 extends Event {",
			"  public String fieldE1;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1(final String fieldE1) {",
			"    super();",
			"    this.fieldE1 = fieldE1;",
			"  }",
			"",
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
			"    E1 other = (E1) obj;",
			"    if (!Objects.equals(this.fieldE1, other.fieldE1))",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.fieldE1);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E1 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"fieldE1\", this.fieldE1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 922375097L;",
			"}",
			"");

	private static final String JAVA_CODE_10_E2 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E2 extends E1 {",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final String fieldE1) {",
			"    super(fieldE1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 1108586578L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing inheritance w/ field in E1")
	@Tag("sarlParsing")
	public void parsingInheritanceFieldE1() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_10);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling inheritance w/ field in E1")
	@Tag("compileToJava")
	public void compilingInheritanceFieldE1() throws Exception {
		getCompileHelper().compile(SARL_CODE_10, (it) -> {
			final String actualE1 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E1");
			assertEqualsExceptNewLines(JAVA_CODE_10_E1, actualE1);
			final String actualE2 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E2");
			assertEqualsExceptNewLines(JAVA_CODE_10_E2, actualE2);
		});
	}

	private static final String SARL_CODE_11 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E1 {",
			"  var fieldE1 : String",
			"}",
			"event E2 extends E1 {",
			"  var fieldE2 : int",
			"}");

	private static final String JAVA_CODE_11_E1 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E1 extends Event {",
			"  public String fieldE1;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1(final String fieldE1) {",
			"    super();",
			"    this.fieldE1 = fieldE1;",
			"  }",
			"",
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
			"    E1 other = (E1) obj;",
			"    if (!Objects.equals(this.fieldE1, other.fieldE1))",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.fieldE1);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E1 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"fieldE1\", this.fieldE1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 922375097L;",
			"}",
			"");

	private static final String JAVA_CODE_11_E2 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E2 extends E1 {",
			"  public int fieldE2;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final int fieldE2) {",
			"    super();",
			"    this.fieldE2 = fieldE2;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final String fieldE1) {",
			"    super(fieldE1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final String fieldE1, final int fieldE2) {",
			"    super(fieldE1);",
			"    this.fieldE2 = fieldE2;",
			"  }",
			"",
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
			"    E2 other = (E2) obj;",
			"    if (other.fieldE2 != this.fieldE2)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.fieldE2);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E2 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"fieldE2\", this.fieldE2);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 247438152L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing inheritance w/ fields in E1 E2")
	@Tag("sarlParsing")
	public void parsingInheritanceFieldE1E2() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_11);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling inheritance w/ field in E1 E2")
	@Tag("compileToJava")
	public void compilingInheritanceFieldE1E2() throws Exception {
		getCompileHelper().compile(SARL_CODE_11, (it) -> {
			final String actualE1 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E1");
			assertEqualsExceptNewLines(JAVA_CODE_11_E1, actualE1);
			final String actualE2 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E2");
			assertEqualsExceptNewLines(JAVA_CODE_11_E2, actualE2);
		});
	}

	private static final String SARL_CODE_12 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E1 {",
			"  var fieldE1 : String",
			"  new(param : float) {",
			"  }",
			"}",
			"event E2 extends E1 {",
			"  var fieldE2 : int",
			"}");

	private static final String JAVA_CODE_12_E1 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E1 extends Event {",
			"  public String fieldE1;",
			"",
			"  public E1(final float param) {",
			"  }",
			"",
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
			"    E1 other = (E1) obj;",
			"    if (!Objects.equals(this.fieldE1, other.fieldE1))",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.fieldE1);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E1 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"fieldE1\", this.fieldE1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = -349271756L;",
			"}",
			"");

	private static final String JAVA_CODE_12_E2 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E2 extends E1 {",
			"  public int fieldE2;",
			"",
			"  @SyntheticMember",
			"  public E2(final float param) {",
			"    super(param);",
			"  }",
			"",
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
			"    E2 other = (E2) obj;",
			"    if (other.fieldE2 != this.fieldE2)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.fieldE2);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E2 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"fieldE2\", this.fieldE2);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 247438152L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing inheritance w/ manual constructor")
	@Tag("sarlParsing")
	public void parsingInheritanceManualConstructor() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_12);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling inheritance w/ manual constructor")
	@Tag("compileToJava")
	public void compilingInheritanceManualConstructor() throws Exception {
		getCompileHelper().compile(SARL_CODE_12, (it) -> {
			final String actualE1 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E1");
			assertEqualsExceptNewLines(JAVA_CODE_12_E1, actualE1);
			final String actualE2 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E2");
			assertEqualsExceptNewLines(JAVA_CODE_12_E2, actualE2);
		});
	}

	private static final String SARL_CODE_13 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E1",
			"event E2 extends E1 {",
			"  var field1 : int",
			"}");

	private static final String JAVA_CODE_13_E1 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E1 extends Event {",
			"  @SyntheticMember",
			"  public E1() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  public E1(final Address arg0) {",
			"    super(arg0);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 588368462L;",
			"}",
			"");

	private static final String JAVA_CODE_13_E2 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E2 extends E1 {",
			"  public int field1;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final int field1) {",
			"    super();",
			"    this.field1 = field1;",
			"  }",
			"",
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
			"    E2 other = (E2) obj;",
			"    if (other.field1 != this.field1)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.field1);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E2 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"field1\", this.field1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = -166017352L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing no inheritance var")
	@Tag("sarlParsing")
	public void parsingNoInheritanceVar() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_13);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling no inheritance var")
	@Tag("compileToJava")
	public void compilingNoInheritanceVar() throws Exception {
		getCompileHelper().compile(SARL_CODE_13, (it) -> {
			final String actualE1 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E1");
			assertEqualsExceptNewLines(JAVA_CODE_13_E1, actualE1);
			final String actualE2 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E2");
			assertEqualsExceptNewLines(JAVA_CODE_13_E2, actualE2);
		});
	}

	private static final String SARL_CODE_14 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E1",
			"event E2 extends E1 {",
			"  val field1 : int",
			"}");

	private static final String JAVA_CODE_14_E1 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E1 extends Event {",
			"  @SyntheticMember",
			"  public E1() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  public E1(final Address arg0) {",
			"    super(arg0);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 588368462L;",
			"}",
			"");

	private static final String JAVA_CODE_14_E2 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructorMandatoryParameter;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E2 extends E1 {",
			"  public final int field1;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(@SyntheticEventConstructorMandatoryParameter final int field1) {",
			"    super();",
			"    this.field1 = field1;",
			"  }",
			"",
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
			"    E2 other = (E2) obj;",
			"    if (other.field1 != this.field1)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.field1);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E2 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"field1\", this.field1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = -166017352L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing no inheritance val")
	@Tag("sarlParsing")
	public void parsingNoInheritanceVal() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_14);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling no inheritance val")
	@Tag("compileToJava")
	public void compilingNoInheritanceVal() throws Exception {
		getCompileHelper().compile(SARL_CODE_14, (it) -> {
			final String actualE1 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E1");
			assertEqualsExceptNewLines(JAVA_CODE_14_E1, actualE1);
			final String actualE2 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E2");
			assertEqualsExceptNewLines(JAVA_CODE_14_E2, actualE2);
		});
	}

	private static final String SARL_CODE_15 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E1 {",
			"  var field1 : int",
			"}",
			"event E2 extends E1");

	private static final String JAVA_CODE_15_E1 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E1 extends Event {",
			"  public int field1;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1(final int field1) {",
			"    super();",
			"    this.field1 = field1;",
			"  }",
			"",
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
			"    E1 other = (E1) obj;",
			"    if (other.field1 != this.field1)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.field1);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E1 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"field1\", this.field1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = -686235468L;",
			"}",
			"");

	private static final String JAVA_CODE_15_E2 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E2 extends E1 {",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final int field1) {",
			"    super(field1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 1108586578L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing inheritance no local")
	@Tag("sarlParsing")
	public void parsingInheritanceNoLocal() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_15);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling inheritance no local")
	@Tag("compileToJava")
	public void compilingNoLocal() throws Exception {
		getCompileHelper().compile(SARL_CODE_15, (it) -> {
			final String actualE1 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E1");
			assertEqualsExceptNewLines(JAVA_CODE_15_E1, actualE1);
			final String actualE2 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E2");
			assertEqualsExceptNewLines(JAVA_CODE_15_E2, actualE2);
		});
	}

	private static final String SARL_CODE_16 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E1 {",
			"  var fieldE1a : String",
			"  var fieldE1b : float",
			"}",
			"event E2 extends E1 {",
			"  var fieldE2 : int",
			"}");

	private static final String JAVA_CODE_16_E1 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E1 extends Event {",
			"  public String fieldE1a;",
			"",
			"  public float fieldE1b;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1(final float fieldE1b) {",
			"    super();",
			"    this.fieldE1b = fieldE1b;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1(final String fieldE1a) {",
			"    super();",
			"    this.fieldE1a = fieldE1a;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1(final String fieldE1a, final float fieldE1b) {",
			"    super();",
			"    this.fieldE1a = fieldE1a;",
			"    this.fieldE1b = fieldE1b;",
			"  }",
			"",
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
			"    E1 other = (E1) obj;",
			"    if (!Objects.equals(this.fieldE1a, other.fieldE1a))",
			"      return false;",
			"    if (Float.floatToIntBits(other.fieldE1b) != Float.floatToIntBits(this.fieldE1b))",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.fieldE1a);",
			"    result = prime * result + Float.hashCode(this.fieldE1b);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E1 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"fieldE1a\", this.fieldE1a);",
			"    builder.add(\"fieldE1b\", this.fieldE1b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 23084870L;",
			"}",
			"");

	private static final String JAVA_CODE_16_E2 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E2 extends E1 {",
			"  public int fieldE2;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final float fieldE1b) {",
			"    super(fieldE1b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final int fieldE2) {",
			"    super();",
			"    this.fieldE2 = fieldE2;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final String fieldE1a) {",
			"    super(fieldE1a);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final float fieldE1b, final int fieldE2) {",
			"    super(fieldE1b);",
			"    this.fieldE2 = fieldE2;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final String fieldE1a, final float fieldE1b) {",
			"    super(fieldE1a, fieldE1b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final String fieldE1a, final int fieldE2) {",
			"    super(fieldE1a);",
			"    this.fieldE2 = fieldE2;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final String fieldE1a, final float fieldE1b, final int fieldE2) {",
			"    super(fieldE1a, fieldE1b);",
			"    this.fieldE2 = fieldE2;",
			"  }",
			"",
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
			"    E2 other = (E2) obj;",
			"    if (other.fieldE2 != this.fieldE2)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.fieldE2);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E2 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"fieldE2\", this.fieldE2);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 247438152L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing inheritance w/ fields in 2*E1 E2")
	@Tag("sarlParsing")
	public void parsingInheritanceField2E1E2() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_16);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling inheritance w/ field in 2*E1 E2")
	@Tag("compileToJava")
	public void compilingInheritanceField2E1E2() throws Exception {
		getCompileHelper().compile(SARL_CODE_16, (it) -> {
			final String actualE1 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E1");
			assertEqualsExceptNewLines(JAVA_CODE_16_E1, actualE1);
			final String actualE2 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E2");
			assertEqualsExceptNewLines(JAVA_CODE_16_E2, actualE2);
		});
	}

	private static final String SARL_CODE_17 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event E1 {",
			"  var fieldE1a : String",
			"  val fieldE1b : float",
			"}",
			"event E2 extends E1 {",
			"  var fieldE2 : int",
			"}");

	private static final String JAVA_CODE_17_E1 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructorMandatoryParameter;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E1 extends Event {",
			"  public String fieldE1a;",
			"",
			"  public final float fieldE1b;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1(@SyntheticEventConstructorMandatoryParameter final float fieldE1b) {",
			"    super();",
			"    this.fieldE1b = fieldE1b;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E1(final String fieldE1a, @SyntheticEventConstructorMandatoryParameter final float fieldE1b) {",
			"    super();",
			"    this.fieldE1a = fieldE1a;",
			"    this.fieldE1b = fieldE1b;",
			"  }",
			"",
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
			"    E1 other = (E1) obj;",
			"    if (!Objects.equals(this.fieldE1a, other.fieldE1a))",
			"      return false;",
			"    if (Float.floatToIntBits(other.fieldE1b) != Float.floatToIntBits(this.fieldE1b))",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.fieldE1a);",
			"    result = prime * result + Float.hashCode(this.fieldE1b);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E1 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"fieldE1a\", this.fieldE1a);",
			"    builder.add(\"fieldE1b\", this.fieldE1b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 23084870L;",
			"}",
			"");

	private static final String JAVA_CODE_17_E2 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructorMandatoryParameter;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E2 extends E1 {",
			"  public int fieldE2;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(@SyntheticEventConstructorMandatoryParameter final float fieldE1b) {",
			"    super(fieldE1b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(@SyntheticEventConstructorMandatoryParameter final float fieldE1b, final int fieldE2) {",
			"    super(fieldE1b);",
			"    this.fieldE2 = fieldE2;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final String fieldE1a, @SyntheticEventConstructorMandatoryParameter final float fieldE1b) {",
			"    super(fieldE1a, fieldE1b);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final String fieldE1a, @SyntheticEventConstructorMandatoryParameter final float fieldE1b, final int fieldE2) {",
			"    super(fieldE1a, fieldE1b);",
			"    this.fieldE2 = fieldE2;",
			"  }",
			"",
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
			"    E2 other = (E2) obj;",
			"    if (other.fieldE2 != this.fieldE2)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.fieldE2);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E2 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"fieldE2\", this.fieldE2);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 247438152L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing inheritance w/ fields in 2f*E1 E2")
	@Tag("sarlParsing")
	public void parsingInheritanceField2fE1E2() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_17);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling inheritance w/ field in 2f*E1 E2")
	@Tag("compileToJava")
	public void compilingInheritanceField2fE1E2() throws Exception {
		getCompileHelper().compile(SARL_CODE_17, (it) -> {
			final String actualE1 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E1");
			assertEqualsExceptNewLines(JAVA_CODE_17_E1, actualE1);
			final String actualE2 = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E2");
			assertEqualsExceptNewLines(JAVA_CODE_17_E2, actualE2);
		});
	}

	private static final String SARL_CODE_18 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"import java.util.Collection",
			"event E {",
			"   val a : Collection<String>",
			"   var b : int",
			"   val c : int",
			"}");

	private static final String JAVA_CODE_18 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructorMandatoryParameter;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Collection;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E extends Event {",
			"  public final Collection<String> a;",
			"",
			"  public int b;",
			"",
			"  public final int c;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(@SyntheticEventConstructorMandatoryParameter final Collection<String> a, @SyntheticEventConstructorMandatoryParameter final int c) {",
			"    super();",
			"    this.a = a;",
			"    this.c = c;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E(@SyntheticEventConstructorMandatoryParameter final Collection<String> a, final int b, @SyntheticEventConstructorMandatoryParameter final int c) {",
			"    super();",
			"    this.a = a;",
			"    this.b = b;",
			"    this.c = c;",
			"  }",
			"",
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
			"    E other = (E) obj;",
			"    if (other.b != this.b)",
			"      return false;",
			"    if (other.c != this.c)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.b);",
			"    result = prime * result + Integer.hashCode(this.c);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"a\", this.a);",
			"    builder.add(\"b\", this.b);",
			"    builder.add(\"c\", this.c);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 405110253L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing var-val-s")
	@Tag("sarlParsing")
	public void parsingVarVals() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_18);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling var-val-s")
	@Tag("compileToJava")
	public void compilingVarVals() throws Exception {
		getCompileHelper().compile(SARL_CODE_18, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E");
			assertEqualsExceptNewLines(JAVA_CODE_18, actual);
		});
	}

	private static final String SARL_CODE_19 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"import java.util.Collection",
			"event E1 {",
			"   val a : Collection<String>",
			"   var b : int",
			"   val c : int",
			"}",
			"event E2 extends E1 {",
			"   var a : Float",
			"}");

	private static final String JAVA_CODE_19_E2 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructorMandatoryParameter;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class E2 extends E1 {",
			"  public Float a;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(@SyntheticEventConstructorMandatoryParameter final int c) {",
			"    super(c);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final int b, @SyntheticEventConstructorMandatoryParameter final int c) {",
			"    super(b, c);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(@SyntheticEventConstructorMandatoryParameter final int c, final Float a) {",
			"    super(c);",
			"    this.a = a;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public E2(final int b, @SyntheticEventConstructorMandatoryParameter final int c, final Float a) {",
			"    super(b, c);",
			"    this.a = a;",
			"  }",
			"",
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
			"    E2 other = (E2) obj;",
			"    if (other.a == null) {",
			"      if (this.a != null)",
			"        return false;",
			"    } else if (this.a == null)",
			"      return false;",
			"    if (other.a != null && Float.floatToIntBits(other.a.floatValue()) != Float.floatToIntBits(this.a.floatValue()))",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.a);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the E2 event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"a\", this.a);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 580706875L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing inheritance var-val-s")
	@Tag("sarlParsing")
	public void parsingGenericVarVals() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_19);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				SarlPackage.eINSTANCE.getSarlField(),
				org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				"field 'a'");
	}

	@Test
	@DisplayName("Compiling inheritance var-val-s")
	@Tag("compileToJava")
	public void compilingGenericVarVals() throws Exception {
		getCompileHelper().compile(SARL_CODE_19, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.E2");
			assertEqualsExceptNewLines(JAVA_CODE_19_E2, actual);
		});
	}

	private static final String SARL_CODE_20 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event MyEvent {",
			"	val string = \"abcd\"",
			"	val number : Integer",
			"}");

	private static final String JAVA_CODE_20 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructorMandatoryParameter;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class MyEvent extends Event {",
			"  public final String string = \"abcd\";",
			"",
			"  public final Integer number;",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public MyEvent(@SyntheticEventConstructorMandatoryParameter final Integer number) {",
			"    super();",
			"    this.number = number;",
			"  }",
			"",
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
			"    MyEvent other = (MyEvent) obj;",
			"    if (!Objects.equals(this.string, other.string))",
		    "      return false;",
		    "    if (other.number == null) {",
		    "      if (this.number != null)",
		    "        return false;",
		    "    } else if (this.number == null)",
		    "      return false;",
		    "    if (other.number != null && other.number.intValue() != this.number.intValue())",
		    "      return false;",
		    "    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.string);",
			"    result = prime * result + Objects.hashCode(this.number);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the MyEvent event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"string\", this.string);",
			"    builder.add(\"number\", this.number);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = -3394798830L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing initialized val")
	@Tag("sarlParsing")
	public void parsingInitializedVal() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_20);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling initialized val")
	@Tag("compileToJava")
	public void compilingInitializeVal() throws Exception {
		getCompileHelper().compile(SARL_CODE_20, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.MyEvent");
			assertEqualsExceptNewLines(JAVA_CODE_20, actual);
		});
	}


	private static final String SARL_CODE_21 = multilineString(
			"package io.sarl.lang.tests.bug1159",
			"event MyEvent {",
			"	val string = \"abcd\"",
			"	val number : Integer",
			"	val obj = new Object",
			"}");

	private static final String JAVA_CODE_21 = multilineString(
			"package io.sarl.lang.tests.bug1159;",
			"",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructor;",
			"import io.sarl.lang.core.annotation.SyntheticEventConstructorMandatoryParameter;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class MyEvent extends Event {",
			"  public final String string = \"abcd\";",
			"",
			"  public final Integer number;",
			"",
			"  public final Object obj = new Object();",
			"",
			"  @SyntheticMember",
			"  @SyntheticEventConstructor",
			"  public MyEvent(@SyntheticEventConstructorMandatoryParameter final Integer number) {",
			"    super();",
			"    this.number = number;",
			"  }",
			"",
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
			"    MyEvent other = (MyEvent) obj;",
			"    if (!Objects.equals(this.string, other.string))",
		    "      return false;",
		    "    if (other.number == null) {",
		    "      if (this.number != null)",
		    "        return false;",
		    "    } else if (this.number == null)",
		    "      return false;",
		    "    if (other.number != null && other.number.intValue() != this.number.intValue())",
		    "      return false;",
		    "    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.string);",
			"    result = prime * result + Objects.hashCode(this.number);",
			"    return result;",
			"  }",
			"",
			"  /**",
			"   * Returns a String representation of the MyEvent event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"string\", this.string);",
			"    builder.add(\"number\", this.number);",
			"    builder.add(\"obj\", this.obj);",
			"  }",
			"",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = -3394689015L;",
			"}",
			"");

	@Test
	@DisplayName("Parsing initialized vals")
	@Tag("sarlParsing")
	public void parsingInitializedVals() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_21);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling initialized vals")
	@Tag("compileToJava")
	public void compilingInitializeVals() throws Exception {
		getCompileHelper().compile(SARL_CODE_21, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1159.MyEvent");
			assertEqualsExceptNewLines(JAVA_CODE_21, actual);
		});
	}

}

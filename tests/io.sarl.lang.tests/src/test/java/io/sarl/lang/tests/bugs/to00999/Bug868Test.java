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

import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Enable {@code @Accessors} annotation and other active annotations to agent-oriented type declarations..
 *
 * <p>https://github.com/sarl/sarl/issues/868
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/868"
 */
@DisplayName("Bug #868")
@SuppressWarnings("all")
@Tag("core")
public class Bug868Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug868",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"class X {",
			"  public static class Y {",
			"    @Accessors",
			"    var myfield : int",
			"  }",
			"  def test(y : Y) : int {",
			"    y.getMyfield",
			"  }",
			"}");

	private static final String EXPECTED01 = multilineString(
			"package io.sarl.lang.tests.bug868;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"  public static class Y {",
			"    @Accessors",
			"    private int myfield;",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public boolean equals(final Object obj) {",
			"      if (this == obj)",
			"        return true;",
			"      if (obj == null)",
			"        return false;",
			"      if (getClass() != obj.getClass())",
			"        return false;",
			"      Y other = (Y) obj;",
			"      if (other.myfield != this.myfield)",
			"        return false;",
			"      return super.equals(obj);",
			"    }",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public int hashCode() {",
			"      int result = super.hashCode();",
			"      final int prime = 31;",
			"      result = prime * result + Integer.hashCode(this.myfield);",
			"      return result;",
			"    }",
			"    ",
			"    @SyntheticMember",
			"    public Y() {",
			"      super();",
			"    }",
			"    ",
			"    @Pure",
			"    public int getMyfield() {",
			"      return this.myfield;",
			"    }",
			"    ",
			"    public void setMyfield(final int myfield) {",
			"      this.myfield = myfield;",
			"    }",
			"  }",
			"  ",
			"  @Pure",
			"  public int test(final X.Y y) {",
			"    return y.getMyfield();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug868.X");
			assertEquals(EXPECTED01, actual);
		});
	}

	private static final String SNIPSET02 = multilineString(
			"package io.sarl.lang.tests.bug868",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"import org.eclipse.xtend.lib.annotations.AccessorType",
			"class X {",
			"  public static class Y {",
			"    @Accessors(AccessorType::PUBLIC_GETTER)",
			"    var myfield : int",
			"  }",
			"  def test(y : Y) : int {",
			"    y.getMyfield",
			"  }",
			"}");

	private static final String EXPECTED02 = multilineString(
			"package io.sarl.lang.tests.bug868;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtend.lib.annotations.AccessorType;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"  public static class Y {",
			"    @Accessors(AccessorType.PUBLIC_GETTER)",
			"    private int myfield;",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public boolean equals(final Object obj) {",
			"      if (this == obj)",
			"        return true;",
			"      if (obj == null)",
			"        return false;",
			"      if (getClass() != obj.getClass())",
			"        return false;",
			"      Y other = (Y) obj;",
			"      if (other.myfield != this.myfield)",
			"        return false;",
			"      return super.equals(obj);",
			"    }",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public int hashCode() {",
			"      int result = super.hashCode();",
			"      final int prime = 31;",
			"      result = prime * result + Integer.hashCode(this.myfield);",
			"      return result;",
			"    }",
			"    ",
			"    @SyntheticMember",
			"    public Y() {",
			"      super();",
			"    }",
			"    ",
			"    @Pure",
			"    public int getMyfield() {",
			"      return this.myfield;",
			"    }",
			"  }",
			"  ",
			"  @Pure",
			"  public int test(final X.Y y) {",
			"    return y.getMyfield();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug868.X");
			assertEquals(EXPECTED02, actual);
		});
	}

	private static final String SNIPSET03 = multilineString(
			"package io.sarl.lang.tests.bug868",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"agent X {",
			"  static class Y {",
			"    @Accessors",
			"    var myfield : int",
			"  }",
			"  def test(y : Y) : int {",
			"    y.getMyfield",
			"  }",
			"}");

	private static final String EXPECTED03 = multilineString(
			"package io.sarl.lang.tests.bug868;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class X extends Agent {",
			"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"  protected static class Y {",
			"    @Accessors",
			"    private int myfield;",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public boolean equals(final Object obj) {",
			"      if (this == obj)",
			"        return true;",
			"      if (obj == null)",
			"        return false;",
			"      if (getClass() != obj.getClass())",
			"        return false;",
			"      Y other = (Y) obj;",
			"      if (other.myfield != this.myfield)",
			"        return false;",
			"      return super.equals(obj);",
			"    }",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public int hashCode() {",
			"      int result = super.hashCode();",
			"      final int prime = 31;",
			"      result = prime * result + Integer.hashCode(this.myfield);",
			"      return result;",
			"    }",
			"    ",
			"    @SyntheticMember",
			"    public Y() {",
			"      super();",
			"    }",
			"    ",
			"    @Pure",
			"    public int getMyfield() {",
			"      return this.myfield;",
			"    }",
			"    ",
			"    public void setMyfield(final int myfield) {",
			"      this.myfield = myfield;",
			"    }",
			"  }",
			"  ",
			"  @Pure",
			"  protected int test(final X.Y y) {",
			"    return y.getMyfield();",
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

	@Test
	@Tag("sarlValidation")
	public void parsing_03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_03() throws Exception {
		getCompileHelper().compile(SNIPSET03, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug868.X");
			assertEquals(EXPECTED03, actual);
		});
	}

	private static final String SNIPSET04 = multilineString(
			"package io.sarl.lang.tests.bug868",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"import org.eclipse.xtend.lib.annotations.AccessorType",
			"agent X {",
			"  static class Y {",
			"    @Accessors(AccessorType::PUBLIC_GETTER)",
			"    var myfield : int",
			"  }",
			"  def test(y : Y) : int {",
			"    y.getMyfield",
			"  }",
			"}");

	private static final String EXPECTED04 = multilineString(
			"package io.sarl.lang.tests.bug868;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtend.lib.annotations.AccessorType;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class X extends Agent {",
			"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"  protected static class Y {",
			"    @Accessors(AccessorType.PUBLIC_GETTER)",
			"    private int myfield;",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public boolean equals(final Object obj) {",
			"      if (this == obj)",
			"        return true;",
			"      if (obj == null)",
			"        return false;",
			"      if (getClass() != obj.getClass())",
			"        return false;",
			"      Y other = (Y) obj;",
			"      if (other.myfield != this.myfield)",
			"        return false;",
			"      return super.equals(obj);",
			"    }",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public int hashCode() {",
			"      int result = super.hashCode();",
			"      final int prime = 31;",
			"      result = prime * result + Integer.hashCode(this.myfield);",
			"      return result;",
			"    }",
			"    ",
			"    @SyntheticMember",
			"    public Y() {",
			"      super();",
			"    }",
			"    ",
			"    @Pure",
			"    public int getMyfield() {",
			"      return this.myfield;",
			"    }",
			"  }",
			"  ",
			"  @Pure",
			"  protected int test(final X.Y y) {",
			"    return y.getMyfield();",
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

	@Test
	@Tag("sarlValidation")
	public void parsing_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_04() throws Exception {
		getCompileHelper().compile(SNIPSET04, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug868.X");
			assertEquals(EXPECTED04, actual);
		});
	}

	private static final String SNIPSET05 = multilineString(
			"package io.sarl.lang.tests.bug868",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"behavior X {",
			"  static class Y {",
			"    @Accessors",
			"    var myfield : int",
			"  }",
			"  def test(y : Y) : int {",
			"    y.getMyfield",
			"  }",
			"}");

	private static final String EXPECTED05 = multilineString(
			"package io.sarl.lang.tests.bug868;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.Behavior;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
			"@SuppressWarnings(\"all\")",
			"public class X extends Behavior {",
			"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"  public static class Y {",
			"    @Accessors",
			"    private int myfield;",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public boolean equals(final Object obj) {",
			"      if (this == obj)",
			"        return true;",
			"      if (obj == null)",
			"        return false;",
			"      if (getClass() != obj.getClass())",
			"        return false;",
			"      Y other = (Y) obj;",
			"      if (other.myfield != this.myfield)",
			"        return false;",
			"      return super.equals(obj);",
			"    }",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public int hashCode() {",
			"      int result = super.hashCode();",
			"      final int prime = 31;",
			"      result = prime * result + Integer.hashCode(this.myfield);",
			"      return result;",
			"    }",
			"    ",
			"    @SyntheticMember",
			"    public Y() {",
			"      super();",
			"    }",
			"    ",
			"    @Pure",
			"    public int getMyfield() {",
			"      return this.myfield;",
			"    }",
			"    ",
			"    public void setMyfield(final int myfield) {",
			"      this.myfield = myfield;",
			"    }",
			"  }",
			"  ",
			"  @Pure",
			"  public int test(final X.Y y) {",
			"    return y.getMyfield();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_05() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET05);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_05() throws Exception {
		getCompileHelper().compile(SNIPSET05, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug868.X");
			assertEquals(EXPECTED05, actual);
		});
	}

	private static final String SNIPSET06 = multilineString(
			"package io.sarl.lang.tests.bug868",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"import org.eclipse.xtend.lib.annotations.AccessorType",
			"behavior X {",
			"  static class Y {",
			"    @Accessors(AccessorType::PUBLIC_GETTER)",
			"    var myfield : int",
			"  }",
			"  def test(y : Y) : int {",
			"    y.getMyfield",
			"  }",
			"}");

	private static final String EXPECTED06 = multilineString(
			"package io.sarl.lang.tests.bug868;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.Behavior;",
			"import org.eclipse.xtend.lib.annotations.AccessorType;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
			"@SuppressWarnings(\"all\")",
			"public class X extends Behavior {",
			"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"  public static class Y {",
			"    @Accessors(AccessorType.PUBLIC_GETTER)",
			"    private int myfield;",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public boolean equals(final Object obj) {",
			"      if (this == obj)",
			"        return true;",
			"      if (obj == null)",
			"        return false;",
			"      if (getClass() != obj.getClass())",
			"        return false;",
			"      Y other = (Y) obj;",
			"      if (other.myfield != this.myfield)",
			"        return false;",
			"      return super.equals(obj);",
			"    }",
			"    ",
			"    @Override",
			"    @Pure",
			"    @SyntheticMember",
			"    public int hashCode() {",
			"      int result = super.hashCode();",
			"      final int prime = 31;",
			"      result = prime * result + Integer.hashCode(this.myfield);",
			"      return result;",
			"    }",
			"    ",
			"    @SyntheticMember",
			"    public Y() {",
			"      super();",
			"    }",
			"    ",
			"    @Pure",
			"    public int getMyfield() {",
			"      return this.myfield;",
			"    }",
			"  }",
			"  ",
			"  @Pure",
			"  public int test(final X.Y y) {",
			"    return y.getMyfield();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_06() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET06);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_06() throws Exception {
		getCompileHelper().compile(SNIPSET06, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug868.X");
			assertEquals(EXPECTED06, actual);
		});
	}

	private static final String SNIPSET07 = multilineString(
			"package io.sarl.lang.tests.bug868",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"import org.eclipse.xtend.lib.annotations.AccessorType",
			"agent X {",
			"  @Accessors(AccessorType::PUBLIC_GETTER)",
			"  var myfield : int",
			"}");

	private static final String EXPECTED07 = multilineString(
			"package io.sarl.lang.tests.bug868;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtend.lib.annotations.AccessorType;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class X extends Agent {",
			"  @Accessors(AccessorType.PUBLIC_GETTER)",
			"  private int myfield;",
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
			"    X other = (X) obj;",
			"    if (other.myfield != this.myfield)",
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
			"    result = prime * result + Integer.hashCode(this.myfield);",
			"    return result;",
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
			"  ",
			"  @Pure",
			"  protected int getMyfield() {",
			"    return this.myfield;",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_07() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET07);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_07() throws Exception {
		getCompileHelper().compile(SNIPSET07, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug868.X");
			assertEquals(EXPECTED07, actual);
		});
	}

	private static final String SNIPSET08 = multilineString(
			"package io.sarl.lang.tests.bug868",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"behavior X {",
			"  @Accessors",
			"  var myfield : int",
			"}");

	private static final String EXPECTED08 = multilineString(
			"package io.sarl.lang.tests.bug868;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.Behavior;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
			"@SuppressWarnings(\"all\")",
			"public class X extends Behavior {",
			"  @Accessors",
			"  private int myfield;",
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
			"    X other = (X) obj;",
			"    if (other.myfield != this.myfield)",
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
			"    result = prime * result + Integer.hashCode(this.myfield);",
			"    return result;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"  ",
			"  @Pure",
			"  public int getMyfield() {",
			"    return this.myfield;",
			"  }",
			"  ",
			"  public void setMyfield(final int myfield) {",
			"    this.myfield = myfield;",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_08() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET08);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_08() throws Exception {
		getCompileHelper().compile(SNIPSET08, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug868.X");
			assertEquals(EXPECTED08, actual);
		});
	}

	private static final String SNIPSET09 = multilineString(
			"package io.sarl.lang.tests.bug868",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"event X {",
			"  @Accessors",
			"  var myfield : int",
			"}");

	private static final String EXPECTED09 = multilineString(
			"package io.sarl.lang.tests.bug868;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.Event;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_EVENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class X extends Event {",
			"  /* @Accessors",
			"   */public int myfield;",
			"  ",
			"  @SyntheticMember",
			"  public X() {",
			"    super();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X(final Address arg0) {",
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
			"    X other = (X) obj;",
			"    if (other.myfield != this.myfield)",
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
			"    result = prime * result + Integer.hashCode(this.myfield);",
			"    return result;",
			"  }",
			"  ",
			"  /**",
			"   * Returns a String representation of the X event's attributes only.",
			"   */",
			"  @SyntheticMember",
			"  @Pure",
			"  protected void toString(final ToStringBuilder builder) {",
			"    super.toString(builder);",
			"    builder.add(\"myfield\", this.myfield);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  private static final long serialVersionUID = 2103681291L;",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_09() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET09);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.FORBIDDEN_REFERENCE,
				"Forbidden annotation");
	}

	@Test
	@Tag("compileToJava")
	public void compiling_09() throws Exception {
		getCompileHelper().compile(SNIPSET09, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug868.X");
			assertEquals(EXPECTED09, actual);
		});
	}

}


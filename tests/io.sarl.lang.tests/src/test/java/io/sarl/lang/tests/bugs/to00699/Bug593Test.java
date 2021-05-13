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

package io.sarl.lang.tests.bugs.to00699;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Generic type not found
 *
 * <p>https://github.com/sarl/sarl/issues/593
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #593")
@SuppressWarnings("all")
@Tag("core")
public class Bug593Test {

	@Nested
	public class ImplementsInAbstractClassTypeTest extends AbstractSarlTest {

		private final String SNIPSET1 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"abstract class Tuple2f<T> implements Serializable {",
				"}");

		private final String SNIPSET2 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"abstract class Tuple2f<T> implements Serializable, Cloneable {",
				"}");

		private final String SNIPSET3 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"abstract class Tuple2f<T> implements Serializable, Cloneable, Comparable<T> {",
				"}");

		private final String SNIPSET4 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"abstract class Tuple2f<T extends Tuple2f<?>> implements Serializable, Cloneable, Comparable<T> {",
				"}");

		@Test
		@Tag("sarlValidation")
		public void parsing_01() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_01() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET1, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.io.Serializable;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class Tuple2f<T extends Object> implements Serializable {",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1L;",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_02() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET2);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_02() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET2, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.io.Serializable;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class Tuple2f<T extends Object> implements Serializable, Cloneable {",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public Tuple2f<T> clone() {",
					"    try {",
					"      return (Tuple2f<T>) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1L;",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_03() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET3);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_03() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET3, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.io.Serializable;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class Tuple2f<T extends Object> implements Serializable, Cloneable, Comparable<T> {",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public Tuple2f<T> clone() {",
					"    try {",
					"      return (Tuple2f<T>) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1L;",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_04() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET4);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_04() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET4, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.io.Serializable;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class Tuple2f<T extends Tuple2f<?>> implements Serializable, Cloneable, Comparable<T> {",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public Tuple2f<T> clone() {",
					"    try {",
					"      return (Tuple2f<T>) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1L;",
					"}",
					""));
		}

	}

	@Nested
	public class ExtendsInAbstractClassTypeTest extends AbstractSarlTest {

		private final String SNIPSET1 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"abstract class Tuple2f<T> extends Object {",
				"}");

		private final String SNIPSET2 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.util.ArrayList",
				"abstract class Tuple2f<T> extends ArrayList<T> {",
				"}");

		private final String SNIPSET3 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.util.ArrayList",
				"abstract class Tuple2f<T extends Tuple2f<?>> extends ArrayList<T> {",
				"}");

		@Test
		@Tag("sarlValidation")
		public void parsing_01() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_01() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET1, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class Tuple2f<T extends Object> {",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_02() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET2);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_02() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET2, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.ArrayList;",
					"import java.util.Collection;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class Tuple2f<T extends Object> extends ArrayList<T> {",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public Tuple2f<T> clone() {",
					"    try {",
					"      return (Tuple2f<T>) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f(final int arg0) {",
					"    super(arg0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f(final Collection<? extends T> arg0) {",
					"    super(arg0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1L;",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_03() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET3);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_03() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET3, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.ArrayList;",
					"import java.util.Collection;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public abstract class Tuple2f<T extends Tuple2f<?>> extends ArrayList<T> {",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public Tuple2f<T> clone() {",
					"    try {",
					"      return (Tuple2f<T>) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f(final int arg0) {",
					"    super(arg0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f(final Collection<? extends T> arg0) {",
					"    super(arg0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1L;",
					"}",
					""));
		}

	}

	@Nested
	public class ImplementsInConcreteClassTypeTest extends AbstractSarlTest {

		private final String SNIPSET1 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"class Tuple2f<T> implements Serializable {",
				"}");

		private final String SNIPSET2 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"class Tuple2f<T> implements Serializable, Cloneable {",
				"}");

		private final String SNIPSET3 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"class Tuple2f<T> implements Serializable, Cloneable, Comparable<T> {",
				"  def compareTo(t : T) : int { return 0 }",
				"}");

		private final String SNIPSET4 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"class Tuple2f<T extends Tuple2f<?>> implements Serializable, Cloneable, Comparable<T> {",
				"  def compareTo(t : T) : int { return 0 }",
				"}");

		@Test
		@Tag("sarlValidation")
		public void parsing_01() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_01() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET1, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.io.Serializable;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Tuple2f<T extends Object> implements Serializable {",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1L;",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_02() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET2);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_02() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET2, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.io.Serializable;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Tuple2f<T extends Object> implements Serializable, Cloneable {",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public Tuple2f<T> clone() {",
					"    try {",
					"      return (Tuple2f<T>) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 1L;",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_03() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET3);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_03() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET3, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.io.Serializable;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Tuple2f<T extends Object> implements Serializable, Cloneable, Comparable<T> {",
					"  public int compareTo(final T t) {",
					"    return 0;",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public Tuple2f<T> clone() {",
					"    try {",
					"      return (Tuple2f<T>) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 2070834854L;",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_04() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET4);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_04() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET4, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.io.Serializable;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Tuple2f<T extends Tuple2f<?>> implements Serializable, Cloneable, Comparable<T> {",
					"  @Pure",
					"  public int compareTo(final T t) {",
					"    return 0;",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public Tuple2f<T> clone() {",
					"    try {",
					"      return (Tuple2f<T>) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 87934145L;",
					"}",
					""));
		}

	}

	@Nested
	public class ExtendsInConcreteClassTypeTest extends AbstractSarlTest {

		private final String SNIPSET1 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"class Tuple2f<T> extends Object {",
				"  public var myvar : T",
				"  def fct : T { null }",
				"}");

		private final String SNIPSET2 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.util.ArrayList",
				"class Tuple2f<T> extends ArrayList<T> {",
				"  public var myvar : T",
				"  def fct : T { null }",
				"}");

		private final String SNIPSET3 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.util.ArrayList",
				"class Tuple2f<T extends Tuple2f<?>> extends ArrayList<T> {",
				"  public var myvar : T",
				"  def fct : T { null }",
				"}");

		@Test
		@Tag("sarlValidation")
		public void parsing_01() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_01() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET1, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Tuple2f<T extends Object> {",
					"  public T myvar;",
					"  ",
					"  @Pure",
					"  public T fct() {",
					"    return null;",
					"  }",
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
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_02() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET2);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_02() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET2, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.ArrayList;",
					"import java.util.Collection;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Tuple2f<T extends Object> extends ArrayList<T> {",
					"  public T myvar;",
					"  ",
					"  @Pure",
					"  public T fct() {",
					"    return null;",
					"  }",
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
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public Tuple2f<T> clone() {",
					"    try {",
					"      return (Tuple2f<T>) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f(final int arg0) {",
					"    super(arg0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f(final Collection<? extends T> arg0) {",
					"    super(arg0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 107525711L;",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_03() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET3);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_03() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET3, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.ArrayList;",
					"import java.util.Collection;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Tuple2f<T extends Tuple2f<?>> extends ArrayList<T> {",
					"  public T myvar;",
					"  ",
					"  @Pure",
					"  public T fct() {",
					"    return null;",
					"  }",
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
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public Tuple2f<T> clone() {",
					"    try {",
					"      return (Tuple2f<T>) super.clone();",
					"    } catch (Throwable exception) {",
					"      throw new Error(exception);",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f(final int arg0) {",
					"    super(arg0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Tuple2f(final Collection<? extends T> arg0) {",
					"    super(arg0);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  private static final long serialVersionUID = 107525711L;",
					"}",
					""));
		}

	}

	@Nested
	public class ExtendsInInterfaceTypeTest extends AbstractSarlTest {

		private final String SNIPSET1 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"interface Tuple2f<T> extends Serializable {",
				"  def fct : T",
				"}");

		private final String SNIPSET2 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"interface Tuple2f<T> extends Serializable, Cloneable {",
				"  def fct : T",
				"}");

		private final String SNIPSET3 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"interface Tuple2f<T> extends Serializable, Cloneable, Comparable<T> {",
				"  def fct : T",
				"}");

		private final String SNIPSET4 = multilineString(
				"package io.sarl.lang.tests.bug593",
				"import java.io.Serializable",
				"interface Tuple2f<T extends Tuple2f<?>> extends Serializable, Cloneable, Comparable<T> {",
				"  def fct : T",
				"}");

		@Test
		@Tag("sarlValidation")
		public void parsing_01() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_01() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET1, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.io.Serializable;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface Tuple2f<T extends Object> extends Serializable {",
					"  T fct();",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_02() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET2);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_02() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET2, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.io.Serializable;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface Tuple2f<T extends Object> extends Serializable, Cloneable {",
					"  T fct();",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_03() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET3);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_03() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET3, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.io.Serializable;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface Tuple2f<T extends Object> extends Serializable, Cloneable, Comparable<T> {",
					"  T fct();",
					"}",
					""));
		}

		@Test
		@Tag("sarlValidation")
		public void parsing_04() throws Exception {
			SarlScript mas = file(getParseHelper(), SNIPSET4);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		public void compiling_04() throws Exception {
			getCompileHelper().assertCompilesTo(SNIPSET4, multilineString(
					"package io.sarl.lang.tests.bug593;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import java.io.Serializable;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
					"@SuppressWarnings(\"all\")",
					"public interface Tuple2f<T extends Tuple2f<?>> extends Serializable, Cloneable, Comparable<T> {",
					"  T fct();",
					"}",
					""));
		}

	}

}

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

package io.sarl.lang.tests.bugs.to01399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid java generation for default-valued parameters with generic type.
 *
 * <p>https://github.com/sarl/sarl/issues/1035
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1035"
 */
@DisplayName("Bug #1035")
@SuppressWarnings("all")
@Tag("core")
public class Bug1035Test extends AbstractSarlTest {

	private final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1035",
			"class Bug1035Class<T> {",
			"  def syncOut(forceSync : boolean = false, changeCallback : (T, T) => void = null) : T {",
			"    var x = null",
			"    return x",
			"  }",
			"}");
	
	private final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1035;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;", 
			"import io.sarl.lang.annotation.DefaultValueSource;", 
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;", 
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Bug1035Class<T extends Object> {",
			"  @DefaultValueSource",
			"  @Pure",
			"  public T syncOut(@DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_0\") final boolean forceSync, @DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_1\") final Procedure2<? super T, ? super T> changeCallback) {",
			"    Object x = null;",
			"    return ((T)x);",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter forceSync",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"false\")",
			"  private final boolean $DEFAULT_VALUE$SYNCOUT_0() {",
			"    return false;",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter changeCallback",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private final Procedure2 $DEFAULT_VALUE$SYNCOUT_1() {",
			"    return null;",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final T syncOut() {",
			"    return (T)syncOut($DEFAULT_VALUE$SYNCOUT_0(), $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final T syncOut(final Procedure2<? super T, ? super T> changeCallback) {",
			"    return (T)syncOut($DEFAULT_VALUE$SYNCOUT_0(), changeCallback);",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final T syncOut(final boolean forceSync) {",
			"    return (T)syncOut(forceSync, $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug1035Class() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Generic return type paring")
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Generic return type compiling")
	@Tag("compileToJava")
	public void compiling01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertEquals(JAVA_CODE_01, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertNotNull(type);
		});
	}

	private final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1035",
			"class Bug1035Class<T> {",
			"  def syncOut(x : T, forceSync : boolean = false, changeCallback : (T, T) => void = null) : void {",
			"  }",
			"}");
	
	private final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1035;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;", 
			"import io.sarl.lang.annotation.DefaultValueSource;", 
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;", 
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Bug1035Class<T extends Object> {",
			"  @DefaultValueSource",
			"  public void syncOut(final T x, @DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_0\") final boolean forceSync, @DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_1\") final Procedure2<? super T, ? super T> changeCallback) {",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter forceSync",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"false\")",
			"  private final boolean $DEFAULT_VALUE$SYNCOUT_0() {",
			"    return false;",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter changeCallback",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private final Procedure2 $DEFAULT_VALUE$SYNCOUT_1() {",
			"    return null;",
			"  }",
			"  ",
			"  @DefaultValueUse(\"java.lang.Object,boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  public final void syncOut(final T x) {",
			"    syncOut(x, $DEFAULT_VALUE$SYNCOUT_0(), $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @DefaultValueUse(\"java.lang.Object,boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  public final void syncOut(final T x, final Procedure2<? super T, ? super T> changeCallback) {",
			"    syncOut(x, $DEFAULT_VALUE$SYNCOUT_0(), changeCallback);",
			"  }",
			"  ",
			"  @DefaultValueUse(\"java.lang.Object,boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  public final void syncOut(final T x, final boolean forceSync) {",
			"    syncOut(x, forceSync, $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug1035Class() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Generic formal parameter parsing")
	@Tag("sarlParsing")
	public void parsing02() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Generic formal parameter compiling")
	@Tag("compileToJava")
	public void compiling02() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertEquals(JAVA_CODE_02, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertNotNull(type);
		});
	}

	private final String SARL_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1035",
			"class Bug1035Class<T> {",
			"  def syncOut(forceSync : boolean = false, changeCallback : (T, T) => void = null) : T[] {",
			"    var x = null",
			"    return x",
			"  }",
			"}");
	
	private final String JAVA_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1035;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;", 
			"import io.sarl.lang.annotation.DefaultValueSource;", 
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;", 
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Bug1035Class<T extends Object> {",
			"  @DefaultValueSource",
			"  @Pure",
			"  public T[] syncOut(@DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_0\") final boolean forceSync, @DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_1\") final Procedure2<? super T, ? super T> changeCallback) {",
			"    Object x = null;",
			"    return ((T[])x);",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter forceSync",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"false\")",
			"  private final boolean $DEFAULT_VALUE$SYNCOUT_0() {",
			"    return false;",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter changeCallback",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private final Procedure2 $DEFAULT_VALUE$SYNCOUT_1() {",
			"    return null;",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final T[] syncOut() {",
			"    return (T[])syncOut($DEFAULT_VALUE$SYNCOUT_0(), $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final T[] syncOut(final Procedure2<? super T, ? super T> changeCallback) {",
			"    return (T[])syncOut($DEFAULT_VALUE$SYNCOUT_0(), changeCallback);",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final T[] syncOut(final boolean forceSync) {",
			"    return (T[])syncOut(forceSync, $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug1035Class() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Generic array return type paring")
	@Tag("sarlParsing")
	public void parsing03() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Generic array return type compiling")
	@Tag("compileToJava")
	public void compiling03() throws Exception {
		getCompileHelper().compile(SARL_CODE_03, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertEquals(JAVA_CODE_03, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertNotNull(type);
		});
	}

	private final String SARL_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug1035",
			"class Bug1035Class<T> {",
			"  def syncOut(forceSync : boolean = false, changeCallback : (T, T) => void = null) : (int, boolean) => void {",
			"    var x = null",
			"    return x",
			"  }",
			"}");
	
	private final String JAVA_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug1035;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;", 
			"import io.sarl.lang.annotation.DefaultValueSource;", 
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;", 
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Bug1035Class<T extends Object> {",
			"  @DefaultValueSource",
			"  @Pure",
			"  public Procedure2<? super Integer, ? super Boolean> syncOut(@DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_0\") final boolean forceSync, @DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_1\") final Procedure2<? super T, ? super T> changeCallback) {",
			"    Object x = null;",
			"    return ((Procedure2<? super Integer, ? super Boolean>)x);",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter forceSync",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"false\")",
			"  private final boolean $DEFAULT_VALUE$SYNCOUT_0() {",
			"    return false;",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter changeCallback",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private final Procedure2 $DEFAULT_VALUE$SYNCOUT_1() {",
			"    return null;",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final Procedure2<? super Integer, ? super Boolean> syncOut() {",
			"    return syncOut($DEFAULT_VALUE$SYNCOUT_0(), $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final Procedure2<? super Integer, ? super Boolean> syncOut(final Procedure2<? super T, ? super T> changeCallback) {",
			"    return syncOut($DEFAULT_VALUE$SYNCOUT_0(), changeCallback);",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final Procedure2<? super Integer, ? super Boolean> syncOut(final boolean forceSync) {",
			"    return syncOut(forceSync, $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug1035Class() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Function ref return type paring")
	@Tag("sarlParsing")
	public void parsing04() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Function ref return type compiling")
	@Tag("compileToJava")
	public void compiling04() throws Exception {
		getCompileHelper().compile(SARL_CODE_04, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertEquals(JAVA_CODE_04, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertNotNull(type);
		});
	}

	private final String SARL_CODE_05 = multilineString(
			"package io.sarl.lang.tests.bug1035",
			"class Bug1035Class<T> {",
			"  def syncOut(forceSync : boolean = false, changeCallback : (int, boolean) => void = null) : T {",
			"    var x = null",
			"    return x",
			"  }",
			"}");
	
	private final String JAVA_CODE_05 = multilineString(
			"package io.sarl.lang.tests.bug1035;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;", 
			"import io.sarl.lang.annotation.DefaultValueSource;", 
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;", 
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Bug1035Class<T extends Object> {",
			"  @DefaultValueSource",
			"  @Pure",
			"  public T syncOut(@DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_0\") final boolean forceSync, @DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_1\") final Procedure2<? super Integer, ? super Boolean> changeCallback) {",
			"    Object x = null;",
			"    return ((T)x);",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter forceSync",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"false\")",
			"  private final boolean $DEFAULT_VALUE$SYNCOUT_0() {",
			"    return false;",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter changeCallback",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private final Procedure2 $DEFAULT_VALUE$SYNCOUT_1() {",
			"    return null;",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(int, boolean)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final T syncOut() {",
			"    return (T)syncOut($DEFAULT_VALUE$SYNCOUT_0(), $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(int, boolean)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final T syncOut(final Procedure2<? super Integer, ? super Boolean> changeCallback) {",
			"    return (T)syncOut($DEFAULT_VALUE$SYNCOUT_0(), changeCallback);",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(int, boolean)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final T syncOut(final boolean forceSync) {",
			"    return (T)syncOut(forceSync, $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug1035Class() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Only generic return type paring")
	@Tag("sarlParsing")
	public void parsing05() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_05);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Only generic return type compiling")
	@Tag("compileToJava")
	public void compiling05() throws Exception {
		getCompileHelper().compile(SARL_CODE_05, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertEquals(JAVA_CODE_05, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertNotNull(type);
		});
	}

	private final String SARL_CODE_06 = multilineString(
			"package io.sarl.lang.tests.bug1035",
			"class Bug1035Class<T> {",
			"  def syncOut(forceSync : boolean = false, changeCallback : (T, T) => void = null) : (T, T) => void {",
			"    var x = null",
			"    return x",
			"  }",
			"}");
	
	private final String JAVA_CODE_06 = multilineString(
			"package io.sarl.lang.tests.bug1035;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;", 
			"import io.sarl.lang.annotation.DefaultValueSource;", 
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;", 
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Bug1035Class<T extends Object> {",
			"  @DefaultValueSource",
			"  @Pure",
			"  public Procedure2<? super T, ? super T> syncOut(@DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_0\") final boolean forceSync, @DefaultValue(\"io.sarl.lang.tests.bug1035.Bug1035Class#SYNCOUT_1\") final Procedure2<? super T, ? super T> changeCallback) {",
			"    Object x = null;",
			"    return ((Procedure2<? super T, ? super T>)x);",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter forceSync",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"false\")",
			"  private final boolean $DEFAULT_VALUE$SYNCOUT_0() {",
			"    return false;",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter changeCallback",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private final Procedure2 $DEFAULT_VALUE$SYNCOUT_1() {",
			"    return null;",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final Procedure2<? super T, ? super T> syncOut() {",
			"    return (org.eclipse.xtext.xbase.lib.Procedures.Procedure2<? super T, ? super T>)syncOut($DEFAULT_VALUE$SYNCOUT_0(), $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final Procedure2<? super T, ? super T> syncOut(final Procedure2<? super T, ? super T> changeCallback) {",
			"    return (org.eclipse.xtext.xbase.lib.Procedures.Procedure2<? super T, ? super T>)syncOut($DEFAULT_VALUE$SYNCOUT_0(), changeCallback);",
			"  }",
			"  ",
			"  @DefaultValueUse(\"boolean,(T, T)=>void\")",
			"  @SyntheticMember",
			"  @Pure",
			"  public final Procedure2<? super T, ? super T> syncOut(final boolean forceSync) {",
			"    return (org.eclipse.xtext.xbase.lib.Procedures.Procedure2<? super T, ? super T>)syncOut(forceSync, $DEFAULT_VALUE$SYNCOUT_1());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public Bug1035Class() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Generic function ref return type paring")
	@Tag("sarlParsing")
	public void parsing06() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_06);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Generic function ref return type compiling")
	@Tag("compileToJava")
	public void compiling06() throws Exception {
		getCompileHelper().compile(SARL_CODE_06, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertEquals(JAVA_CODE_06, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1035.Bug1035Class");
			assertNotNull(type);
		});
	}

}

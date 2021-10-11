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

/** Testing class for issue: Valid combination of methods with default parameters causes Maven compilation failure.
 *
 * <p>https://github.com/sarl/sarl/issues/729
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #729")
@SuppressWarnings("all")
@Tag("core")
public class Bug729Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug729",
			"",
			"import io.sarl.lang.core.Address", 
			"",
			"interface Scope<T> { }",
			"class Scopes {",
			"  public static def allParticipants : Scope<T> with T { null }",
			"}",
			"capacity Example ", 
			"{", 
			"    def firstExampleMethod(scopeWithDefault : Scope<Address> = Scopes.<Address>allParticipants)",
			"    def secondExampleMethod(paramWithDefault : boolean = true)",
			"}");

	private final String EXPECTED1 = multilineString(
			"package io.sarl.lang.tests.bug729;",
			"", 
			"import io.sarl.lang.annotation.DefaultValue;", 
			"import io.sarl.lang.annotation.DefaultValueSource;", 
			"import io.sarl.lang.annotation.DefaultValueUse;", 
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSourceCode;", 
			"import io.sarl.lang.annotation.SarlSpecification;", 
			"import io.sarl.lang.annotation.SyntheticMember;", 
			"import io.sarl.lang.core.Address;", 
			"import io.sarl.lang.core.AgentTrait;", 
			"import io.sarl.lang.core.Capacity;", 
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"", 
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType("+ SarlPackage.SARL_CAPACITY + ")", 
			"@SuppressWarnings(\"all\")", 
			"public interface Example extends Capacity {", 
			"  @DefaultValueSource", 
			"  void firstExampleMethod(@DefaultValue(\"io.sarl.lang.tests.bug729.Example#FIRSTEXAMPLEMETHOD_0\") final Scope<Address> scopeWithDefault);", 
			"  ", 
			"  @DefaultValueSource", 
			"  void secondExampleMethod(@DefaultValue(\"io.sarl.lang.tests.bug729.Example#SECONDEXAMPLEMETHOD_0\") final boolean paramWithDefault);", 
			"  ", 
			"  /**", 
			"   * Default value for the parameter scopeWithDefault", 
			"   */", 
			"  @Pure",
			"  @SyntheticMember", 
			"  @SarlSourceCode(\"Scopes.<Address>allParticipants\")", 
			"  default Scope $DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_0() {",
			"    Scope<Address> _allParticipants = Scopes.<Address>allParticipants();",
			"    return _allParticipants;",
			"  }",
			"  ", 
			"  /**", 
			"   * Default value for the parameter paramWithDefault", 
			"   */",
			"  @Pure",
			"  @SyntheticMember", 
			"  @SarlSourceCode(\"true\")", 
			"  default boolean $DEFAULT_VALUE$SECONDEXAMPLEMETHOD_0() {",
			"    return true;",
			"  }",
			"  ", 
			"  @DefaultValueUse(\"io.sarl.lang.tests.bug729.Scope\")", 
			"  @SyntheticMember", 
			"  default void firstExampleMethod() {", 
			"    firstExampleMethod($DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_0());", 
			"  }", 
			"  ", 
			"  @DefaultValueUse(\"boolean\")", 
			"  @SyntheticMember", 
			"  default void secondExampleMethod() {", 
			"    secondExampleMethod($DEFAULT_VALUE$SECONDEXAMPLEMETHOD_0());", 
			"  }", 
			"  ", 
			"  /**",
			"   * @ExcludeFromApidoc",
			"   */",
			"  class ContextAwareCapacityWrapper<C extends Example> extends Capacity.ContextAwareCapacityWrapper<C> implements Example {", 
			"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {", 
			"      super(capacity, caller);", 
			"    }", 
			"    ", 
			"    public void firstExampleMethod(final Scope<Address> scopeWithDefault) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.firstExampleMethod(scopeWithDefault);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void secondExampleMethod(final boolean paramWithDefault) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.secondExampleMethod(paramWithDefault);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void firstExampleMethod() {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.firstExampleMethod();", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void secondExampleMethod() {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.secondExampleMethod();", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"  }", 
			"}", 
			"");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug729",
			"",
			"import io.sarl.lang.core.Address",
			"", 
			"interface Scope<T> { }",
			"class Scopes {",
			"  public static def allParticipants : Scope<T> with T { null }",
			"}",
			"interface Example ", 
			"{", 
			"    def firstExampleMethod(scopeWithDefault : Scope<Address> = Scopes.<Address>allParticipants)", 
			"    def secondExampleMethod(paramWithDefault : String = \"\")",
			"}");

	private final String EXPECTED2 = multilineString(
			"package io.sarl.lang.tests.bug729;",
			"", 
			"import io.sarl.lang.annotation.DefaultValue;", 
			"import io.sarl.lang.annotation.DefaultValueSource;", 
			"import io.sarl.lang.annotation.DefaultValueUse;", 
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSourceCode;", 
			"import io.sarl.lang.annotation.SarlSpecification;", 
			"import io.sarl.lang.annotation.SyntheticMember;", 
			"import io.sarl.lang.core.Address;", 
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"", 
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType("+ SarlPackage.SARL_INTERFACE + ")", 
			"@SuppressWarnings(\"all\")", 
			"public interface Example {", 
			"  @DefaultValueSource", 
			"  void firstExampleMethod(@DefaultValue(\"io.sarl.lang.tests.bug729.Example#FIRSTEXAMPLEMETHOD_0\") final Scope<Address> scopeWithDefault);", 
			"  ", 
			"  @DefaultValueSource", 
			"  void secondExampleMethod(@DefaultValue(\"io.sarl.lang.tests.bug729.Example#SECONDEXAMPLEMETHOD_0\") final String paramWithDefault);", 
			"  ", 
			"  /**", 
			"   * Default value for the parameter scopeWithDefault", 
			"   */", 
			"  @Pure",
			"  @SyntheticMember", 
			"  @SarlSourceCode(\"Scopes.<Address>allParticipants\")", 
			"  default Scope $DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_0() {",
			"    Scope<Address> _allParticipants = Scopes.<Address>allParticipants();",
			"    return _allParticipants;",
			"  }",
			"  ", 
			"  /**", 
			"   * Default value for the parameter paramWithDefault", 
			"   */",
			"  @Pure",
			"  @SyntheticMember", 
			"  @SarlSourceCode(\"\\\"\\\"\")", 
			"  default String $DEFAULT_VALUE$SECONDEXAMPLEMETHOD_0() {",
			"    return \"\";",
			"  }",
			"  ", 
			"  @DefaultValueUse(\"io.sarl.lang.tests.bug729.Scope\")", 
			"  @SyntheticMember", 
			"  default void firstExampleMethod() {", 
			"    firstExampleMethod($DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_0());", 
			"  }", 
			"  ", 
			"  @DefaultValueUse(\"java.lang.String\")", 
			"  @SyntheticMember", 
			"  default void secondExampleMethod() {", 
			"    secondExampleMethod($DEFAULT_VALUE$SECONDEXAMPLEMETHOD_0());", 
			"  }", 
			"}", 
			"");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug729",
			"",
			"import io.sarl.lang.core.Address", 
			"", 
			"capacity Example ", 
			"{", 
			"    def firstExampleMethod(scopeWithDefault : Object = new Object)",
			"    def secondExampleMethod(paramWithDefault : boolean = true)",
			"}");

	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug729",
			"",
			"import java.util.Collections",
			"", 
			"capacity Example ", 
			"{", 
			"    def firstExampleMethod(scopeWithDefault : Object = Collections.emptyList)",
			"    def secondExampleMethod(paramWithDefault : boolean = true)",
			"}");

	private static final String SNIPSET5 = multilineString(
			"package io.sarl.lang.tests.bug729",
			"",
			"import io.sarl.lang.core.Address", 
			"", 
			"interface Scope<T> { }",
			"class Scopes {",
			"  public static def allParticipants : Scope<T> with T { null }",
			"}",
			"capacity Example ", 
			"{", 
			"    def firstExampleMethod(scopeWithDefault : Scope<Address> = Scopes.<Address>allParticipants)",
			"    def firstExampleMethod(a : int, scopeWithDefault : Scope<Address> = Scopes.<Address>allParticipants)",
			"    def secondExampleMethod(paramWithDefault : boolean = true)",
			"}");

	private final String EXPECTED5 = multilineString(
			"package io.sarl.lang.tests.bug729;",
			"", 
			"import io.sarl.lang.annotation.DefaultValue;", 
			"import io.sarl.lang.annotation.DefaultValueSource;", 
			"import io.sarl.lang.annotation.DefaultValueUse;", 
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSourceCode;", 
			"import io.sarl.lang.annotation.SarlSpecification;", 
			"import io.sarl.lang.annotation.SyntheticMember;", 
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.AgentTrait;", 
			"import io.sarl.lang.core.Capacity;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"", 
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType("+ SarlPackage.SARL_CAPACITY + ")", 
			"@SuppressWarnings(\"all\")", 
			"public interface Example extends Capacity {", 
			"  @DefaultValueSource", 
			"  void firstExampleMethod(@DefaultValue(\"io.sarl.lang.tests.bug729.Example#FIRSTEXAMPLEMETHOD_0\") final Scope<Address> scopeWithDefault);", 
			"  ", 
			"  @DefaultValueSource", 
			"  void firstExampleMethod(final int a, @DefaultValue(\"io.sarl.lang.tests.bug729.Example#FIRSTEXAMPLEMETHOD_1\") final Scope<Address> scopeWithDefault);",
			"  ", 
			"  @DefaultValueSource", 
			"  void secondExampleMethod(@DefaultValue(\"io.sarl.lang.tests.bug729.Example#SECONDEXAMPLEMETHOD_0\") final boolean paramWithDefault);", 
			"  ", 
			"  /**", 
			"   * Default value for the parameter scopeWithDefault", 
			"   */", 
			"  @Pure",
			"  @SyntheticMember", 
			"  @SarlSourceCode(\"Scopes.<Address>allParticipants\")", 
			"  default Scope $DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_0() {",
			"    Scope<Address> _allParticipants = Scopes.<Address>allParticipants();",
			"    return _allParticipants;",
			"  }",
			"  ",
			"  /**", 
			"   * Default value for the parameter scopeWithDefault", 
			"   */",
			"  @Pure",
			"  @SyntheticMember", 
			"  @SarlSourceCode(\"Scopes.<Address>allParticipants\")", 
			"  default Scope $DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_1() {",
			"    Scope<Address> _allParticipants = Scopes.<Address>allParticipants();",
			"    return _allParticipants;",
			"  }",
			"  ", 
			"  /**", 
			"   * Default value for the parameter paramWithDefault", 
			"   */",
			"  @Pure",
			"  @SyntheticMember", 
			"  @SarlSourceCode(\"true\")", 
			"  default boolean $DEFAULT_VALUE$SECONDEXAMPLEMETHOD_0() {",
			"    return true;",
			"  }",
			"  ", 
			"  @DefaultValueUse(\"io.sarl.lang.tests.bug729.Scope\")", 
			"  @SyntheticMember", 
			"  default void firstExampleMethod() {", 
			"    firstExampleMethod($DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_0());", 
			"  }", 
			"  ", 
			"  @DefaultValueUse(\"int,io.sarl.lang.tests.bug729.Scope\")", 
			"  @SyntheticMember", 
			"  default void firstExampleMethod(final int a) {", 
			"    firstExampleMethod(a, $DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_1());", 
			"  }", 
			"  ", 
			"  @DefaultValueUse(\"boolean\")", 
			"  @SyntheticMember", 
			"  default void secondExampleMethod() {", 
			"    secondExampleMethod($DEFAULT_VALUE$SECONDEXAMPLEMETHOD_0());", 
			"  }",
			"  ", 
			"  /**",
			"   * @ExcludeFromApidoc",
			"   */",
			"  class ContextAwareCapacityWrapper<C extends Example> extends Capacity.ContextAwareCapacityWrapper<C> implements Example {", 
			"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {", 
			"      super(capacity, caller);", 
			"    }", 
			"    ", 
			"    public void firstExampleMethod(final Scope<Address> scopeWithDefault) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.firstExampleMethod(scopeWithDefault);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void firstExampleMethod(final int a, final Scope<Address> scopeWithDefault) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.firstExampleMethod(a, scopeWithDefault);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void secondExampleMethod(final boolean paramWithDefault) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.secondExampleMethod(paramWithDefault);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void firstExampleMethod() {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.firstExampleMethod();", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void firstExampleMethod(final int a) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.firstExampleMethod(a);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void secondExampleMethod() {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.secondExampleMethod();", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"  }",
			"}", 
			"");

	private static final String SNIPSET6 = multilineString(
			"package io.sarl.lang.tests.bug729",
			"",
			"import io.sarl.lang.core.Address", 
			"", 
			"interface Scope<T> { }",
			"class Scopes {",
			"  public static def allParticipants : Scope<T> with T { null }",
			"}",
			"capacity Example ", 
			"{", 
			"    def firstExampleMethod(scopeWithDefault : Scope<Address> = Scopes.<Address>allParticipants)",
			"    def firstExampleMethod(a : int, scopeWithDefault : Scope<Address> = Scopes.<Address>allParticipants)",
			"    def secondExampleMethod(paramWithDefault : String = new String)",
			"}");

	private final String EXPECTED6 = multilineString(
			"package io.sarl.lang.tests.bug729;",
			"", 
			"import io.sarl.lang.annotation.DefaultValue;", 
			"import io.sarl.lang.annotation.DefaultValueSource;", 
			"import io.sarl.lang.annotation.DefaultValueUse;", 
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSourceCode;", 
			"import io.sarl.lang.annotation.SarlSpecification;", 
			"import io.sarl.lang.annotation.SyntheticMember;", 
			"import io.sarl.lang.core.Address;",
			"import io.sarl.lang.core.AgentTrait;", 
			"import io.sarl.lang.core.Capacity;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"", 
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType("+ SarlPackage.SARL_CAPACITY + ")", 
			"@SuppressWarnings(\"all\")", 
			"public interface Example extends Capacity {", 
			"  @DefaultValueSource", 
			"  void firstExampleMethod(@DefaultValue(\"io.sarl.lang.tests.bug729.Example#FIRSTEXAMPLEMETHOD_0\") final Scope<Address> scopeWithDefault);", 
			"  ", 
			"  @DefaultValueSource", 
			"  void firstExampleMethod(final int a, @DefaultValue(\"io.sarl.lang.tests.bug729.Example#FIRSTEXAMPLEMETHOD_1\") final Scope<Address> scopeWithDefault);",
			"  ", 
			"  @DefaultValueSource", 
			"  void secondExampleMethod(@DefaultValue(\"io.sarl.lang.tests.bug729.Example#SECONDEXAMPLEMETHOD_0\") final String paramWithDefault);", 
			"  ", 
			"  /**", 
			"   * Default value for the parameter scopeWithDefault", 
			"   */", 
			"  @Pure",
			"  @SyntheticMember", 
			"  @SarlSourceCode(\"Scopes.<Address>allParticipants\")", 
			"  default Scope $DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_0() {",
			"    Scope<Address> _allParticipants = Scopes.<Address>allParticipants();",
			"    return _allParticipants;",
			"  }",
			"  ",
			"  /**", 
			"   * Default value for the parameter scopeWithDefault", 
			"   */",
			"  @Pure",
			"  @SyntheticMember", 
			"  @SarlSourceCode(\"Scopes.<Address>allParticipants\")", 
			"  default Scope $DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_1() {",
			"    Scope<Address> _allParticipants = Scopes.<Address>allParticipants();",
			"    return _allParticipants;",
			"  }",
			"  ", 
			"  /**", 
			"   * Default value for the parameter paramWithDefault", 
			"   */",
			"  @Pure",
			"  @SyntheticMember", 
			"  @SarlSourceCode(\"new String\")", 
			"  default String $DEFAULT_VALUE$SECONDEXAMPLEMETHOD_0() {",
			"    String _string = new String();",
			"    return _string;",
			"  }",
			"  ", 
			"  @DefaultValueUse(\"io.sarl.lang.tests.bug729.Scope\")", 
			"  @SyntheticMember", 
			"  default void firstExampleMethod() {", 
			"    firstExampleMethod($DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_0());", 
			"  }", 
			"  ", 
			"  @DefaultValueUse(\"int,io.sarl.lang.tests.bug729.Scope\")", 
			"  @SyntheticMember", 
			"  default void firstExampleMethod(final int a) {", 
			"    firstExampleMethod(a, $DEFAULT_VALUE$FIRSTEXAMPLEMETHOD_1());", 
			"  }", 
			"  ", 
			"  @DefaultValueUse(\"java.lang.String\")", 
			"  @SyntheticMember", 
			"  default void secondExampleMethod() {", 
			"    secondExampleMethod($DEFAULT_VALUE$SECONDEXAMPLEMETHOD_0());", 
			"  }",
			"  ", 
			"  /**",
			"   * @ExcludeFromApidoc",
			"   */",
			"  class ContextAwareCapacityWrapper<C extends Example> extends Capacity.ContextAwareCapacityWrapper<C> implements Example {", 
			"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {", 
			"      super(capacity, caller);", 
			"    }", 
			"    ", 
			"    public void firstExampleMethod(final Scope<Address> scopeWithDefault) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.firstExampleMethod(scopeWithDefault);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void firstExampleMethod(final int a, final Scope<Address> scopeWithDefault) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.firstExampleMethod(a, scopeWithDefault);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void secondExampleMethod(final String paramWithDefault) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.secondExampleMethod(paramWithDefault);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void firstExampleMethod() {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.firstExampleMethod();", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void firstExampleMethod(final int a) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.firstExampleMethod(a);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"    ", 
			"    public void secondExampleMethod() {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.secondExampleMethod();", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"  }",
			"}", 
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET1, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug729.Example");
			assertEquals(EXPECTED1, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET2);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET2, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug729.Example");
			assertEquals(EXPECTED2, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_03() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET3);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET4);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_05() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET5);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_05() throws Exception {
		getCompileHelper().compile(SNIPSET5, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug729.Example");
			assertEquals(EXPECTED5, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_06() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET6);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_06() throws Exception {
		getCompileHelper().compile(SNIPSET6, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug729.Example");
			assertEquals(EXPECTED6, actual);
		});
	}

}

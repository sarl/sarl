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

/** Testing class for issue: Incorrect generic method compilation.
 *
 * <p>https://github.com/sarl/sarl/issues/720
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #720")
@SuppressWarnings("all")
@Tag("core")
public class Bug720Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug720",
			"capacity ExampleCapacity", 
			"{", 
			"    def exampleMethod(clazz : Class<T>) with T", 
			"}");

	private final String EXPECTED1 = multilineString(
			"package io.sarl.lang.tests.bug720;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSpecification;", 
			"import io.sarl.lang.core.AgentTrait;", 
			"import io.sarl.lang.core.Capacity;", 
			"", 
			"@FunctionalInterface", 
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")", 
			"@SuppressWarnings(\"all\")", 
			"public interface ExampleCapacity extends Capacity {", 
			"  <T extends Object> void exampleMethod(final Class<T> clazz);", 
			"  ", 
			"  /**",
			"   * @ExcludeFromApidoc",
			"   */",
			"  class ContextAwareCapacityWrapper<C extends ExampleCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements ExampleCapacity {", 
			"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {", 
			"      super(capacity, caller);", 
			"    }", 
			"    ", 
			"    public <T extends Object> void exampleMethod(final Class<T> clazz) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.exampleMethod(clazz);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"  }", 
			"}",
			"");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug720",
			"capacity ExampleCapacity", 
			"{", 
			"    def exampleMethod(clazz : Class<T>) with T extends Number", 
			"}");

	private final String EXPECTED2 = multilineString(
			"package io.sarl.lang.tests.bug720;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSpecification;", 
			"import io.sarl.lang.core.AgentTrait;", 
			"import io.sarl.lang.core.Capacity;", 
			"", 
			"@FunctionalInterface", 
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")", 
			"@SuppressWarnings(\"all\")", 
			"public interface ExampleCapacity extends Capacity {", 
			"  <T extends Number> void exampleMethod(final Class<T> clazz);", 
			"  ", 
			"  /**",
			"   * @ExcludeFromApidoc",
			"   */",
			"  class ContextAwareCapacityWrapper<C extends ExampleCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements ExampleCapacity {", 
			"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {", 
			"      super(capacity, caller);", 
			"    }", 
			"    ", 
			"    public <T extends Number> void exampleMethod(final Class<T> clazz) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.exampleMethod(clazz);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"  }", 
			"}",
			"");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug720",
			"capacity ExampleCapacity", 
			"{", 
			"    def exampleMethod(clazz : Class<T>, clazz2 : T2) with T extends Number, T2 extends T", 
			"}");

	private final String EXPECTED3 = multilineString(
			"package io.sarl.lang.tests.bug720;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSpecification;", 
			"import io.sarl.lang.core.AgentTrait;", 
			"import io.sarl.lang.core.Capacity;", 
			"", 
			"@FunctionalInterface", 
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")", 
			"@SuppressWarnings(\"all\")", 
			"public interface ExampleCapacity extends Capacity {", 
			"  <T extends Number, T2 extends T> void exampleMethod(final Class<T> clazz, final T2 clazz2);", 
			"  ", 
			"  /**",
			"   * @ExcludeFromApidoc",
			"   */",
			"  class ContextAwareCapacityWrapper<C extends ExampleCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements ExampleCapacity {", 
			"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {", 
			"      super(capacity, caller);", 
			"    }", 
			"    ", 
			"    public <T extends Number, T2 extends T> void exampleMethod(final Class<T> clazz, final T2 clazz2) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.exampleMethod(clazz, clazz2);", 
			"      } finally {", 
			"        resetCallerInLocalThread();", 
			"      }", 
			"    }", 
			"  }", 
			"}",
			"");

	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug720",
			"capacity ExampleCapacity", 
			"{", 
			"    def exampleMethod(plan : (T) => void) with T", 
			"}");

	private final String EXPECTED4 = multilineString(
			"package io.sarl.lang.tests.bug720;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSpecification;", 
			"import io.sarl.lang.core.AgentTrait;", 
			"import io.sarl.lang.core.Capacity;",
			"import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;",
			"", 
			"@FunctionalInterface", 
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")", 
			"@SuppressWarnings(\"all\")", 
			"public interface ExampleCapacity extends Capacity {", 
			"  <T extends Object> void exampleMethod(final Procedure1<? super T> plan);", 
			"  ", 
			"  /**",
			"   * @ExcludeFromApidoc",
			"   */",
			"  class ContextAwareCapacityWrapper<C extends ExampleCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements ExampleCapacity {", 
			"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {", 
			"      super(capacity, caller);", 
			"    }", 
			"    ", 
			"    public <T extends Object> void exampleMethod(final Procedure1<? super T> plan) {", 
			"      try {", 
			"        ensureCallerInLocalThread();", 
			"        this.capacity.exampleMethod(plan);", 
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
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug720.ExampleCapacity");
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
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug720.ExampleCapacity");
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
	@Tag("compileToJava")
	public void compiling_03() throws Exception {
		getCompileHelper().compile(SNIPSET3, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug720.ExampleCapacity");
			assertEquals(EXPECTED3, actual);
		});
	}

	@Test
	@Tag("sarlValidation")
	public void parsing_04() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET4);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_04() throws Exception {
		getCompileHelper().compile(SNIPSET4, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug720.ExampleCapacity");
			assertEquals(EXPECTED4, actual);
		});
	}

}

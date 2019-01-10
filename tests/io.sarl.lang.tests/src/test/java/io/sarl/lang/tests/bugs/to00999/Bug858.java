/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.core.Event;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Invalid inlining of isFromMe.
 *
 * <p>https://github.com/sarl/sarl/issues/858
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/858"
 */
@SuppressWarnings("all")
public class Bug858 extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug858",
			"import org.eclipse.xtext.xbase.lib.Inline",
			"import io.sarl.lang.core.Event",
			"import foo.BasicInlineTest",
			"event MyEvent",
			"class SomeType extends BasicInlineTest {",
			"  def fct(o : Event) : boolean {",
			"    o.isFromMe",
			"  }",
			"}");

	private static final String EXPECTED01 = multilineString(
			"package io.sarl.lang.tests.bug858;",
			"",
			"import foo.BasicInlineTest;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Event;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeType extends BasicInlineTest {",
			"  @Pure",
			"  public boolean fct(final Event o) {",
			"    return (o != null && this.isMe(o.getSource()));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeType() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(SNIPSET01);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug858.SomeType");
			assertEquals(EXPECTED01, actual);
		});
	}

	private static final String SNIPSET02 = multilineString(
			"package io.sarl.lang.tests.bug858",
			"import org.eclipse.xtext.xbase.lib.Inline",
			"import io.sarl.lang.core.Event",
			"import foo.BasicInlineTest",
			"event MyEvent",
			"class SomeType extends BasicInlineTest {",
			"  def fct(o : Event) : boolean {",
			"    o.isFromMe2",
			"  }",
			"}");

	private static final String EXPECTED02 = multilineString(
			"package io.sarl.lang.tests.bug858;",
			"",
			"import foo.BasicInlineTest;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Event;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeType extends BasicInlineTest {",
			"  @Pure",
			"  public boolean fct(final Event o) {",
			"    return (o != null && this.isMe2(o.getSource()));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeType() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(SNIPSET02);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug858.SomeType");
			assertEquals(EXPECTED02, actual);
		});
	}

	private static final String SNIPSET03 = multilineString(
			"package io.sarl.lang.tests.bug858",
			"import org.eclipse.xtext.xbase.lib.Inline",
			"import io.sarl.lang.core.Event",
			"import foo.BasicInlineTest",
			"event MyEvent",
			"class SomeType extends BasicInlineTest {",
			"  def fct(o : Event) : boolean {",
			"    o.isFromMe3",
			"  }",
			"}");

	private static final String EXPECTED03 = multilineString(
			"package io.sarl.lang.tests.bug858;",
			"",
			"import foo.BasicInlineTest;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Event;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeType extends BasicInlineTest {",
			"  @Pure",
			"  public boolean fct(final Event o) {",
			"    return (o != null && o.getSource().getUUID() != null);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeType() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void parsing_03() throws Exception {
		SarlScript mas = file(SNIPSET03);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_03() throws Exception {
		getCompileHelper().compile(SNIPSET03, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug858.SomeType");
			assertEquals(EXPECTED03, actual);
		});
	}

}


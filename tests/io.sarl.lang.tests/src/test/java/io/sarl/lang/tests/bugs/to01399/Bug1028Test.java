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

import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Bug in java code generation for inline
 *  innner class definition within a object constructor.
 *
 * <p>https://github.com/sarl/sarl/issues/1028
 * 
 * <p>See similar tests in the Janus SRE.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1028"
 */
@DisplayName("Bug #1028")
@SuppressWarnings("all")
@Tag("core")
public class Bug1028Test extends AbstractSarlTest {

	private final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1028",
			"import java.util.EventListener",
			"class Bug1028Class {",
			"  var object : Object",
			"  var listener : EventListener",
			"  new {",
			"    super();",
			"    var defaultSpacesListener : EventListener = new EventListener() {",
			"    };",
			"    this.object = new Object();",
			"    this.listener = defaultSpacesListener;",
			"  }",
			"}");
	
	private final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1028;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.EventListener;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class Bug1028Class {",
			"  private Object object;",
			"  ",
			"  private EventListener listener;",
			"  ",
			"  public Bug1028Class() {",
			"    super();",
			"    EventListener defaultSpacesListener = new EventListener() {",
			"    };",
			"    Object _object = new Object();",
			"    this.object = _object;",
			"    this.listener = defaultSpacesListener;",
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
			"}",
			"");

	@Test
	@DisplayName("Basic parsing")
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Basic transformation")
	@Tag("compileToJava")
	public void compiling01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1028.Bug1028Class");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

}

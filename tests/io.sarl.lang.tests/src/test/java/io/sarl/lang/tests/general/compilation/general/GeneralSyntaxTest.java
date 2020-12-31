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
@DisplayName("Compilation: general syntax")
@Tag("core")
@Tag("compileToJava")
public class GeneralSyntaxTest extends AbstractSarlTest {

	@Test
	public void noParamNoReturnActionInClass() throws Exception {
		String source = multilineString(
				"abstract class Light {",
				"	def turnOn",
				"	def turnOff",
				"}",
				"");
		String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class Light {",
				"  public abstract void turnOn();",
				"  ",
				"  public abstract void turnOff();",
				"  ",
				"  @SyntheticMember",
				"  public Light() {",
				"    super();",
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void noParamNoReturnActionInInterface() throws Exception {
		String source = multilineString(
				"interface Light {",
				"	def turnOn",
				"	def turnOff",
				"}",
				"");
		String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
				"@SuppressWarnings(\"all\")",
				"public interface Light {",
				"  void turnOn();",
				"  ",
				"  void turnOff();",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
	}

}

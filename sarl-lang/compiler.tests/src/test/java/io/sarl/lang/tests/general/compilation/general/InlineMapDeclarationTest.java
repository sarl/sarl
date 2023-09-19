/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 */
@SuppressWarnings("all")
@DisplayName("Compilation: inline map")
@Tag("core")
@Tag("compileToJava")
public class InlineMapDeclarationTest extends AbstractSarlTest {

	@Test
	public void mapDeclaration() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  static val coefs = #{'I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000}",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import java.util.Collections;",
				"import java.util.Map;",
				"import org.eclipse.xtext.xbase.lib.CollectionLiterals;",
				"import org.eclipse.xtext.xbase.lib.Pair;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  private static final Map<String, Integer> coefs = Collections.<String, Integer>unmodifiableMap(CollectionLiterals.<String, Integer>newHashMap(Pair.<String, Integer>of(\"I\", Integer.valueOf(1)), Pair.<String, Integer>of(\"V\", Integer.valueOf(5)), Pair.<String, Integer>of(\"X\", Integer.valueOf(10)), Pair.<String, Integer>of(\"L\", Integer.valueOf(50)), Pair.<String, Integer>of(\"C\", Integer.valueOf(100)), Pair.<String, Integer>of(\"D\", Integer.valueOf(500)), Pair.<String, Integer>of(\"M\", Integer.valueOf(1000))));",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

}

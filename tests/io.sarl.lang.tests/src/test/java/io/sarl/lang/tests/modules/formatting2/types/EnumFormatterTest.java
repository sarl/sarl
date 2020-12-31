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
package io.sarl.lang.tests.modules.formatting2.types;

import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.tests.modules.formatting2.AbstractFormatterTest;

/** Tests for formatting enumerations.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("format: enum")
@Tag("core")
@Tag("codeFormat")
public class EnumFormatterTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	public class FormatterAPITest extends AbstractFormatterTest {

		@Test
		public void empty() throws Exception {
			String source = "enum  EntityX{CONST}";
			String expected = multilineString(
					"enum EntityX {",
					"	CONST",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void modifiers() throws Exception {
			String source = "public    static    enum EntityX{CONST}";
			String expected = multilineString(
					"public static enum EntityX {",
					"	CONST",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void twoAnnotations() throws Exception {
			String source = "@Pure@Beta    enum EntityX{CONST}";
			String expected = multilineString(
					"@Pure @Beta enum EntityX {",
					"	CONST",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void threeAnnotations() throws Exception {
			String source = multilineString(
					"@Pure@Beta",
					"@Hello    enum EntityX{CONST}");
			String expected = multilineString(
					"@Pure @Beta",
					"@Hello enum EntityX {",
					"	CONST",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationValue() throws Exception {
			String source = "@SuppressWarnings(        value= \"name\"   )enum EntityX{CONST}";
			String expected = multilineString(
					"@SuppressWarnings(value = \"name\") enum EntityX {",
					"	CONST",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationImplicitValue() throws Exception {
			String source = "@SuppressWarnings(   \"name\"   )enum EntityX{CONST}";
			String expected = multilineString(
					"@SuppressWarnings(\"name\") enum EntityX {",
					"	CONST",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void multipleLiterals() throws Exception {
			String source = "enum EntityX{CONST1,CONST2,CONST3}";
			String expected = multilineString(
					"enum EntityX {",
					"	CONST1,",
					"	CONST2,",
					"	CONST3",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment1() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"* That's the second line.",
					"*/enum A{CONST1}");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"enum A {",
					"\tCONST1",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment2() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"That's the second line.*/enum A{CONST1}");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"enum A {",
					"\tCONST1",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment3() throws Exception {
			String source = "/*      Hello world.      */enum A{CONST1}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"enum A {",
					"\tCONST1",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment4() throws Exception {
			String source = "/*      Hello world.      */enum A{/*Second comment*/CONST1}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"enum A {",
					"",
					"\t/* Second comment",
					"\t */",
					"\tCONST1",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlJavaComment() throws Exception {
			String source = multilineString(
					"/**Hello world.",
					"That's the second line.*/enum A{CONST1}");
			String expected = multilineString(
					"/** Hello world.",
					" * That's the second line.",
					" */",
					"enum A {",
					"\tCONST1",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment() throws Exception {
			String source = multilineString(
					"",
					"//Hello world.",
					"enum A{CONST1}");
			String expected = multilineString(
					"// Hello world.",
					"enum A {",
					"\tCONST1",
					"}",
					"");
			assertFormatted(source, expected);
		}

	}

}

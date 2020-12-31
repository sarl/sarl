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

/** Tests for formatting capacities.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("format: capacity")
@Tag("core")
@Tag("codeFormat")
public class CapacityFormatterTest {

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
			String source = "capacity  EntityX{}";
			String expected = multilineString(
					"capacity EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void modifiers() throws Exception {
			String source = "public    static    capacity EntityX{}";
			String expected = multilineString(
					"public static capacity EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void twoAnnotations() throws Exception {
			String source = "@Pure@Beta    capacity EntityX{}";
			String expected = multilineString(
					"@Pure @Beta capacity EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void threeAnnotations() throws Exception {
			String source = multilineString(
					"@Pure@Beta",
					"@Hello    capacity EntityX{}");
			String expected = multilineString(
					"@Pure @Beta",
					"@Hello capacity EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationValue() throws Exception {
			String source = "@SuppressWarnings(        value= \"name\"   )capacity EntityX{}";
			String expected = multilineString(
					"@SuppressWarnings(value = \"name\") capacity EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationImplicitValue() throws Exception {
			String source = "@SuppressWarnings(   \"name\"   )capacity EntityX{}";
			String expected = multilineString(
					"@SuppressWarnings(\"name\") capacity EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void extend() throws Exception {
			String source = "capacity EntityX    extends    EntityY {}";
			String expected = multilineString(
					"capacity EntityX extends EntityY {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void twoExtends() throws Exception {
			String source = "capacity EntityX    extends    EntityY,   EntityZ{}";
			String expected = multilineString(
					"capacity EntityX extends EntityY, EntityZ {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void memberIndent() throws Exception {
			String source = "capacity EntityX{def x:int}";
			String expected = multilineString(
					"capacity EntityX {",
					"	def x : int",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment1() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"*     That's the second line.",
					"*/capacity A{}");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"capacity A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment2() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"That's the second line.*/capacity A{}");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"capacity A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment3() throws Exception {
			String source = "/*     Hello world.     */capacity A{}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"capacity A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment4() throws Exception {
			String source = "/*     Hello world.     */capacity A{/*Second comment*/}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"capacity A {",
					"",
					"\t/* Second comment",
					"\t */",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlJavaComment() throws Exception {
			String source = multilineString(
					"/**Hello world.",
					"That's the second line.*/capacity A{}");
			String expected = multilineString(
					"/** Hello world.",
					" * That's the second line.",
					" */",
					"capacity A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment1() throws Exception {
			String source = multilineString(
					"",
					"//Hello world.",
					"capacity A{}");
			String expected = multilineString(
					"// Hello world.",
					"capacity A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment2() throws Exception {
			String source = multilineString(
					"",
					"//",
					"capacity A{}");
			String expected = multilineString(
					"//",
					"capacity A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment3() throws Exception {
			String source = multilineString(
					"",
					"//      ",
					"capacity A{}");
			String expected = multilineString(
					"//",
					"capacity A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment4() throws Exception {
			String source = multilineString(
					"",
					"//      Hello world.",
					"capacity A{}");
			String expected = multilineString(
					"// Hello world.",
					"capacity A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment5() throws Exception {
			String source = multilineString(
					"",
					"// Hello world.",
					"capacity A{}");
			String expected = multilineString(
					"// Hello world.",
					"capacity A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

	}

}

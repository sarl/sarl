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
@DisplayName("format: annotation")
@Tag("core")
@Tag("codeFormat")
public class AnnotationTypeFormatterTest {

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
			String source = "annotation  EntityX{}";
			String expected = multilineString(
					"annotation EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void modifiers() throws Exception {
			String source = "public    static    annotation EntityX{}";
			String expected = multilineString(
					"public static annotation EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void twoAnnotations() throws Exception {
			String source = "@Pure@Beta    annotation EntityX{}";
			String expected = multilineString(
					"@Pure @Beta annotation EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void threeAnnotations() throws Exception {
			String source = multilineString(
					"@Pure@Beta",
					"@Hello    annotation EntityX{}");
			String expected = multilineString(
					"@Pure @Beta",
					"@Hello annotation EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationValue() throws Exception {
			String source = "@SuppressWarnings(        value= \"name\"   )annotation EntityX{}";
			String expected = multilineString(
					"@SuppressWarnings(value = \"name\") annotation EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationImplicitValue() throws Exception {
			String source = "@SuppressWarnings(   \"name\"   )annotation EntityX{}";
			String expected = multilineString(
					"@SuppressWarnings(\"name\") annotation EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void memberIndent() throws Exception {
			String source = "annotation EntityX{var value:int}";
			String expected = multilineString(
					"annotation EntityX {",
					"	var value : int",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment1() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"* That's the second line.",
					"*/annotation A{}");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"annotation A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment2() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"That's the second line.*/annotation A{}");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"annotation A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment3() throws Exception {
			String source = "/*     Hello world.     */annotation A{}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"annotation A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment4() throws Exception {
			String source = "/*     Hello world.     */annotation A{/*Second comment*/}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"annotation A {",
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
					"That's the second line.*/annotation A{}");
			String expected = multilineString(
					"/** Hello world.",
					" * That's the second line.",
					" */",
					"annotation A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment() throws Exception {
			String source = multilineString(
					"",
					"//Hello world.",
					"annotation A{}");
			String expected = multilineString(
					"// Hello world.",
					"annotation A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

	}

}

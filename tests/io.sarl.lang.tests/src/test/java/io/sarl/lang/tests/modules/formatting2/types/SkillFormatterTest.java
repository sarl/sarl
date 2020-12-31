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

/** Tests for formatting skills.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("format: skill")
@Tag("core")
@Tag("codeFormat")
public class SkillFormatterTest {

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
			String source = "skill  EntityX{}";
			String expected = multilineString(
					"skill EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void modifiers() throws Exception {
			String source = "public    static    skill EntityX{}";
			String expected = multilineString(
					"public static skill EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void twoAnnotations() throws Exception {
			String source = "@Pure@Beta    skill EntityX{}";
			String expected = multilineString(
					"@Pure @Beta skill EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void threeAnnotations() throws Exception {
			String source = multilineString(
					"@Pure@Beta",
					"@Hello    skill EntityX{}");
			String expected = multilineString(
					"@Pure @Beta",
					"@Hello skill EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationValue() throws Exception {
			String source = "@SuppressWarnings(        value= \"name\"   )skill EntityX{}";
			String expected = multilineString(
					"@SuppressWarnings(value = \"name\") skill EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationImplicitValue() throws Exception {
			String source = "@SuppressWarnings(   \"name\"   )skill EntityX{}";
			String expected = multilineString(
					"@SuppressWarnings(\"name\") skill EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void extend() throws Exception {
			String source = "skill EntityX    extends    EntityY {}";
			String expected = multilineString(
					"skill EntityX extends EntityY {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void oneImplement() throws Exception {
			String source = "skill EntityX    implements    EntityY {}";
			String expected = multilineString(
					"skill EntityX implements EntityY {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void twoImplements() throws Exception {
			String source = "skill EntityX    implements    EntityY  ,    EntityZ {}";
			String expected = multilineString(
					"skill EntityX implements EntityY, EntityZ {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void memberIndent() throws Exception {
			String source = "skill EntityX{var x:int}";
			String expected = multilineString(
					"skill EntityX {",
					"	var x : int",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment1() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"* That's the second line.",
					"*/skill A implements B{}");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"skill A implements B {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment2() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"That's the second line.*/skill A implements B{}");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"skill A implements B {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment3() throws Exception {
			String source = "/*     Hello world.     */skill A implements B{}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"skill A implements B {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment4() throws Exception {
			String source = "/*     Hello world.     */skill A implements B{/*Second comment*/}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"skill A implements B {",
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
					"That's the second line.*/skill A implements B{}");
			String expected = multilineString(
					"/** Hello world.",
					" * That's the second line.",
					" */",
					"skill A implements B {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment() throws Exception {
			String source = multilineString(
					"",
					"//Hello world.",
					"skill A implements B{}");
			String expected = multilineString(
					"// Hello world.",
					"skill A implements B {",
					"}",
					"");
			assertFormatted(source, expected);
		}

	}

}

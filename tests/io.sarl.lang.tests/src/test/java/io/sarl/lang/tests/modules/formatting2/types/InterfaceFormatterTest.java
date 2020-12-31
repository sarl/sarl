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

/** Tests for formatting interfaces.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("format: interface")
@Tag("core")
@Tag("codeFormat")
public class InterfaceFormatterTest {

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
			String source = "interface  EntityX{}";
			String expected = multilineString(
					"interface EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void modifiers() throws Exception {
			String source = "public    static    interface EntityX{}";
			String expected = multilineString(
					"public static interface EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void twoAnnotations() throws Exception {
			String source = "@Pure@Beta    interface EntityX{}";
			String expected = multilineString(
					"@Pure @Beta interface EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void threeAnnotations() throws Exception {
			String source = multilineString(
					"@Pure@Beta",
					"@Hello    interface EntityX{}");
			String expected = multilineString(
					"@Pure @Beta",
					"@Hello interface EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationValue() throws Exception {
			String source = "@SuppressWarnings(        value= \"name\"   )interface EntityX{}";
			String expected = multilineString(
					"@SuppressWarnings(value = \"name\") interface EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationImplicitValue() throws Exception {
			String source = "@SuppressWarnings(   \"name\"   )interface EntityX{}";
			String expected = multilineString(
					"@SuppressWarnings(\"name\") interface EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void typeParameter() throws Exception {
			String source = "interface EntityX    <  T   > {}";
			String expected = multilineString(
					"interface EntityX<T> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void typeParameters() throws Exception {
			String source = "interface EntityX    <  T , TT   extends T  > {}";
			String expected = multilineString(
					"interface EntityX<T, TT extends T> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void extend() throws Exception {
			String source = "interface EntityX    extends    EntityY {}";
			String expected = multilineString(
					"interface EntityX extends EntityY {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void extendGeneric() throws Exception {
			String source = "interface EntityX    extends    EntityY <   Boolean > {}";
			String expected = multilineString(
					"interface EntityX extends EntityY<Boolean> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void genericExtendGeneric() throws Exception {
			String source = "interface EntityX  < T   >    extends    EntityY <   T >{}";
			String expected = multilineString(
					"interface EntityX<T> extends EntityY<T> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void twoExtends() throws Exception {
			String source = "interface EntityX    extends    EntityY,   EntityZ{}";
			String expected = multilineString(
					"interface EntityX extends EntityY, EntityZ {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void twoExtendsGeneric() throws Exception {
			String source = "interface EntityX    extends    EntityY <   Boolean >,   EntityZ{}";
			String expected = multilineString(
					"interface EntityX extends EntityY<Boolean>, EntityZ {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void genericTwoExtendsGeneric() throws Exception {
			String source = "interface EntityX  < T   >    extends    EntityY <   T >,   EntityZ{}";
			String expected = multilineString(
					"interface EntityX<T> extends EntityY<T>, EntityZ {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void memberIndent() throws Exception {
			String source = "interface EntityX{def x:int}";
			String expected = multilineString(
					"interface EntityX {",
					"	def x : int",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment1() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"* That's the second line.",
					"*/interface A{}");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"interface A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment2() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"That's the second line.*/interface A{}");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"interface A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment3() throws Exception {
			String source = "/*     Hello world.     */interface A{}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"interface A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment4() throws Exception {
			String source = "/*     Hello world.     */interface A{/*Second comment*/}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"interface A {",
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
					"That's the second line.*/interface A{}");
			String expected = multilineString(
					"/** Hello world.",
					" * That's the second line.",
					" */",
					"interface A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment() throws Exception {
			String source = multilineString(
					"",
					"//Hello world.",
					"interface A{}");
			String expected = multilineString(
					"// Hello world.",
					"interface A {",
					"}",
					"");
			assertFormatted(source, expected);
		}

	}

}

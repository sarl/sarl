/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

/** Tests for formatting events.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("format: event")
@Tag("core")
@Tag("codeFormat")
public class EventFormatterTest {

	@Nested
	@DisplayName("Without type parameter")
	public class WithoutTypeParameter extends AbstractFormatterTest {

		@Test
		@DisplayName("event {}")
		public void empty() throws Exception {
			String source = "event  EntityX{}";
			String expected = multilineString(
					"event EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("public static event {}")
		public void modifiers() throws Exception {
			String source = "public    static    event EntityX{}";
			String expected = multilineString(
					"public static event EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("@Pure@Beta event {}")
		public void twoAnnotations() throws Exception {
			String source = "@Pure@Beta    event EntityX{}";
			String expected = multilineString(
					"@Pure @Beta event EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("@Pure@Beta@Hello event {}")
		public void threeAnnotations() throws Exception {
			String source = multilineString(
					"@Pure@Beta",
					"@Hello    event EntityX{}");
			String expected = multilineString(
					"@Pure @Beta",
					"@Hello event EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("@SuppressWarnings(v=x) event {}")
		public void annotationValue() throws Exception {
			String source = "@SuppressWarnings(        value= \"name\"   )event EntityX{}";
			String expected = multilineString(
					"@SuppressWarnings(value = \"name\") event EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("@SuppressWarnings(x) event {}")
		public void annotationImplicitValue() throws Exception {
			String source = "@SuppressWarnings(   \"name\"   )event EntityX{}";
			String expected = multilineString(
					"@SuppressWarnings(\"name\") event EntityX {",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("event extends {}")
		public void extend() throws Exception {
			String source = "event EntityX    extends    EntityY {}";
			String expected = multilineString(
					"event EntityX extends EntityY {",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("event {int}")
		public void memberIndent() throws Exception {
			String source = "event EntityX{var x:int}";
			String expected = multilineString(
					"event EntityX {",
					"	var x : int",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("/*...*/ event")
		public void mlStandardComment1() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"* That's the second line.",
					"*/event A");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"event A",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("/*..*/ event")
		public void mlStandardComment2() throws Exception {
			String source = multilineString(
					"/*Hello world.",
					"That's the second line.*/event A");
			String expected = multilineString(
					"/* Hello world.",
					" * That's the second line.",
					" */",
					"event A",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("/*.*/ event")
		public void mlStandardComment3() throws Exception {
			String source = "/*     Hello world.     */event A";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"event A",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("/*.*/ event {/*.*/}")
		public void mlStandardComment4() throws Exception {
			String source = "/*     Hello world.     */event A{/*Second comment*/}";
			String expected = multilineString(
					"/* Hello world.",
					" */",
					"event A {",
					"",
					"\t/* Second comment",
					"\t */",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("/**..*/ event")
		public void mlJavaComment() throws Exception {
			String source = multilineString(
					"/**Hello world.",
					"That's the second line.*/event A");
			String expected = multilineString(
					"/** Hello world.",
					" * That's the second line.",
					" */",
					"event A",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("// event")
		public void slComment() throws Exception {
			String source = multilineString(
					"",
					"//Hello world.",
					"event A");
			String expected = multilineString(
					"// Hello world.",
					"event A",
					"");
			assertFormatted(source, expected);
		}

	}

	@Nested
	@DisplayName("With type parameters")
	public class WithTypeParameter extends AbstractFormatterTest {

		@Test
		@DisplayName("event<A> {}")
		public void empty_0() throws Exception {
			String source = "event  EntityX<A>{}";
			String expected = multilineString(
					"event EntityX<A> {",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("event<A extends Number> {}")
		public void empty_1() throws Exception {
			String source = "event  EntityX<A extends Number>{}";
			String expected = multilineString(
					"event EntityX<A extends Number> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		@DisplayName("event<A extends Number, B> {}")
		public void empty_2() throws Exception {
			String source = "event  EntityX<A extends Number,B>{}";
			String expected = multilineString(
					"event EntityX<A extends Number, B> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		@DisplayName("event<A extends Number, B extends String> {}")
		public void empty_3() throws Exception {
			String source = "event  EntityX<A extends Number,B extends String>{}";
			String expected = multilineString(
					"event EntityX<A extends Number, B extends String> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		@DisplayName("event<A, B extends String> {}")
		public void empty_4() throws Exception {
			String source = "event  EntityX<A,B extends String>{}";
			String expected = multilineString(
					"event EntityX<A, B extends String> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		@DisplayName("event<A, B> {}")
		public void empty_5() throws Exception {
			String source = "event  EntityX<A,B>{}";
			String expected = multilineString(
					"event EntityX<A, B> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		@DisplayName("public static event<A> {}")
		public void modifiers_0() throws Exception {
			String source = "public    static    event EntityX<A>{}";
			String expected = multilineString(
					"public static event EntityX<A> {",
					"}",
					"");
			assertFormatted(source, expected);
		}
	
		@Test
		@DisplayName("public static event<A extends String> {}")
		public void modifiers_1() throws Exception {
			String source = "public    static    event EntityX<A extends String>{}";
			String expected = multilineString(
					"public static event EntityX<A extends String> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		@DisplayName("public static event<A extends String, B> {}")
		public void modifiers_2() throws Exception {
			String source = "public    static    event EntityX<A extends String,B>{}";
			String expected = multilineString(
					"public static event EntityX<A extends String, B> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		@DisplayName("public static event<A extends String, B extends Number> {}")
		public void modifiers_3() throws Exception {
			String source = "public    static    event EntityX<A extends String,B extends Number>{}";
			String expected = multilineString(
					"public static event EntityX<A extends String, B extends Number> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		@DisplayName("public static event<A, B extends Number> {}")
		public void modifiers_4() throws Exception {
			String source = "public    static    event EntityX<A,B extends Number>{}";
			String expected = multilineString(
					"public static event EntityX<A, B extends Number> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

		@Test
		@DisplayName("public static event<A, B> {}")
		public void modifiers_5() throws Exception {
			String source = "public    static    event EntityX<A,B>{}";
			String expected = multilineString(
					"public static event EntityX<A, B> {",
					"}",
					"");
			assertFormatted(source, expected);
		}

	}

}

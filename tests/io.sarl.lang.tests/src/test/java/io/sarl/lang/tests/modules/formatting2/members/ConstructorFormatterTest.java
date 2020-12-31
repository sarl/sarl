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
package io.sarl.lang.tests.modules.formatting2.members;

import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.tests.modules.formatting2.AbstractMemberFormatterTest;

/** Tests for formatting constructors.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("format: new")
@Tag("core")
@Tag("codeFormat")
public class ConstructorFormatterTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	public class FormatterAPITest extends AbstractMemberFormatterTest {

		@Test
		public void noParam() throws Exception {
			String source = unformattedCode("new {System.out.println(\"abc\")}");
			String expected = formattedCode(
					"	new {",
					"		System.out.println(\"abc\")",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void param() throws Exception {
			String source = unformattedCode("new(a:int){System.out.println(a)}");
			String expected = formattedCode(
					"	new(a : int) {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void params() throws Exception {
			String source = unformattedCode("new(a:int,b:boolean){System.out.println(a)}");
			String expected = formattedCode(
					"	new(a : int, b : boolean) {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void defaultValuedParam() throws Exception {
			String source = unformattedCode("new(a:int=5){System.out.println(a)}");
			String expected = formattedCode(
					"	new(a : int = 5) {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void defaultValuedParams() throws Exception {
			String source = unformattedCode("new(a:int=5,b:boolean=false){System.out.println(a)}");
			String expected = formattedCode(
					"	new(a : int = 5, b : boolean = false) {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void varArg() throws Exception {
			String source = unformattedCode("new(a:int  * ){System.out.println(a)}");
			String expected = formattedCode(
					"	new(a : int*) {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void javaGeneric() throws Exception {
			String source = unformattedCode("new<T>(a:T){System.out.println(a)}");
			String expected = formattedCode(
					"	new <T> (a : T) {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void javaGenerics() throws Exception {
			String source = unformattedCode("new<T,TT extends T>(a:T,b:TT){System.out.println(a)}");
			String expected = formattedCode(
					"	new <T, TT extends T> (a : T, b : TT) {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void sarlGeneric() throws Exception {
			String source = unformattedCode("new(a:T)with T{System.out.println(a)}");
			String expected = formattedCode(
					"	new(a : T) with T {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void sarlGenerics() throws Exception {
			String source = unformattedCode("new(a:T,b:TT)with T,TT extends T{System.out.println(a)}");
			String expected = formattedCode(
					"	new(a : T, b : TT) with T, TT extends T {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void exceptionNoParam() throws Exception {
			String source = unformattedCode("new throws Exception{System.out.println(a)}");
			String expected = formattedCode(
					"	new throws Exception {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void exceptionsNoParam() throws Exception {
			String source = unformattedCode("new throws Exception,Exception{System.out.println(a)}");
			String expected = formattedCode(
					"	new throws Exception, Exception {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void exceptionEmptyParam() throws Exception {
			String source = unformattedCode("new()throws Exception{System.out.println(a)}");
			String expected = formattedCode(
					"	new() throws Exception {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void exceptionsEmptyParam() throws Exception {
			String source = unformattedCode("new()throws Exception,Exception{System.out.println(a)}");
			String expected = formattedCode(
					"	new() throws Exception, Exception {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void modifiers() throws Exception {
			String source = unformattedCode("public    new {System.out.println(a)}");
			String expected = formattedCode(
					"	public new {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void annotation() throws Exception {
			String source = unformattedCode("@Annotation new{System.out.println(a)}");
			String expected = formattedCode(
					"	@Annotation new {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void annotations() throws Exception {
			String source = unformattedCode("@Annotation@Beta new{System.out.println(a)}");
			String expected = formattedCode(
					"	@Annotation @Beta new {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationValue() throws Exception {
			String source = unformattedCode("@SuppressWarnings(\"name\")new{System.out.println(a)}");
			String expected = formattedCode(
					"	@SuppressWarnings(\"name\") new {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment1() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"* That's the second line.",
					"*/new{System.out.println(a)}"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tnew {",
					"\t\tSystem.out.println(a)",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment2() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/new{System.out.println(a)}"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tnew {",
					"\t\tSystem.out.println(a)",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment3() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/new{System.out.println(a)}/*Second comment.*/new(a:int){System.out.println(a)}"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tnew {",
					"\t\tSystem.out.println(a)",
					"\t}",
					"",
					"\t/* Second comment.",
					"\t */",
					"\tnew(a : int) {",
					"\t\tSystem.out.println(a)",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment4() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/new{System.out.println(a)}/*Second comment.*/"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tnew {",
					"\t\tSystem.out.println(a)",
					"\t}",
					"",
					"\t/* Second comment.",
					"\t */");
			assertFormatted(source, expected);
		}

		@Test
		public void mlJavaComment() throws Exception {
			String source = unformattedCode(multilineString(
					"/**Hello world.",
					"That's the second line.*/new{System.out.println(a)}"));
			String expected = formattedCode(
					"",
					"\t/** Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tnew {",
					"\t\tSystem.out.println(a)",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment1() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"//Hello world.",
					"new{System.out.println(a)}"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tnew {",
					"\t\tSystem.out.println(a)",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment2() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"//      Hello world.",
					"new{System.out.println(a)}"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tnew {",
					"\t\tSystem.out.println(a)",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment3() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"new{System.out.println(a)}"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tnew {",
					"\t\tSystem.out.println(a)",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment4() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"new{System.out.println(a)}",
					"//Second comment",
					""));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tnew {",
					"\t\tSystem.out.println(a)",
					"\t}",
					"\t// Second comment");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment5() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"new{System.out.println(a)}",
					"//Second comment",
					"new(a:int){System.out.println(a)}"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tnew {",
					"\t\tSystem.out.println(a)",
					"\t}",
					"",
					"\t// Second comment",
					"\tnew(a : int) {",
					"\t\tSystem.out.println(a)",
					"\t}");
			assertFormatted(source, expected);
		}

	}

}

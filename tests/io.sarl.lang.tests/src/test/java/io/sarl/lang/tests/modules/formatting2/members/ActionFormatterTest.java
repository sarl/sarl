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

/** Tests for formatting actions.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("format: def")
@Tag("core")
@Tag("codeFormat")
public class ActionFormatterTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	public class FormatterAPITest extends AbstractMemberFormatterTest {

		@Override
		protected String declarationKeyword() {
			return "class";
		}

		@Test
		public void noParamNoReturn() throws Exception {
			String source = unformattedCode("def myfct {System.out.println(\"abc\")}");
			String expected = formattedCode(
					"	def myfct {",
					"		System.out.println(\"abc\")",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void noParamReturn() throws Exception {
			String source = unformattedCode("def myfct:boolean{return true}");
			String expected = formattedCode(
					"	def myfct : boolean {",
					"		return true",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void paramNoReturn() throws Exception {
			String source = unformattedCode("def myfct(a:int){System.out.println(a)}");
			String expected = formattedCode(
					"	def myfct(a : int) {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void paramsNoReturn() throws Exception {
			String source = unformattedCode("def myfct(a:int,b:boolean){System.out.println(a)}");
			String expected = formattedCode(
					"	def myfct(a : int, b : boolean) {",
					"		System.out.println(a)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void paramReturn() throws Exception {
			String source = unformattedCode("def myfct(a:int):boolean{a==2}");
			String expected = formattedCode(
					"	def myfct(a : int) : boolean {",
					"		a == 2",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void paramsReturn() throws Exception {
			String source = unformattedCode("def myfct(a:int,b:boolean):boolean{b}");
			String expected = formattedCode(
					"	def myfct(a : int, b : boolean) : boolean {",
					"		b",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void noParamNoBody() throws Exception {
			String source = unformattedCode("def myfct ;");
			String expected = formattedCode("	def myfct;");
			assertFormatted(source, expected);
		}

		@Test
		public void paramNoBody() throws Exception {
			String source = unformattedCode("def myfct(a:int);");
			String expected = formattedCode("	def myfct(a : int);");
			assertFormatted(source, expected);
		}

		@Test
		public void paramsNoBody() throws Exception {
			String source = unformattedCode("def myfct(a:int,b:boolean);");
			String expected = formattedCode("	def myfct(a : int, b : boolean);");
			assertFormatted(source, expected);
		}

		@Test
		public void defaultValuedParam() throws Exception {
			String source = unformattedCode("def myfct(a:int=5);");
			String expected = formattedCode("	def myfct(a : int = 5);");
			assertFormatted(source, expected);
		}

		@Test
		public void defaultValuedParams() throws Exception {
			String source = unformattedCode("def myfct(a:int=5,b:boolean=false);");
			String expected = formattedCode("	def myfct(a : int = 5, b : boolean = false);");
			assertFormatted(source, expected);
		}

		@Test
		public void varArg() throws Exception {
			String source = unformattedCode("def myfct(a:int  * );");
			String expected = formattedCode("	def myfct(a : int*);");
			assertFormatted(source, expected);
		}

		@Test
		public void javaGeneric() throws Exception {
			String source = unformattedCode("def<T>myfct(a:T);");
			String expected = formattedCode("	def <T> myfct(a : T);");
			assertFormatted(source, expected);
		}

		@Test
		public void javaGenerics() throws Exception {
			String source = unformattedCode("def<T,TT extends T>myfct(a:T,b:TT);");
			String expected = formattedCode("	def <T, TT extends T> myfct(a : T, b : TT);");
			assertFormatted(source, expected);
		}

		@Test
		public void sarlGeneric() throws Exception {
			String source = unformattedCode("def myfct(a:T)with T;");
			String expected = formattedCode("	def myfct(a : T) with T;");
			assertFormatted(source, expected);
		}

		@Test
		public void sarlGenerics() throws Exception {
			String source = unformattedCode("def myfct(a:T,b:TT)with T,TT extends T;");
			String expected = formattedCode("	def myfct(a : T, b : TT) with T, TT extends T;");
			assertFormatted(source, expected);
		}

		@Test
		public void exceptionNoParam() throws Exception {
			String source = unformattedCode("def myfct throws Exception;");
			String expected = formattedCode("	def myfct throws Exception;");
			assertFormatted(source, expected);
		}

		@Test
		public void exceptionsNoParam() throws Exception {
			String source = unformattedCode("def myfct throws Exception,Exception;");
			String expected = formattedCode("	def myfct throws Exception, Exception;");
			assertFormatted(source, expected);
		}

		@Test
		public void exceptionEmptyParam() throws Exception {
			String source = unformattedCode("def myfct()throws Exception;");
			String expected = formattedCode("	def myfct() throws Exception;");
			assertFormatted(source, expected);
		}

		@Test
		public void exceptionsEmptyParam() throws Exception {
			String source = unformattedCode("def myfct()throws Exception,Exception;");
			String expected = formattedCode("	def myfct() throws Exception, Exception;");
			assertFormatted(source, expected);
		}

		@Test
		public void eventNoParam() throws Exception {
			String source = unformattedCode("def myfct fires Event;");
			String expected = formattedCode("	def myfct fires Event;");
			assertFormatted(source, expected);
		}

		@Test
		public void eventsNoParam() throws Exception {
			String source = unformattedCode("def myfct fires Event,Event;");
			String expected = formattedCode("	def myfct fires Event, Event;");
			assertFormatted(source, expected);
		}

		@Test
		public void eventEmptyParam() throws Exception {
			String source = unformattedCode("def myfct()fires Event;");
			String expected = formattedCode("	def myfct() fires Event;");
			assertFormatted(source, expected);
		}

		@Test
		public void eventsEmptyParam() throws Exception {
			String source = unformattedCode("def myfct()fires Event,Event;");
			String expected = formattedCode("	def myfct() fires Event, Event;");
			assertFormatted(source, expected);
		}

		@Test
		public void modifiers() throws Exception {
			String source = unformattedCode("public   static   def myfct;");
			String expected = formattedCode("	public static def myfct;");
			assertFormatted(source, expected);
		}

		@Test
		public void annotation() throws Exception {
			String source = unformattedCode("@Annotation def myfct;");
			String expected = formattedCode("	@Annotation def myfct;");
			assertFormatted(source, expected);
		}

		@Test
		public void annotations() throws Exception {
			String source = unformattedCode("@Annotation@Beta def myfct;");
			String expected = formattedCode("	@Annotation @Beta def myfct;");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationValue() throws Exception {
			String source = unformattedCode("@SuppressWarnings(\"name\")def myfct;");
			String expected = formattedCode("	@SuppressWarnings(\"name\") def myfct;");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment1() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"* That's the second line.",
					"*/def myfct"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tdef myfct");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment2() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/def myfct"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tdef myfct");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment3() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/def myfct /*Second comment.*/def myfct2"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tdef myfct",
					"",
					"\t/* Second comment.",
					"\t */",
					"\tdef myfct2");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment4() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/def myfct /*Second comment.*/"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tdef myfct",
					"",
					"\t/* Second comment.",
					"\t */");
			assertFormatted(source, expected);
		}

		@Test
		public void mlJavaComment() throws Exception {
			String source = unformattedCode(multilineString(
					"/**Hello world.",
					"That's the second line.*/def myfct"));
			String expected = formattedCode(
					"",
					"\t/** Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tdef myfct");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment1() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"//Hello world.",
					"def myfct"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tdef myfct");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment2() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"//      Hello world.",
					"def myfct"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tdef myfct");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment3() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"def myfct"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tdef myfct");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment4() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"def myfct",
					"//Second comment",
					""));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tdef myfct",
					"\t// Second comment");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment5() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"def myfct",
					"//Second comment",
					"def myfct2"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tdef myfct",
					"",
					"\t// Second comment",
					"\tdef myfct2");
			assertFormatted(source, expected);
		}

	}

}

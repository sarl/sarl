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

/** Tests for formatting behavior units.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("format: on")
@Tag("core")
@Tag("codeFormat")
public class BehaviorUnitFormatterTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	public class FormatterAPITest extends AbstractMemberFormatterTest {

		@Test
		public void simple() throws Exception {
			String source = unformattedCode("on Event{System.out.println(occurrence)}");
			String expected = formattedCode(
					"	on Event {",
					"		System.out.println(occurrence)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void trueGuard() throws Exception {
			String source = unformattedCode("on Event[true]{System.out.println(occurrence)}");
			String expected = formattedCode(
					"	on Event [true] {",
					"		System.out.println(occurrence)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void falseGuard() throws Exception {
			String source = unformattedCode("on Event[false]{System.out.println(occurrence)}");
			String expected = formattedCode(
					"	on Event [false] {",
					"		System.out.println(occurrence)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void guard() throws Exception {
			String source = unformattedCode("on Event[occurrence.forMe]{System.out.println(occurrence)}");
			String expected = formattedCode(
					"	on Event [occurrence.forMe] {",
					"		System.out.println(occurrence)",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void twoAnnotations() throws Exception {
			String source = unformattedCode("@Pure@Beta on Event{}");
			String expected = formattedCode(
					"	@Pure @Beta on Event {",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void threeAnnotations() throws Exception {
			String source = unformattedCode(multilineString(
					"@Pure@Beta",
					"@Hello on Event{}"));
			String expected = formattedCode(
					"	@Pure @Beta",
					"	@Hello on Event {",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationValue() throws Exception {
			String source = unformattedCode("@SuppressWarnings(        value= \"name\"   )on Event{}");
			String expected = formattedCode(
					"	@SuppressWarnings(value = \"name\") on Event {",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void annotationImplicitValue() throws Exception {
			String source = unformattedCode("@SuppressWarnings(   \"name\"   )on Event{}");
			String expected = formattedCode(
					"	@SuppressWarnings(\"name\") on Event {",
					"	}");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment1() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"* That's the second line.",
					"*/on Event{}"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\ton Event {",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment2() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/on Event{}"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\ton Event {",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment3() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/on Event{}/*Second comment.*/on Event{}"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\ton Event {",
					"\t}",
					"",
					"\t/* Second comment.",
					"\t */",
					"\ton Event {",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment4() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/on Event{}/*Second comment.*/"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\ton Event {",
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
					"That's the second line.*/on Event{}"));
			String expected = formattedCode(
					"",
					"\t/** Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\ton Event {",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment1() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"//Hello world.",
					"on Event{}"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\ton Event {",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment2() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"//      Hello world.",
					"on Event{}"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\ton Event {",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment3() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"on Event{}"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\ton Event {",
					"\t}");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment4() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"on Event{}",
					"//Second comment",
					""));
			String expected = formattedCode(
					"\t// Hello world.",
					"\ton Event {",
					"\t}",
					"\t// Second comment");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment5() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"on Event{}",
					"//Second comment",
					"on Event{}"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\ton Event {",
					"\t}",
					"",
					"\t// Second comment",
					"\ton Event {",
					"\t}");
			assertFormatted(source, expected);
		}

	}

}

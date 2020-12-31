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

/** Tests for formatting capacity uses.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("format: uses")
@Tag("core")
@Tag("codeFormat")
public class CapacityUsesFormatterTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	public class FormatterAPITest extends AbstractMemberFormatterTest {

		@Test
		public void one() throws Exception {
			String source = unformattedCode("uses    Capacity1");
			String expected = formattedCode("	uses Capacity1");
			assertFormatted(source, expected);
		}

		@Test
		public void two() throws Exception {
			String source = unformattedCode("uses Capacity1,Capacity2");
			String expected = formattedCode("	uses Capacity1, Capacity2");
			assertFormatted(source, expected);
		}

		@Test
		public void three() throws Exception {
			String source = unformattedCode("uses Capacity1,Capacity2,    Capacity3");
			String expected = formattedCode("	uses Capacity1, Capacity2, Capacity3");
			assertFormatted(source, expected);
		}

		@Test
		public void twoStatements_two() throws Exception {
			String source = unformattedCode("uses Capacity1 uses Capacity2");
			String expected = formattedCode(
					"\tuses Capacity1",
					"\tuses Capacity2");
			assertFormatted(source, expected);
		}

		@Test
		public void twoStatements_three() throws Exception {
			String source = unformattedCode("uses Capacity1 uses Capacity2,    Capacity3");
			String expected = formattedCode(
					"\tuses Capacity1",
					"\tuses Capacity2, Capacity3");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment1() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"* That's the second line.",
					"*/uses Capacity1"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tuses Capacity1");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment2() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/uses Capacity1"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tuses Capacity1");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment3() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/uses Capacity1 /*Second comment.*/uses Capacity2"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tuses Capacity1",
					"",
					"\t/* Second comment.",
					"\t */",
					"\tuses Capacity2");
			assertFormatted(source, expected);
		}

		@Test
		public void mlStandardComment4() throws Exception {
			String source = unformattedCode(multilineString(
					"/*Hello world.",
					"That's the second line.*/uses Capacity1/*Second comment.*/"));
			String expected = formattedCode(
					"",
					"\t/* Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tuses Capacity1",
					"",
					"\t/* Second comment.",
					"\t */");
			assertFormatted(source, expected);
		}

		@Test
		public void mlJavaComment() throws Exception {
			String source = unformattedCode(multilineString(
					"/**Hello world.",
					"That's the second line.*/uses Capacity1"));
			String expected = formattedCode(
					"",
					"\t/** Hello world.",
					"\t * That's the second line.",
					"\t */",
					"\tuses Capacity1");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment1() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"//Hello world.",
					"uses Capacity1"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tuses Capacity1");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment2() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"//      Hello world.",
					"uses Capacity1"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tuses Capacity1");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment3() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"uses Capacity1"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tuses Capacity1");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment4() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"uses Capacity1",
					"//Second comment",
					""));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tuses Capacity1",
					"\t// Second comment");
			assertFormatted(source, expected);
		}

		@Test
		public void slComment5() throws Exception {
			String source = unformattedCode(multilineString(
					"",
					"// Hello world.",
					"uses Capacity1",
					"//Second comment",
					"uses Capacity2"));
			String expected = formattedCode(
					"\t// Hello world.",
					"\tuses Capacity1",
					"\t// Second comment",
					"\tuses Capacity2");
			assertFormatted(source, expected);
		}

	}

}

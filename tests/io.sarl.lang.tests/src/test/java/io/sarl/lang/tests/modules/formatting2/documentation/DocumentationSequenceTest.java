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
package io.sarl.lang.tests.modules.formatting2.documentation;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.tests.modules.formatting2.AbstractMemberFormatterTest;

/** Tests for sequence of documented members.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
@SuppressWarnings("all")
@DisplayName("format: sequence of comments")
@Tag("core")
@Tag("codeFormat")
public class DocumentationSequenceTest extends AbstractMemberFormatterTest {

	@Override
	protected String declarationKeyword() {
		return "agent";
	}

	@Test
	public void multilineComments01() throws Exception {
		String source = unformattedCode(
				"uses Logging, Schedules, Lifecycle",
				"/**",
				" * Hello",
				" *     @param x",
				" *  @return     x",
				" */",
				"on Initialize {",
				"  System.out.println(\"Z\")",
				"}",
				"/** Hello",
				"*    @param x",
				" *  @return x",
				"*/",
				"def fct1 {",
				"  System.out.println(\"Z\")",
				"}",
				"def fct2 { System.out.println(\"Z\") }");
		String expected = formattedCode(
				"\tuses Logging, Schedules, Lifecycle",
				"",
				"\t/** ",
				"\t * Hello",
				"\t * @param x",
				"\t * @return     x",
				"\t */",
				"\ton Initialize {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\t/** Hello",
				"\t * @param x",
				"\t * @return x",
				"\t */",
				"\tdef fct1 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\tdef fct2 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}");
		assertFormatted(source, expected);
	}

	@Test
	public void multilineComments02() throws Exception {
		String source = unformattedCode(
				"uses Logging, Schedules, Lifecycle",
				"/**",
				" * Hello",
				" *     @param x",
				" *  @return     x",
				" */",
				"def fct0 {",
				"  System.out.println(\"Z\")",
				"}",
				"/** Hello",
				"*    @param x",
				" *  @return x",
				"*/",
				"def fct1 {",
				"  System.out.println(\"Z\")",
				"}",
				"def fct2 { System.out.println(\"Z\") }");
		String expected = formattedCode(
				"\tuses Logging, Schedules, Lifecycle",
				"",
				"\t/** ",
				"\t * Hello",
				"\t * @param x",
				"\t * @return     x",
				"\t */",
				"\tdef fct0 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\t/** Hello",
				"\t * @param x",
				"\t * @return x",
				"\t */",
				"\tdef fct1 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\tdef fct2 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}");
		assertFormatted(source, expected);
	}

	@Test
	public void multilineComments03() throws Exception {
		String source = unformattedCode(
				"/**",
				" * Hello",
				" *     @param x",
				" *  @return     x",
				" */",
				"def fct0 {",
				"  System.out.println(\"Z\")",
				"}",
				"/** Hello",
				"*    @param x",
				" *  @return x",
				"*/",
				"def fct1 {",
				"  System.out.println(\"Z\")",
				"}",
				"def fct2 { System.out.println(\"Z\") }");
		String expected = formattedCode(
				"",
				"\t/** ",
				"\t * Hello",
				"\t * @param x",
				"\t * @return     x",
				"\t */",
				"\tdef fct0 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\t/** Hello",
				"\t * @param x",
				"\t * @return x",
				"\t */",
				"\tdef fct1 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\tdef fct2 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}");
		assertFormatted(source, expected);
	}

	@Test
	public void noComments01() throws Exception {
		String source = unformattedCode(
				"uses Logging, Schedules, Lifecycle",
				"def fct0 {",
				"  System.out.println(\"Z\")",
				"}",
				"def fct1 {",
				"  System.out.println(\"Z\")",
				"}",
				"def fct2 { System.out.println(\"Z\") }");
		String expected = formattedCode(
				"\tuses Logging, Schedules, Lifecycle",
				"",
				"\tdef fct0 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\tdef fct1 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\tdef fct2 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}");
		assertFormatted(source, expected);
	}

	@Test
	public void singlelineComments01() throws Exception {
		String source = unformattedCode(
				"uses Logging, Schedules, Lifecycle",
				"// Hello",
				"on Initialize {",
				"  System.out.println(\"Z\")",
				"}",
				"// Hello",
				"def fct1 {",
				"  System.out.println(\"Z\")",
				"}",
				"def fct2 { System.out.println(\"Z\") }");
		String expected = formattedCode(
				"\tuses Logging, Schedules, Lifecycle",
				"",
				"\t// Hello",
				"\ton Initialize {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\t// Hello",
				"\tdef fct1 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\tdef fct2 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}");
		assertFormatted(source, expected);
	}

	@Test
	public void singlelineComments02() throws Exception {
		String source = unformattedCode(
				"uses Logging, Schedules, Lifecycle",
				"",
				"// Hello",
				"on Initialize {",
				"  System.out.println(\"Z\")",
				"}",
				"",
				"// Hello",
				"",
				"def fct1 {",
				"  System.out.println(\"Z\")",
				"}",
				"def fct2 { System.out.println(\"Z\") }");
		String expected = formattedCode(
				"\tuses Logging, Schedules, Lifecycle",
				"",
				"\t// Hello",
				"\ton Initialize {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\t// Hello",
				"\tdef fct1 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\tdef fct2 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}");
		assertFormatted(source, expected);
	}

	@Test
	public void singlelineComments03() throws Exception {
		String source = unformattedCode(
				"uses Logging, Schedules, Lifecycle",
				"",
				"",
				"// Hello",
				"on Initialize {",
				"  System.out.println(\"Z\")",
				"}",
				"",
				"",
				"// Hello",
				"",
				"def fct1 {",
				"  System.out.println(\"Z\")",
				"}",
				"def fct2 { System.out.println(\"Z\") }");
		String expected = formattedCode(
				"\tuses Logging, Schedules, Lifecycle",
				"",
				"\t// Hello",
				"\ton Initialize {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\t// Hello",
				"\tdef fct1 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}",
				"",
				"\tdef fct2 {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}");
		assertFormatted(source, expected);
	}

	@Test
	public void singlelineComments04() throws Exception {
		String source = unformattedCode(
				"uses Logging, Schedules, Lifecycle",
				"",
				"",
				"// Hello",
				"// Hello2",
				"on Initialize {",
				"  System.out.println(\"Z\")",
				"}");
		String expected = formattedCode(
				"\tuses Logging, Schedules, Lifecycle",
				"",
				"\t// Hello",
				"\t// Hello2",
				"\ton Initialize {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}");
		assertFormatted(source, expected);
	}

	@Test
	public void singlelineComments05() throws Exception {
		String source = unformattedCode(
				"uses Logging, Schedules, Lifecycle",
				"",
				"",
				"// Hello",
				"",
				"// Hello2",
				"on Initialize {",
				"  System.out.println(\"Z\")",
				"}");
		String expected = formattedCode(
				"\tuses Logging, Schedules, Lifecycle",
				"",
				"\t// Hello",
				"\t// Hello2",
				"\ton Initialize {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}");
		assertFormatted(source, expected);
	}

	@Test
	public void singlelineComments06() throws Exception {
		String source = unformattedCode(
				"uses Logging, Schedules, Lifecycle",
				"",
				"",
				"// Hello",
				"",
				"",
				"// Hello2",
				"on Initialize {",
				"  System.out.println(\"Z\")",
				"}");
		String expected = formattedCode(
				"\tuses Logging, Schedules, Lifecycle",
				"",
				"\t// Hello",
				"\t// Hello2",
				"\ton Initialize {",
				"\t\tSystem.out.println(\"Z\")",
				"\t}");
		assertFormatted(source, expected);
	}

}

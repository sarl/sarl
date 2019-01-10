/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import com.google.inject.Inject;
import org.eclipse.xtext.util.Strings;
import org.junit.ComparisonFailure;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.documentation.IDocumentationFormatter;
import io.sarl.tests.api.AbstractSarlTest;

/** Abstract test of a SARL documentation formatter.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	DocumentationFormatterTest.WithoutIndentation.class,
	DocumentationFormatterTest.WithIndentation.class,
})
@SuppressWarnings("all")
public class DocumentationFormatterTest {

	public static class WithIndentation extends AbstractSarlTest {

		@Inject
		private IDocumentationFormatter formatter;

		/** Assert formatting
		 *
		 * @param input the input.
		 * @param expected the expected input.
		 */
		protected void assertMLFormatted(String input, final String expected) {
			String actual = this.formatter.formatMultilineComment(input, "\t");
			if (!Strings.equal(expected, actual)) {
				throw new ComparisonFailure("not same formatting", expected, actual);
			}
		}

		/** Assert formatting
		 *
		 * @param input the input.
		 * @param expected the expected input.
		 */
		protected void assertSLFormatted(String input, final String expected) {
			String actual = this.formatter.formatSinglelineComment(input, "\t");
			if (!Strings.equal(expected, actual)) {
				throw new ComparisonFailure("not same formatting", expected, actual);
			}
		}

		@Test
		public void ml_oneLine_withEnclosingSymbols_noExternalText() throws Exception {
			String source = "/*    The first   line    */";
			String expected = multilineString(
					"/* The first   line",
					"\t */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_oneLine_withEnclosingSymbols_externalText() throws Exception {
			String source = "    /*    The first   line    */";
			String expected = multilineString(
					"    /* The first   line",
					"\t */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_oneLine_withoutEnclosingSymbols() throws Exception {
			String source = "    The first   line    ";
			String expected = multilineString(
					"/* The first   line",
					"\t */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_multiline_withEnclosingSymbols_noExternalText() throws Exception {
			String source = multilineString(
					"/*    The first",
					"     line    */");
			String expected = multilineString(
					"/* The first",
					"\t * line",
					"\t */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_multiline_withEnclosingSymbols_externalText() throws Exception {
			String source = multilineString(
					"    /*    The first",
					"     line    */");
			String expected = multilineString(
					"    /* The first",
					"\t * line",
					"\t */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_multiline_withoutEnclosingSymbols() throws Exception {
			String source = multilineString(
					"    The first",
					"     line    ");
			String expected = multilineString(
					"/* The first",
					"\t * line",
					"\t */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void sl_withoutStartSymbols_noCarriageReturn() throws Exception {
			String source = "    The first     line    ";
			String expected = "// The first     line";
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withStartSymbols_noCarriageReturn() throws Exception {
			String source = "    //    The first     line    ";
			String expected = "    // The first     line";
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withoutStartSymbols_carriageReturn() throws Exception {
			String source = multilineString(
					"    The first     ",
					"    line    ");
			String expected = multilineString(
					"// The first",
					"\t// line");
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withStartSymbols_carriageReturn() throws Exception {
			String source = multilineString(
					"    //    The first     ",
					"    line    ");
			String expected = multilineString(
					"    // The first",
					"\t// line");
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withStartSymbols_noText() throws Exception {
			String source = "    //    ";
			String expected = "    //";
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withoutStartSymbols_noText() throws Exception {
			String source = "        ";
			String expected = "//";
			assertSLFormatted(source, expected);
		}

	}

	public static class WithoutIndentation extends AbstractSarlTest {

		@Inject
		private IDocumentationFormatter formatter;

		/** Assert formatting
		 *
		 * @param input the input.
		 * @param expected the expected input.
		 */
		protected void assertMLFormatted(String input, final String expected) {
			String actual = this.formatter.formatMultilineComment(input);
			if (!Strings.equal(expected, actual)) {
				throw new ComparisonFailure("not same formatting", expected, actual);
			}
		}

		/** Assert formatting
		 *
		 * @param input the input.
		 * @param expected the expected input.
		 */
		protected void assertSLFormatted(String input, final String expected) {
			String actual = this.formatter.formatSinglelineComment(input);
			if (!Strings.equal(expected, actual)) {
				throw new ComparisonFailure("not same formatting", expected, actual);
			}
		}

		@Test
		public void ml_oneLine_withEnclosingSymbols_noExternalText() throws Exception {
			String source = "/*    The first   line    */";
			String expected = multilineString(
					"/* The first   line",
					" */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_oneLine_withEnclosingSymbols_externalText() throws Exception {
			String source = "    /*    The first   line    */";
			String expected = multilineString(
					"    /* The first   line",
					" */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_oneLine_withoutEnclosingSymbols() throws Exception {
			String source = "    The first   line    ";
			String expected = multilineString(
					"/* The first   line",
					" */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_multiline_withEnclosingSymbols_noExternalText() throws Exception {
			String source = multilineString(
					"/*    The first",
					"     line    */");
			String expected = multilineString(
					"/* The first",
					" * line",
					" */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_multiline_withEnclosingSymbols_externalText() throws Exception {
			String source = multilineString(
					"    /*    The first",
					"     line    */");
			String expected = multilineString(
					"    /* The first",
					" * line",
					" */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_multiline_withoutEnclosingSymbols() throws Exception {
			String source = multilineString(
					"    The first",
					"     line    ");
			String expected = multilineString(
					"/* The first",
					" * line",
					" */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void sl_withoutStartSymbols_noCarriageReturn() throws Exception {
			String source = "    The first     line    ";
			String expected = "// The first     line";
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withStartSymbols_noCarriageReturn() throws Exception {
			String source = "    //    The first     line    ";
			String expected = "    // The first     line";
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withoutStartSymbols_carriageReturn() throws Exception {
			String source = multilineString(
					"    The first     ",
					"    line    ");
			String expected = multilineString(
					"// The first",
					"// line");
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withStartSymbols_carriageReturn() throws Exception {
			String source = multilineString(
					"    //    The first     ",
					"    line    ");
			String expected = multilineString(
					"    // The first",
					"// line");
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withStartSymbols_noText() throws Exception {
			String source = "    //    ";
			String expected = "    //";
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withoutStartSymbols_noText() throws Exception {
			String source = "        ";
			String expected = "//";
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_commentFromEcoreSerializer() throws Exception {
			String source = multilineString(
					"",
					"//TODO Auto-generated code.",
					"");
			String expected = multilineString(
					"",
					"// TODO Auto-generated code.",
					"");
			assertSLFormatted(source, expected);
		}

	}

}

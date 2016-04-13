/*
 * Copyright (C) 2014-2015 the original authors or authors.
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
package io.sarl.lang.tests.formatting2;

import java.io.InputStream;
import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.List;

import javax.inject.Named;
import javax.swing.text.StyledEditorKit.BoldAction;

import com.google.inject.Inject;
import junit.framework.TestSuite;
import org.eclipse.jdt.annotation.Nullable;
import org.eclipse.xtext.junit4.formatter.FormatterTestRequest;
import org.eclipse.xtext.junit4.formatter.FormatterTester;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.junit.Assume;
import org.junit.AssumptionViolatedException;
import org.junit.ComparisonFailure;
import org.junit.Rule;
import org.junit.Test;
import org.junit.internal.builders.AllDefaultPossibilitiesBuilder;
import org.junit.rules.TestName;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.Statement;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.sarl.lang.documentation.IDocumentationFormatter;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.TestScope;

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
			String source = "/*    The first\n     line    */";
			String expected = multilineString(
					"/* The first",
					"\t * line",
					"\t */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_multiline_withEnclosingSymbols_externalText() throws Exception {
			String source = "    /*    The first\n     line    */";
			String expected = multilineString(
					"    /* The first",
					"\t * line",
					"\t */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_multiline_withoutEnclosingSymbols() throws Exception {
			String source = "    The first\n     line    ";
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
			String source = "    The first     \n    line    ";
			String expected = multilineString(
					"// The first",
					"\t// line");
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withStartSymbols_carriageReturn() throws Exception {
			String source = "    //    The first     \n    line    ";
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
			String source = "/*    The first\n     line    */";
			String expected = multilineString(
					"/* The first",
					" * line",
					" */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_multiline_withEnclosingSymbols_externalText() throws Exception {
			String source = "    /*    The first\n     line    */";
			String expected = multilineString(
					"    /* The first",
					" * line",
					" */");
			assertMLFormatted(source, expected);
		}

		@Test
		public void ml_multiline_withoutEnclosingSymbols() throws Exception {
			String source = "    The first\n     line    ";
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
			String source = "    The first     \n    line    ";
			String expected = multilineString(
					"// The first",
					"// line");
			assertSLFormatted(source, expected);
		}

		@Test
		public void sl_withStartSymbols_carriageReturn() throws Exception {
			String source = "    //    The first     \n    line    ";
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

	}

}
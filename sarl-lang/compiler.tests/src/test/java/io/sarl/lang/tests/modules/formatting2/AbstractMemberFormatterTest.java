/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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
package io.sarl.lang.tests.modules.formatting2;

import static io.sarl.tests.api.tools.TestUtils.getLineSeparator;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestUtils.multilineString2;

import com.google.inject.Inject;
import org.eclipse.xtext.testing.formatter.FormatterTestHelper;

import io.sarl.lang.tests.api.AbstractSarlTest;

/** Abstract test of a SARL formatter.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractMemberFormatterTest extends AbstractSarlTest {

	@Inject
	private FormatterTestHelper tester;
	
	/** Assert formatting
	 *
	 * @param input the input.
	 * @param expected the expected input.
	 */
	protected void assertFormatted(final CharSequence input, final CharSequence expected) {
		this.tester.assertFormatted((it) -> AbstractFormatterTest.configureFormatterTestRequest(input, expected, it));
	}
	
	/** Replies the keyword for declaring the top element.
	 */
	protected String declarationKeyword() {
		return "agent";
	}
	
	/** Replies the prefix code.
	 */
	protected String prefix() {
		return declarationKeyword() + " EntityX{";
	}
	
	/** Replies the formatted prefix code.
	 */
	protected String formattedPrefix() {
		return declarationKeyword() + " EntityX {";
	}

	/** Replies the postfix code.
	 */
	protected String postfix() {
		return "}";
	}

	/** Replies the formatted postfix code.
	 */
	protected String formattedPostfix() {
		return "}" + getLineSeparator();
	}
	
	/** Build an unformatted code.
	 *
	 * @param addNewLineBeforePostfix indicates if a new line should be added before postfix text.
	 * @param code the line of code.
	 * @return the unformatted code with the prefix and postfix.
	 * @since 0.13
	 */
	protected String unformattedCode(boolean addNewLineBeforePostfix, String... code) {
		String output = prefix() + multilineString(code);
		if (addNewLineBeforePostfix) {
			output += getLineSeparator();
		}
		output += postfix();
		return output;
	}

	/** Build an unformatted code.
	 *
	 * @param code the line of code.
	 * @return the unformatted code with the prefix and postfix.
	 */
	protected final String unformattedCode(String... code) {
		return unformattedCode(false, code);
	}

	/** Build a formatted code.
	 *
	 * @param lines the lines of code.
	 * @return the formatted code with the prefix and postfix.
	 */
	protected String formattedCode(String... lines) {
		return formattedPrefix() + getLineSeparator() + multilineString2(false, lines) + getLineSeparator() + formattedPostfix();
	}

}

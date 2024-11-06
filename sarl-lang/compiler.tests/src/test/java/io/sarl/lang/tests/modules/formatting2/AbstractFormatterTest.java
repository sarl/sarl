/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import com.google.inject.Inject;
import org.eclipse.xtext.testing.formatter.FormatterTestHelper;
import org.eclipse.xtext.testing.formatter.FormatterTestRequest;

import io.sarl.lang.tests.api.AbstractSarlTest;

/** Abstract test of a SARL formatter.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 */
@SuppressWarnings("all")
public abstract class AbstractFormatterTest extends AbstractSarlTest {

	@Inject
	private FormatterTestHelper helper;

	/** Configure the formatter request for testing.
	 *
	 * @param input the input string.
	 * @param expected the expected string.
	 * @param it the request.
	 */
	static void configureFormatterTestRequest(CharSequence input, CharSequence expected, FormatterTestRequest it) {
		it.setToBeFormatted(input);
		it.setExpectation(expected);
		it.setAllowSyntaxErrors(false);
		it.setAllowUnformattedWhitespace(true);
		it.setUseNodeModel(true);
		it.setUseSerializer(false);
	}

	/** Assert formatting
	 *
	 * @param input the input.
	 * @param expected the expected input.
	 */
	protected void assertFormatted(final String input, final String expected) {
		this.helper.assertFormatted((it) -> configureFormatterTestRequest(input, expected, it));
	}

}

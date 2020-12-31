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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.validation;

import java.text.MessageFormat;
import javax.inject.Inject;

import org.antlr.runtime.RecognitionException;
import org.eclipse.xtext.nodemodel.SyntaxErrorMessage;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.conversion.XbaseValueConverterService;
import org.eclipse.xtext.xtext.parser.CardinalityAwareSyntaxErrorMessageProvider;

import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Provider of messages for syntax errors.
 *
 * <p>This provider enhances the error messages when a keyword is misplaced.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLSyntaxErrorMessageProvider extends CardinalityAwareSyntaxErrorMessageProvider {

	@Inject
	private SARLGrammarKeywordAccess grammarAccess;

	@Inject
	private XbaseValueConverterService converter;

	@Override
	public SyntaxErrorMessage getSyntaxErrorMessage(IParserErrorContext context) {
		if (context != null) {
			final RecognitionException recognitionException = context.getRecognitionException();
			if (recognitionException != null && recognitionException.token != null) {
				final String text = recognitionException.token.getText();
				if (!Strings.isEmpty(text)) {
					if (this.grammarAccess.isPureKeyword(text)) {
						final String protectedText = this.converter.getQualifiedNameValueConverter().toString(text);
			            return new SyntaxErrorMessage(
			            		MessageFormat.format(Messages.SARLSyntaxErrorMessageProvider_0, text, protectedText),
			            		SyntaxIssueCodes.USED_RESERVED_KEYWORD,
			            		new String[] {text, protectedText});
					} else if (this.grammarAccess.isKeyword(text)) {
			            return new SyntaxErrorMessage(
			            		MessageFormat.format(Messages.SARLSyntaxErrorMessageProvider_1, text),
			            		SyntaxIssueCodes.USED_RESERVED_KEYWORD);
					}
				}
			}
		}
		return super.getSyntaxErrorMessage(context);
	}

}

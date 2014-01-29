/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.formatting

import com.google.inject.Inject
import io.sarl.lang.services.SARLGrammarAccess
import org.eclipse.xtext.Keyword
import org.eclipse.xtext.formatting.impl.AbstractDeclarativeFormatter
import org.eclipse.xtext.formatting.impl.FormattingConfig
import org.eclipse.xtext.xbase.lib.Pair

/**
 * This class contains custom formatting description.
 * 
 * see : http://www.eclipse.org/Xtext/documentation.html#formatting
 * on how and when to use it 
 * 
 * Also see {@link org.eclipse.xtext.xtext.XtextFormattingTokenSerializer} as an example
 */
class SARLFormatter extends AbstractDeclarativeFormatter {

	@Inject extension SARLGrammarAccess f

	override protected void configureFormatting(FormattingConfig c) {

		c.setAutoLinewrap(120)
		for (pair : f.findKeywordPairs("{", "}")) {
			c.setIndentation(pair.getFirst(), pair.getSecond());
			//c.setLinewrap(1).before(pair.getFirst());
			
			c.setLinewrap(1).after(pair.getFirst());
			c.setLinewrap(1).before(pair.getSecond());
			c.setLinewrap(1).after(pair.getSecond());
		}

		// find common keywords an specify formatting for them
		for (pair : f.findKeywordPairs("(", ")")) {
			c.setNoSpace().before(pair.first);
			c.setNoSpace().after(pair.first);
			c.setNoSpace().before(pair.second);
		}

		for (Keyword comma : f.findKeywords(",")) {
			c.setNoSpace().before(comma);
		}

		for (Keyword dot : f.findKeywords(".")) {
			c.setNoSpace().around(dot);
		}
		for (pair : f.findKeywordPairs("<", ">")) {
			c.setNoSpace().after(pair.getFirst());
			c.setNoSpace().before(pair.getSecond());
		}

		c.setLinewrap(0, 1, 2).before(XImportSectionRule);
		c.setLinewrap(0, 0, 1).before(XImportDeclarationRule);

		c.setLinewrap(0, 1, 2).before(agentRule);
		c.setLinewrap(0, 1, 2).before(behaviorRule);
		c.setLinewrap(0, 1, 2).before(eventRule);
		c.setLinewrap(0, 1, 2).before(capacityRule);
		c.setLinewrap(0, 1, 2).before(skillRule);
		

		c.setLinewrap(0, 1, 2).before(attributeRule);
		c.setLinewrap(0, 1, 2).before(behaviorUnitRule);
		c.setLinewrap(0, 1, 2).before(actionSignatureRule);
		c.setLinewrap(0, 1, 2).before(requiredCapacityRule);

//		//CurlyBracket wraps
//		c.setLinewrap(0, 1, 2).before(agentAccess.rightCurlyBracketKeyword_4);
//		c.setLinewrap(0, 1, 2).before(skillAccess.rightCurlyBracketKeyword_7);
//		c.setLinewrap(0, 1, 2).before(capacityAccess.rightCurlyBracketKeyword_5);
//		c.setLinewrap(0, 1, 2).before(behaviorAccess.rightCurlyBracketKeyword_5);
//		c.setLinewrap(0, 1, 2).before(eventAccess.rightCurlyBracketKeyword_4);
//		c.setLinewrap(0, 1, 2).before(MASAccess.rightCurlyBracketKeyword_4);
//
//		c.setLinewrap(0, 1, 2).before(XBlockExpressionAccess.rightCurlyBracketKeyword_3);
//		c.setLinewrap(0, 1, 2).after(XBlockExpressionAccess.leftCurlyBracketKeyword_1);

//		// formatting for grammar rule Agent Indentation
//		c.setIndentationIncrement.after(agentAccess.leftCurlyBracketKeyword_2);
//		c.setIndentationDecrement.before(agentAccess.rightCurlyBracketKeyword_4);
//
//		// formatting for grammar rule CAPACITY Indentation
//		c.setIndentationIncrement.after(capacityAccess.leftCurlyBracketKeyword_3);
//		c.setIndentationDecrement.before(capacityAccess.rightCurlyBracketKeyword_5);
//
//		// formatting for grammar rule SKILL Indentation
//		c.setIndentationIncrement.after(skillAccess.leftCurlyBracketKeyword_5);
//		c.setIndentationDecrement.before(skillAccess.rightCurlyBracketKeyword_7);
//
//		// formatting for grammar rule BEHAVIOR Indentation
//		c.setIndentationIncrement.after(behaviorAccess.leftCurlyBracketKeyword_3);
//		c.setIndentationDecrement.before(behaviorAccess.rightCurlyBracketKeyword_5);
//
//		// formatting for grammar rule EVENT Indentation
//		c.setIndentationIncrement.after(eventAccess.leftCurlyBracketKeyword_2);
//		c.setIndentationDecrement.before(eventAccess.rightCurlyBracketKeyword_4);
//
//		// formatting for grammar rule MAS Indentation
//		c.setIndentationIncrement.after(MASAccess.leftCurlyBracketKeyword_2);
//		c.setIndentationDecrement.before(MASAccess.rightCurlyBracketKeyword_4);
//
//		// formatting for grammar rule XBlockExpression Indentation
//		c.setIndentationIncrement.after(XBlockExpressionAccess.leftCurlyBracketKeyword_1);
//		c.setIndentationDecrement.before(XBlockExpressionAccess.rightCurlyBracketKeyword_3);

		// formatting for Comments 
		c.setLinewrap(0, 1, 2).before(getSL_COMMENTRule());
		c.setLinewrap(0, 1, 2).before(getML_COMMENTRule());
		c.setLinewrap(0, 1, 1).after(getML_COMMENTRule());
	}
}

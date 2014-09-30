/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.formatting

import com.google.inject.Inject
import io.sarl.lang.services.SARLGrammarAccess
import io.sarl.lang.services.SARLGrammarAccess.ActionSignatureElements
import io.sarl.lang.services.SARLGrammarAccess.AgentElements
import io.sarl.lang.services.SARLGrammarAccess.AttributeElements
import io.sarl.lang.services.SARLGrammarAccess.BehaviorElements
import io.sarl.lang.services.SARLGrammarAccess.BehaviorUnitElements
import io.sarl.lang.services.SARLGrammarAccess.CapacityElements
import io.sarl.lang.services.SARLGrammarAccess.CapacityUsesElements
import io.sarl.lang.services.SARLGrammarAccess.ConstructorElements
import io.sarl.lang.services.SARLGrammarAccess.EventElements
import io.sarl.lang.services.SARLGrammarAccess.RequiredCapacityElements
import io.sarl.lang.services.SARLGrammarAccess.SkillElements
import io.sarl.lang.services.SARLGrammarAccess.XVariableDeclarationElements
import org.eclipse.xtext.formatting.impl.FormattingConfig
import org.eclipse.xtext.xbase.formatting.XbaseFormatter
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XAssignmentElements
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XBlockExpressionElements
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XCatchClauseElements
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XForLoopExpressionElements
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XIfExpressionElements
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XMemberFeatureCallElements
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XTryCatchFinallyExpressionElements
import org.eclipse.xtext.xbase.services.XtypeGrammarAccess.XImportDeclarationElements

/**
 * This class contains custom formatting description.
 * 
 * see : http://www.eclipse.org/Xtext/documentation.html#formatting
 * on how and when to use it 
 * 
 * Also see {@code org.eclipse.xtext.xtext.XtextFormattingTokenSerializer} as an example
 * 
 * FIXME: This formatter may extend the Xtend formatter.
 */
class SARLFormatter extends XbaseFormatter {

	@Inject extension SARLGrammarAccess f

	override protected void configureFormatting(FormattingConfig c) {

		c.setAutoLinewrap(400)
		c.configureXImportDeclaration(XImportDeclarationAccess)
		c.configureAgent(agentAccess)

		c.configureAttributes(attributeAccess)
		c.configureEvents(eventAccess)
		c.configureUses(capacityUsesAccess)
		c.configureBehaviorUnit(behaviorUnitAccess)

		c.configureXVariableDeclaration(XVariableDeclarationAccess)
		c.configureXAssignmentElements(XAssignmentAccess)
		c.configureActionSignatures(actionSignatureAccess)
		c.configureCapacities(capacityAccess)
		c.configureBehaviors(behaviorAccess)
		c.configureRequires(requiredCapacityAccess)
		c.configureSkill(skillAccess)
		c.configureConstructor(constructorAccess)

		//package
		c.setLinewrap(2).after(sarlScriptAccess.group_0)

		super.configure(c, xbaseGrammarAccess);

		// formatting for Comments 
		c.setLinewrap(0, 1, 2).before(getSL_COMMENTRule());
		c.setLinewrap(0, 1, 2).before(getML_COMMENTRule());
		c.setLinewrap(0, 1, 1).after(getML_COMMENTRule());
	}

	def configureConstructor(FormattingConfig c, ConstructorElements ele) {
		c.setLinewrap(2).before(ele.group)
		c.setNoSpace.around(ele.leftParenthesisKeyword_2_0)
		c.setNoSpace.before(ele.rightParenthesisKeyword_2_2)
		c.setNoSpace.before(ele.commaKeyword_2_1_1_0)

	}

	def configureSkill(FormattingConfig c, SkillElements ele) {
		c.setLinewrap(1, 1, 2).before(ele.group)
		c.setIndentation(ele.leftCurlyBracketKeyword_4, ele.rightCurlyBracketKeyword_6)
		c.setLinewrap.around(ele.rightCurlyBracketKeyword_6)
	}

	def configureRequires(FormattingConfig c, RequiredCapacityElements ele) {
		c.setLinewrap.around(ele.group)
		c.setNoSpace.before(ele.commaKeyword_3_0)
	}

	def configureBehaviors(FormattingConfig c, BehaviorElements ele) {
		c.setLinewrap(2).before(ele.group)
		c.setIndentation(ele.leftCurlyBracketKeyword_4, ele.rightCurlyBracketKeyword_6)
		c.setLinewrap.around(ele.rightCurlyBracketKeyword_6)
	}

	def configureCapacities(FormattingConfig c, CapacityElements ele) {
		c.setLinewrap(1, 1, 2).before(ele.group)
		c.setIndentation(ele.leftCurlyBracketKeyword_4, ele.rightCurlyBracketKeyword_6)
		c.setLinewrap.around(ele.rightCurlyBracketKeyword_6)
	}

	def configureActionSignatures(FormattingConfig c, ActionSignatureElements ele) {
		c.setLinewrap(2).before(ele.group)
		c.setNoSpace.around(ele.leftParenthesisKeyword_3_0)
		c.setNoSpace.before(ele.rightParenthesisKeyword_3_2)
		c.setNoSpace.before(ele.commaKeyword_3_1_1_0)
		c.setNoSpace.before(ele.commaKeyword_5_2_0)
	}

	def void configureXImportDeclaration(FormattingConfig c, XImportDeclarationElements ele) {
		c.setLinewrap(1, 1, 2).after(ele.group)
		c.setNoSpace.before(ele.alternatives_1_0_3)
	}

	def void configureAttributes(FormattingConfig c, AttributeElements ele) {
		c.setLinewrap.around(ele.group)
	}

	def void configureAgent(FormattingConfig c, AgentElements ele) {
		c.setLinewrap(2).after(ele.group);
		c.setLinewrap.before(ele.rightCurlyBracketKeyword_6);
		c.setIndentation(ele.leftCurlyBracketKeyword_4, ele.rightCurlyBracketKeyword_6)
	}

	def void configureEvents(FormattingConfig c, EventElements ele) {
		c.setLinewrap(2).after(ele.group);
		c.setLinewrap.before(ele.rightCurlyBracketKeyword_4_2);
		c.setIndentation(ele.leftCurlyBracketKeyword_4_0, ele.rightCurlyBracketKeyword_4_2)
	}

	def void configureUses(FormattingConfig c, CapacityUsesElements ele) {
		c.setLinewrap.around(ele.group);
		c.setNoSpace.before(ele.commaKeyword_3_0)
	}

	def void configureBehaviorUnit(FormattingConfig c, BehaviorUnitElements ele) {
		c.setLinewrap(2).before(ele.group);
		c.setLinewrap.after(ele.group);

		c.setNoSpace.after(ele.leftSquareBracketKeyword_3_0)
		c.setNoSpace.before(ele.rightSquareBracketKeyword_3_2)

	}

	override void configureXIfExpression(FormattingConfig c, XIfExpressionElements ele) {
		c.setNoSpace().around(ele.getLeftParenthesisKeyword_2());
		c.setNoSpace().before(ele.getRightParenthesisKeyword_4());

		c.setLinewrap(1, 1, 2).before(ele.group)

		c.setLinewrap(0, 1, 2).after(ele.thenAssignment_5)

	}

	def void configureXVariableDeclaration(FormattingConfig c, XVariableDeclarationElements ele) {
		c.setLinewrap(0, 1, 2).around(ele.group)
	}

	def configureXAssignmentElements(FormattingConfig c, XAssignmentElements ele) {
		c.setLinewrap(0, 1, 2).around(ele.group_0)
	}

	override void configureXTryCatchFinallyExpression(FormattingConfig c, XTryCatchFinallyExpressionElements ele) {
//		c.setIndentationIncrement().before(ele.getExpressionAssignment_2());
		c.setLinewrap().around(ele.getExpressionAssignment_2());
//		c.setIndentationDecrement().after(ele.getExpressionAssignment_2());
//		c.setIndentationIncrement().before(ele.getFinallyExpressionAssignment_3_0_1_1());
		c.setLinewrap().around(ele.getFinallyExpressionAssignment_3_0_1_1());
//		c.setIndentationDecrement().after(ele.getFinallyExpressionAssignment_3_0_1_1());
//		c.setIndentationIncrement().before(ele.getFinallyExpressionAssignment_3_1_1());
		c.setLinewrap().around(ele.getFinallyExpressionAssignment_3_1_1());
//		c.setIndentationDecrement().after(ele.getFinallyExpressionAssignment_3_1_1());
	}

	override configureXForLoopExpression(FormattingConfig c, XForLoopExpressionElements ele) {
		c.setNoSpace().around(ele.leftParenthesisKeyword_0_0_2);
		c.setNoSpace().around(ele.colonKeyword_0_0_4);
		c.setNoSpace().around(ele.rightParenthesisKeyword_2);

		//		c.setIndentationIncrement().before(ele.getEachExpressionAssignment_7());
		c.setLinewrap().around(ele.eachExpressionAssignment_3);

		//		c.setIndentationDecrement().after(ele.getEachExpressionAssignment_7());
		c.setLinewrap(2).before(ele.group)
	}

	override configureXMemberFeatureCall(FormattingConfig c, XMemberFeatureCallElements ele) {
		super.configureXMemberFeatureCall(c, ele)
		c.setLinewrap(0, 1, 2).around(ele.group)
	}

	override configureXBlockExpression(FormattingConfig c, XBlockExpressionElements ele) {
		super.configureXBlockExpression(c, ele)
		c.setLinewrap(0, 1, 2).after(ele.getRightCurlyBracketKeyword_3());
	}

	override void configureXCatchClause(FormattingConfig c, XCatchClauseElements ele) {
		c.setNoSpace().around(ele.getLeftParenthesisKeyword_1());
		c.setNoSpace().before(ele.getRightParenthesisKeyword_3());

		c.setLinewrap().around(ele.getExpressionAssignment_4());

	}

}

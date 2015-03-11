/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.formatting;

import io.sarl.lang.services.SARLGrammarAccess;
import io.sarl.lang.services.SARLGrammarAccess.ActionElements;
import io.sarl.lang.services.SARLGrammarAccess.AgentElements;
import io.sarl.lang.services.SARLGrammarAccess.BehaviorElements;
import io.sarl.lang.services.SARLGrammarAccess.BehaviorUnitElements;
import io.sarl.lang.services.SARLGrammarAccess.CapacityElements;
import io.sarl.lang.services.SARLGrammarAccess.CapacityUsesElements;
import io.sarl.lang.services.SARLGrammarAccess.ConstructorElements;
import io.sarl.lang.services.SARLGrammarAccess.EventElements;
import io.sarl.lang.services.SARLGrammarAccess.FieldElements;
import io.sarl.lang.services.SARLGrammarAccess.RequiredCapacityElements;
import io.sarl.lang.services.SARLGrammarAccess.SkillElements;
import io.sarl.lang.services.SARLGrammarAccess.XVariableDeclarationElements;

import org.eclipse.xtext.formatting.impl.FormattingConfig;
import org.eclipse.xtext.xbase.formatting.XbaseFormatter;
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XAssignmentElements;
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XBlockExpressionElements;
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XCatchClauseElements;
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XForLoopExpressionElements;
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XIfExpressionElements;
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XMemberFeatureCallElements;
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess.XTryCatchFinallyExpressionElements;
import org.eclipse.xtext.xbase.services.XtypeGrammarAccess.XImportDeclarationElements;

import com.google.inject.Inject;

/**
 * This class contains custom formatting description.
 *
 * see : http://www.eclipse.org/Xtext/documentation.html#formatting
 * on how and when to use it
 *
 * Also see {@code org.eclipse.xtext.xtext.XtextFormattingTokenSerializer} as an example
 *
 * FIXME: This formatter may extend the Xtend formatter.
 * FIXME: This formatter should be replaced by the new version of the formatting API, available in Xbase 2.8.0.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLFormatter extends XbaseFormatter {

	private static final int LINE_WRAP = 400;

	@Inject
	private SARLGrammarAccess access;

	@Override
	public void configureFormatting(FormattingConfig c) {

		c.setAutoLinewrap(LINE_WRAP);
		configureXImportDeclaration(c, this.access.getXImportDeclarationAccess());
		configureAgent(c, this.access.getAgentAccess());

		configureAttributes(c, this.access.getFieldAccess());
		configureEvents(c, this.access.getEventAccess());
		configureUses(c, this.access.getCapacityUsesAccess());
		configureBehaviorUnit(c, this.access.getBehaviorUnitAccess());

		configureXVariableDeclaration(c, this.access.getXVariableDeclarationAccess());
		configureXAssignmentElements(c, this.access.getXAssignmentAccess());
		configureActions(c, this.access.getActionAccess());
		configureCapacities(c, this.access.getCapacityAccess());
		configureBehaviors(c, this.access.getBehaviorAccess());
		configureRequires(c, this.access.getRequiredCapacityAccess());
		configureSkill(c, this.access.getSkillAccess());
		configureConstructor(c, this.access.getConstructorAccess());

		//package
		c.setLinewrap(2).after(this.access.getSarlScriptAccess().getGroup_1());

		super.configure(c, this.access.getXtendGrammarAccess().getXbaseWithAnnotationsGrammarAccess());

		// formatting for Comments
		c.setLinewrap(0, 1, 2).before(this.access.getSL_COMMENTRule());
		c.setLinewrap(0, 1, 2).before(this.access.getML_COMMENTRule());
		c.setLinewrap(0, 1, 1).after(this.access.getML_COMMENTRule());
	}

	/** Configure the constructor formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureConstructor(FormattingConfig c, ConstructorElements ele) {
		c.setLinewrap(2).before(ele.getGroup());
		c.setNoSpace().around(ele.getLeftParenthesisKeyword_6_0());
		c.setNoSpace().before(ele.getRightParenthesisKeyword_6_2());
		c.setNoSpace().before(ele.getCommaKeyword_5_2_0());
	}

	/** Configure the skill formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureSkill(FormattingConfig c, SkillElements ele) {
		c.setLinewrap(1, 1, 2).before(ele.getGroup());
		c.setIndentation(ele.getLeftCurlyBracketKeyword_7(), ele.getRightCurlyBracketKeyword_9());
		c.setLinewrap().around(ele.getRightCurlyBracketKeyword_9());
	}

	/** Configure the capacity requirement formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureRequires(FormattingConfig c, RequiredCapacityElements ele) {
		c.setLinewrap().around(ele.getGroup());
		c.setNoSpace().before(ele.getCommaKeyword_3_0());
	}

	/** Configure the behavior formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureBehaviors(FormattingConfig c, BehaviorElements ele) {
		c.setLinewrap(2).before(ele.getGroup());
		c.setIndentation(ele.getLeftCurlyBracketKeyword_7(), ele.getRightCurlyBracketKeyword_9());
		c.setLinewrap().around(ele.getRightCurlyBracketKeyword_9());
	}

	/** Configure the capacity formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureCapacities(FormattingConfig c, CapacityElements ele) {
		c.setLinewrap(1, 1, 2).before(ele.getGroup());
		c.setIndentation(ele.getLeftCurlyBracketKeyword_7(), ele.getRightCurlyBracketKeyword_9());
		c.setLinewrap().around(ele.getRightCurlyBracketKeyword_9());
	}

	/** Configure the action formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureActions(FormattingConfig c, ActionElements ele) {
		c.setLinewrap(2).before(ele.getGroup());
		c.setNoSpace().around(ele.getLeftParenthesisKeyword_8_0());
		c.setNoSpace().before(ele.getRightParenthesisKeyword_8_2());
		c.setNoSpace().before(ele.getCommaKeyword_6_2_0());
		c.setNoSpace().before(ele.getCommaKeyword_8_1_1_0());
		c.setNoSpace().before(ele.getCommaKeyword_10_0_2_0());
		c.setNoSpace().before(ele.getCommaKeyword_10_1_2_0());
	}

	/** Configure the import formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureXImportDeclaration(FormattingConfig c, XImportDeclarationElements ele) {
		c.setLinewrap(1, 1, 2).after(ele.getGroup());
		c.setNoSpace().before(ele.getAlternatives_1_0_3());
	}

	/** Configure the attribute formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureAttributes(FormattingConfig c, FieldElements ele) {
		c.setLinewrap().around(ele.getGroup());
	}

	/** Configure the agent formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureAgent(FormattingConfig c, AgentElements ele) {
		c.setLinewrap(2).after(ele.getGroup());
		c.setLinewrap().before(ele.getRightCurlyBracketKeyword_9());
		c.setIndentation(ele.getLeftCurlyBracketKeyword_7(), ele.getRightCurlyBracketKeyword_9());
	}

	/** Configure the event formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureEvents(FormattingConfig c, EventElements ele) {
		c.setLinewrap(2).after(ele.getGroup());
		c.setLinewrap().before(ele.getRightCurlyBracketKeyword_7_2());
		c.setIndentation(ele.getLeftCurlyBracketKeyword_7_0(), ele.getRightCurlyBracketKeyword_7_2());
	}

	/** Configure the capacity use formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureUses(FormattingConfig c, CapacityUsesElements ele) {
		c.setLinewrap().around(ele.getGroup());
		c.setNoSpace().before(ele.getCommaKeyword_3_0());
	}

	/** Configure the behavior unit formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureBehaviorUnit(FormattingConfig c, BehaviorUnitElements ele) {
		c.setLinewrap(2).before(ele.getGroup());
		c.setLinewrap().after(ele.getGroup());

		c.setNoSpace().after(ele.getLeftSquareBracketKeyword_5_0());
		c.setNoSpace().before(ele.getRightSquareBracketKeyword_5_2());

	}

	@Override
	public void configureXIfExpression(FormattingConfig c, XIfExpressionElements ele) {
		c.setNoSpace().around(ele.getLeftParenthesisKeyword_2());
		c.setNoSpace().before(ele.getRightParenthesisKeyword_4());

		c.setLinewrap(1, 1, 2).before(ele.getGroup());

		c.setLinewrap(0, 1, 2).after(ele.getThenAssignment_5());

	}

	/** Configure the variable declaration formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureXVariableDeclaration(FormattingConfig c, XVariableDeclarationElements ele) {
		c.setLinewrap(0, 1, 2).around(ele.getGroup());
	}

	/** Configure the assignment formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureXAssignmentElements(FormattingConfig c, XAssignmentElements ele) {
		c.setLinewrap(0, 1, 2).around(ele.getGroup_0());
	}

	@Override
	public void configureXTryCatchFinallyExpression(FormattingConfig c, XTryCatchFinallyExpressionElements ele) {
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

	@Override
	public void configureXForLoopExpression(FormattingConfig c, XForLoopExpressionElements ele) {
		c.setNoSpace().around(ele.getLeftParenthesisKeyword_0_0_2());
		c.setNoSpace().around(ele.getColonKeyword_0_0_4());
		c.setNoSpace().around(ele.getRightParenthesisKeyword_2());

		//		c.setIndentationIncrement().before(ele.getEachExpressionAssignment_7());
		c.setLinewrap().around(ele.getEachExpressionAssignment_3());

		//		c.setIndentationDecrement().after(ele.getEachExpressionAssignment_7());
		c.setLinewrap(2).before(ele.getGroup());
	}

	@Override
	public void configureXMemberFeatureCall(FormattingConfig c, XMemberFeatureCallElements ele) {
		super.configureXMemberFeatureCall(c, ele);
		c.setLinewrap(0, 1, 2).around(ele.getGroup());
	}

	@Override
	public void configureXBlockExpression(FormattingConfig c, XBlockExpressionElements ele) {
		super.configureXBlockExpression(c, ele);
		c.setLinewrap(0, 1, 2).after(ele.getRightCurlyBracketKeyword_3());
	}

	@Override
	public void configureXCatchClause(FormattingConfig c, XCatchClauseElements ele) {
		c.setNoSpace().around(ele.getLeftParenthesisKeyword_1());
		c.setNoSpace().before(ele.getRightParenthesisKeyword_3());

		c.setLinewrap().around(ele.getExpressionAssignment_4());
	}

}

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
package io.sarl.lang.formatting;

import io.sarl.lang.services.SARLGrammarAccess;
import io.sarl.lang.services.SARLGrammarAccess.SarlActionElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlActionSignatureElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlAgentElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlBehaviorElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlBehaviorUnitElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlCapacityElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlCapacityUsesElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlConstructorElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlEventElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlFieldElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlRequiredCapacityElements;
import io.sarl.lang.services.SARLGrammarAccess.SarlSkillElements;
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
		configureAgent(c, this.access.getSarlAgentAccess());

		configureAttributes(c, this.access.getAttributeAccess());
		configureEvents(c, this.access.getEventAccess());
		configureUses(c, this.access.getCapacityUsesAccess());
		configureBehaviorUnit(c, this.access.getBehaviorUnitAccess());

		configureXVariableDeclaration(c, this.access.getXVariableDeclarationAccess());
		configureXAssignmentElements(c, this.access.getXAssignmentAccess());
		configureActions(c, this.access.getActionAccess());
		configureActionSignatures(c, this.access.getActionSignatureAccess());
		configureCapacities(c, this.access.getCapacityAccess());
		configureBehaviors(c, this.access.getBehaviorAccess());
		configureRequires(c, this.access.getRequiredCapacityAccess());
		configureSkill(c, this.access.getSkillAccess());
		configureConstructor(c, this.access.getConstructorAccess());

		//package
		c.setLinewrap(2).after(this.access.getSarlScriptAccess().getGroup_0());

		super.configure(c, this.access.getXbaseGrammarAccess());

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
	public void configureConstructor(FormattingConfig c, SarlConstructorElements ele) {
		c.setLinewrap(2).before(ele.getGroup());
		c.setNoSpace().around(ele.getLeftParenthesisKeyword_2_0());
		c.setNoSpace().before(ele.getRightParenthesisKeyword_2_2());
		c.setNoSpace().before(ele.getCommaKeyword_2_1_1_0());

	}

	/** Configure the skill formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureSkill(FormattingConfig c, SarlSkillElements ele) {
		c.setLinewrap(1, 1, 2).before(ele.getGroup());
		c.setIndentation(ele.getLeftCurlyBracketKeyword_4(), ele.getRightCurlyBracketKeyword_6());
		c.setLinewrap().around(ele.getRightCurlyBracketKeyword_6());
	}

	/** Configure the capacity requirement formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureRequires(FormattingConfig c, SarlRequiredCapacityElements ele) {
		c.setLinewrap().around(ele.getGroup());
		c.setNoSpace().before(ele.getCommaKeyword_3_0());
	}

	/** Configure the behavior formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureBehaviors(FormattingConfig c, SarlBehaviorElements ele) {
		c.setLinewrap(2).before(ele.getGroup());
		c.setIndentation(ele.getLeftCurlyBracketKeyword_4(), ele.getRightCurlyBracketKeyword_6());
		c.setLinewrap().around(ele.getRightCurlyBracketKeyword_6());
	}

	/** Configure the capacity formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureCapacities(FormattingConfig c, SarlCapacityElements ele) {
		c.setLinewrap(1, 1, 2).before(ele.getGroup());
		c.setIndentation(ele.getLeftCurlyBracketKeyword_4(), ele.getRightCurlyBracketKeyword_6());
		c.setLinewrap().around(ele.getRightCurlyBracketKeyword_6());
	}

	/** Configure the action signature formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureActionSignatures(FormattingConfig c, SarlActionSignatureElements ele) {
		c.setLinewrap(2).before(ele.getGroup());
		c.setNoSpace().around(ele.getLeftParenthesisKeyword_3_0());
		c.setNoSpace().before(ele.getRightParenthesisKeyword_3_2());
		c.setNoSpace().before(ele.getCommaKeyword_3_1_1_0());
		c.setNoSpace().before(ele.getCommaKeyword_5_2_0());
	}

	/** Configure the action formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureActions(FormattingConfig c, SarlActionElements ele) {
		c.setLinewrap(2).before(ele.getGroup());
		c.setNoSpace().around(ele.getLeftParenthesisKeyword_3_0());
		c.setNoSpace().before(ele.getRightParenthesisKeyword_3_2());
		c.setNoSpace().before(ele.getCommaKeyword_3_1_1_0());
		c.setNoSpace().before(ele.getCommaKeyword_5_2_0());
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
	public void configureAttributes(FormattingConfig c, SarlFieldElements ele) {
		c.setLinewrap().around(ele.getGroup());
	}

	/** Configure the agent formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureAgent(FormattingConfig c, SarlAgentElements ele) {
		c.setLinewrap(2).after(ele.getGroup());
		c.setLinewrap().before(ele.getRightCurlyBracketKeyword_6());
		c.setIndentation(ele.getLeftCurlyBracketKeyword_4(), ele.getRightCurlyBracketKeyword_6());
	}

	/** Configure the event formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureEvents(FormattingConfig c, SarlEventElements ele) {
		c.setLinewrap(2).after(ele.getGroup());
		c.setLinewrap().before(ele.getRightCurlyBracketKeyword_4_2());
		c.setIndentation(ele.getLeftCurlyBracketKeyword_4_0(), ele.getRightCurlyBracketKeyword_4_2());
	}

	/** Configure the capacity use formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureUses(FormattingConfig c, SarlCapacityUsesElements ele) {
		c.setLinewrap().around(ele.getGroup());
		c.setNoSpace().before(ele.getCommaKeyword_3_0());
	}

	/** Configure the behavior unit formatting.
	 *
	 * @param c - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureBehaviorUnit(FormattingConfig c, SarlBehaviorUnitElements ele) {
		c.setLinewrap(2).before(ele.getGroup());
		c.setLinewrap().after(ele.getGroup());

		c.setNoSpace().after(ele.getLeftSquareBracketKeyword_3_0());
		c.setNoSpace().before(ele.getRightSquareBracketKeyword_3_2());

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

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import com.google.inject.Inject;
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

/**
 * This class contains custom formatting description.
 *
 * <p>See : <a href="http://www.eclipse.org/Xtext/documentation.html#formatting">the documentation</a>
 * on how and when to use it.
 *
 * <p>Also see {@code org.eclipse.xtext.xtext.XtextFormattingTokenSerializer} as an example
 *
 * <p>FIXME: This formatter may extend the Xtend formatter.
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
	public void configureFormatting(FormattingConfig config) {

		config.setAutoLinewrap(LINE_WRAP);
		configureXImportDeclaration(config, this.access.getXImportDeclarationAccess());
		configureAgent(config, this.access.getAgentAccess());

		configureAttributes(config, this.access.getFieldAccess());
		configureEvents(config, this.access.getEventAccess());
		configureUses(config, this.access.getCapacityUsesAccess());
		configureBehaviorUnit(config, this.access.getBehaviorUnitAccess());

		configureXVariableDeclaration(config, this.access.getXVariableDeclarationAccess());
		configureXAssignmentElements(config, this.access.getXAssignmentAccess());
		configureActions(config, this.access.getActionAccess());
		configureCapacities(config, this.access.getCapacityAccess());
		configureBehaviors(config, this.access.getBehaviorAccess());
		configureRequires(config, this.access.getRequiredCapacityAccess());
		configureSkill(config, this.access.getSkillAccess());
		configureConstructor(config, this.access.getConstructorAccess());

		//package
		config.setLinewrap(2).after(this.access.getSarlScriptAccess().getGroup_1());

		super.configure(config, this.access.getXtendGrammarAccess().getXbaseWithAnnotationsGrammarAccess());

		// formatting for Comments
		config.setLinewrap(0, 1, 2).before(this.access.getSL_COMMENTRule());
		config.setLinewrap(0, 1, 2).before(this.access.getML_COMMENTRule());
		config.setLinewrap(0, 1, 1).after(this.access.getML_COMMENTRule());
	}

	/** Configure the constructor formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureConstructor(FormattingConfig config, ConstructorElements ele) {
		config.setLinewrap(2).before(ele.getGroup());
		config.setNoSpace().around(ele.getLeftParenthesisKeyword_5_0());
		config.setNoSpace().before(ele.getRightParenthesisKeyword_5_2());
		config.setNoSpace().before(ele.getCommaKeyword_5_1_1_0());
	}

	/** Configure the skill formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureSkill(FormattingConfig config, SkillElements ele) {
		config.setLinewrap(1, 1, 2).before(ele.getGroup());
		config.setIndentation(ele.getLeftCurlyBracketKeyword_6(), ele.getRightCurlyBracketKeyword_8());
		config.setLinewrap().around(ele.getRightCurlyBracketKeyword_8());
	}

	/** Configure the capacity requirement formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureRequires(FormattingConfig config, RequiredCapacityElements ele) {
		config.setLinewrap().around(ele.getGroup());
		config.setNoSpace().before(ele.getCommaKeyword_3_0());
	}

	/** Configure the behavior formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureBehaviors(FormattingConfig config, BehaviorElements ele) {
		config.setLinewrap(2).before(ele.getGroup());
		config.setIndentation(ele.getLeftCurlyBracketKeyword_6(), ele.getRightCurlyBracketKeyword_8());
		config.setLinewrap().around(ele.getRightCurlyBracketKeyword_8());
	}

	/** Configure the capacity formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureCapacities(FormattingConfig config, CapacityElements ele) {
		config.setLinewrap(1, 1, 2).before(ele.getGroup());
		config.setIndentation(ele.getLeftCurlyBracketKeyword_6(), ele.getRightCurlyBracketKeyword_8());
		config.setLinewrap().around(ele.getRightCurlyBracketKeyword_8());
	}

	/** Configure the action formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureActions(FormattingConfig config, ActionElements ele) {
		config.setLinewrap(2).before(ele.getGroup());
		config.setNoSpace().around(ele.getLeftParenthesisKeyword_7_0());
		config.setNoSpace().before(ele.getRightParenthesisKeyword_7_2());
		config.setNoSpace().before(ele.getCommaKeyword_5_2_0());
		config.setNoSpace().before(ele.getCommaKeyword_7_1_1_0());
		config.setNoSpace().before(ele.getCommaKeyword_9_0_2_0());
		config.setNoSpace().before(ele.getCommaKeyword_9_1_2_0());
	}

	/** Configure the import formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureXImportDeclaration(FormattingConfig config, XImportDeclarationElements ele) {
		config.setLinewrap(1, 1, 2).after(ele.getGroup());
		config.setNoSpace().before(ele.getAlternatives_1_0_3());
	}

	/** Configure the attribute formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureAttributes(FormattingConfig config, FieldElements ele) {
		config.setLinewrap().around(ele.getGroup());
	}

	/** Configure the agent formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureAgent(FormattingConfig config, AgentElements ele) {
		config.setLinewrap(2).after(ele.getGroup());
		config.setLinewrap().before(ele.getRightCurlyBracketKeyword_8());
		config.setIndentation(ele.getLeftCurlyBracketKeyword_6(), ele.getRightCurlyBracketKeyword_8());
	}

	/** Configure the event formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureEvents(FormattingConfig config, EventElements ele) {
		config.setLinewrap(2).after(ele.getGroup());
		config.setLinewrap().before(ele.getRightCurlyBracketKeyword_6_2());
		config.setIndentation(ele.getLeftCurlyBracketKeyword_6_0(), ele.getRightCurlyBracketKeyword_6_2());
	}

	/** Configure the capacity use formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureUses(FormattingConfig config, CapacityUsesElements ele) {
		config.setLinewrap().around(ele.getGroup());
		config.setNoSpace().before(ele.getCommaKeyword_3_0());
	}

	/** Configure the behavior unit formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureBehaviorUnit(FormattingConfig config, BehaviorUnitElements ele) {
		config.setLinewrap(2).before(ele.getGroup());
		config.setLinewrap().after(ele.getGroup());

		config.setNoSpace().after(ele.getLeftSquareBracketKeyword_4_0());
		config.setNoSpace().before(ele.getRightSquareBracketKeyword_4_2());

	}

	@Override
	public void configureXIfExpression(FormattingConfig config, XIfExpressionElements ele) {
		config.setNoSpace().around(ele.getLeftParenthesisKeyword_2());
		config.setNoSpace().before(ele.getRightParenthesisKeyword_4());

		config.setLinewrap(1, 1, 2).before(ele.getGroup());

		config.setLinewrap(0, 1, 2).after(ele.getThenAssignment_5());

	}

	/** Configure the variable declaration formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureXVariableDeclaration(FormattingConfig config, XVariableDeclarationElements ele) {
		config.setLinewrap(0, 1, 2).around(ele.getGroup());
	}

	/** Configure the assignment formatting.
	 *
	 * @param config - the configuration.
	 * @param ele - the elements.
	 */
	@SuppressWarnings("static-method")
	public void configureXAssignmentElements(FormattingConfig config, XAssignmentElements ele) {
		config.setLinewrap(0, 1, 2).around(ele.getGroup_0());
	}

	@Override
	public void configureXTryCatchFinallyExpression(FormattingConfig config, XTryCatchFinallyExpressionElements ele) {
		//		c.setIndentationIncrement().before(ele.getExpressionAssignment_2());
		config.setLinewrap().around(ele.getExpressionAssignment_2());
		//		c.setIndentationDecrement().after(ele.getExpressionAssignment_2());
		//		c.setIndentationIncrement().before(ele.getFinallyExpressionAssignment_3_0_1_1());
		config.setLinewrap().around(ele.getFinallyExpressionAssignment_3_0_1_1());
		//		c.setIndentationDecrement().after(ele.getFinallyExpressionAssignment_3_0_1_1());
		//		c.setIndentationIncrement().before(ele.getFinallyExpressionAssignment_3_1_1());
		config.setLinewrap().around(ele.getFinallyExpressionAssignment_3_1_1());
		//		c.setIndentationDecrement().after(ele.getFinallyExpressionAssignment_3_1_1());
	}

	@Override
	public void configureXForLoopExpression(FormattingConfig config, XForLoopExpressionElements ele) {
		config.setNoSpace().around(ele.getLeftParenthesisKeyword_0_0_2());
		config.setNoSpace().around(ele.getColonKeyword_0_0_4());
		config.setNoSpace().around(ele.getRightParenthesisKeyword_2());

		//		c.setIndentationIncrement().before(ele.getEachExpressionAssignment_7());
		config.setLinewrap().around(ele.getEachExpressionAssignment_3());

		//		c.setIndentationDecrement().after(ele.getEachExpressionAssignment_7());
		config.setLinewrap(2).before(ele.getGroup());
	}

	@Override
	public void configureXMemberFeatureCall(FormattingConfig config, XMemberFeatureCallElements ele) {
		super.configureXMemberFeatureCall(config, ele);
		config.setLinewrap(0, 1, 2).around(ele.getGroup());
	}

	@Override
	public void configureXBlockExpression(FormattingConfig config, XBlockExpressionElements ele) {
		super.configureXBlockExpression(config, ele);
		config.setLinewrap(0, 1, 2).after(ele.getRightCurlyBracketKeyword_3());
	}

	@Override
	public void configureXCatchClause(FormattingConfig config, XCatchClauseElements ele) {
		config.setNoSpace().around(ele.getLeftParenthesisKeyword_1());
		config.setNoSpace().before(ele.getRightParenthesisKeyword_3());

		config.setLinewrap().around(ele.getExpressionAssignment_4());
	}

}

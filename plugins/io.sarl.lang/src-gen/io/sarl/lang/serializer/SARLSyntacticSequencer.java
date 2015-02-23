package io.sarl.lang.serializer;

import com.google.inject.Inject;
import io.sarl.lang.services.SARLGrammarAccess;
import java.util.List;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.IGrammarAccess;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.serializer.analysis.GrammarAlias.AbstractElementAlias;
import org.eclipse.xtext.serializer.analysis.GrammarAlias.GroupAlias;
import org.eclipse.xtext.serializer.analysis.GrammarAlias.TokenAlias;
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.ISynNavigable;
import org.eclipse.xtext.serializer.analysis.ISyntacticSequencerPDAProvider.ISynTransition;
import org.eclipse.xtext.serializer.sequencer.AbstractSyntacticSequencer;

@SuppressWarnings("all")
public class SARLSyntacticSequencer extends AbstractSyntacticSequencer {

	protected SARLGrammarAccess grammarAccess;
	protected AbstractElementAlias match_AnnotationField_SemicolonKeyword_2_0_2_q;
	protected AbstractElementAlias match_AnnotationField_SemicolonKeyword_2_3_6_q;
	protected AbstractElementAlias match_File_SemicolonKeyword_0_2_q;
	protected AbstractElementAlias match_SarlActionSignature_SemicolonKeyword_11_q;
	protected AbstractElementAlias match_SarlActionSignature___LeftParenthesisKeyword_8_0_RightParenthesisKeyword_8_2__q;
	protected AbstractElementAlias match_SarlAction___LeftParenthesisKeyword_8_0_RightParenthesisKeyword_8_2__q;
	protected AbstractElementAlias match_SarlCapacityUses_SemicolonKeyword_4_q;
	protected AbstractElementAlias match_SarlConstructor___LeftParenthesisKeyword_6_0_RightParenthesisKeyword_6_3__q;
	protected AbstractElementAlias match_SarlEnum_SemicolonKeyword_8_q;
	protected AbstractElementAlias match_SarlEvent___LeftCurlyBracketKeyword_7_0_RightCurlyBracketKeyword_7_2__q;
	protected AbstractElementAlias match_SarlField_SemicolonKeyword_7_q;
	protected AbstractElementAlias match_SarlRequiredCapacity_SemicolonKeyword_4_q;
	protected AbstractElementAlias match_SarlScript_SemicolonKeyword_0_2_q;
	protected AbstractElementAlias match_XAnnotation___LeftParenthesisKeyword_3_0_RightParenthesisKeyword_3_2__q;
	protected AbstractElementAlias match_XBlockExpression_SemicolonKeyword_2_1_q;
	protected AbstractElementAlias match_XExpressionInClosure_SemicolonKeyword_1_1_q;
	protected AbstractElementAlias match_XFunctionTypeRef___LeftParenthesisKeyword_0_0_RightParenthesisKeyword_0_2__q;
	protected AbstractElementAlias match_XImportDeclaration_SemicolonKeyword_2_q;
	protected AbstractElementAlias match_XParenthesizedExpression_LeftParenthesisKeyword_0_a;
	protected AbstractElementAlias match_XParenthesizedExpression_LeftParenthesisKeyword_0_p;
	
	@Inject
	protected void init(IGrammarAccess access) {
		grammarAccess = (SARLGrammarAccess) access;
		match_AnnotationField_SemicolonKeyword_2_0_2_q = new TokenAlias(false, true, grammarAccess.getAnnotationFieldAccess().getSemicolonKeyword_2_0_2());
		match_AnnotationField_SemicolonKeyword_2_3_6_q = new TokenAlias(false, true, grammarAccess.getAnnotationFieldAccess().getSemicolonKeyword_2_3_6());
		match_File_SemicolonKeyword_0_2_q = new TokenAlias(false, true, grammarAccess.getFileAccess().getSemicolonKeyword_0_2());
		match_SarlActionSignature_SemicolonKeyword_11_q = new TokenAlias(false, true, grammarAccess.getSarlActionSignatureAccess().getSemicolonKeyword_11());
		match_SarlActionSignature___LeftParenthesisKeyword_8_0_RightParenthesisKeyword_8_2__q = new GroupAlias(false, true, new TokenAlias(false, false, grammarAccess.getSarlActionSignatureAccess().getLeftParenthesisKeyword_8_0()), new TokenAlias(false, false, grammarAccess.getSarlActionSignatureAccess().getRightParenthesisKeyword_8_2()));
		match_SarlAction___LeftParenthesisKeyword_8_0_RightParenthesisKeyword_8_2__q = new GroupAlias(false, true, new TokenAlias(false, false, grammarAccess.getSarlActionAccess().getLeftParenthesisKeyword_8_0()), new TokenAlias(false, false, grammarAccess.getSarlActionAccess().getRightParenthesisKeyword_8_2()));
		match_SarlCapacityUses_SemicolonKeyword_4_q = new TokenAlias(false, true, grammarAccess.getSarlCapacityUsesAccess().getSemicolonKeyword_4());
		match_SarlConstructor___LeftParenthesisKeyword_6_0_RightParenthesisKeyword_6_3__q = new GroupAlias(false, true, new TokenAlias(false, false, grammarAccess.getSarlConstructorAccess().getLeftParenthesisKeyword_6_0()), new TokenAlias(false, false, grammarAccess.getSarlConstructorAccess().getRightParenthesisKeyword_6_3()));
		match_SarlEnum_SemicolonKeyword_8_q = new TokenAlias(false, true, grammarAccess.getSarlEnumAccess().getSemicolonKeyword_8());
		match_SarlEvent___LeftCurlyBracketKeyword_7_0_RightCurlyBracketKeyword_7_2__q = new GroupAlias(false, true, new TokenAlias(false, false, grammarAccess.getSarlEventAccess().getLeftCurlyBracketKeyword_7_0()), new TokenAlias(false, false, grammarAccess.getSarlEventAccess().getRightCurlyBracketKeyword_7_2()));
		match_SarlField_SemicolonKeyword_7_q = new TokenAlias(false, true, grammarAccess.getSarlFieldAccess().getSemicolonKeyword_7());
		match_SarlRequiredCapacity_SemicolonKeyword_4_q = new TokenAlias(false, true, grammarAccess.getSarlRequiredCapacityAccess().getSemicolonKeyword_4());
		match_SarlScript_SemicolonKeyword_0_2_q = new TokenAlias(false, true, grammarAccess.getSarlScriptAccess().getSemicolonKeyword_0_2());
		match_XAnnotation___LeftParenthesisKeyword_3_0_RightParenthesisKeyword_3_2__q = new GroupAlias(false, true, new TokenAlias(false, false, grammarAccess.getXAnnotationAccess().getLeftParenthesisKeyword_3_0()), new TokenAlias(false, false, grammarAccess.getXAnnotationAccess().getRightParenthesisKeyword_3_2()));
		match_XBlockExpression_SemicolonKeyword_2_1_q = new TokenAlias(false, true, grammarAccess.getXBlockExpressionAccess().getSemicolonKeyword_2_1());
		match_XExpressionInClosure_SemicolonKeyword_1_1_q = new TokenAlias(false, true, grammarAccess.getXExpressionInClosureAccess().getSemicolonKeyword_1_1());
		match_XFunctionTypeRef___LeftParenthesisKeyword_0_0_RightParenthesisKeyword_0_2__q = new GroupAlias(false, true, new TokenAlias(false, false, grammarAccess.getXFunctionTypeRefAccess().getLeftParenthesisKeyword_0_0()), new TokenAlias(false, false, grammarAccess.getXFunctionTypeRefAccess().getRightParenthesisKeyword_0_2()));
		match_XImportDeclaration_SemicolonKeyword_2_q = new TokenAlias(false, true, grammarAccess.getXImportDeclarationAccess().getSemicolonKeyword_2());
		match_XParenthesizedExpression_LeftParenthesisKeyword_0_a = new TokenAlias(true, true, grammarAccess.getXParenthesizedExpressionAccess().getLeftParenthesisKeyword_0());
		match_XParenthesizedExpression_LeftParenthesisKeyword_0_p = new TokenAlias(true, false, grammarAccess.getXParenthesizedExpressionAccess().getLeftParenthesisKeyword_0());
	}
	
	@Override
	protected String getUnassignedRuleCallToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if(ruleCall.getRule() == grammarAccess.getArrayBracketsRule())
			return getArrayBracketsToken(semanticObject, ruleCall, node);
		else if(ruleCall.getRule() == grammarAccess.getOpSingleAssignRule())
			return getOpSingleAssignToken(semanticObject, ruleCall, node);
		else if(ruleCall.getRule() == grammarAccess.getVarArgTokenRule())
			return getVarArgTokenToken(semanticObject, ruleCall, node);
		return "";
	}
	
	/**
	 * ArrayBrackets :
	 * 	'[' ']'
	 * ;
	 */
	protected String getArrayBracketsToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if (node != null)
			return getTokenText(node);
		return "[]";
	}
	
	/**
	 * OpSingleAssign:
	 * 	'='
	 * ;
	 */
	protected String getOpSingleAssignToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if (node != null)
			return getTokenText(node);
		return "=";
	}
	
	/**
	 * VarArgToken:
	 * 	'*'
	 * ;
	 */
	protected String getVarArgTokenToken(EObject semanticObject, RuleCall ruleCall, INode node) {
		if (node != null)
			return getTokenText(node);
		return "*";
	}
	
	@Override
	protected void emitUnassignedTokens(EObject semanticObject, ISynTransition transition, INode fromNode, INode toNode) {
		if (transition.getAmbiguousSyntaxes().isEmpty()) return;
		List<INode> transitionNodes = collectNodes(fromNode, toNode);
		for (AbstractElementAlias syntax : transition.getAmbiguousSyntaxes()) {
			List<INode> syntaxNodes = getNodesFor(transitionNodes, syntax);
			if(match_AnnotationField_SemicolonKeyword_2_0_2_q.equals(syntax))
				emit_AnnotationField_SemicolonKeyword_2_0_2_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_AnnotationField_SemicolonKeyword_2_3_6_q.equals(syntax))
				emit_AnnotationField_SemicolonKeyword_2_3_6_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_File_SemicolonKeyword_0_2_q.equals(syntax))
				emit_File_SemicolonKeyword_0_2_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SarlActionSignature_SemicolonKeyword_11_q.equals(syntax))
				emit_SarlActionSignature_SemicolonKeyword_11_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SarlActionSignature___LeftParenthesisKeyword_8_0_RightParenthesisKeyword_8_2__q.equals(syntax))
				emit_SarlActionSignature___LeftParenthesisKeyword_8_0_RightParenthesisKeyword_8_2__q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SarlAction___LeftParenthesisKeyword_8_0_RightParenthesisKeyword_8_2__q.equals(syntax))
				emit_SarlAction___LeftParenthesisKeyword_8_0_RightParenthesisKeyword_8_2__q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SarlCapacityUses_SemicolonKeyword_4_q.equals(syntax))
				emit_SarlCapacityUses_SemicolonKeyword_4_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SarlConstructor___LeftParenthesisKeyword_6_0_RightParenthesisKeyword_6_3__q.equals(syntax))
				emit_SarlConstructor___LeftParenthesisKeyword_6_0_RightParenthesisKeyword_6_3__q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SarlEnum_SemicolonKeyword_8_q.equals(syntax))
				emit_SarlEnum_SemicolonKeyword_8_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SarlEvent___LeftCurlyBracketKeyword_7_0_RightCurlyBracketKeyword_7_2__q.equals(syntax))
				emit_SarlEvent___LeftCurlyBracketKeyword_7_0_RightCurlyBracketKeyword_7_2__q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SarlField_SemicolonKeyword_7_q.equals(syntax))
				emit_SarlField_SemicolonKeyword_7_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SarlRequiredCapacity_SemicolonKeyword_4_q.equals(syntax))
				emit_SarlRequiredCapacity_SemicolonKeyword_4_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_SarlScript_SemicolonKeyword_0_2_q.equals(syntax))
				emit_SarlScript_SemicolonKeyword_0_2_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_XAnnotation___LeftParenthesisKeyword_3_0_RightParenthesisKeyword_3_2__q.equals(syntax))
				emit_XAnnotation___LeftParenthesisKeyword_3_0_RightParenthesisKeyword_3_2__q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_XBlockExpression_SemicolonKeyword_2_1_q.equals(syntax))
				emit_XBlockExpression_SemicolonKeyword_2_1_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_XExpressionInClosure_SemicolonKeyword_1_1_q.equals(syntax))
				emit_XExpressionInClosure_SemicolonKeyword_1_1_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_XFunctionTypeRef___LeftParenthesisKeyword_0_0_RightParenthesisKeyword_0_2__q.equals(syntax))
				emit_XFunctionTypeRef___LeftParenthesisKeyword_0_0_RightParenthesisKeyword_0_2__q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_XImportDeclaration_SemicolonKeyword_2_q.equals(syntax))
				emit_XImportDeclaration_SemicolonKeyword_2_q(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_XParenthesizedExpression_LeftParenthesisKeyword_0_a.equals(syntax))
				emit_XParenthesizedExpression_LeftParenthesisKeyword_0_a(semanticObject, getLastNavigableState(), syntaxNodes);
			else if(match_XParenthesizedExpression_LeftParenthesisKeyword_0_p.equals(syntax))
				emit_XParenthesizedExpression_LeftParenthesisKeyword_0_p(semanticObject, getLastNavigableState(), syntaxNodes);
			else acceptNodes(getLastNavigableState(), syntaxNodes);
		}
	}

	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_AnnotationField_SemicolonKeyword_2_0_2_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_AnnotationField_SemicolonKeyword_2_3_6_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_File_SemicolonKeyword_0_2_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_SarlActionSignature_SemicolonKeyword_11_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ('(' ')')?
	 */
	protected void emit_SarlActionSignature___LeftParenthesisKeyword_8_0_RightParenthesisKeyword_8_2__q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ('(' ')')?
	 */
	protected void emit_SarlAction___LeftParenthesisKeyword_8_0_RightParenthesisKeyword_8_2__q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_SarlCapacityUses_SemicolonKeyword_4_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ('(' ')')?
	 */
	protected void emit_SarlConstructor___LeftParenthesisKeyword_6_0_RightParenthesisKeyword_6_3__q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_SarlEnum_SemicolonKeyword_8_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ('{' '}')?
	 */
	protected void emit_SarlEvent___LeftCurlyBracketKeyword_7_0_RightCurlyBracketKeyword_7_2__q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_SarlField_SemicolonKeyword_7_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_SarlRequiredCapacity_SemicolonKeyword_4_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_SarlScript_SemicolonKeyword_0_2_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ('(' ')')?
	 */
	protected void emit_XAnnotation___LeftParenthesisKeyword_3_0_RightParenthesisKeyword_3_2__q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_XBlockExpression_SemicolonKeyword_2_1_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_XExpressionInClosure_SemicolonKeyword_1_1_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ('(' ')')?
	 */
	protected void emit_XFunctionTypeRef___LeftParenthesisKeyword_0_0_RightParenthesisKeyword_0_2__q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     ';'?
	 */
	protected void emit_XImportDeclaration_SemicolonKeyword_2_q(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     '('*
	 */
	protected void emit_XParenthesizedExpression_LeftParenthesisKeyword_0_a(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
	/**
	 * Syntax:
	 *     '('+
	 */
	protected void emit_XParenthesizedExpression_LeftParenthesisKeyword_0_p(EObject semanticObject, ISynNavigable transition, List<INode> nodes) {
		acceptNodes(transition, nodes);
	}
	
}

/**
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
package io.sarl.lang.formatting;

import com.google.inject.Inject;
import io.sarl.lang.services.SARLGrammarAccess;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.Group;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.TerminalRule;
import org.eclipse.xtext.formatting.impl.FormattingConfig;
import org.eclipse.xtext.xbase.formatting.XbaseFormatter;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.services.XbaseGrammarAccess;
import org.eclipse.xtext.xbase.services.XtypeGrammarAccess;

/**
 * This class contains custom formatting description.
 * 
 * see : http://www.eclipse.org/Xtext/documentation.html#formatting
 * on how and when to use it
 * 
 * Also see {@link org.eclipse.xtext.xtext.XtextFormattingTokenSerializer} as an example
 */
@SuppressWarnings("all")
public class SARLFormatter extends XbaseFormatter {
  @Inject
  @Extension
  private SARLGrammarAccess f;
  
  protected void configureFormatting(final FormattingConfig c) {
    c.setAutoLinewrap(400);
    XtypeGrammarAccess.XImportDeclarationElements _xImportDeclarationAccess = this.f.getXImportDeclarationAccess();
    this.configureXImportDeclaration(c, _xImportDeclarationAccess);
    SARLGrammarAccess.AgentElements _agentAccess = this.f.getAgentAccess();
    this.configureAgent(c, _agentAccess);
    SARLGrammarAccess.AttributeElements _attributeAccess = this.f.getAttributeAccess();
    this.configureAttributes(c, _attributeAccess);
    SARLGrammarAccess.EventElements _eventAccess = this.f.getEventAccess();
    this.configureEvents(c, _eventAccess);
    SARLGrammarAccess.CapacityUsesElements _capacityUsesAccess = this.f.getCapacityUsesAccess();
    this.configureUses(c, _capacityUsesAccess);
    SARLGrammarAccess.BehaviorUnitElements _behaviorUnitAccess = this.f.getBehaviorUnitAccess();
    this.configureBehaviorUnit(c, _behaviorUnitAccess);
    SARLGrammarAccess.XVariableDeclarationElements _xVariableDeclarationAccess = this.f.getXVariableDeclarationAccess();
    this.configureXVariableDeclaration(c, _xVariableDeclarationAccess);
    XbaseGrammarAccess.XAssignmentElements _xAssignmentAccess = this.f.getXAssignmentAccess();
    this.configureXAssignmentElements(c, _xAssignmentAccess);
    SARLGrammarAccess.ActionSignatureElements _actionSignatureAccess = this.f.getActionSignatureAccess();
    this.configureActionSignatures(c, _actionSignatureAccess);
    SARLGrammarAccess.CapacityElements _capacityAccess = this.f.getCapacityAccess();
    this.configureCapacities(c, _capacityAccess);
    SARLGrammarAccess.BehaviorElements _behaviorAccess = this.f.getBehaviorAccess();
    this.configureBehaviors(c, _behaviorAccess);
    SARLGrammarAccess.RequiredCapacityElements _requiredCapacityAccess = this.f.getRequiredCapacityAccess();
    this.configureRequires(c, _requiredCapacityAccess);
    SARLGrammarAccess.SkillElements _skillAccess = this.f.getSkillAccess();
    this.configureSkill(c, _skillAccess);
    SARLGrammarAccess.ConstructorElements _constructorAccess = this.f.getConstructorAccess();
    this.configureConstructor(c, _constructorAccess);
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(2);
    SARLGrammarAccess.ModelElements _modelAccess = this.f.getModelAccess();
    Group _group_0 = _modelAccess.getGroup_0();
    _setLinewrap.after(_group_0);
    XbaseGrammarAccess _xbaseGrammarAccess = this.f.getXbaseGrammarAccess();
    super.configure(c, _xbaseGrammarAccess);
    FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap(0, 1, 2);
    TerminalRule _sL_COMMENTRule = this.f.getSL_COMMENTRule();
    _setLinewrap_1.before(_sL_COMMENTRule);
    FormattingConfig.LinewrapLocator _setLinewrap_2 = c.setLinewrap(0, 1, 2);
    TerminalRule _mL_COMMENTRule = this.f.getML_COMMENTRule();
    _setLinewrap_2.before(_mL_COMMENTRule);
    FormattingConfig.LinewrapLocator _setLinewrap_3 = c.setLinewrap(0, 1, 1);
    TerminalRule _mL_COMMENTRule_1 = this.f.getML_COMMENTRule();
    _setLinewrap_3.after(_mL_COMMENTRule_1);
  }
  
  public void configureConstructor(final FormattingConfig c, final SARLGrammarAccess.ConstructorElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(2);
    Group _group = ele.getGroup();
    _setLinewrap.before(_group);
    FormattingConfig.NoSpaceLocator _setNoSpace = c.setNoSpace();
    Keyword _leftParenthesisKeyword_1_0 = ele.getLeftParenthesisKeyword_1_0();
    _setNoSpace.around(_leftParenthesisKeyword_1_0);
    FormattingConfig.NoSpaceLocator _setNoSpace_1 = c.setNoSpace();
    Keyword _rightParenthesisKeyword_1_3 = ele.getRightParenthesisKeyword_1_3();
    _setNoSpace_1.before(_rightParenthesisKeyword_1_3);
    FormattingConfig.NoSpaceLocator _setNoSpace_2 = c.setNoSpace();
    Keyword _commaKeyword_1_2_0 = ele.getCommaKeyword_1_2_0();
    _setNoSpace_2.before(_commaKeyword_1_2_0);
  }
  
  public void configureSkill(final FormattingConfig c, final SARLGrammarAccess.SkillElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(1, 1, 2);
    Group _group = ele.getGroup();
    _setLinewrap.before(_group);
    Keyword _leftCurlyBracketKeyword_5 = ele.getLeftCurlyBracketKeyword_5();
    Keyword _rightCurlyBracketKeyword_7 = ele.getRightCurlyBracketKeyword_7();
    c.setIndentation(_leftCurlyBracketKeyword_5, _rightCurlyBracketKeyword_7);
    FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap();
    Keyword _rightCurlyBracketKeyword_7_1 = ele.getRightCurlyBracketKeyword_7();
    _setLinewrap_1.around(_rightCurlyBracketKeyword_7_1);
  }
  
  public void configureRequires(final FormattingConfig c, final SARLGrammarAccess.RequiredCapacityElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap();
    Group _group = ele.getGroup();
    _setLinewrap.around(_group);
    FormattingConfig.NoSpaceLocator _setNoSpace = c.setNoSpace();
    Keyword _commaKeyword_2_0 = ele.getCommaKeyword_2_0();
    _setNoSpace.before(_commaKeyword_2_0);
  }
  
  public void configureBehaviors(final FormattingConfig c, final SARLGrammarAccess.BehaviorElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(2);
    Group _group = ele.getGroup();
    _setLinewrap.before(_group);
    Keyword _leftCurlyBracketKeyword_3 = ele.getLeftCurlyBracketKeyword_3();
    Keyword _rightCurlyBracketKeyword_5 = ele.getRightCurlyBracketKeyword_5();
    c.setIndentation(_leftCurlyBracketKeyword_3, _rightCurlyBracketKeyword_5);
    FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap();
    Keyword _rightCurlyBracketKeyword_5_1 = ele.getRightCurlyBracketKeyword_5();
    _setLinewrap_1.around(_rightCurlyBracketKeyword_5_1);
  }
  
  public void configureCapacities(final FormattingConfig c, final SARLGrammarAccess.CapacityElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(1, 1, 2);
    Group _group = ele.getGroup();
    _setLinewrap.before(_group);
    Keyword _leftCurlyBracketKeyword_3 = ele.getLeftCurlyBracketKeyword_3();
    Keyword _rightCurlyBracketKeyword_5 = ele.getRightCurlyBracketKeyword_5();
    c.setIndentation(_leftCurlyBracketKeyword_3, _rightCurlyBracketKeyword_5);
    FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap();
    Keyword _rightCurlyBracketKeyword_5_1 = ele.getRightCurlyBracketKeyword_5();
    _setLinewrap_1.around(_rightCurlyBracketKeyword_5_1);
  }
  
  public void configureActionSignatures(final FormattingConfig c, final SARLGrammarAccess.ActionSignatureElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(2);
    Group _group = ele.getGroup();
    _setLinewrap.before(_group);
    FormattingConfig.NoSpaceLocator _setNoSpace = c.setNoSpace();
    Keyword _leftParenthesisKeyword_2_0 = ele.getLeftParenthesisKeyword_2_0();
    _setNoSpace.around(_leftParenthesisKeyword_2_0);
    FormattingConfig.NoSpaceLocator _setNoSpace_1 = c.setNoSpace();
    Keyword _rightParenthesisKeyword_2_3 = ele.getRightParenthesisKeyword_2_3();
    _setNoSpace_1.before(_rightParenthesisKeyword_2_3);
    FormattingConfig.NoSpaceLocator _setNoSpace_2 = c.setNoSpace();
    Keyword _commaKeyword_2_2_0 = ele.getCommaKeyword_2_2_0();
    _setNoSpace_2.before(_commaKeyword_2_2_0);
    FormattingConfig.NoSpaceLocator _setNoSpace_3 = c.setNoSpace();
    Keyword _commaKeyword_4_2_0 = ele.getCommaKeyword_4_2_0();
    _setNoSpace_3.before(_commaKeyword_4_2_0);
  }
  
  public void configureXImportDeclaration(final FormattingConfig c, final XtypeGrammarAccess.XImportDeclarationElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(1, 1, 2);
    Group _group = ele.getGroup();
    _setLinewrap.after(_group);
    FormattingConfig.NoSpaceLocator _setNoSpace = c.setNoSpace();
    Keyword _fullStopKeyword_1_0_3 = ele.getFullStopKeyword_1_0_3();
    _setNoSpace.around(_fullStopKeyword_1_0_3);
  }
  
  public void configureAttributes(final FormattingConfig c, final SARLGrammarAccess.AttributeElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap();
    Group _group = ele.getGroup();
    _setLinewrap.around(_group);
  }
  
  public void configureAgent(final FormattingConfig c, final SARLGrammarAccess.AgentElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(2);
    Group _group = ele.getGroup();
    _setLinewrap.after(_group);
    FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap();
    Keyword _rightCurlyBracketKeyword_5 = ele.getRightCurlyBracketKeyword_5();
    _setLinewrap_1.before(_rightCurlyBracketKeyword_5);
    Keyword _leftCurlyBracketKeyword_3 = ele.getLeftCurlyBracketKeyword_3();
    Keyword _rightCurlyBracketKeyword_5_1 = ele.getRightCurlyBracketKeyword_5();
    c.setIndentation(_leftCurlyBracketKeyword_3, _rightCurlyBracketKeyword_5_1);
  }
  
  public void configureEvents(final FormattingConfig c, final SARLGrammarAccess.EventElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(2);
    Group _group = ele.getGroup();
    _setLinewrap.after(_group);
    FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap();
    Keyword _rightCurlyBracketKeyword_5 = ele.getRightCurlyBracketKeyword_5();
    _setLinewrap_1.before(_rightCurlyBracketKeyword_5);
    Keyword _leftCurlyBracketKeyword_3 = ele.getLeftCurlyBracketKeyword_3();
    Keyword _rightCurlyBracketKeyword_5_1 = ele.getRightCurlyBracketKeyword_5();
    c.setIndentation(_leftCurlyBracketKeyword_3, _rightCurlyBracketKeyword_5_1);
  }
  
  public void configureUses(final FormattingConfig c, final SARLGrammarAccess.CapacityUsesElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap();
    Group _group = ele.getGroup();
    _setLinewrap.around(_group);
    FormattingConfig.NoSpaceLocator _setNoSpace = c.setNoSpace();
    Keyword _commaKeyword_2_0 = ele.getCommaKeyword_2_0();
    _setNoSpace.before(_commaKeyword_2_0);
  }
  
  public void configureBehaviorUnit(final FormattingConfig c, final SARLGrammarAccess.BehaviorUnitElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(2);
    Group _group = ele.getGroup();
    _setLinewrap.before(_group);
    FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap();
    Group _group_1 = ele.getGroup();
    _setLinewrap_1.after(_group_1);
    FormattingConfig.NoSpaceLocator _setNoSpace = c.setNoSpace();
    Keyword _leftSquareBracketKeyword_2_0 = ele.getLeftSquareBracketKeyword_2_0();
    _setNoSpace.after(_leftSquareBracketKeyword_2_0);
    FormattingConfig.NoSpaceLocator _setNoSpace_1 = c.setNoSpace();
    Keyword _rightSquareBracketKeyword_2_2 = ele.getRightSquareBracketKeyword_2_2();
    _setNoSpace_1.before(_rightSquareBracketKeyword_2_2);
  }
  
  public void configureXIfExpression(final FormattingConfig c, final XbaseGrammarAccess.XIfExpressionElements ele) {
    FormattingConfig.NoSpaceLocator _setNoSpace = c.setNoSpace();
    Keyword _leftParenthesisKeyword_2 = ele.getLeftParenthesisKeyword_2();
    _setNoSpace.around(_leftParenthesisKeyword_2);
    FormattingConfig.NoSpaceLocator _setNoSpace_1 = c.setNoSpace();
    Keyword _rightParenthesisKeyword_4 = ele.getRightParenthesisKeyword_4();
    _setNoSpace_1.before(_rightParenthesisKeyword_4);
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(1, 1, 2);
    Group _group = ele.getGroup();
    _setLinewrap.before(_group);
    FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap(0, 1, 2);
    Assignment _thenAssignment_5 = ele.getThenAssignment_5();
    _setLinewrap_1.after(_thenAssignment_5);
  }
  
  public void configureXVariableDeclaration(final FormattingConfig c, final SARLGrammarAccess.XVariableDeclarationElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(0, 1, 2);
    Group _group = ele.getGroup();
    _setLinewrap.around(_group);
  }
  
  public void configureXAssignmentElements(final FormattingConfig c, final XbaseGrammarAccess.XAssignmentElements ele) {
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(0, 1, 2);
    Group _group_0 = ele.getGroup_0();
    _setLinewrap.around(_group_0);
  }
  
  public void configureXForLoopExpression(final FormattingConfig c, final XbaseGrammarAccess.XForLoopExpressionElements ele) {
    FormattingConfig.NoSpaceLocator _setNoSpace = c.setNoSpace();
    Keyword _leftParenthesisKeyword_2 = ele.getLeftParenthesisKeyword_2();
    _setNoSpace.around(_leftParenthesisKeyword_2);
    FormattingConfig.NoSpaceLocator _setNoSpace_1 = c.setNoSpace();
    Keyword _colonKeyword_4 = ele.getColonKeyword_4();
    _setNoSpace_1.around(_colonKeyword_4);
    FormattingConfig.NoSpaceLocator _setNoSpace_2 = c.setNoSpace();
    Keyword _rightParenthesisKeyword_6 = ele.getRightParenthesisKeyword_6();
    _setNoSpace_2.around(_rightParenthesisKeyword_6);
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap();
    Assignment _eachExpressionAssignment_7 = ele.getEachExpressionAssignment_7();
    _setLinewrap.around(_eachExpressionAssignment_7);
    FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap(2);
    Group _group = ele.getGroup();
    _setLinewrap_1.before(_group);
  }
  
  public void configureXMemberFeatureCall(final FormattingConfig c, final XbaseGrammarAccess.XMemberFeatureCallElements ele) {
    super.configureXMemberFeatureCall(c, ele);
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(0, 1, 2);
    Group _group = ele.getGroup();
    _setLinewrap.around(_group);
  }
}

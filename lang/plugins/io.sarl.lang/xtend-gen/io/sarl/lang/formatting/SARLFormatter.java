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
import java.util.List;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.ParserRule;
import org.eclipse.xtext.TerminalRule;
import org.eclipse.xtext.formatting.impl.AbstractDeclarativeFormatter;
import org.eclipse.xtext.formatting.impl.FormattingConfig;
import org.eclipse.xtext.util.Pair;
import org.eclipse.xtext.xbase.lib.Extension;

/**
 * This class contains custom formatting description.
 * 
 * see : http://www.eclipse.org/Xtext/documentation.html#formatting
 * on how and when to use it
 * 
 * Also see {@link org.eclipse.xtext.xtext.XtextFormattingTokenSerializer} as an example
 */
@SuppressWarnings("all")
public class SARLFormatter extends AbstractDeclarativeFormatter {
  @Inject
  @Extension
  private SARLGrammarAccess f;
  
  protected void configureFormatting(final FormattingConfig c) {
    c.setAutoLinewrap(120);
    List<Pair<Keyword,Keyword>> _findKeywordPairs = this.f.findKeywordPairs("{", "}");
    for (final Pair<Keyword,Keyword> pair : _findKeywordPairs) {
      {
        Keyword _first = pair.getFirst();
        Keyword _second = pair.getSecond();
        c.setIndentation(_first, _second);
        FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(1);
        Keyword _first_1 = pair.getFirst();
        _setLinewrap.after(_first_1);
        FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap(1);
        Keyword _second_1 = pair.getSecond();
        _setLinewrap_1.before(_second_1);
        FormattingConfig.LinewrapLocator _setLinewrap_2 = c.setLinewrap(1);
        Keyword _second_2 = pair.getSecond();
        _setLinewrap_2.after(_second_2);
      }
    }
    List<Pair<Keyword,Keyword>> _findKeywordPairs_1 = this.f.findKeywordPairs("(", ")");
    for (final Pair<Keyword,Keyword> pair_1 : _findKeywordPairs_1) {
      {
        FormattingConfig.NoSpaceLocator _setNoSpace = c.setNoSpace();
        Keyword _first = pair_1.getFirst();
        _setNoSpace.before(_first);
        FormattingConfig.NoSpaceLocator _setNoSpace_1 = c.setNoSpace();
        Keyword _first_1 = pair_1.getFirst();
        _setNoSpace_1.after(_first_1);
        FormattingConfig.NoSpaceLocator _setNoSpace_2 = c.setNoSpace();
        Keyword _second = pair_1.getSecond();
        _setNoSpace_2.before(_second);
      }
    }
    List<Keyword> _findKeywords = this.f.findKeywords(",");
    for (final Keyword comma : _findKeywords) {
      FormattingConfig.NoSpaceLocator _setNoSpace = c.setNoSpace();
      _setNoSpace.before(comma);
    }
    List<Keyword> _findKeywords_1 = this.f.findKeywords(".");
    for (final Keyword dot : _findKeywords_1) {
      FormattingConfig.NoSpaceLocator _setNoSpace_1 = c.setNoSpace();
      _setNoSpace_1.around(dot);
    }
    List<Pair<Keyword,Keyword>> _findKeywordPairs_2 = this.f.findKeywordPairs("<", ">");
    for (final Pair<Keyword,Keyword> pair_2 : _findKeywordPairs_2) {
      {
        FormattingConfig.NoSpaceLocator _setNoSpace_2 = c.setNoSpace();
        Keyword _first = pair_2.getFirst();
        _setNoSpace_2.after(_first);
        FormattingConfig.NoSpaceLocator _setNoSpace_3 = c.setNoSpace();
        Keyword _second = pair_2.getSecond();
        _setNoSpace_3.before(_second);
      }
    }
    FormattingConfig.LinewrapLocator _setLinewrap = c.setLinewrap(0, 1, 2);
    ParserRule _xImportSectionRule = this.f.getXImportSectionRule();
    _setLinewrap.before(_xImportSectionRule);
    FormattingConfig.LinewrapLocator _setLinewrap_1 = c.setLinewrap(0, 0, 1);
    ParserRule _xImportDeclarationRule = this.f.getXImportDeclarationRule();
    _setLinewrap_1.before(_xImportDeclarationRule);
    FormattingConfig.LinewrapLocator _setLinewrap_2 = c.setLinewrap(0, 1, 2);
    ParserRule _agentRule = this.f.getAgentRule();
    _setLinewrap_2.before(_agentRule);
    FormattingConfig.LinewrapLocator _setLinewrap_3 = c.setLinewrap(0, 1, 2);
    ParserRule _behaviorRule = this.f.getBehaviorRule();
    _setLinewrap_3.before(_behaviorRule);
    FormattingConfig.LinewrapLocator _setLinewrap_4 = c.setLinewrap(0, 1, 2);
    ParserRule _eventRule = this.f.getEventRule();
    _setLinewrap_4.before(_eventRule);
    FormattingConfig.LinewrapLocator _setLinewrap_5 = c.setLinewrap(0, 1, 2);
    ParserRule _capacityRule = this.f.getCapacityRule();
    _setLinewrap_5.before(_capacityRule);
    FormattingConfig.LinewrapLocator _setLinewrap_6 = c.setLinewrap(0, 1, 2);
    ParserRule _skillRule = this.f.getSkillRule();
    _setLinewrap_6.before(_skillRule);
    FormattingConfig.LinewrapLocator _setLinewrap_7 = c.setLinewrap(0, 1, 2);
    ParserRule _attributeRule = this.f.getAttributeRule();
    _setLinewrap_7.before(_attributeRule);
    FormattingConfig.LinewrapLocator _setLinewrap_8 = c.setLinewrap(0, 1, 2);
    ParserRule _behaviorUnitRule = this.f.getBehaviorUnitRule();
    _setLinewrap_8.before(_behaviorUnitRule);
    FormattingConfig.LinewrapLocator _setLinewrap_9 = c.setLinewrap(0, 1, 2);
    ParserRule _actionSignatureRule = this.f.getActionSignatureRule();
    _setLinewrap_9.before(_actionSignatureRule);
    FormattingConfig.LinewrapLocator _setLinewrap_10 = c.setLinewrap(0, 1, 2);
    ParserRule _requiredCapacityRule = this.f.getRequiredCapacityRule();
    _setLinewrap_10.before(_requiredCapacityRule);
    FormattingConfig.LinewrapLocator _setLinewrap_11 = c.setLinewrap(0, 1, 2);
    TerminalRule _sL_COMMENTRule = this.f.getSL_COMMENTRule();
    _setLinewrap_11.before(_sL_COMMENTRule);
    FormattingConfig.LinewrapLocator _setLinewrap_12 = c.setLinewrap(0, 1, 2);
    TerminalRule _mL_COMMENTRule = this.f.getML_COMMENTRule();
    _setLinewrap_12.before(_mL_COMMENTRule);
    FormattingConfig.LinewrapLocator _setLinewrap_13 = c.setLinewrap(0, 1, 1);
    TerminalRule _mL_COMMENTRule_1 = this.f.getML_COMMENTRule();
    _setLinewrap_13.after(_mL_COMMENTRule_1);
  }
}

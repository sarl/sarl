/**
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND
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
package io.sarl.docs.reference;

import com.google.inject.Inject;
import io.sarl.docs.utils.SARLParser;
import io.sarl.docs.utils.SARLSpecCreator;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Extension;
import org.jnario.runner.CreateWith;
import org.jnario.runner.ExampleGroupRunner;
import org.jnario.runner.Named;
import org.jnario.runner.Order;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * This document describes the general syntax of the SARL Language.
 * While we will use the agent definition, it is also valid for other concepts.
 * Please see the specific Reference documentation for details.
 */
@Named("General Syntax Reference")
@RunWith(ExampleGroupRunner.class)
@CreateWith(SARLSpecCreator.class)
@SuppressWarnings("all")
public class GeneralSyntaxReferenceSpec {
  @Inject
  @Extension
  @org.jnario.runner.Extension
  public SARLParser _sARLParser;
  
  /**
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Variable definition")
  @Order(1)
  public void _variableDefinition() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("//Variable defnition");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("var name : String");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("//final variable definition - It can not be reassigned");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("val string : String = \"string value\"");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("//Infered type to String");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("var lastname = \"Lastname\"");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    this._sARLParser.parsesSuccessfully(_builder);
  }
  
  /**
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Java compatibility")
  @Order(2)
  public void _javaCompatibility() throws Exception {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("//Variable defnition");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("var date : java.util.Date = new java.util.Date()");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("//final variable definition - It can not be reassigned");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("val otherDate : java.util.Date = new java.util.Date()");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("//Infered type to java.lang.String");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("var lastname = \"Lastname\"");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    this._sARLParser.parsesSuccessfully(_builder);
  }
  
  /**
   * @filter(.* = '''|'''|.parsesSuccessfully.*)
   */
  @Test
  @Named("Action definition")
  @Order(3)
  public void _actionDefinition() throws Exception {
    Assert.assertTrue(true);
  }
}

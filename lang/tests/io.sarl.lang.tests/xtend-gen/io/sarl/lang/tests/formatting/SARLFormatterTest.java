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
package io.sarl.lang.tests.formatting;

import com.google.inject.Inject;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.SarlScript;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.formatting.INodeModelFormatter;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.parser.IParseResult;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author $Author: Sebastian Rodriguez$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
@SuppressWarnings("all")
public class SARLFormatterTest {
  @Inject
  @Extension
  private ParseHelper<SarlScript> _parseHelper;
  
  @Inject
  @Extension
  private INodeModelFormatter _iNodeModelFormatter;
  
  @Test
  public void testAgents() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A1 { } agent A2 {}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A1 {");
    _builder_1.newLine();
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("agent A2 {");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testBehaviors() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("behavior B1 { } behavior B2 {}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("behavior B1 {");
    _builder_1.newLine();
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("behavior B2 {");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testBehaviors2() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("behavior B1 { ");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("requires Cap1, Cap2 ");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("uses Cap1");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("uses Cap2");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("def a1 {");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("//something");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("on E [] {");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("if(true == true) { println(\"Nice\")}");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("behavior B2 {}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("behavior B1 {");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("requires Cap1, Cap2");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("uses Cap1");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("uses Cap2");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("def a1 {");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("//something");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("on E [] {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("if(true == true) {");
    _builder_1.newLine();
    _builder_1.append("\t\t\t");
    _builder_1.append("println(\"Nice\")");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("behavior B2 {");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testAgentsSpaces() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A1 { }");
    _builder.newLine();
    _builder.append(" ");
    _builder.newLine();
    _builder.append(" ");
    _builder.newLine();
    _builder.append(" ");
    _builder.newLine();
    _builder.append(" ");
    _builder.append("agent A2 {}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A1 {");
    _builder_1.newLine();
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("agent A2 {");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testAgentActions() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A {");
    _builder.newLine();
    _builder.append("uses Cap1, Cap");
    _builder.newLine();
    _builder.newLine();
    _builder.append("on E [occurrence.name == \"Hello\"] {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("println(\"event is hello\") ");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("action2");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t\t\t\t\t");
    _builder.append("action2");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t\t\t\t\t\t\t\t\t");
    _builder.append("action2 \t\t\t\taction2");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    _builder.newLine();
    _builder.append("def action2 {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("println(\"action2\")");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("println(\"action2..\") println(\"action2..\")");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A {");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("uses Cap1, Cap");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("on E [occurrence.name == \"Hello\"] {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("println(\"event is hello\")");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("action2");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("action2");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("action2 action2");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("def action2 {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("println(\"action2\")");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("println(\"action2..\") println(\"action2..\")");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testSkill() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("skill S implements C{");
    _builder.newLine();
    _builder.append("new(ag:Agent){super(ag)}");
    _builder.newLine();
    _builder.append("def action(a:String){");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("println(a)");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("skill S implements C {");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("new(ag : Agent) {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("super(ag)");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("def action(a : String) {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("println(a)");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testAttributes() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A1 { var cmdAddr : Address var cmdAddr2 : Address }");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A1 {");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("var cmdAddr : Address");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("var cmdAddr2 : Address");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testVarDeclaration() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A1 { var cmdAddr : Address var cmdAddr2 : Address ");
    _builder.newLine();
    _builder.append("on E {");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("val cmd :Address");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("val cmd2 : Address");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A1 {");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("var cmdAddr : Address");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("var cmdAddr2 : Address");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("on E {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("val cmd : Address");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("val cmd2 : Address");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testVarAssigement() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A1 { var cmdAddr : Address var cmdAddr2 : Address ");
    _builder.newLine();
    _builder.append("on E {");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("val cmd :Address");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("val cmd2 : Address");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("cmd = null ");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("cmd2 = null");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A1 {");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("var cmdAddr : Address");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("var cmdAddr2 : Address");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("on E {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("val cmd : Address");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("val cmd2 : Address");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("cmd = null");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("cmd2 = null");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testUses() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A1 { uses Cap1 , Cap2, Cap3 }");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A1 {");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("uses Cap1, Cap2, Cap3");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testBehaviorUnitOnAgent() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A1 { uses Cap1");
    _builder.newLine();
    _builder.append("on Initialize {} }");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A1 {");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("uses Cap1");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("on Initialize {");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testBehaviorUnit() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("on Initialize { if(true) {println(\"hello\")} }");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A {");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("on Initialize {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("if(true) {");
    _builder_1.newLine();
    _builder_1.append("\t\t\t");
    _builder_1.append("println(\"hello\")");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testXbase() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("on Initialize {");
    _builder.newLine();
    _builder.append("\t\t\t\t\t");
    _builder.append("if(!dir.exists){");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("throw new IllegalArgumentException(\"Path [\"+myPath+\"] is not a Directory\")");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t\t\t");
    _builder.append("for(f : dir.listFiles()){");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("if(f.directory){");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("if(!f.name.equals(\".\")&& !f.name.equals(\"..\")) {");
    _builder.newLine();
    _builder.append("\t\t\t");
    _builder.append("val targetID = FileSearchAgent.spawnInContext(innerContext,#[f.absolutePath,false.toString]);");
    _builder.newLine();
    _builder.append("\t\t\t");
    _builder.append("innerContext.defaultSpace.emit(new Search =>[source=innerContext.defaultSpace.getAddress(ID); date=maxDate;fileMatch = fMatch], AddressScope.getScope(innerContext.defaultSpace.getAddress(targetID)));");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}else{");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("if((maxDate.time >= f.lastModified) && !(f.name.startsWith(\".\")) && (f.name.endsWith(fMatch))){");
    _builder.newLine();
    _builder.append("\t\t\t");
    _builder.append("emit(new FoundFile =>[file = f.absolutePath],AddressScope.getScope(cmdAddr))");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    _builder.append("if (true == true){prinln (\"cool!\")}else{}");
    _builder.newLine();
    _builder.append("\t\t\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A {");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("on Initialize {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("if(!dir.exists) {");
    _builder_1.newLine();
    _builder_1.append("\t\t\t");
    _builder_1.append("throw new IllegalArgumentException(\"Path [\" + myPath + \"] is not a Directory\")");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("for ( f : dir.listFiles() ) {");
    _builder_1.newLine();
    _builder_1.append("\t\t\t");
    _builder_1.append("if(f.directory) {");
    _builder_1.newLine();
    _builder_1.append("\t\t\t\t");
    _builder_1.append("if(!f.name.equals(\".\") && !f.name.equals(\"..\")) {");
    _builder_1.newLine();
    _builder_1.append("\t\t\t\t\t");
    _builder_1.append("val targetID = FileSearchAgent.spawnInContext(innerContext, # [ f.absolutePath , false.toString ]);");
    _builder_1.newLine();
    _builder_1.append("\t\t\t\t\t");
    _builder_1.append("innerContext.defaultSpace.emit(new Search => [source = innerContext.defaultSpace.getAddress(ID) ; date = maxDate ; fileMatch = fMatch], AddressScope.getScope(innerContext.defaultSpace.getAddress(targetID)));");
    _builder_1.newLine();
    _builder_1.append("\t\t\t\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("\t\t\t");
    _builder_1.append("} else {");
    _builder_1.newLine();
    _builder_1.append("\t\t\t\t");
    _builder_1.append("if((maxDate.time >= f.lastModified) && !(f.name.startsWith(\".\")) && (f.name.endsWith(fMatch))) {");
    _builder_1.newLine();
    _builder_1.append("\t\t\t\t\t");
    _builder_1.append("emit(new FoundFile => [file = f.absolutePath], AddressScope.getScope(cmdAddr))");
    _builder_1.newLine();
    _builder_1.append("\t\t\t\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("\t\t\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("if(true == true) {");
    _builder_1.newLine();
    _builder_1.append("\t\t\t");
    _builder_1.append("prinln(\"cool!\")");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("} else {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testBehaviorUnitIfNoBrackets() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("on Initialize { if(true) println(\"hello\") else println(\"goodbye\") }");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A {");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("on Initialize {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("if(true) println(\"hello\") else println(\"goodbye\")");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testImports() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("import io.sarl.core.Agent import io.sarl.core.Event");
    _builder.newLine();
    _builder.newLine();
    _builder.append("import io.sarl.core.Agent ");
    _builder.newLine();
    _builder.append("import io.sarl.core.Event");
    _builder.newLine();
    _builder.newLine();
    _builder.append("import static java.lang.String . *");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("import io.sarl.core.Agent");
    _builder_1.newLine();
    _builder_1.append("import io.sarl.core.Event");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("import io.sarl.core.Agent");
    _builder_1.newLine();
    _builder_1.append("import io.sarl.core.Event");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("import static java.lang.String.*");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testImports2() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("import io.sarl.core.Agent import io.sarl.core.Event");
    _builder.newLine();
    _builder.newLine();
    _builder.append("import io.sarl.core.Agent ");
    _builder.newLine();
    _builder.append("import io.sarl.core.Event");
    _builder.newLine();
    _builder.newLine();
    _builder.append("agent A {}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("import io.sarl.core.Agent");
    _builder_1.newLine();
    _builder_1.append("import io.sarl.core.Event");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("import io.sarl.core.Agent");
    _builder_1.newLine();
    _builder_1.append("import io.sarl.core.Event");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("agent A {");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testTryCatch() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("agent A{");
    _builder.newLine();
    _builder.append("\t\t\t");
    _builder.append("def asInteger(s : String) : Integer {");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("var res : Integer = null");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("try\t\t\t{");
    _builder.newLine();
    _builder.append("\t\t\t\t");
    _builder.append("res = Integer.parseInt(s)");
    _builder.newLine();
    _builder.append("\t\t\t");
    _builder.append("}\t\tcatch(NumberFormatException nfe){");
    _builder.newLine();
    _builder.append("\t\t\t");
    _builder.append("} ");
    _builder.newLine();
    _builder.append("\t\t\t");
    _builder.newLine();
    _builder.append("\t\t\t");
    _builder.append("return res");
    _builder.newLine();
    _builder.append("\t\t");
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("agent A {");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("def asInteger(s : String) : Integer {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("var res : Integer = null");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("try");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("{");
    _builder_1.newLine();
    _builder_1.append("\t\t\t");
    _builder_1.append("res = Integer.parseInt(s)");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("catch(NumberFormatException nfe)");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("{");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("return res");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testEvents() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("event E1 {} event E2 {}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("event E1 {");
    _builder_1.newLine();
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("event E2 {");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testEventsWithAgent() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("event E1 {} agent A {}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("event E1 {");
    _builder_1.newLine();
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("agent A {");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testEventsWithAgentAndImport() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("import io.sarl.core.Schedules");
    _builder.newLine();
    _builder.newLine();
    _builder.append("event E1 {} agent A {}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("import io.sarl.core.Schedules");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("event E1 {");
    _builder_1.newLine();
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("agent A {");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testActions() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("\t");
    _builder.append("import io.sarl.core.Schedules");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("event E1 {} agent A {");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("on Destroy {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("println(\"I\'m about to die, bye :-)\")");
    _builder.newLine();
    _builder.append("} def isInternal ( e : Event ) : boolean {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("return e.source.spaceId.equals(innerContext.defaultSpace.ID);");
    _builder.newLine();
    _builder.append("} def hasMembers : boolean {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("return innerContext.defaultSpace.participants.size > 1");
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("import io.sarl.core.Schedules");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("event E1 {");
    _builder_1.newLine();
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("agent A {");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("on Destroy {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("println(\"I\'m about to die, bye :-)\")");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("def isInternal(e : Event) : boolean {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("return e.source.spaceId.equals(innerContext.defaultSpace.ID);");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("def hasMembers : boolean {");
    _builder_1.newLine();
    _builder_1.append("\t\t");
    _builder_1.append("return innerContext.defaultSpace.participants.size > 1");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("}");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testCapacity() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("capacity ExternalContextAccess {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("/**");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* Replies all contexts this agent is a member of, including the default context");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("*/\t ");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("def getAllContexts : Collection<AgentContext>");
    _builder.newLine();
    _builder.newLine();
    _builder.append("\t");
    _builder.append("/**");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* Replies the AgentContext for the given ID.");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* The agent must have joined the context before calling this action or use its parentContextID");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* @see{Agent#getParentID}");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* @see{#join}");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* ");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* @throws UnknownContextException if the context specified context is not known by the agent.");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* @param contextID the ID of the context to get.");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("*/");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("def getContext(contextID : UUID): AgentContext");
    _builder.newLine();
    _builder.newLine();
    _builder.append("    ");
    _builder.append("/**");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* Joins a new parent context (a new super holon).");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* This actions registers the agent in the default Space of the parent Context.");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* . ");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* @fires ContextJoined in its inner Context default space (Behaviors#wake).");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* @fires MemberJoined in its parent Context default Space ");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("*/");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("def join(futureContext : UUID, futureContextDefaultSpaceID : UUID) fires ContextJoined, MemberJoined");
    _builder.newLine();
    _builder.newLine();
    _builder.append("\t");
    _builder.append("/**");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* Leaves the parent\'s context. ");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* @fires ContextLeft in its inner Context default space (Behaviors#wake).");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("* @fires MemberLeft in its parent Context default Space ");
    _builder.newLine();
    _builder.append("\t ");
    _builder.append("*/");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("def leave(contextID : UUID) fires ContextLeft, MemberLeft");
    _builder.newLine();
    _builder.newLine();
    _builder.append("}");
    _builder.newLine();
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("capacity ExternalContextAccess {");
    _builder_1.newLine();
    _builder_1.append("/**");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* Replies all contexts this agent is a member of, including the default context");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("*/");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("def getAllContexts : Collection <AgentContext>");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("/**");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* Replies the AgentContext for the given ID.");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* The agent must have joined the context before calling this action or use its parentContextID");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* @see{Agent#getParentID}");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* @see{#join}");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* ");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* @throws UnknownContextException if the context specified context is not known by the agent.");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* @param contextID the ID of the context to get.");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("*/");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("def getContext(contextID : UUID) : AgentContext");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("/**");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* Joins a new parent context (a new super holon).");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* This actions registers the agent in the default Space of the parent Context.");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* . ");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* @fires ContextJoined in its inner Context default space (Behaviors#wake).");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* @fires MemberJoined in its parent Context default Space ");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("*/");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("def join(futureContext : UUID, futureContextDefaultSpaceID : UUID) fires ContextJoined, MemberJoined");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("/**");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* Leaves the parent\'s context. ");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* @fires ContextLeft in its inner Context default space (Behaviors#wake).");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("* @fires MemberLeft in its parent Context default Space ");
    _builder_1.newLine();
    _builder_1.append("\t ");
    _builder_1.append("*/");
    _builder_1.newLine();
    _builder_1.append("\t");
    _builder_1.append("def leave(contextID : UUID) fires ContextLeft, MemberLeft");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  @Test
  public void testPackage() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("package test");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("import io.sarl.core.Agent import io.sarl.core.Event");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("import io.sarl.core.Agent ");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("import io.sarl.core.Event");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("import static java.lang.String . *");
    _builder.newLine();
    _builder.append("\t");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("event E {");
    _builder.newLine();
    _builder.append("\t");
    _builder.append("}");
    _builder.newLine();
    StringConcatenation _builder_1 = new StringConcatenation();
    _builder_1.append("package test");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("import io.sarl.core.Agent");
    _builder_1.newLine();
    _builder_1.append("import io.sarl.core.Event");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("import io.sarl.core.Agent");
    _builder_1.newLine();
    _builder_1.append("import io.sarl.core.Event");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("import static java.lang.String.*");
    _builder_1.newLine();
    _builder_1.newLine();
    _builder_1.append("event E {");
    _builder_1.newLine();
    _builder_1.append("}");
    this.assertFormattedAs(_builder, _builder_1);
  }
  
  public void assertFormattedAs(final CharSequence input, final CharSequence expected) {
    try {
      String _string = expected.toString();
      SarlScript _parse = this._parseHelper.parse(input);
      Resource _eResource = _parse.eResource();
      IParseResult _parseResult = ((XtextResource) _eResource).getParseResult();
      ICompositeNode _rootNode = _parseResult.getRootNode();
      int _length = input.length();
      INodeModelFormatter.IFormattedRegion _format = this._iNodeModelFormatter.format(_rootNode, 0, _length);
      String _formattedText = _format.getFormattedText();
      Assert.assertEquals(_string, _formattedText);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}

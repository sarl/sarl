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
package io.sarl.lang.tests.parsing;

import com.google.inject.Inject;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.AbstractElement;
import io.sarl.lang.sarl.Model;
import io.sarl.lang.sarl.SarlPackage;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author $Author: srodriguez$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
@SuppressWarnings("all")
public class AgentParsingTest {
  @Inject
  @Extension
  private ParseHelper<Model> _parseHelper;
  
  @Inject
  @Extension
  private ValidationTestHelper _validationTestHelper;
  
  @Test
  public void testParse() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("package test");
      _builder.newLine();
      _builder.append("agent A1 {}");
      _builder.newLine();
      _builder.append("agent A2 {}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void parseBehaviorDeclaration() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E {}");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void parseBehaviorWithGuard() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E {}");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E [ occurrence.source != null] {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void parseAgentWithAttributes() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var name : String = \"Hello\"");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var number : Integer");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void parseAgentWithConstAttributes() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val name : String = \"Hello\"");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var number : Integer");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void eventsMustBeDeclared() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E  {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      EClass _behaviorUnit = SarlPackage.eINSTANCE.getBehaviorUnit();
      this._validationTestHelper.assertError(mas, _behaviorUnit, Diagnostic.LINKING_DIAGNOSTIC, 
        "Couldn\'t resolve reference to Event \'E\'.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  @Ignore("not ready yet")
  public void constAttributesMustHaveIniatlizer() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val name : String = \"Hello\"");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val number : Integer");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      EClass _attribute = SarlPackage.eINSTANCE.getAttribute();
      this._validationTestHelper.assertError(mas, _attribute, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "Constant attribute \'number\' must be initialized .");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void capacityMustBeDeclaredBeforeUse() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("uses MyCap");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      EClass _capacityUses = SarlPackage.eINSTANCE.getCapacityUses();
      this._validationTestHelper.assertError(mas, _capacityUses, 
        Diagnostic.LINKING_DIAGNOSTIC, 
        "Couldn\'t resolve reference to Capacity \'MyCap\'.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void agentCanDeclareCapacityUses() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity MyCap {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def my_operation");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("uses MyCap");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}

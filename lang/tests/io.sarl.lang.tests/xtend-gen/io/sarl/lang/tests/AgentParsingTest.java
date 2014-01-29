/**
 * $Id$
 * 
 * SARL is an open-source multiagent language.
 * More details on &lt;http://www.sarl.io&gt;
 * Copyright (C) 2013 SARL Core Developers
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see &lt;http://www.gnu.org/licenses/&gt;.
 */
package io.sarl.lang.tests;

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
 * @author $Author: Sebastian Rodriguez$
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
      _builder.append("on E [ e.source != null] {}");
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
      _builder.append("var String name = \"Hello\"");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var Integer number\t\t\t\t");
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
      _builder.append("const String name = \"Hello\"");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var Integer number");
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
      _builder.append("const String name = \"Hello\"");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("const Integer number");
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

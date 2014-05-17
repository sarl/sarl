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
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
@SuppressWarnings("all")
public class ArgDefaultValueParsingTest {
  @Inject
  @Extension
  private ParseHelper<Model> _parseHelper;
  
  @Inject
  @Extension
  private ValidationTestHelper _validationTestHelper;
  
  @Test
  public void inAgentAction_1p() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int=4) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_1p_invalid1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int=4...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_1p_invalid2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int...=4) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      EClass _actionSignature = SarlPackage.eINSTANCE.getActionSignature();
      this._validationTestHelper.assertError(mas, _actionSignature, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "mismatched input \'=\' expecting \')\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_5p_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_5p_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_5p_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_5p_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_5p_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_5p_0_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_5p_0_3_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\") {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_5p_0_2_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\") {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_5p_0_1_2_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_5p_0_1_2_3_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\") {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_3p_vararg_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int, arg2 : int=45...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_3p_vararg_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int=45, arg2 : int...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_3p_vararg_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int, arg2 : int...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inAgentAction_3p_vararg_0_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int=56, arg2 : int...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_1p() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : int=4) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_1p_invalid1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : int=4...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_1p_invalid2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg : int...=4) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      EClass _constructor = SarlPackage.eINSTANCE.getConstructor();
      this._validationTestHelper.assertError(mas, _constructor, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "mismatched input \'=\' expecting \')\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_5p_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_5p_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_5p_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_5p_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_5p_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_5p_0_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_5p_0_3_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\") {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_5p_0_2_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\") {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_5p_0_1_2_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_5p_0_1_2_3_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\") {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_3p_vararg_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : int, arg2 : int=45...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_3p_vararg_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int, arg1 : int=45, arg2 : int...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_3p_vararg_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=45, arg1 : int, arg2 : int...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inBehaviorConstructor_3p_vararg_0_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=45, arg1 : int=56, arg2 : int...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final Model mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<AbstractElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}

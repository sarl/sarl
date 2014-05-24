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
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.TopElement;
import io.sarl.lang.validation.IssueCodes;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XbasePackage;
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
  private ParseHelper<SarlScript> _parseHelper;
  
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      _builder.append("def myaction(arg : int=4*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _formalParameter = SarlPackage.eINSTANCE.getFormalParameter();
      this._validationTestHelper.assertError(mas, _formalParameter, 
        IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER, 
        "A default value cannot be declared for the variadic formal parameter");
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
      _builder.append("def myaction(arg : int*=4) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _actionSignature = SarlPackage.eINSTANCE.getActionSignature();
      this._validationTestHelper.assertError(mas, _actionSignature, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "mismatched input \'*=\' expecting \')\'");
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      _builder.append("def myaction(arg0 : int, arg1 : int, arg2 : int=45*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _formalParameter = SarlPackage.eINSTANCE.getFormalParameter();
      this._validationTestHelper.assertError(mas, _formalParameter, 
        IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER, 
        "A default value cannot be declared for the variadic formal parameter");
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
      _builder.append("def myaction(arg0 : int, arg1 : int=45, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      _builder.append("def myaction(arg0 : int=45, arg1 : int, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      _builder.append("def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      _builder.append("new(arg : int=4*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _formalParameter = SarlPackage.eINSTANCE.getFormalParameter();
      this._validationTestHelper.assertError(mas, _formalParameter, 
        IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER, 
        "A default value cannot be declared for the variadic formal parameter");
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
      _builder.append("new(arg : int*=4) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _constructor = SarlPackage.eINSTANCE.getConstructor();
      this._validationTestHelper.assertError(mas, _constructor, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "mismatched input \'*=\' expecting \')\'");
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      _builder.append("new(arg0 : int, arg1 : int, arg2 : int=45*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _formalParameter = SarlPackage.eINSTANCE.getFormalParameter();
      this._validationTestHelper.assertError(mas, _formalParameter, 
        IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER, 
        "A default value cannot be declared for the variadic formal parameter");
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
      _builder.append("new(arg0 : int, arg1 : int=45, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      _builder.append("new(arg0 : int=45, arg1 : int, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
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
      _builder.append("new(arg0 : int=45, arg1 : int=56, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_1p() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int=4)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_1p_invalid1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int=4*)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _formalParameter = SarlPackage.eINSTANCE.getFormalParameter();
      this._validationTestHelper.assertError(mas, _formalParameter, 
        IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER, 
        "A default value cannot be declared for the variadic formal parameter");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_1p_invalid2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int*=4)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _actionSignature = SarlPackage.eINSTANCE.getActionSignature();
      this._validationTestHelper.assertError(mas, _actionSignature, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "mismatched input \'*=\' expecting \')\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_5p_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_5p_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_5p_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_5p_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_5p_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\")");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_5p_0_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_5p_0_3_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\")");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_5p_0_2_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\")");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_5p_0_1_2_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_5p_0_1_2_3_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\")");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_3p_vararg_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int, arg2 : int=45*)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _formalParameter = SarlPackage.eINSTANCE.getFormalParameter();
      this._validationTestHelper.assertError(mas, _formalParameter, 
        IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER, 
        "A default value cannot be declared for the variadic formal parameter");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_3p_vararg_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int=45, arg2 : int*)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_3p_vararg_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int, arg2 : int*)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inCapacity_3p_vararg_0_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_1p() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int=4) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_1p_invalid1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int=4*) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _formalParameter = SarlPackage.eINSTANCE.getFormalParameter();
      this._validationTestHelper.assertError(mas, _formalParameter, 
        IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER, 
        "A default value cannot be declared for the variadic formal parameter");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_1p_invalid2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg : int*=4) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _actionSignature = SarlPackage.eINSTANCE.getActionSignature();
      this._validationTestHelper.assertError(mas, _actionSignature, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "mismatched input \'*=\' expecting \')\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_5p_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int, arg4 : String) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_5p_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String=\"abc\", arg2 : int, arg3 : int, arg4 : String) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_5p_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_5p_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int = 34, arg4 : String) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_5p_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : String, arg2 : int, arg3 : int, arg4 : String=\"xyz\") {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_5p_0_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_5p_0_3_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int, arg3 : int=56, arg4 : String=\"def\") {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_5p_0_2_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String, arg2 : int=18, arg3 : int, arg4 : String=\"def\") {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_5p_0_1_2_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_5p_0_1_2_3_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=4, arg1 : String=\"ghj\", arg2 : int=18, arg3 : int=98, arg4 : String=\"klm\") {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_3p_vararg_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int, arg2 : int=45*) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _formalParameter = SarlPackage.eINSTANCE.getFormalParameter();
      this._validationTestHelper.assertError(mas, _formalParameter, 
        IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER, 
        "A default value cannot be declared for the variadic formal parameter");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_3p_vararg_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int=45, arg2 : int*) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_3p_vararg_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int, arg2 : int*) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inSkill_3p_vararg_0_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void constructorCast_String2int() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("package io.sarl.test");
      _builder.newLine();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=45, arg1 : int=\"S\", arg2 : int) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _formalParameter = SarlPackage.eINSTANCE.getFormalParameter();
      this._validationTestHelper.assertError(mas, _formalParameter, 
        org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST, 
        "Cannot cast from String to int");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void actionCast_String2int() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int=\"S\", arg2 : int) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _formalParameter = SarlPackage.eINSTANCE.getFormalParameter();
      this._validationTestHelper.assertError(mas, _formalParameter, 
        org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST, 
        "Cannot cast from String to int");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void constructorCast_int2double() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=45, arg1 : double=18, arg2 : int) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void actionCast_int2double() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : double=18, arg2 : int) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void constructorCast_double2int() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("new(arg0 : int=45, arg1 : int=18.0, arg2 : int) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _xNumberLiteral = XbasePackage.eINSTANCE.getXNumberLiteral();
      this._validationTestHelper.assertError(mas, _xNumberLiteral, 
        org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES, 
        "Type mismatch: cannot convert from double to int");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void actionCast_double2int() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int=18.0, arg2 : int) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(arg0)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _xNumberLiteral = XbasePackage.eINSTANCE.getXNumberLiteral();
      this._validationTestHelper.assertError(mas, _xNumberLiteral, 
        org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES, 
        "Type mismatch: cannot convert from double to int");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void overridingCapacitySkill() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int=45, arg1 : int=56, arg2 : int*)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def capAction {}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int, arg2 : int*) {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(2, _size);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleActionDefinitionsInBehavior() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int=42, arg2 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(\"valid\")");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int*) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(\"invalid\")");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _action = SarlPackage.eINSTANCE.getAction();
      this._validationTestHelper.assertError(mas, _action, 
        IssueCodes.ACTION_ALREADY_DEFINED, 
        "Cannot define many times the same feature in \'B1\': myaction(arg0 : int, arg1 : int)");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleActionDefinitionsInAgent() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int=42, arg2 : int...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(\"valid\")");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(\"invalid\")");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _action = SarlPackage.eINSTANCE.getAction();
      this._validationTestHelper.assertError(mas, _action, 
        IssueCodes.ACTION_ALREADY_DEFINED, 
        "Cannot define many times the same feature in \'A1\': myaction(arg0 : int, arg1 : int)");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleActionDefinitionsInSkill() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int=42, arg2 : int...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(\"valid\")");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(arg0 : int, arg1 : int...) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(\"invalid\")");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _action = SarlPackage.eINSTANCE.getAction();
      this._validationTestHelper.assertError(mas, _action, 
        IssueCodes.ACTION_ALREADY_DEFINED, 
        "Cannot define many times the same feature in \'S1\': myaction(arg0 : int, arg1 : int)");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void missedActionImplementation_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction1(a : int=4)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2(b : float=6, c : boolean)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1, C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction1(x : int) { }");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2(y : float, z : boolean) { }");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void missedActionImplementation_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction1(a : int=4)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2(b : float=6, c : boolean)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1, C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2(b : float, c : boolean) { }");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _skill = SarlPackage.eINSTANCE.getSkill();
      this._validationTestHelper.assertError(mas, _skill, 
        IssueCodes.MISSING_ACTION_IMPLEMENTATION, 
        "The operation myaction1(int) must be implemented.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void missedActionImplementation_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction1(a : int=4)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2(b : float=6, c : boolean)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements C1, C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction1(x : float) { }");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2(y : float, z : boolean) { }");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _skill = SarlPackage.eINSTANCE.getSkill();
      this._validationTestHelper.assertError(mas, _skill, 
        IssueCodes.MISSING_ACTION_IMPLEMENTATION, 
        "The operation myaction1(int) must be implemented.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}

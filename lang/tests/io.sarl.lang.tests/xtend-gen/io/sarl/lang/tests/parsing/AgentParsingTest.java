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
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
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
  private ParseHelper<SarlScript> _parseHelper;
  
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
      final SarlScript mas = this._parseHelper.parse(_builder);
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
      final SarlScript mas = this._parseHelper.parse(_builder);
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
      final SarlScript mas = this._parseHelper.parse(_builder);
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertError(mas, _jvmParameterizedTypeReference, 
        Diagnostic.LINKING_DIAGNOSTIC, 
        "Couldn\'t resolve reference to JvmType \'E\'.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmField = TypesPackage.eINSTANCE.getJvmField();
      this._validationTestHelper.assertError(mas, _jvmField, 
        IssueCodes.MISSING_INITIALIZATION, 
        "The blank final field \'number\' may not have been initialized");
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertError(mas, _jvmParameterizedTypeReference, 
        Diagnostic.LINKING_DIAGNOSTIC, 
        "Couldn\'t resolve reference to JvmType \'MyCap\'");
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
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleActionDefinitionInAgent() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int, b : int) { }");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) { }");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) { }");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _action = SarlPackage.eINSTANCE.getAction();
      this._validationTestHelper.assertError(mas, _action, 
        io.sarl.lang.validation.IssueCodes.DUPLICATE_METHOD, 
        "Duplicate action in \'A1\': myaction(a : int)");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleVariableDefinition() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myfield : int");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myfield1 : String");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myfield : double");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _attribute = SarlPackage.eINSTANCE.getAttribute();
      this._validationTestHelper.assertError(mas, _attribute, 
        io.sarl.lang.validation.IssueCodes.DUPLICATE_FIELD, 
        "Duplicate field in \'A1\': myfield");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleValueDefinition() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val myfield : int = 4");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val myfield1 : String = \"\"");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val myfield : double = 5");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _attribute = SarlPackage.eINSTANCE.getAttribute();
      this._validationTestHelper.assertError(mas, _attribute, 
        io.sarl.lang.validation.IssueCodes.DUPLICATE_FIELD, 
        "Duplicate field in \'A1\': myfield");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidActionName() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(\"ok\")");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def _handle_myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(\"ko\")");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2 {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(\"ok\")");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _actionSignature = SarlPackage.eINSTANCE.getActionSignature();
      this._validationTestHelper.assertError(mas, _actionSignature, 
        io.sarl.lang.validation.IssueCodes.INVALID_MEMBER_NAME, 
        "Invalid action name \'_handle_myaction\'.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidAttributeName_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myfield1 = 4.5");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myfield2 = true");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _attribute = SarlPackage.eINSTANCE.getAttribute();
      this._validationTestHelper.assertError(mas, _attribute, 
        io.sarl.lang.validation.IssueCodes.INVALID_MEMBER_NAME, 
        "Invalid attribute name \'___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD\'. You must not give to an attribute a name that is starting with \'___FORMAL_PARAMETER_DEFAULT_VALUE_\'. This prefix is reserved by the SARL compiler.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidAttributeName_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val myfield1 = 4.5");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = \"String\"");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val myfield2 = true");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _attribute = SarlPackage.eINSTANCE.getAttribute();
      this._validationTestHelper.assertError(mas, _attribute, 
        io.sarl.lang.validation.IssueCodes.INVALID_MEMBER_NAME, 
        "Invalid attribute name \'___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD\'. You must not give to an attribute a name that is starting with \'___FORMAL_PARAMETER_DEFAULT_VALUE_\'. This prefix is reserved by the SARL compiler.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void missedFinalFieldInitialization() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val field1 : int = 5");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val field2 : String");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmField = TypesPackage.eINSTANCE.getJvmField();
      this._validationTestHelper.assertError(mas, _jvmField, 
        IssueCodes.MISSING_INITIALIZATION, 
        "The blank final field \'field2\' may not have been initialized");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void completeFinalFieldInitialization() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val field1 : int = 5");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val field2 : String = \"\"");
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
  public void fieldNameShadowing() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val field1 : int = 5");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) { }");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent A2 extends A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val field1 : int = 5");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) { }");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmField = TypesPackage.eINSTANCE.getJvmField();
      this._validationTestHelper.assertWarning(mas, _jvmField, 
        IssueCodes.VARIABLE_NAME_SHADOWING, 
        "The field \'field1\' in \'A2\' is hidding the inherited field \'A1.field1\'.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void incompatibleReturnType_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : int {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("return 0");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent A2 extends A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : float {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("return 0f");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmOperation = TypesPackage.eINSTANCE.getJvmOperation();
      this._validationTestHelper.assertError(mas, _jvmOperation, 
        IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
        "Incompatible return type between \'float\' and \'int\' for myaction(int).");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void incompatibleReturnType_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("// void");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent A2 extends A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : int {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("return 0");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmOperation = TypesPackage.eINSTANCE.getJvmOperation();
      this._validationTestHelper.assertError(mas, _jvmOperation, 
        IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
        "Incompatible return type between \'int\' and \'void\' for myaction(int).");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void incompatibleReturnType_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : int {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("return 0");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent A2 extends A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("// void");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmOperation = TypesPackage.eINSTANCE.getJvmOperation();
      this._validationTestHelper.assertError(mas, _jvmOperation, 
        IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
        "Incompatible return type between \'void\' and \'int\' for myaction(int).");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void compatibleReturnType_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : Number {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("return 0.0");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent A2 extends A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : Double {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("return 0.0");
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
  public void compatibleReturnType_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : float {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("return 0f");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent A2 extends A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : float {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("return 0f");
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
  public void invalidExtend() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent A1 extends C1 {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _agent = SarlPackage.eINSTANCE.getAgent();
      this._validationTestHelper.assertError(mas, _agent, 
        IssueCodes.TYPE_BOUNDS_MISMATCH, 
        "Invalid super-type: \'C1\'. Only the type \'io.sarl.lang.core.Agent\' and one of its subtypes are allowed for \'A1\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidFires() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1");
      _builder.newLine();
      _builder.append("behavior B1 { }");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction1 fires E1, B1");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _actionSignature = SarlPackage.eINSTANCE.getActionSignature();
      this._validationTestHelper.assertError(mas, _actionSignature, 
        IssueCodes.TYPE_BOUNDS_MISMATCH, 
        "Invalid type: \'B1\'. Only events can be used after the keyword \'fires\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidBehaviorUnit_EventType() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on String {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _behaviorUnit = SarlPackage.eINSTANCE.getBehaviorUnit();
      this._validationTestHelper.assertError(mas, _behaviorUnit, 
        IssueCodes.TYPE_BOUNDS_MISMATCH, 
        "Invalid type: \'java.lang.String\'. Only events are allowed after the keyword \'on\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidBehaviorUnit_GuardType() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E1 [ \"hello\" ] {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _xExpression = XbasePackage.eINSTANCE.getXExpression();
      this._validationTestHelper.assertError(mas, _xExpression, 
        IssueCodes.INCOMPATIBLE_TYPES, 
        "Type mismatch: cannot convert from String to boolean");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void trueGuardBehaviorUnit() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E1 [ true ] {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _xBooleanLiteral = XbasePackage.eINSTANCE.getXBooleanLiteral();
      this._validationTestHelper.assertWarning(mas, _xBooleanLiteral, 
        io.sarl.lang.validation.IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION, 
        "Discouraged boolean value. The guard is always true.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void falseGuardBehaviorUnit() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E1 [ false ] {}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _behaviorUnit = SarlPackage.eINSTANCE.getBehaviorUnit();
      this._validationTestHelper.assertWarning(mas, _behaviorUnit, 
        IssueCodes.UNREACHABLE_CODE, 
        "Dead code. The guard is always false.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}

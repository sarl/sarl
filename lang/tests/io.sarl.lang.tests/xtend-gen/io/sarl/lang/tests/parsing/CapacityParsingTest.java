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

import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.TopElement;
import io.sarl.lang.validation.IssueCodes;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
@SuppressWarnings("all")
public class CapacityParsingTest {
  @Inject
  @Extension
  private ParseHelper<SarlScript> _parseHelper;
  
  @Inject
  @Extension
  private ValidationTestHelper _validationTestHelper;
  
  private SarlScript mas;
  
  private Iterable<Capacity> knownCapacities;
  
  @Before
  public void setUp() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("package test");
      _builder.newLine();
      _builder.append("capacity C {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op1");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op2");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C3 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op3");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C4 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op4");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.newLine();
      _builder.append("skill S implements C {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op1 {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("//do Something");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S2 implements C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def op1 {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("//do Something");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("event E {}");
      _builder.newLine();
      _builder.append("agent A {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("val shouldChange = true");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("if (shouldChange) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("C.setSkill(S)\t");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}else {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.newLine();
      _builder.append("\t\t ");
      _builder.append("while(true) {");
      _builder.newLine();
      _builder.append("\t\t ");
      _builder.append("C3.op3 \t");
      _builder.newLine();
      _builder.append("\t\t ");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t\t ");
      _builder.append("C3.read ");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("C2.op1");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent B {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("on E {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("doSomething(C3)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("C4.op4");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      SarlScript _parse = this._parseHelper.parse(_builder);
      this.mas = _parse;
      EList<TopElement> _elements = this.mas.getElements();
      Iterable<Capacity> _filter = Iterables.<Capacity>filter(_elements, Capacity.class);
      this.knownCapacities = _filter;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void testParsedElements() {
    EList<TopElement> _elements = this.mas.getElements();
    int _size = _elements.size();
    Assert.assertEquals(9, _size);
  }
  
  public void testErrorFreeExampleCode() {
    this._validationTestHelper.assertNoErrors(this.mas);
  }
  
  @Test
  public void testAgentFind() {
    EList<TopElement> _elements = this.mas.getElements();
    final Iterable<Agent> agents = Iterables.<Agent>filter(_elements, Agent.class);
    int _size = IterableExtensions.size(agents);
    Assert.assertEquals(2, _size);
    Agent _head = IterableExtensions.<Agent>head(agents);
    String _name = _head.getName();
    Assert.assertEquals("A", _name);
  }
  
  @Test
  public void testFindCapacityReferences() {
    EList<TopElement> _elements = this.mas.getElements();
    final Iterable<Agent> agents = Iterables.<Agent>filter(_elements, Agent.class);
    int _size = IterableExtensions.size(agents);
    Assert.assertEquals(2, _size);
  }
  
  @Test
  public void testCapacityDirectImplementation() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import io.sarl.lang.sarl.Capacity");
      _builder.newLine();
      _builder.append("skill S1 implements Capacity {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _skill = SarlPackage.eINSTANCE.getSkill();
      this._validationTestHelper.assertError(mas, _skill, 
        Diagnostic.LINKING_DIAGNOSTIC, 
        "Couldn\'t resolve reference to InheritingElement \'Capacity\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleActionDefinitionInCapacity() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int, b : int)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _actionSignature = SarlPackage.eINSTANCE.getActionSignature();
      this._validationTestHelper.assertError(mas, _actionSignature, 
        IssueCodes.ACTION_ALREADY_DEFINED, 
        "Cannot define many times the same feature in \'C1\': myaction(a : int)");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleActionDefinitionInSkill() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
        IssueCodes.ACTION_ALREADY_DEFINED, 
        "Cannot define many times the same feature in \'S1\': myaction(a : int)");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleVariableDefinitionInSkill() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
        IssueCodes.FIELD_ALREADY_DEFINED, 
        "Cannot define many times the same feature in \'S1\': myfield");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleValueDefinitionInSkill() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
        IssueCodes.FIELD_ALREADY_DEFINED, 
        "Cannot define many times the same feature in \'S1\': myfield");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidActionNameInCapacity() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def _handle_myaction");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _actionSignature = SarlPackage.eINSTANCE.getActionSignature();
      this._validationTestHelper.assertError(mas, _actionSignature, 
        IssueCodes.INVALID_ACTION_NAME, 
        "Invalid action name \'_handle_myaction\'.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidActionNameInSkill() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
        IssueCodes.INVALID_ACTION_NAME, 
        "Invalid action name \'_handle_myaction\'.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void missedFinalFieldInitialization() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
        org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_INITIALIZATION, 
        "The blank final field \'field2\' may not have been initialized");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void completeFinalFieldInitialization() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
  public void fieldNameShadowingInSkill() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("capacity C2 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val field1 : int = 5");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) { }");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S2 extends S1 implements C2 {");
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
        IssueCodes.FIELD_NAME_SHADOWING, 
        "The field \'field1\' in \'S2\' is hidding the inherited field \'S1.field1\'.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void redundantCapacity_fromSuperType() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {}");
      _builder.newLine();
      _builder.append("capacity C2 {}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 { }");
      _builder.newLine();
      _builder.append("skill S2 extends S1 implements C2, C1 { }");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertWarning(mas, _jvmParameterizedTypeReference, 
        IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION, 
        "The feature \'C1\' is already implemented by the super type \'S1\'.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void redundantCapacity_duplicate() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {}");
      _builder.newLine();
      _builder.append("capacity C2 {}");
      _builder.newLine();
      _builder.append("capacity C3 {}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 { }");
      _builder.newLine();
      _builder.append("skill S2 extends S1 implements C2, C3, C2 { }");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertWarning(mas, _jvmParameterizedTypeReference, 
        IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION, 
        "The feature \'C2\' is already implemented by the preceding interface \'C2\'.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void redundantCapacity_fromPreviousCapacity() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {}");
      _builder.newLine();
      _builder.append("capacity C2 {}");
      _builder.newLine();
      _builder.append("capacity C3 extends C2 {}");
      _builder.newLine();
      _builder.append("skill S1 implements C1 { }");
      _builder.newLine();
      _builder.append("skill S2 extends S1 implements C3, C2 { }");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertWarning(mas, _jvmParameterizedTypeReference, 
        IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION, 
        "The feature \'C2\' is already implemented by the preceding interface \'C3\'.");
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
      _builder.append("def myaction1(a : int)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2(b : float, c : boolean)");
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
      _builder.append("def myaction1(a : int)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2(b : float, c : boolean)");
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
      _builder.append("def myaction1(a : int)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C2 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction2(b : float, c : boolean)");
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
  
  @Test
  public void incompatibleReturnType_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("capacity C2 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
      _builder.append("skill S2 extends S1 implements C2 {");
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
        org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
        "Incompatible return type between \'float\' and \'int\' for myaction(int).");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void incompatibleReturnType_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("capacity C2 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
      _builder.append("skill S2 extends S1 implements C2 {");
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
        org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
        "Incompatible return type between \'int\' and \'void\' for myaction(int).");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void incompatibleReturnType_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("capacity C2 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
      _builder.append("skill S2 extends S1 implements C2 {");
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
        org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
        "Incompatible return type between \'void\' and \'int\' for myaction(int).");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void incompatibleReturnType_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : int");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S2 implements C1 {");
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
        org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
        "Incompatible return type between \'float\' and \'int\' for myaction(int).");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void incompatibleReturnType_4() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) // void");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S2 implements C1 {");
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
        org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
        "Incompatible return type between \'int\' and \'void\' for myaction(int).");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void incompatibleReturnType_5() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : int");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S2 implements C1 {");
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
        org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
        "Incompatible return type between \'void\' and \'int\' for myaction(int).");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void compatibleReturnType_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("capacity C2 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
      _builder.append("skill S2 extends S1 implements C2 {");
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
      _builder.append("capacity C1 { }");
      _builder.newLine();
      _builder.append("capacity C2 { }");
      _builder.newLine();
      _builder.append("skill S1 implements C1 {");
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
      _builder.append("skill S2 extends S1 implements C2 {");
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
  public void compatibleReturnType_2() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : Number");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S2 implements C1 {");
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
  public void compatibleReturnType_3() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : float");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S2 implements C1 {");
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
}

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
import org.eclipse.emf.ecore.EClass;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.validation.IssueCodes;
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
  
  @Test
  public void testCapacityDirectImplementation() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import io.sarl.lang.core.Capacity");
      _builder.newLine();
      _builder.append("skill S1 implements Capacity {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertError(mas, _jvmParameterizedTypeReference, 
        IssueCodes.TYPE_BOUNDS_MISMATCH, 
        "Invalid implemented type: \'io.sarl.lang.core.Capacity\'. Only subtypes of \'io.sarl.lang.core.Capacity\' are allowed for \'S1\'");
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
        io.sarl.lang.validation.IssueCodes.DUPLICATE_METHOD, 
        "Duplicate action in \'C1\': myaction(a : int)");
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
      EClass _actionSignature = SarlPackage.eINSTANCE.getActionSignature();
      this._validationTestHelper.assertError(mas, _actionSignature, 
        io.sarl.lang.validation.IssueCodes.DUPLICATE_METHOD, 
        "Duplicate action in \'S1\': myaction(a : int)");
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
        io.sarl.lang.validation.IssueCodes.DUPLICATE_FIELD, 
        "Duplicate field in \'S1\': myfield");
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
        io.sarl.lang.validation.IssueCodes.DUPLICATE_FIELD, 
        "Duplicate field in \'S1\': myfield");
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
        io.sarl.lang.validation.IssueCodes.INVALID_MEMBER_NAME, 
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
        io.sarl.lang.validation.IssueCodes.INVALID_MEMBER_NAME, 
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
        IssueCodes.VARIABLE_NAME_SHADOWING, 
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
        io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION, 
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
        io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION, 
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
        io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION, 
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
        io.sarl.lang.validation.IssueCodes.MISSING_METHOD_IMPLEMENTATION, 
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
        io.sarl.lang.validation.IssueCodes.MISSING_METHOD_IMPLEMENTATION, 
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
        IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
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
        IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
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
        IssueCodes.INCOMPATIBLE_RETURN_TYPE, 
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
  
  @Test
  public void invalidCapacityTypeForUses() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : float");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("event E1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var abc : int");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("uses C1, E1");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertError(mas, _jvmParameterizedTypeReference, 
        IssueCodes.TYPE_BOUNDS_MISMATCH, 
        "Invalid type: \'E1\'. Only capacities can be used after the keyword \'uses\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidCapacityTypeForRequires() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction(a : int) : float");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("event E1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var abc : int");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("requires C1, E1");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertError(mas, _jvmParameterizedTypeReference, 
        IssueCodes.TYPE_BOUNDS_MISMATCH, 
        "Invalid type: \'E1\'. Only capacities can be used after the keyword \'requires\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidCapacityExtend() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity C1 extends A1 {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertError(mas, _jvmParameterizedTypeReference, 
        IssueCodes.TYPE_BOUNDS_MISMATCH, 
        "Invalid super-type: \'A1\'. Only the type \'io.sarl.lang.core.Capacity\' and one of its subtypes are allowed for \'C1\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidSkillExtend_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity C1 {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 extends A1 implements C1 {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertError(mas, _jvmParameterizedTypeReference, 
        IssueCodes.TYPE_BOUNDS_MISMATCH, 
        "Invalid super-type: \'A1\'. Only the type \'io.sarl.lang.core.Skill\' and one of its subtypes are allowed for \'S1\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidSkillExtend_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("behavior B1 {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("skill S1 implements B1 {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertError(mas, _jvmParameterizedTypeReference, 
        IssueCodes.TYPE_BOUNDS_MISMATCH, 
        "Invalid implemented type: \'B1\'. Only subtypes of \'io.sarl.lang.core.Capacity\' are allowed");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void inheritance() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity CapTest1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def func1 : int");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("capacity CapTest2 extends CapTest1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def func2(a : int)");
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

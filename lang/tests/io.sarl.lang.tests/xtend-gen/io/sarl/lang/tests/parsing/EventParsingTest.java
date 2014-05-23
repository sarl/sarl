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
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
@SuppressWarnings("all")
public class EventParsingTest {
  @Inject
  @Extension
  private ParseHelper<SarlScript> _parseHelper;
  
  @Inject
  @Extension
  private ValidationTestHelper _validationTestHelper;
  
  @Test
  public void missedFinalFieldInitialization() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1 {");
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
      _builder.append("event E1 {");
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
  public void invalidAttributeName_0() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1 {");
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
        io.sarl.lang.validation.IssueCodes.INVALID_ATTRIBUTE_NAME, 
        "Invalid attribute name \'___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD\'. You must not give to an attribute a name that is starting with \'___FORMAL_PARAMETER_DEFAULT_VALUE_\'. This prefix is reserved by the SARL compiler.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void invalidAttributeName_1() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1 {");
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
        io.sarl.lang.validation.IssueCodes.INVALID_ATTRIBUTE_NAME, 
        "Invalid attribute name \'___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD\'. You must not give to an attribute a name that is starting with \'___FORMAL_PARAMETER_DEFAULT_VALUE_\'. This prefix is reserved by the SARL compiler.");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleVariableDefinition() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1 {");
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
        io.sarl.lang.validation.IssueCodes.FIELD_ALREADY_DEFINED, 
        "Cannot define many times the same feature in \'E1\': myfield");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multipleValueDefinition() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("event E1 {");
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
        io.sarl.lang.validation.IssueCodes.FIELD_ALREADY_DEFINED, 
        "Cannot define many times the same feature in \'E1\': myfield");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}

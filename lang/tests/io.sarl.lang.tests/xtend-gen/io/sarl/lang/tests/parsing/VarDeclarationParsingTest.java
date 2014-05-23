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
public class VarDeclarationParsingTest {
  @Inject
  @Extension
  private ParseHelper<SarlScript> _parseHelper;
  
  @Inject
  @Extension
  private ValidationTestHelper _validationTestHelper;
  
  @Test
  public void variableDeclaration_attributeScope_xtend() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import java.util.List");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var List<Integer> list");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var i = 45");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var double j = 45");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _attribute = SarlPackage.eINSTANCE.getAttribute();
      this._validationTestHelper.assertError(mas, _attribute, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "no viable alternative at input \'List\'");
      EClass _attribute_1 = SarlPackage.eINSTANCE.getAttribute();
      this._validationTestHelper.assertError(mas, _attribute_1, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "no viable alternative at input \'double\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void variableDeclaration_localScope_xtend() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("var int i");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("var j = 45");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("var double k = 45");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(i)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(j)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(k)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _xFeatureCall = XbasePackage.eINSTANCE.getXFeatureCall();
      this._validationTestHelper.assertError(mas, _xFeatureCall, 
        Diagnostic.LINKING_DIAGNOSTIC, 
        "Couldn\'t resolve reference to JvmIdentifiableElement \'i\'");
      EClass _xAssignment = XbasePackage.eINSTANCE.getXAssignment();
      this._validationTestHelper.assertError(mas, _xAssignment, 
        Diagnostic.LINKING_DIAGNOSTIC, 
        "The method k(int) is undefined");
      EClass _xFeatureCall_1 = XbasePackage.eINSTANCE.getXFeatureCall();
      this._validationTestHelper.assertError(mas, _xFeatureCall_1, 
        Diagnostic.LINKING_DIAGNOSTIC, 
        "Couldn\'t resolve reference to JvmIdentifiableElement \'k\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void variableDeclaration_attributeScope() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import java.util.List");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var list : List<Integer>");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var i = 45");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var j : double = 45");
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
  public void variableDeclaration_localScope() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import java.util.List");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("var i : List<Integer>");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("var j = 45");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("var k : double = 45");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(i)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(j)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(k)");
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
  public void valueDeclaration_attributeScope_xtend() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import java.util.List");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val List<Integer> list");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val i = 45");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val double j = 45");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _attribute = SarlPackage.eINSTANCE.getAttribute();
      this._validationTestHelper.assertError(mas, _attribute, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "no viable alternative at input \'List\'");
      EClass _attribute_1 = SarlPackage.eINSTANCE.getAttribute();
      this._validationTestHelper.assertError(mas, _attribute_1, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "no viable alternative at input \'double\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void valueDeclaration_localScope_xtend() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("val int i");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("val j = 45");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("val double k = 45");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(i)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(j)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(k)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _xFeatureCall = XbasePackage.eINSTANCE.getXFeatureCall();
      this._validationTestHelper.assertError(mas, _xFeatureCall, 
        Diagnostic.LINKING_DIAGNOSTIC, 
        "Couldn\'t resolve reference to JvmIdentifiableElement \'i\'");
      EClass _xAssignment = XbasePackage.eINSTANCE.getXAssignment();
      this._validationTestHelper.assertError(mas, _xAssignment, 
        Diagnostic.LINKING_DIAGNOSTIC, 
        "The method k(int) is undefined");
      EClass _xFeatureCall_1 = XbasePackage.eINSTANCE.getXFeatureCall();
      this._validationTestHelper.assertError(mas, _xFeatureCall_1, 
        Diagnostic.LINKING_DIAGNOSTIC, 
        "Couldn\'t resolve reference to JvmIdentifiableElement \'k\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void valueDeclaration_attributeScope() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import java.util.List");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val list : List<Integer> = null");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val i = 45");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("val j : double = 45");
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
  public void valueDeclaration_localScope() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("val j = 45");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("val k : double = 45");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(j)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("System.out.println(k)");
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
  public void forLoop_xtend() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import java.util.List");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var list : List<Integer>");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("for( Number i : list) {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(i)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _xFeatureCall = XbasePackage.eINSTANCE.getXFeatureCall();
      this._validationTestHelper.assertError(mas, _xFeatureCall, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "missing \';\' at \'i\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void forLoop_inferredType() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import java.util.List");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var list : List<Integer>");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("for( i : list) {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(i)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
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
  public void forLoop_explicitType() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("import java.util.List");
      _builder.newLine();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var list : List<Integer>");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("for( i as Number : list) {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(i)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
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
  public void catch_xtend() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("try {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(\"G\")");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("catch(Throwable e) {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(e)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertError(mas, _jvmParameterizedTypeReference, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "missing \':\' at \'e\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void catch_oneType() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("try {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(\"G\")");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("catch(e : Throwable) {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(e)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
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
  public void multicatch_xtend() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("try {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(\"G\")");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("catch(Exception e) {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(e)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("catch(Throwable e) {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(e)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _jvmParameterizedTypeReference = TypesPackage.eINSTANCE.getJvmParameterizedTypeReference();
      this._validationTestHelper.assertError(mas, _jvmParameterizedTypeReference, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "missing \':\' at \'e\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void multicatch_oneType() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("try {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(\"G\")");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("catch(e : Exception) {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(e)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("catch(e : Throwable) {");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("System.out.println(e)");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("}");
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
  public void closure_xtend() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def mycall(a : int, f : (Number,Number) => int) {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("return a + f.apply");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("mycall(4) [ Float a, Integer b |");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("2 * a.floatValue + b.intValue");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("]");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      EClass _xClosure = XbasePackage.eINSTANCE.getXClosure();
      this._validationTestHelper.assertError(mas, _xClosure, 
        Diagnostic.SYNTAX_DIAGNOSTIC, 
        "mismatched input \',\' expecting \']\'");
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void closure_twoParams() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def mycall(a : int, f : (Float,Integer) => float) : float {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("return a + f.apply(5.45f, 6)");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("}");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def myaction : void {");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("mycall(4) [ a : Float, b : Integer |");
      _builder.newLine();
      _builder.append("\t\t\t");
      _builder.append("2f * a.floatValue + b.intValue");
      _builder.newLine();
      _builder.append("\t\t");
      _builder.append("]");
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
}

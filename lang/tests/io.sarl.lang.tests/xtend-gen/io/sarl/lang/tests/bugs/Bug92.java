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
package io.sarl.lang.tests.bugs;

import com.google.common.base.Objects;
import com.google.inject.Inject;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.TopElement;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmPrimitiveType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
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
public class Bug92 {
  @Inject
  @Extension
  private ParseHelper<SarlScript> _parseHelper;
  
  @Inject
  @Extension
  private ValidationTestHelper _validationTestHelper;
  
  @Inject
  @Extension
  private CompilationTestHelper _compilationTestHelper;
  
  protected static void assertInstance(final Class<?> expected, final Object actual) {
    boolean _and = false;
    boolean _notEquals = (!Objects.equal(actual, null));
    if (!_notEquals) {
      _and = false;
    } else {
      boolean _isInstance = expected.isInstance(actual);
      boolean _not = (!_isInstance);
      _and = _not;
    }
    if (_and) {
      String _name = expected.getName();
      String _plus = ("Unexpected type of object. The object must be a " + _name);
      String _plus_1 = (_plus + " but it is: ");
      String _plus_2 = (_plus_1 + actual);
      Assert.fail(_plus_2);
    }
  }
  
  protected static JvmType createType(final Class<?> t) {
    JvmGenericType result = TypesFactory.eINSTANCE.createJvmGenericType();
    String _simpleName = t.getSimpleName();
    result.setSimpleName(_simpleName);
    Package _package = t.getPackage();
    String _name = _package.getName();
    result.setPackageName(_name);
    return result;
  }
  
  protected static JvmType createPrimitiveType(final Class<?> t) {
    JvmPrimitiveType result = TypesFactory.eINSTANCE.createJvmPrimitiveType();
    String _name = t.getName();
    result.setSimpleName(_name);
    return result;
  }
  
  @Test
  public void attributeDeclarationSyntax_inferredDouble() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myDouble = 0d");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
      EList<TopElement> _elements_1 = mas.getElements();
      TopElement _get = _elements_1.get(0);
      Bug92.assertInstance(Agent.class, _get);
      EList<TopElement> _elements_2 = mas.getElements();
      TopElement _get_1 = _elements_2.get(0);
      Agent ag = ((Agent) _get_1);
      EList<EObject> _features = ag.getFeatures();
      int _size_1 = _features.size();
      Assert.assertEquals(1, _size_1);
      EList<EObject> _features_1 = ag.getFeatures();
      EObject _get_2 = _features_1.get(0);
      Bug92.assertInstance(Attribute.class, _get_2);
      EList<EObject> _features_2 = ag.getFeatures();
      EObject _get_3 = _features_2.get(0);
      Attribute attr = ((Attribute) _get_3);
      String _name = attr.getName();
      Assert.assertEquals("myDouble", _name);
      boolean _isWriteable = attr.isWriteable();
      Assert.assertTrue(_isWriteable);
      JvmTypeReference _type = attr.getType();
      Assert.assertNull(_type);
      XExpression _initialValue = attr.getInitialValue();
      Bug92.assertInstance(XNumberLiteral.class, _initialValue);
      XExpression _initialValue_1 = attr.getInitialValue();
      XNumberLiteral literal = ((XNumberLiteral) _initialValue_1);
      String _value = literal.getValue();
      Assert.assertEquals("0d", _value);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void attributeDeclarationSyntax_Double() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myDouble : Double = 0d");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
      EList<TopElement> _elements_1 = mas.getElements();
      TopElement _get = _elements_1.get(0);
      Bug92.assertInstance(Agent.class, _get);
      EList<TopElement> _elements_2 = mas.getElements();
      TopElement _get_1 = _elements_2.get(0);
      Agent ag = ((Agent) _get_1);
      EList<EObject> _features = ag.getFeatures();
      int _size_1 = _features.size();
      Assert.assertEquals(1, _size_1);
      EList<EObject> _features_1 = ag.getFeatures();
      EObject _get_2 = _features_1.get(0);
      Bug92.assertInstance(Attribute.class, _get_2);
      EList<EObject> _features_2 = ag.getFeatures();
      EObject _get_3 = _features_2.get(0);
      Attribute attr = ((Attribute) _get_3);
      String _name = attr.getName();
      Assert.assertEquals("myDouble", _name);
      boolean _isWriteable = attr.isWriteable();
      Assert.assertTrue(_isWriteable);
      JvmTypeReference _type = attr.getType();
      Bug92.assertInstance(JvmParameterizedTypeReference.class, _type);
      JvmTypeReference _type_1 = attr.getType();
      JvmParameterizedTypeReference type = ((JvmParameterizedTypeReference) _type_1);
      JvmType _createType = Bug92.createType(Double.class);
      String _qualifiedName = _createType.getQualifiedName();
      JvmType _type_2 = type.getType();
      String _qualifiedName_1 = _type_2.getQualifiedName();
      Assert.assertEquals(_qualifiedName, _qualifiedName_1);
      XExpression _initialValue = attr.getInitialValue();
      Bug92.assertInstance(XNumberLiteral.class, _initialValue);
      XExpression _initialValue_1 = attr.getInitialValue();
      XNumberLiteral literal = ((XNumberLiteral) _initialValue_1);
      String _value = literal.getValue();
      Assert.assertEquals("0d", _value);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void attributeDeclarationSyntax_double() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myDouble : double = 0d");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final SarlScript mas = this._parseHelper.parse(_builder);
      this._validationTestHelper.assertNoErrors(mas);
      EList<TopElement> _elements = mas.getElements();
      int _size = _elements.size();
      Assert.assertEquals(1, _size);
      EList<TopElement> _elements_1 = mas.getElements();
      int _size_1 = _elements_1.size();
      Assert.assertEquals(1, _size_1);
      EList<TopElement> _elements_2 = mas.getElements();
      TopElement _get = _elements_2.get(0);
      Bug92.assertInstance(Agent.class, _get);
      EList<TopElement> _elements_3 = mas.getElements();
      TopElement _get_1 = _elements_3.get(0);
      Agent ag = ((Agent) _get_1);
      EList<EObject> _features = ag.getFeatures();
      int _size_2 = _features.size();
      Assert.assertEquals(1, _size_2);
      EList<EObject> _features_1 = ag.getFeatures();
      EObject _get_2 = _features_1.get(0);
      Bug92.assertInstance(Attribute.class, _get_2);
      EList<EObject> _features_2 = ag.getFeatures();
      EObject _get_3 = _features_2.get(0);
      Attribute attr = ((Attribute) _get_3);
      String _name = attr.getName();
      Assert.assertEquals("myDouble", _name);
      boolean _isWriteable = attr.isWriteable();
      Assert.assertTrue(_isWriteable);
      JvmTypeReference _type = attr.getType();
      Bug92.assertInstance(JvmParameterizedTypeReference.class, _type);
      JvmTypeReference _type_1 = attr.getType();
      JvmParameterizedTypeReference type = ((JvmParameterizedTypeReference) _type_1);
      JvmType _createPrimitiveType = Bug92.createPrimitiveType(double.class);
      String _qualifiedName = _createPrimitiveType.getQualifiedName();
      JvmType _type_2 = type.getType();
      String _qualifiedName_1 = _type_2.getQualifiedName();
      Assert.assertEquals(_qualifiedName, _qualifiedName_1);
      XExpression _initialValue = attr.getInitialValue();
      Bug92.assertInstance(XNumberLiteral.class, _initialValue);
      XExpression _initialValue_1 = attr.getInitialValue();
      XNumberLiteral literal = ((XNumberLiteral) _initialValue_1);
      String _value = literal.getValue();
      Assert.assertEquals("0d", _value);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void attributeDeclarationCompiler_inferredDouble() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myDouble = 0d");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Agent;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class A1 extends Agent {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public A1(final java.util.UUID parentID) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super(parentID);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("protected double myDouble = 0d;");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void attributeDeclarationCompiler_Double() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myDouble : Double = 0d");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Agent;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class A1 extends Agent {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public A1(final java.util.UUID parentID) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super(parentID);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("protected Double myDouble = Double.valueOf(0d);");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void attributeDeclarationCompiler_double() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("agent A1 {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var myDouble : double = 0d");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Agent;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public class A1 extends Agent {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("/**");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("* Creates a new Agent of type A1");
      _builder_1.newLine();
      _builder_1.append("   ");
      _builder_1.append("*/");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public A1(final java.util.UUID parentID) {");
      _builder_1.newLine();
      _builder_1.append("    ");
      _builder_1.append("super(parentID);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("}");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("protected double myDouble = 0d;");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      this._compilationTestHelper.assertCompilesTo(_builder, _builder_1);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void originialCode_withDoubleType() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity ComputeEnergyCapacity {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def getEnergy(currentTime : Double, deltaTime : Double, wantedEnergy : Double) : Double");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def setVoltage(currentVoltage : Double)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent EntityAgent {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent DeviceAgent extends EntityAgent {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("uses ComputeEnergyCapacity");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("requires ComputeEnergyCapacity");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var busTime : Double = 0d");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var wantedIntensity : Double");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final String source = _builder.toString();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Capacity;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public interface ComputeEnergyCapacity extends Capacity {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public void setVoltage(final Double currentVoltage);");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      final String expected1 = _builder_1.toString();
      StringConcatenation _builder_2 = new StringConcatenation();
      _builder_2.append("@SuppressWarnings(\"all\")");
      _builder_2.newLine();
      _builder_2.append("public class DeviceAgent extends EntityAgent {");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("/**");
      _builder_2.newLine();
      _builder_2.append("   ");
      _builder_2.append("* Creates a new Agent of type DeviceAgent");
      _builder_2.newLine();
      _builder_2.append("   ");
      _builder_2.append("*/");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("public DeviceAgent(final java.util.UUID parentID) {");
      _builder_2.newLine();
      _builder_2.append("    ");
      _builder_2.append("super(parentID);");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("}");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("public void setVoltage(final Double currentVoltage) {");
      _builder_2.newLine();
      _builder_2.append("    ");
      _builder_2.append("getSkill(ComputeEnergyCapacity.class).setVoltage(currentVoltage);");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("}");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("public Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy) {");
      _builder_2.newLine();
      _builder_2.append("    ");
      _builder_2.append("return getSkill(ComputeEnergyCapacity.class).getEnergy(currentTime, deltaTime, wantedEnergy);");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("}");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("protected Double busTime = Double.valueOf(0d);");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("protected Double wantedIntensity;");
      _builder_2.newLine();
      _builder_2.append("}");
      _builder_2.newLine();
      final String expected2 = _builder_2.toString();
      StringConcatenation _builder_3 = new StringConcatenation();
      _builder_3.append("import io.sarl.lang.core.Agent;");
      _builder_3.newLine();
      _builder_3.newLine();
      _builder_3.append("@SuppressWarnings(\"all\")");
      _builder_3.newLine();
      _builder_3.append("public class EntityAgent extends Agent {");
      _builder_3.newLine();
      _builder_3.append("  ");
      _builder_3.append("/**");
      _builder_3.newLine();
      _builder_3.append("   ");
      _builder_3.append("* Creates a new Agent of type EntityAgent");
      _builder_3.newLine();
      _builder_3.append("   ");
      _builder_3.append("*/");
      _builder_3.newLine();
      _builder_3.append("  ");
      _builder_3.append("public EntityAgent(final java.util.UUID parentID) {");
      _builder_3.newLine();
      _builder_3.append("    ");
      _builder_3.append("super(parentID);");
      _builder_3.newLine();
      _builder_3.append("  ");
      _builder_3.append("}");
      _builder_3.newLine();
      _builder_3.append("}");
      _builder_3.newLine();
      final String expected3 = _builder_3.toString();
      final IAcceptor<CompilationTestHelper.Result> _function = new IAcceptor<CompilationTestHelper.Result>() {
        public void accept(final CompilationTestHelper.Result r) {
          String _generatedCode = r.getGeneratedCode("ComputeEnergyCapacity");
          Assert.assertEquals(expected1, _generatedCode);
          String _generatedCode_1 = r.getGeneratedCode("DeviceAgent");
          Assert.assertEquals(expected2, _generatedCode_1);
          String _generatedCode_2 = r.getGeneratedCode("EntityAgent");
          Assert.assertEquals(expected3, _generatedCode_2);
        }
      };
      this._compilationTestHelper.compile(source, _function);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Test
  public void originialCode_withoutDoubleType() {
    try {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("capacity ComputeEnergyCapacity {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def getEnergy(currentTime : Double, deltaTime : Double, wantedEnergy : Double) : Double");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("def setVoltage(currentVoltage : Double)");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent EntityAgent {");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      _builder.append("agent DeviceAgent extends EntityAgent {");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("uses ComputeEnergyCapacity");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("requires ComputeEnergyCapacity");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var busTime = 0d");
      _builder.newLine();
      _builder.append("\t");
      _builder.append("var wantedIntensity : Double");
      _builder.newLine();
      _builder.append("}");
      _builder.newLine();
      final String source = _builder.toString();
      StringConcatenation _builder_1 = new StringConcatenation();
      _builder_1.append("import io.sarl.lang.core.Capacity;");
      _builder_1.newLine();
      _builder_1.newLine();
      _builder_1.append("@SuppressWarnings(\"all\")");
      _builder_1.newLine();
      _builder_1.append("public interface ComputeEnergyCapacity extends Capacity {");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy);");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.newLine();
      _builder_1.append("  ");
      _builder_1.append("public void setVoltage(final Double currentVoltage);");
      _builder_1.newLine();
      _builder_1.append("}");
      _builder_1.newLine();
      final String expected1 = _builder_1.toString();
      StringConcatenation _builder_2 = new StringConcatenation();
      _builder_2.append("@SuppressWarnings(\"all\")");
      _builder_2.newLine();
      _builder_2.append("public class DeviceAgent extends EntityAgent {");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("/**");
      _builder_2.newLine();
      _builder_2.append("   ");
      _builder_2.append("* Creates a new Agent of type DeviceAgent");
      _builder_2.newLine();
      _builder_2.append("   ");
      _builder_2.append("*/");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("public DeviceAgent(final java.util.UUID parentID) {");
      _builder_2.newLine();
      _builder_2.append("    ");
      _builder_2.append("super(parentID);");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("}");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("public void setVoltage(final Double currentVoltage) {");
      _builder_2.newLine();
      _builder_2.append("    ");
      _builder_2.append("getSkill(ComputeEnergyCapacity.class).setVoltage(currentVoltage);");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("}");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("public Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy) {");
      _builder_2.newLine();
      _builder_2.append("    ");
      _builder_2.append("return getSkill(ComputeEnergyCapacity.class).getEnergy(currentTime, deltaTime, wantedEnergy);");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("}");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("protected double busTime = 0d;");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.newLine();
      _builder_2.append("  ");
      _builder_2.append("protected Double wantedIntensity;");
      _builder_2.newLine();
      _builder_2.append("}");
      _builder_2.newLine();
      final String expected2 = _builder_2.toString();
      StringConcatenation _builder_3 = new StringConcatenation();
      _builder_3.append("import io.sarl.lang.core.Agent;");
      _builder_3.newLine();
      _builder_3.newLine();
      _builder_3.append("@SuppressWarnings(\"all\")");
      _builder_3.newLine();
      _builder_3.append("public class EntityAgent extends Agent {");
      _builder_3.newLine();
      _builder_3.append("  ");
      _builder_3.append("/**");
      _builder_3.newLine();
      _builder_3.append("   ");
      _builder_3.append("* Creates a new Agent of type EntityAgent");
      _builder_3.newLine();
      _builder_3.append("   ");
      _builder_3.append("*/");
      _builder_3.newLine();
      _builder_3.append("  ");
      _builder_3.append("public EntityAgent(final java.util.UUID parentID) {");
      _builder_3.newLine();
      _builder_3.append("    ");
      _builder_3.append("super(parentID);");
      _builder_3.newLine();
      _builder_3.append("  ");
      _builder_3.append("}");
      _builder_3.newLine();
      _builder_3.append("}");
      _builder_3.newLine();
      final String expected3 = _builder_3.toString();
      final IAcceptor<CompilationTestHelper.Result> _function = new IAcceptor<CompilationTestHelper.Result>() {
        public void accept(final CompilationTestHelper.Result r) {
          String _generatedCode = r.getGeneratedCode("ComputeEnergyCapacity");
          Assert.assertEquals(expected1, _generatedCode);
          String _generatedCode_1 = r.getGeneratedCode("DeviceAgent");
          Assert.assertEquals(expected2, _generatedCode_1);
          String _generatedCode_2 = r.getGeneratedCode("EntityAgent");
          Assert.assertEquals(expected3, _generatedCode_2);
        }
      };
      this._compilationTestHelper.compile(source, _function);
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
}

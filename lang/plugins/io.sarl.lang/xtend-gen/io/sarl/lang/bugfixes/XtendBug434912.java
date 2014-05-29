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
package io.sarl.lang.bugfixes;

import com.google.common.base.Objects;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

/**
 * Patches for the bug 434912 in Xtend.
 * <p>
 * Pull-request: {@link "https://github.com/eclipse/xtext/pull/2"}
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class XtendBug434912 {
  private final JvmTypesBuilder typesBuilder;
  
  public XtendBug434912(final JvmTypesBuilder typesBuilder) {
    this.typesBuilder = typesBuilder;
  }
  
  /**
   * FIXME: Remove this function if it is fixed in Xtext: https://bugs.eclipse.org/bugs/show_bug.cgi?id=434912
   * 
   * Copied/pasted from {@link JvmTypesBuilder#toEquals}.
   * Updated for fixing the issue {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=434912"}
   * 
   * Pull-request: {@link "https://github.com/eclipse/xtext/pull/2"}
   * 
   * @param owner
   * @param sourceElement
   * @param declaredType
   * @param isDelegateToSuperEquals
   * @param jvmFields
   * @return the operation.
   */
  public JvmOperation toEqualsMethod(final JvmGenericType owner, final EObject sourceElement, final JvmDeclaredType declaredType, final boolean isDelegateToSuperEquals, final JvmField... jvmFields) {
    JvmTypeReference _newTypeRef = this.typesBuilder.newTypeRef(sourceElement, Boolean.TYPE);
    JvmOperation result = this.typesBuilder.toMethod(sourceElement, "equals", _newTypeRef, 
      null);
    EList<JvmAnnotationReference> _annotations = result.getAnnotations();
    JvmAnnotationReference _annotation = this.typesBuilder.toAnnotation(sourceElement, Override.class);
    _annotations.add(_annotation);
    EList<JvmFormalParameter> _parameters = result.getParameters();
    JvmTypeReference _newTypeRef_1 = this.typesBuilder.newTypeRef(sourceElement, Object.class);
    JvmFormalParameter _parameter = this.typesBuilder.toParameter(sourceElement, "obj", _newTypeRef_1);
    _parameters.add(_parameter);
    final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
      public void apply(final ITreeAppendable it) {
        ITreeAppendable _append = it.append("if (this == obj)");
        _append.increaseIndentation();
        ITreeAppendable _newLine = it.newLine();
        ITreeAppendable _append_1 = _newLine.append("return true;");
        _append_1.decreaseIndentation();
        ITreeAppendable _newLine_1 = it.newLine();
        ITreeAppendable _append_2 = _newLine_1.append("if (obj == null)");
        _append_2.increaseIndentation();
        ITreeAppendable _newLine_2 = it.newLine();
        ITreeAppendable _append_3 = _newLine_2.append("return false;");
        _append_3.decreaseIndentation();
        ITreeAppendable _newLine_3 = it.newLine();
        ITreeAppendable _append_4 = _newLine_3.append("if (getClass() != obj.getClass())");
        _append_4.increaseIndentation();
        ITreeAppendable _newLine_4 = it.newLine();
        ITreeAppendable _append_5 = _newLine_4.append("return false;");
        _append_5.decreaseIndentation();
        if (isDelegateToSuperEquals) {
          ITreeAppendable _newLine_5 = it.newLine();
          ITreeAppendable _append_6 = _newLine_5.append("if (!super.equals(obj))");
          _append_6.increaseIndentation();
          ITreeAppendable _newLine_6 = it.newLine();
          ITreeAppendable _append_7 = _newLine_6.append("return false;");
          _append_7.decreaseIndentation();
        }
        ITreeAppendable _newLine_7 = it.newLine();
        String _simpleName = declaredType.getSimpleName();
        String _plus = (_simpleName + " other = (");
        String _simpleName_1 = declaredType.getSimpleName();
        String _plus_1 = (_plus + _simpleName_1);
        String _plus_2 = (_plus_1 + ") obj;");
        _newLine_7.append(_plus_2);
        for (final JvmField field : jvmFields) {
          {
            JvmTypeReference _type = field.getType();
            String typeName = _type.getIdentifier();
            boolean _or = false;
            boolean _or_1 = false;
            boolean _or_2 = false;
            boolean _or_3 = false;
            boolean _or_4 = false;
            String _name = Boolean.TYPE.getName();
            boolean _equals = Objects.equal(_name, typeName);
            if (_equals) {
              _or_4 = true;
            } else {
              String _name_1 = Integer.TYPE.getName();
              boolean _equals_1 = Objects.equal(_name_1, typeName);
              _or_4 = _equals_1;
            }
            if (_or_4) {
              _or_3 = true;
            } else {
              String _name_2 = Long.TYPE.getName();
              boolean _equals_2 = Objects.equal(_name_2, typeName);
              _or_3 = _equals_2;
            }
            if (_or_3) {
              _or_2 = true;
            } else {
              String _name_3 = Character.TYPE.getName();
              boolean _equals_3 = Objects.equal(_name_3, typeName);
              _or_2 = _equals_3;
            }
            if (_or_2) {
              _or_1 = true;
            } else {
              String _name_4 = Byte.TYPE.getName();
              boolean _equals_4 = Objects.equal(_name_4, typeName);
              _or_1 = _equals_4;
            }
            if (_or_1) {
              _or = true;
            } else {
              String _name_5 = Short.TYPE.getName();
              boolean _equals_5 = Objects.equal(_name_5, typeName);
              _or = _equals_5;
            }
            if (_or) {
              ITreeAppendable _newLine_8 = it.newLine();
              String _simpleName_2 = field.getSimpleName();
              String _plus_3 = ("if (other." + _simpleName_2);
              String _plus_4 = (_plus_3 + " != this.");
              String _simpleName_3 = field.getSimpleName();
              String _plus_5 = (_plus_4 + _simpleName_3);
              String _plus_6 = (_plus_5 + ")");
              ITreeAppendable _append_8 = _newLine_8.append(_plus_6);
              _append_8.increaseIndentation();
              ITreeAppendable _newLine_9 = it.newLine();
              ITreeAppendable _append_9 = _newLine_9.append("return false;");
              _append_9.decreaseIndentation();
            } else {
              String _name_6 = Double.TYPE.getName();
              boolean _equals_6 = Objects.equal(_name_6, typeName);
              if (_equals_6) {
                ITreeAppendable _newLine_10 = it.newLine();
                String _simpleName_4 = field.getSimpleName();
                String _plus_7 = ("if (Double.doubleToLongBits(other." + _simpleName_4);
                String _plus_8 = (_plus_7 + ") != Double.doubleToLongBits(this.");
                String _simpleName_5 = field.getSimpleName();
                String _plus_9 = (_plus_8 + _simpleName_5);
                String _plus_10 = (_plus_9 + "))");
                ITreeAppendable _append_10 = _newLine_10.append(_plus_10);
                _append_10.increaseIndentation();
                ITreeAppendable _newLine_11 = it.newLine();
                ITreeAppendable _append_11 = _newLine_11.append("return false;");
                _append_11.decreaseIndentation();
              } else {
                String _name_7 = Float.TYPE.getName();
                boolean _equals_7 = Objects.equal(_name_7, typeName);
                if (_equals_7) {
                  ITreeAppendable _newLine_12 = it.newLine();
                  String _simpleName_6 = field.getSimpleName();
                  String _plus_11 = ("if (Float.floatToIntBits(other." + _simpleName_6);
                  String _plus_12 = (_plus_11 + ") != Float.floatToIntBits(this.");
                  String _simpleName_7 = field.getSimpleName();
                  String _plus_13 = (_plus_12 + _simpleName_7);
                  String _plus_14 = (_plus_13 + "))");
                  ITreeAppendable _append_12 = _newLine_12.append(_plus_14);
                  _append_12.increaseIndentation();
                  ITreeAppendable _newLine_13 = it.newLine();
                  ITreeAppendable _append_13 = _newLine_13.append("return false;");
                  _append_13.decreaseIndentation();
                } else {
                  ITreeAppendable _newLine_14 = it.newLine();
                  String _simpleName_8 = field.getSimpleName();
                  String _plus_15 = ("if (this." + _simpleName_8);
                  String _plus_16 = (_plus_15 + " == null) {");
                  ITreeAppendable _append_14 = _newLine_14.append(_plus_16);
                  _append_14.increaseIndentation();
                  ITreeAppendable _newLine_15 = it.newLine();
                  String _simpleName_9 = field.getSimpleName();
                  String _plus_17 = ("if (other." + _simpleName_9);
                  String _plus_18 = (_plus_17 + " != null)");
                  ITreeAppendable _append_15 = _newLine_15.append(_plus_18);
                  _append_15.increaseIndentation();
                  ITreeAppendable _newLine_16 = it.newLine();
                  ITreeAppendable _append_16 = _newLine_16.append("return false;");
                  _append_16.decreaseIndentation();
                  it.decreaseIndentation();
                  ITreeAppendable _newLine_17 = it.newLine();
                  String _simpleName_10 = field.getSimpleName();
                  String _plus_19 = ("} else if (!this." + _simpleName_10);
                  String _plus_20 = (_plus_19 + ".equals(other.");
                  String _simpleName_11 = field.getSimpleName();
                  String _plus_21 = (_plus_20 + _simpleName_11);
                  String _plus_22 = (_plus_21 + "))");
                  ITreeAppendable _append_17 = _newLine_17.append(_plus_22);
                  _append_17.increaseIndentation();
                  ITreeAppendable _newLine_18 = it.newLine();
                  ITreeAppendable _append_18 = _newLine_18.append("return false;");
                  _append_18.decreaseIndentation();
                }
              }
            }
          }
        }
        ITreeAppendable _newLine_8 = it.newLine();
        _newLine_8.append("return true;");
      }
    };
    this.typesBuilder.setBody(result, _function);
    return result;
  }
}

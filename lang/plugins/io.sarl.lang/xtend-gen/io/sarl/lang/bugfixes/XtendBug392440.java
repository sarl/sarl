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
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

/**
 * Patches for the bug 434912 in Xtend.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class XtendBug392440 {
  private final JvmTypesBuilder typesBuilder;
  
  public XtendBug392440(final JvmTypesBuilder typesBuilder) {
    this.typesBuilder = typesBuilder;
  }
  
  /**
   * FIXME: Remove this function if it is fixed in Xtext: https://bugs.eclipse.org/bugs/show_bug.cgi?id=392440
   * 
   * Copied/pasted from {@link JvmTypesBuilder#toHashCodeMethod(EObject, boolean, JvmField...)}.
   * Updated for fixing the issue {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=392440"}
   * 
   * @param owner
   * @param sourceElement
   * @param extendsSomethingWithProperHashCode
   * @param jvmFields
   * @return the operation.
   */
  public JvmOperation toHashCodeMethod(final JvmGenericType owner, final EObject sourceElement, final boolean extendsSomethingWithProperHashCode, final JvmField... jvmFields) {
    boolean _tripleEquals = (sourceElement == null);
    if (_tripleEquals) {
      return null;
    }
    JvmTypeReference _newTypeRef = this.typesBuilder.newTypeRef(sourceElement, Integer.TYPE);
    JvmOperation result = this.typesBuilder.toMethod(sourceElement, "hashCode", _newTypeRef, null);
    boolean _tripleEquals_1 = (result == null);
    if (_tripleEquals_1) {
      return null;
    }
    EList<JvmAnnotationReference> _annotations = result.getAnnotations();
    JvmAnnotationReference _annotation = this.typesBuilder.toAnnotation(sourceElement, Override.class);
    _annotations.add(_annotation);
    final Procedure1<ITreeAppendable> _function = new Procedure1<ITreeAppendable>() {
      public void apply(final ITreeAppendable it) {
        it.append("final int prime = 31;");
        if (extendsSomethingWithProperHashCode) {
          ITreeAppendable _newLine = it.newLine();
          _newLine.append("int result = super.hashCode();");
        } else {
          ITreeAppendable _newLine_1 = it.newLine();
          _newLine_1.append("int result = 1;");
        }
        for (final JvmField field : jvmFields) {
          {
            JvmTypeReference _type = field.getType();
            String typeName = _type.getIdentifier();
            String _name = Boolean.TYPE.getName();
            boolean _equals = Objects.equal(_name, typeName);
            if (_equals) {
              ITreeAppendable _newLine_2 = it.newLine();
              String _simpleName = field.getSimpleName();
              String _plus = ("result = prime * result + (this." + _simpleName);
              String _plus_1 = (_plus + " ? 1231 : 1237);");
              _newLine_2.append(_plus_1);
            } else {
              boolean _or = false;
              boolean _or_1 = false;
              boolean _or_2 = false;
              String _name_1 = Integer.TYPE.getName();
              boolean _equals_1 = Objects.equal(_name_1, typeName);
              if (_equals_1) {
                _or_2 = true;
              } else {
                String _name_2 = Character.TYPE.getName();
                boolean _equals_2 = Objects.equal(_name_2, typeName);
                _or_2 = _equals_2;
              }
              if (_or_2) {
                _or_1 = true;
              } else {
                String _name_3 = Byte.TYPE.getName();
                boolean _equals_3 = Objects.equal(_name_3, typeName);
                _or_1 = _equals_3;
              }
              if (_or_1) {
                _or = true;
              } else {
                String _name_4 = Short.TYPE.getName();
                boolean _equals_4 = Objects.equal(_name_4, typeName);
                _or = _equals_4;
              }
              if (_or) {
                ITreeAppendable _newLine_3 = it.newLine();
                String _simpleName_1 = field.getSimpleName();
                String _plus_2 = ("result = prime * result + this." + _simpleName_1);
                String _plus_3 = (_plus_2 + ";");
                _newLine_3.append(_plus_3);
              } else {
                String _name_5 = Long.TYPE.getName();
                boolean _equals_5 = Objects.equal(_name_5, typeName);
                if (_equals_5) {
                  ITreeAppendable _newLine_4 = it.newLine();
                  String _simpleName_2 = field.getSimpleName();
                  String _plus_4 = ("result = prime * result + (int) (this." + _simpleName_2);
                  String _plus_5 = (_plus_4 + " ^ (this.");
                  String _simpleName_3 = field.getSimpleName();
                  String _plus_6 = (_plus_5 + _simpleName_3);
                  String _plus_7 = (_plus_6 + " >>> 32));");
                  _newLine_4.append(_plus_7);
                } else {
                  String _name_6 = Float.TYPE.getName();
                  boolean _equals_6 = Objects.equal(_name_6, typeName);
                  if (_equals_6) {
                    ITreeAppendable _newLine_5 = it.newLine();
                    String _simpleName_4 = field.getSimpleName();
                    String _plus_8 = ("result = prime * result + Float.floatToIntBits(this." + _simpleName_4);
                    String _plus_9 = (_plus_8 + ");");
                    _newLine_5.append(_plus_9);
                  } else {
                    String _name_7 = Double.TYPE.getName();
                    boolean _equals_7 = Objects.equal(_name_7, typeName);
                    if (_equals_7) {
                      ITreeAppendable _newLine_6 = it.newLine();
                      String _simpleName_5 = field.getSimpleName();
                      String _plus_10 = ("result = prime * result + (int) (Double.doubleToLongBits(this." + _simpleName_5);
                      String _plus_11 = (_plus_10 + ") ^ (Double.doubleToLongBits(this.");
                      String _simpleName_6 = field.getSimpleName();
                      String _plus_12 = (_plus_11 + _simpleName_6);
                      String _plus_13 = (_plus_12 + ") >>> 32));");
                      _newLine_6.append(_plus_13);
                    } else {
                      ITreeAppendable _newLine_7 = it.newLine();
                      String _simpleName_7 = field.getSimpleName();
                      String _plus_14 = ("result = prime * result + ((this." + _simpleName_7);
                      String _plus_15 = (_plus_14 + "== null) ? 0 : this.");
                      String _simpleName_8 = field.getSimpleName();
                      String _plus_16 = (_plus_15 + _simpleName_8);
                      String _plus_17 = (_plus_16 + ".hashCode());");
                      _newLine_7.append(_plus_17);
                    }
                  }
                }
              }
            }
          }
        }
        ITreeAppendable _newLine_2 = it.newLine();
        _newLine_2.append("return result;");
      }
    };
    this.typesBuilder.setBody(result, _function);
    return result;
  }
}

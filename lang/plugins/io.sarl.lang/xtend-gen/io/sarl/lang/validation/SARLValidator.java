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
package io.sarl.lang.validation;

import com.google.common.base.Objects;
import com.google.inject.Inject;
import io.sarl.lang.SARLKeywords;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.FormalParameter;
import io.sarl.lang.sarl.ParameterizedFeature;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.signature.ActionKey;
import io.sarl.lang.signature.ActionNameKey;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.InferredActionSignature;
import io.sarl.lang.signature.SignatureKey;
import io.sarl.lang.validation.AbstractSARLValidator;
import io.sarl.lang.validation.IssueCodes;
import java.util.Set;
import java.util.TreeSet;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XTypeLiteral;
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.typesystem.computation.NumberLiterals;
import org.eclipse.xtext.xbase.typesystem.legacy.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.OwnedConverter;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

/**
 * Custom validation rules.
 * 
 * see http://www.eclipse.org/Xtext/documentation.html#validation
 */
@SuppressWarnings("all")
public class SARLValidator extends AbstractSARLValidator {
  @Inject
  private ILogicalContainerProvider logicalContainerProvider;
  
  @Inject
  private TypeReferences typeReferences;
  
  @Inject
  private NumberLiterals numberLiterals;
  
  @Inject
  private ActionSignatureProvider sarlSignatureProvider;
  
  @Check
  public void checkNoDefaultValueForVariadicParameter(final ParameterizedFeature feature) {
    boolean _isVarargs = feature.isVarargs();
    if (_isVarargs) {
      EList<FormalParameter> _params = feature.getParams();
      FormalParameter lastParam = IterableExtensions.<FormalParameter>last(_params);
      XExpression _defaultValue = lastParam.getDefaultValue();
      boolean _notEquals = (!Objects.equal(_defaultValue, null));
      if (_notEquals) {
        String _name = lastParam.getName();
        String _format = String.format(
          "A default value cannot be declared for the variadic formal parameter \'%s\'.", _name);
        this.error(_format, lastParam, 
          null, 
          IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER);
      }
    }
  }
  
  private boolean isInterface(final LightweightTypeReference typeRef) {
    JvmType _type = typeRef.getType();
    return this.isInterface(_type);
  }
  
  private boolean isClass(final LightweightTypeReference typeRef) {
    JvmType t = typeRef.getType();
    if ((t instanceof JvmGenericType)) {
      boolean _isInterface = ((JvmGenericType)t).isInterface();
      return (!_isInterface);
    }
    return false;
  }
  
  private LightweightTypeReference actualType(final XExpression expr, final EObject context, final LightweightTypeReference targetType) {
    LightweightTypeReference type = this.getActualType(expr);
    boolean _tripleNotEquals = (type != null);
    if (_tripleNotEquals) {
      return type;
    }
    if ((expr instanceof XNullLiteral)) {
    }
    JvmTypeReference jvmType = null;
    if ((expr instanceof XBooleanLiteral)) {
      JvmTypeReference _typeForName = this.typeReferences.getTypeForName(Boolean.TYPE, context);
      jvmType = _typeForName;
    } else {
      if ((expr instanceof XStringLiteral)) {
        boolean _and = false;
        boolean _or = false;
        boolean _isType = targetType.isType(Character.TYPE);
        if (_isType) {
          _or = true;
        } else {
          boolean _isType_1 = targetType.isType(Character.class);
          _or = _isType_1;
        }
        if (!_or) {
          _and = false;
        } else {
          boolean _and_1 = false;
          String _value = ((XStringLiteral)expr).getValue();
          boolean _notEquals = (!Objects.equal(_value, null));
          if (!_notEquals) {
            _and_1 = false;
          } else {
            String _value_1 = ((XStringLiteral)expr).getValue();
            int _length = _value_1.length();
            boolean _lessEqualsThan = (_length <= 1);
            _and_1 = _lessEqualsThan;
          }
          _and = _and_1;
        }
        if (_and) {
          JvmTypeReference _typeForName_1 = this.typeReferences.getTypeForName(Character.TYPE, context);
          jvmType = _typeForName_1;
        } else {
          JvmTypeReference _typeForName_2 = this.typeReferences.getTypeForName(String.class, context);
          jvmType = _typeForName_2;
        }
      } else {
        if ((expr instanceof XNumberLiteral)) {
          Class<? extends Number> jType = this.numberLiterals.getJavaType(((XNumberLiteral)expr));
          JvmTypeReference _typeForName_3 = this.typeReferences.getTypeForName(jType, expr);
          jvmType = _typeForName_3;
        } else {
          if ((expr instanceof XTypeLiteral)) {
            JvmType _type = ((XTypeLiteral)expr).getType();
            JvmParameterizedTypeReference _createTypeRef = this.typeReferences.createTypeRef(_type);
            jvmType = _createTypeRef;
          }
        }
      }
    }
    boolean _tripleNotEquals_1 = (jvmType != null);
    if (_tripleNotEquals_1) {
      CommonTypeComputationServices _services = this.getServices();
      StandardTypeReferenceOwner _standardTypeReferenceOwner = new StandardTypeReferenceOwner(_services, context);
      final OwnedConverter converter = new OwnedConverter(_standardTypeReferenceOwner, 
        true);
      return converter.toLightweightReference(jvmType);
    }
    return null;
  }
  
  private void checkDefaultValueTypeCompatibleWithParameterType(final FormalParameter param) {
    JvmTypeReference _parameterType = param.getParameterType();
    LightweightTypeReference toType = this.toLightweightTypeReference(_parameterType, true);
    XExpression _defaultValue = param.getDefaultValue();
    LightweightTypeReference fromType = this.actualType(_defaultValue, param, toType);
    boolean _tripleEquals = (fromType == null);
    if (_tripleEquals) {
      String _name = param.getName();
      String _format = String.format(
        "Cannot determine the type of the default value for the parameter \'%s\'.", _name);
      this.error(_format, param, 
        null, 
        org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_TYPE_PARAMETER_BOUNDS);
    }
    boolean _and = false;
    boolean _and_1 = false;
    boolean _and_2 = false;
    boolean _and_3 = false;
    boolean _and_4 = false;
    boolean _or = false;
    JvmType _type = fromType.getType();
    if ((_type instanceof JvmDeclaredType)) {
      _or = true;
    } else {
      boolean _isPrimitive = fromType.isPrimitive();
      _or = _isPrimitive;
    }
    if (!_or) {
      _and_4 = false;
    } else {
      boolean _or_1 = false;
      boolean _isInterface = this.isInterface(fromType);
      boolean _not = (!_isInterface);
      if (_not) {
        _or_1 = true;
      } else {
        boolean _isFinal = this.isFinal(toType);
        _or_1 = _isFinal;
      }
      _and_4 = _or_1;
    }
    if (!_and_4) {
      _and_3 = false;
    } else {
      boolean _or_2 = false;
      boolean _isInterface_1 = this.isInterface(toType);
      boolean _not_1 = (!_isInterface_1);
      if (_not_1) {
        _or_2 = true;
      } else {
        boolean _isFinal_1 = this.isFinal(fromType);
        _or_2 = _isFinal_1;
      }
      _and_3 = _or_2;
    }
    if (!_and_3) {
      _and_2 = false;
    } else {
      boolean _isAssignableFrom = toType.isAssignableFrom(fromType);
      boolean _not_2 = (!_isAssignableFrom);
      _and_2 = _not_2;
    }
    if (!_and_2) {
      _and_1 = false;
    } else {
      boolean _or_3 = false;
      boolean _or_4 = false;
      boolean _isFinal_2 = this.isFinal(fromType);
      if (_isFinal_2) {
        _or_4 = true;
      } else {
        boolean _isFinal_3 = this.isFinal(toType);
        _or_4 = _isFinal_3;
      }
      if (_or_4) {
        _or_3 = true;
      } else {
        boolean _and_5 = false;
        boolean _isClass = this.isClass(fromType);
        if (!_isClass) {
          _and_5 = false;
        } else {
          boolean _isClass_1 = this.isClass(toType);
          _and_5 = _isClass_1;
        }
        _or_3 = _and_5;
      }
      _and_1 = _or_3;
    }
    if (!_and_1) {
      _and = false;
    } else {
      boolean _isAssignableFrom_1 = fromType.isAssignableFrom(toType);
      boolean _not_3 = (!_isAssignableFrom_1);
      _and = _not_3;
    }
    if (_and) {
      String _nameOfTypes = this.getNameOfTypes(fromType);
      String _canonicalName = this.canonicalName(toType);
      String _format_1 = String.format("Cannot cast from %s to %s", _nameOfTypes, _canonicalName);
      this.error(_format_1, param, 
        null, 
        ValidationMessageAcceptor.INSIGNIFICANT_INDEX, 
        org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST);
    } else {
      boolean _and_6 = false;
      boolean _isPrimitive_1 = toType.isPrimitive();
      if (!_isPrimitive_1) {
        _and_6 = false;
      } else {
        boolean _or_5 = false;
        boolean _isPrimitive_2 = fromType.isPrimitive();
        if (_isPrimitive_2) {
          _or_5 = true;
        } else {
          boolean _isWrapper = fromType.isWrapper();
          _or_5 = _isWrapper;
        }
        boolean _not_4 = (!_or_5);
        _and_6 = _not_4;
      }
      if (_and_6) {
        String _nameOfTypes_1 = this.getNameOfTypes(fromType);
        String _canonicalName_1 = this.canonicalName(toType);
        String _format_2 = String.format("Cannot cast from %s to %s", _nameOfTypes_1, _canonicalName_1);
        this.error(_format_2, param, 
          null, 
          ValidationMessageAcceptor.INSIGNIFICANT_INDEX, 
          org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST);
      }
    }
  }
  
  @Check
  public void checkDefaultValueTypeCompatibleWithParameterType(final ParameterizedFeature feature) {
    EList<FormalParameter> _params = feature.getParams();
    for (final FormalParameter param : _params) {
      XExpression _defaultValue = param.getDefaultValue();
      boolean _notEquals = (!Objects.equal(_defaultValue, null));
      if (_notEquals) {
        this.checkDefaultValueTypeCompatibleWithParameterType(param);
      }
    }
  }
  
  @Check
  public void checkNoActionCollision(final FeatureContainer featureContainer) {
    Set<ActionKey> localFunctions = new TreeSet<ActionKey>();
    ActionNameKey actionID = null;
    SignatureKey signatureID = null;
    String name = null;
    JvmIdentifiableElement container = null;
    EList<EObject> _features = featureContainer.getFeatures();
    for (final EObject feature : _features) {
      {
        boolean _tripleEquals = (container == null);
        if (_tripleEquals) {
          JvmIdentifiableElement _nearestLogicalContainer = this.logicalContainerProvider.getNearestLogicalContainer(feature);
          container = _nearestLogicalContainer;
        }
        if ((feature instanceof Action)) {
          ParameterizedFeature _signature = ((Action)feature).getSignature();
          ActionSignature s = ((ActionSignature) _signature);
          String _name = s.getName();
          name = _name;
          ActionNameKey _createFunctionID = this.sarlSignatureProvider.createFunctionID(container, name);
          actionID = _createFunctionID;
          EList<FormalParameter> _params = s.getParams();
          SignatureKey _createSignatureID = this.sarlSignatureProvider.createSignatureID(_params);
          signatureID = _createSignatureID;
        } else {
          if ((feature instanceof ActionSignature)) {
            String _name_1 = ((ActionSignature)feature).getName();
            name = _name_1;
            ActionNameKey _createFunctionID_1 = this.sarlSignatureProvider.createFunctionID(container, name);
            actionID = _createFunctionID_1;
            EList<FormalParameter> _params_1 = ((ActionSignature)feature).getParams();
            SignatureKey _createSignatureID_1 = this.sarlSignatureProvider.createSignatureID(_params_1);
            signatureID = _createSignatureID_1;
          } else {
            if ((feature instanceof Constructor)) {
              name = SARLKeywords.CONSTRUCTOR;
              ActionNameKey _createConstructorID = this.sarlSignatureProvider.createConstructorID(container);
              actionID = _createConstructorID;
              EList<FormalParameter> _params_2 = ((Constructor)feature).getParams();
              SignatureKey _createSignatureID_2 = this.sarlSignatureProvider.createSignatureID(_params_2);
              signatureID = _createSignatureID_2;
            } else {
              actionID = null;
              signatureID = null;
            }
          }
        }
        boolean _and = false;
        boolean _tripleNotEquals = (actionID != null);
        if (!_tripleNotEquals) {
          _and = false;
        } else {
          boolean _tripleNotEquals_1 = (signatureID != null);
          _and = _tripleNotEquals_1;
        }
        if (_and) {
          InferredActionSignature sig = this.sarlSignatureProvider.getSignatures(actionID, signatureID);
          boolean _tripleNotEquals_2 = (sig != null);
          if (_tripleNotEquals_2) {
            Iterable<SignatureKey> _signatureKeys = sig.signatureKeys();
            for (final SignatureKey key : _signatureKeys) {
              ActionKey _actionKey = key.toActionKey(name);
              boolean _add = localFunctions.add(_actionKey);
              boolean _not = (!_add);
              if (_not) {
                String _name_2 = featureContainer.getName();
                String _string = sig.toString();
                String _plus = ((name + "(") + _string);
                String _plus_1 = (_plus + ")");
                String _format = String.format(
                  "Cannot define many times the same feature in \'%s\': %s", _name_2, _plus_1);
                this.error(_format, feature, 
                  null, 
                  IssueCodes.ACTION_COLLISION);
                return;
              }
            }
          }
        }
      }
    }
  }
  
  /**
   * private def Map<ActionNameKey,EList<InferredActionSignature>> getCapacityActionsFromHierarchy(EList<InheritingElement> sources) {
   * var Map<ActionNameKey,EList<InferredActionSignature>> actions = new TreeMap
   * var Set<String> encounteredCapacities = new TreeSet
   * var List<Capacity> capacities = new LinkedList
   * for(p : sources) {
   * if (p instanceof Capacity) {
   * capacities.add(p)
   * }
   * }
   * while (!capacities.empty) {
   * var cap = capacities.remove(0);
   * if (encounteredCapacities.add("")) {
   * for(p : cap.superTypes) {
   * if (p instanceof Capacity) {
   * capacities.add(p)
   * }
   * }
   * var JvmIdentifiableElement container = null
   * for(feature : cap.features) {
   * if (feature instanceof ActionSignature) {
   * if (container===null) {
   * container = logicalContainerProvider.getNearestLogicalContainer(feature)
   * }
   * var ank = sarlSignatureProvider.createFunctionID(container, feature.name)
   * var sk = sarlSignatureProvider.createSignatureID(feature.params)
   * var is = sarlSignatureProvider.getSignatures(ank, sk)
   * actions.add(sk.toActionKey(feature.name))
   * }
   * }
   * }
   * }
   * return actions
   * }
   */
  @Check
  public Object checkSkillActionImplementationPrototype(final Skill skill) {
    return null;
  }
}

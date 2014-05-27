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
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.FormalParameter;
import io.sarl.lang.sarl.InheritingElement;
import io.sarl.lang.sarl.ParameterizedFeature;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.signature.ActionKey;
import io.sarl.lang.signature.ActionNameKey;
import io.sarl.lang.signature.ActionSignatureProvider;
import io.sarl.lang.signature.InferredActionSignature;
import io.sarl.lang.signature.SignatureKey;
import io.sarl.lang.util.ModelUtil;
import io.sarl.lang.validation.AbstractSARLValidator;
import io.sarl.lang.validation.IssueCodes;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociations;
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

/**
 * Validator for the SARL elements.
 * <p>
 * The following issues are not yet supported:<ul>
 * <li>Skill implementation cannot have default value - ERROR</li>
 * </ul>
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#validation"
 */
@SuppressWarnings("all")
public class SARLValidator extends AbstractSARLValidator {
  @Inject
  private ILogicalContainerProvider logicalContainerProvider;
  
  @Inject
  private ActionSignatureProvider sarlSignatureProvider;
  
  /**
   * @param feature
   */
  @Check
  public void checkNoDefaultValueForVariadicParameter(final ParameterizedFeature feature) {
    boolean _isVarargs = feature.isVarargs();
    if (_isVarargs) {
      EList<FormalParameter> _params = feature.getParams();
      FormalParameter lastParam = IterableExtensions.<FormalParameter>last(_params);
      XExpression _defaultValue = lastParam.getDefaultValue();
      boolean _tripleNotEquals = (_defaultValue != null);
      if (_tripleNotEquals) {
        String _name = lastParam.getName();
        String _format = String.format(
          "A default value cannot be declared for the variadic formal parameter \'%s\'.", _name);
        this.error(_format, lastParam, 
          null, 
          IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER);
      }
    }
  }
  
  private void checkDefaultValueTypeCompatibleWithParameterType(final FormalParameter param) {
    JvmTypeReference _parameterType = param.getParameterType();
    LightweightTypeReference toType = this.toLightweightTypeReference(_parameterType, true);
    XExpression _defaultValue = param.getDefaultValue();
    LightweightTypeReference fromType = this.getActualType(_defaultValue);
    boolean _canCast = ModelUtil.canCast(fromType, toType, true);
    boolean _not = (!_canCast);
    if (_not) {
      String _nameOfTypes = this.getNameOfTypes(fromType);
      String _canonicalName = this.canonicalName(toType);
      String _format = String.format("Cannot cast from %s to %s", _nameOfTypes, _canonicalName);
      this.error(_format, param, 
        null, 
        ValidationMessageAcceptor.INSIGNIFICANT_INDEX, 
        org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST);
    }
  }
  
  /**
   * @param feature
   */
  @Check
  public void checkDefaultValueTypeCompatibleWithParameterType(final ParameterizedFeature feature) {
    EList<FormalParameter> _params = feature.getParams();
    for (final FormalParameter param : _params) {
      XExpression _defaultValue = param.getDefaultValue();
      boolean _tripleNotEquals = (_defaultValue != null);
      if (_tripleNotEquals) {
        this.checkDefaultValueTypeCompatibleWithParameterType(param);
      }
    }
  }
  
  /**
   * @param featureContainer
   */
  @Check
  public void checkNoFeatureMultiDefinition(final FeatureContainer featureContainer) {
    final Set<String> localFields = new TreeSet<String>();
    final Set<ActionKey> localFunctions = new TreeSet<ActionKey>();
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
          boolean _isVarargs = s.isVarargs();
          EList<FormalParameter> _params = s.getParams();
          SignatureKey _createSignatureIDFromSarlModel = this.sarlSignatureProvider.createSignatureIDFromSarlModel(_isVarargs, _params);
          signatureID = _createSignatureIDFromSarlModel;
        } else {
          if ((feature instanceof ActionSignature)) {
            String _name_1 = ((ActionSignature)feature).getName();
            name = _name_1;
            ActionNameKey _createFunctionID_1 = this.sarlSignatureProvider.createFunctionID(container, name);
            actionID = _createFunctionID_1;
            boolean _isVarargs_1 = ((ActionSignature)feature).isVarargs();
            EList<FormalParameter> _params_1 = ((ActionSignature)feature).getParams();
            SignatureKey _createSignatureIDFromSarlModel_1 = this.sarlSignatureProvider.createSignatureIDFromSarlModel(_isVarargs_1, _params_1);
            signatureID = _createSignatureIDFromSarlModel_1;
          } else {
            if ((feature instanceof Constructor)) {
              name = SARLKeywords.CONSTRUCTOR;
              ActionNameKey _createConstructorID = this.sarlSignatureProvider.createConstructorID(container);
              actionID = _createConstructorID;
              boolean _isVarargs_2 = ((Constructor)feature).isVarargs();
              EList<FormalParameter> _params_2 = ((Constructor)feature).getParams();
              SignatureKey _createSignatureIDFromSarlModel_2 = this.sarlSignatureProvider.createSignatureIDFromSarlModel(_isVarargs_2, _params_2);
              signatureID = _createSignatureIDFromSarlModel_2;
            } else {
              name = null;
              actionID = null;
              signatureID = null;
              if ((feature instanceof Attribute)) {
                String _name_2 = ((Attribute)feature).getName();
                boolean _add = localFields.add(_name_2);
                boolean _not = (!_add);
                if (_not) {
                  String _name_3 = featureContainer.getName();
                  String _name_4 = ((Attribute)feature).getName();
                  String _format = String.format(
                    "Cannot define many times the same feature in \'%s\': %s", _name_3, _name_4);
                  this.error(_format, feature, 
                    null, 
                    IssueCodes.FIELD_ALREADY_DEFINED);
                }
              }
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
              boolean _add_1 = localFunctions.add(_actionKey);
              boolean _not_1 = (!_add_1);
              if (_not_1) {
                String _name_5 = featureContainer.getName();
                String _string = sig.toString();
                String _plus = ((name + "(") + _string);
                String _plus_1 = (_plus + ")");
                String _format_1 = String.format(
                  "Cannot define many times the same feature in \'%s\': %s", _name_5, _plus_1);
                this.error(_format_1, feature, 
                  null, 
                  IssueCodes.ACTION_ALREADY_DEFINED);
              }
            }
          }
        }
      }
    }
  }
  
  /**
   * @param action
   */
  @Check
  public void checkActionName(final ActionSignature action) {
    String _name = action.getName();
    boolean _isHiddenAction = ModelUtil.isHiddenAction(_name);
    if (_isHiddenAction) {
      String _name_1 = action.getName();
      String _format = String.format(
        "Invalid action name \'%s\'. You must not give to an action a name that is starting with \'_handle_\'. This prefix is reserved by the SARL compiler.", _name_1);
      this.error(_format, action, 
        null, 
        IssueCodes.INVALID_ACTION_NAME);
    }
  }
  
  /**
   * @param attribute
   */
  @Check
  public void checkAttributeName(final Attribute attribute) {
    String _name = attribute.getName();
    boolean _isHiddenAttribute = ModelUtil.isHiddenAttribute(_name);
    if (_isHiddenAttribute) {
      String _name_1 = attribute.getName();
      String _format = String.format(
        "Invalid attribute name \'%s\'. You must not give to an attribute a name that is starting with \'___FORMAL_PARAMETER_DEFAULT_VALUE_\'. This prefix is reserved by the SARL compiler.", _name_1);
      this.error(_format, attribute, 
        null, 
        IssueCodes.INVALID_ATTRIBUTE_NAME);
    }
  }
  
  /**
   * Replies the JVM generic type for the given element.
   * 
   * @param element
   * @return the generic type of the given element.
   */
  protected JvmGenericType getJvmGenericType(final EObject element) {
    CommonTypeComputationServices _services = this.getServices();
    IJvmModelAssociations _jvmModelAssociations = _services.getJvmModelAssociations();
    return ModelUtil.getJvmGenericType(element, _jvmModelAssociations);
  }
  
  /**
   * @param event
   */
  @Check
  public void checkFinalFieldInitialization(final Event event) {
    JvmGenericType type = this.getJvmGenericType(event);
    boolean _tripleNotEquals = (type != null);
    if (_tripleNotEquals) {
      this.checkFinalFieldInitialization(type);
    }
  }
  
  /**
   * @param agent
   */
  @Check
  public void checkFinalFieldInitialization(final Agent agent) {
    JvmGenericType type = this.getJvmGenericType(agent);
    boolean _tripleNotEquals = (type != null);
    if (_tripleNotEquals) {
      this.checkFinalFieldInitialization(type);
    }
  }
  
  /**
   * @param behavior
   */
  @Check
  public void checkFinalFieldInitialization(final Behavior behavior) {
    JvmGenericType type = this.getJvmGenericType(behavior);
    boolean _notEquals = (!Objects.equal(type, null));
    if (_notEquals) {
      this.checkFinalFieldInitialization(type);
    }
  }
  
  /**
   * @param skill
   */
  @Check
  public void checkFinalFieldInitialization(final Skill skill) {
    JvmGenericType type = this.getJvmGenericType(skill);
    boolean _notEquals = (!Objects.equal(type, null));
    if (_notEquals) {
      this.checkFinalFieldInitialization(type);
    }
  }
  
  protected void reportUninitializedField(final JvmField field) {
    String _simpleName = field.getSimpleName();
    String _format = String.format(
      "The blank final field \'%s\' may not have been initialized.", _simpleName);
    this.error(_format, field, 
      null, 
      org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_INITIALIZATION);
  }
  
  private void checkRedundantInterface(final JvmGenericType jvmElement, final JvmTypeReference interfaceReference, final LightweightTypeReference lightweightInterfaceReference, final Iterable<LightweightTypeReference> knownInterfaces) {
    JvmTypeReference _extendedClass = jvmElement.getExtendedClass();
    boolean _tripleNotEquals = (_extendedClass != null);
    if (_tripleNotEquals) {
      JvmTypeReference _extendedClass_1 = jvmElement.getExtendedClass();
      LightweightTypeReference superType = this.toLightweightTypeReference(_extendedClass_1);
      boolean _memberOfTypeHierarchy = this.memberOfTypeHierarchy(superType, lightweightInterfaceReference);
      if (_memberOfTypeHierarchy) {
        String _canonicalName = this.canonicalName(lightweightInterfaceReference);
        String _canonicalName_1 = this.canonicalName(superType);
        String _format = String.format(
          "The feature \'%s\' is already implemented by the super type \'%s\'.", _canonicalName, _canonicalName_1);
        this.warning(_format, interfaceReference, 
          null, 
          IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION);
        return;
      }
    }
    for (final LightweightTypeReference previousInterface : knownInterfaces) {
      boolean _memberOfTypeHierarchy_1 = this.memberOfTypeHierarchy(previousInterface, lightweightInterfaceReference);
      if (_memberOfTypeHierarchy_1) {
        String _canonicalName_2 = this.canonicalName(lightweightInterfaceReference);
        String _canonicalName_3 = this.canonicalName(previousInterface);
        String _format_1 = String.format(
          "The feature \'%s\' is already implemented by the preceding interface \'%s\'.", _canonicalName_2, _canonicalName_3);
        this.warning(_format_1, interfaceReference, 
          null, 
          IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION);
        return;
      }
    }
  }
  
  private void checkRedundantInterfaces(final JvmGenericType jvmElement) {
    boolean _isIgnored = this.isIgnored(IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION);
    boolean _not = (!_isIgnored);
    if (_not) {
      List<LightweightTypeReference> knownInterfaces = new ArrayList<LightweightTypeReference>();
      Iterable<JvmTypeReference> _extendedInterfaces = jvmElement.getExtendedInterfaces();
      for (final JvmTypeReference inter : _extendedInterfaces) {
        {
          LightweightTypeReference interfaceType = this.toLightweightTypeReference(inter);
          this.checkRedundantInterface(jvmElement, inter, interfaceType, knownInterfaces);
          knownInterfaces.add(interfaceType);
        }
      }
    }
  }
  
  /**
   * @param element
   */
  @Check
  public void checkInheritedFeatures(final InheritingElement element) {
    JvmGenericType jvmElement = this.getJvmGenericType(element);
    boolean _tripleNotEquals = (jvmElement != null);
    if (_tripleNotEquals) {
      Map<ActionKey, JvmOperation> finalOperations = new TreeMap<ActionKey, JvmOperation>();
      Map<ActionKey, JvmOperation> overridableOperations = new TreeMap<ActionKey, JvmOperation>();
      Map<String, JvmField> inheritedFields = new TreeMap<String, JvmField>();
      Map<ActionKey, JvmOperation> operationsToImplement = new TreeMap<ActionKey, JvmOperation>();
      Map<SignatureKey, JvmConstructor> superConstructors = new TreeMap<SignatureKey, JvmConstructor>();
      ModelUtil.populateInheritanceContext(jvmElement, finalOperations, overridableOperations, inheritedFields, operationsToImplement, superConstructors, this.sarlSignatureProvider);
      this.checkRedundantInterfaces(jvmElement);
      boolean _isIgnored = this.isIgnored(IssueCodes.FIELD_NAME_SHADOWING);
      boolean _not = (!_isIgnored);
      if (_not) {
        Iterable<JvmField> _declaredFields = jvmElement.getDeclaredFields();
        for (final JvmField feature : _declaredFields) {
          String _simpleName = feature.getSimpleName();
          boolean _isHiddenAttribute = ModelUtil.isHiddenAttribute(_simpleName);
          boolean _not_1 = (!_isHiddenAttribute);
          if (_not_1) {
            String _simpleName_1 = feature.getSimpleName();
            JvmField inheritedField = inheritedFields.get(_simpleName_1);
            boolean _tripleNotEquals_1 = (inheritedField != null);
            if (_tripleNotEquals_1) {
              String _simpleName_2 = feature.getSimpleName();
              String _qualifiedName = jvmElement.getQualifiedName();
              String _qualifiedName_1 = inheritedField.getQualifiedName();
              String _format = String.format(
                "The field \'%s\' in \'%s\' is hidding the inherited field \'%s\'.", _simpleName_2, _qualifiedName, _qualifiedName_1);
              this.warning(_format, feature, 
                null, 
                IssueCodes.FIELD_NAME_SHADOWING);
            }
          }
        }
      }
      Iterable<JvmOperation> _declaredOperations = jvmElement.getDeclaredOperations();
      for (final JvmOperation feature_1 : _declaredOperations) {
        {
          boolean _isVarArgs = feature_1.isVarArgs();
          EList<JvmFormalParameter> _parameters = feature_1.getParameters();
          SignatureKey sig = this.sarlSignatureProvider.createSignatureIDFromJvmModel(_isVarArgs, _parameters);
          String _simpleName_3 = feature_1.getSimpleName();
          ActionKey actionKey = this.sarlSignatureProvider.createActionID(_simpleName_3, sig);
          JvmOperation implementedFunction = operationsToImplement.remove(actionKey);
          boolean _containsKey = finalOperations.containsKey(actionKey);
          if (_containsKey) {
            String _string = actionKey.toString();
            String _format_1 = String.format(
              "Cannot override the operation %s, which is declared a final in the super type.", _string);
            this.error(_format_1, feature_1, 
              null, 
              IssueCodes.OVERRIDE_FINAL_OPERATION);
          } else {
            boolean _tripleNotEquals_2 = (implementedFunction != null);
            if (_tripleNotEquals_2) {
              JvmTypeReference _returnType = feature_1.getReturnType();
              LightweightTypeReference currentReturnType = this.toLightweightTypeReference(_returnType);
              JvmTypeReference _returnType_1 = implementedFunction.getReturnType();
              LightweightTypeReference inheritedReturnType = this.toLightweightTypeReference(_returnType_1);
              boolean _canCast = ModelUtil.canCast(currentReturnType, inheritedReturnType, false);
              boolean _not_2 = (!_canCast);
              if (_not_2) {
                String _canonicalName = this.canonicalName(currentReturnType);
                String _canonicalName_1 = this.canonicalName(inheritedReturnType);
                String _string_1 = actionKey.toString();
                String _format_2 = String.format(
                  "Incompatible return type between \'%s\' and \'%s\' for %s.", _canonicalName, _canonicalName_1, _string_1);
                this.error(_format_2, feature_1, 
                  null, 
                  org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
              }
            } else {
              JvmOperation superOperation = overridableOperations.get(actionKey);
              boolean _tripleNotEquals_3 = (superOperation != null);
              if (_tripleNotEquals_3) {
                JvmTypeReference _returnType_2 = feature_1.getReturnType();
                LightweightTypeReference currentReturnType_1 = this.toLightweightTypeReference(_returnType_2);
                JvmTypeReference _returnType_3 = superOperation.getReturnType();
                LightweightTypeReference inheritedReturnType_1 = this.toLightweightTypeReference(_returnType_3);
                boolean _canCast_1 = ModelUtil.canCast(currentReturnType_1, inheritedReturnType_1, false);
                boolean _not_3 = (!_canCast_1);
                if (_not_3) {
                  String _canonicalName_2 = this.canonicalName(currentReturnType_1);
                  String _canonicalName_3 = this.canonicalName(inheritedReturnType_1);
                  String _string_2 = actionKey.toString();
                  String _format_3 = String.format(
                    "Incompatible return type between \'%s\' and \'%s\' for %s.", _canonicalName_2, _canonicalName_3, _string_2);
                  this.error(_format_3, feature_1, 
                    null, 
                    org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE);
                }
              }
            }
          }
        }
      }
      boolean _and = false;
      boolean _isAbstract = jvmElement.isAbstract();
      boolean _not_2 = (!_isAbstract);
      if (!_not_2) {
        _and = false;
      } else {
        boolean _isInterface = jvmElement.isInterface();
        boolean _not_3 = (!_isInterface);
        _and = _not_3;
      }
      if (_and) {
        Set<ActionKey> _keySet = operationsToImplement.keySet();
        for (final ActionKey key : _keySet) {
          String _string = key.toString();
          String _format_1 = String.format(
            "The operation %s must be implemented.", _string);
          this.error(_format_1, element, 
            null, 
            IssueCodes.MISSING_ACTION_IMPLEMENTATION);
        }
      }
    }
  }
  
  /**
   * @param element
   */
  @Check
  public void checkNoFinalTypeExtension(final InheritingElement element) {
    JvmGenericType jvmElement = this.getJvmGenericType(element);
    boolean _tripleNotEquals = (jvmElement != null);
    if (_tripleNotEquals) {
      EList<JvmTypeReference> _superTypes = jvmElement.getSuperTypes();
      for (final JvmTypeReference superType : _superTypes) {
        {
          LightweightTypeReference ref = this.toLightweightTypeReference(superType);
          boolean _and = false;
          boolean _tripleNotEquals_1 = (ref != null);
          if (!_tripleNotEquals_1) {
            _and = false;
          } else {
            boolean _isFinal = this.isFinal(ref);
            _and = _isFinal;
          }
          if (_and) {
            String _qualifiedName = superType.getQualifiedName();
            String _format = String.format(
              "Cannot extend the final type \'%s\'.", _qualifiedName);
            this.error(_format, element, 
              null, 
              IssueCodes.FINAL_TYPE_EXTENSION);
          }
        }
      }
    }
  }
}

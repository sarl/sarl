/*
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
package io.sarl.lang.validation

import com.google.inject.Inject
import io.sarl.lang.SARLKeywords
import io.sarl.lang.sarl.Action
import io.sarl.lang.sarl.ActionSignature
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.Attribute
import io.sarl.lang.sarl.Behavior
import io.sarl.lang.sarl.Constructor
import io.sarl.lang.sarl.Event
import io.sarl.lang.sarl.FeatureContainer
import io.sarl.lang.sarl.FormalParameter
import io.sarl.lang.sarl.InheritingElement
import io.sarl.lang.sarl.ParameterizedFeature
import io.sarl.lang.sarl.Skill
import io.sarl.lang.signature.ActionKey
import io.sarl.lang.signature.ActionNameKey
import io.sarl.lang.signature.ActionSignatureProvider
import io.sarl.lang.signature.SignatureKey
import java.util.ArrayList
import java.util.List
import java.util.Map
import java.util.Set
import java.util.TreeMap
import java.util.TreeSet
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmIdentifiableElement
import org.eclipse.xtext.common.types.JvmMember
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference
import org.eclipse.xtext.xbase.validation.IssueCodes

import static org.eclipse.xtext.common.types.JvmVisibility.*
import static extension io.sarl.lang.util.JvmElementUtil.*

// 
/**
 * Validator for the SARL elements.
 * <p>
 * The following issues are not yet supported:<ul>
 * <li>Override of final method - ERROR</li>
 * <li>Missed function implementation - ERROR</li>
 * <li>Skill implementation cannot have default value - ERROR</li>
 * <li>Invalid super constructor call - ERROR</li>
 * <li>Missed super call - ERROR</li>
 * <li>Invalid return type for an action against the inherited type</li>
 * <li>Incompatible modifiers for a function</li>
 * </ul>
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class SARLValidator extends AbstractSARLValidator {

	@Inject
	private ILogicalContainerProvider logicalContainerProvider;

	@Inject
	private ActionSignatureProvider sarlSignatureProvider

	@Check
	def checkNoDefaultValueForVariadicParameter(ParameterizedFeature feature) {
		if (feature.varargs) {
			var lastParam = feature.params.last
			if (lastParam.defaultValue!=null) {
				error(
					String.format(
						"A default value cannot be declared for the variadic formal parameter '%s'.",
						lastParam.name
					), 
					lastParam,
					null,
					io.sarl.lang.validation.IssueCodes::DEFAULT_VALUE_FOR_VARIADIC_PARAMETER)
			}
		}
	}
		
	private def checkDefaultValueTypeCompatibleWithParameterType(FormalParameter param) {
		var toType = toLightweightTypeReference(param.parameterType, true)
		var fromType = param.defaultValue.actualType
		if (fromType===null) {
			error(
				String.format(
					"Cannot determine the type of the default value for the parameter '%s'.",
					param.name
				), 
				param,
				null,
				IssueCodes::INVALID_TYPE_PARAMETER_BOUNDS)
		}
		
		if ((fromType.type instanceof JvmDeclaredType || fromType.isPrimitive)
			&&
			(!fromType.type.isInterface || toType.isFinal) // if one of the types is an interface and the other is a non final class (or interface) there always can be a subtype
			&&
			(!fromType.type.isInterface || fromType.isFinal)
			&&
			(!toType.isAssignableFrom(fromType))
			&&
			(   fromType.isFinal || toType.isFinal
				|| fromType.isClass && toType.isClass)
			&&
			(!fromType.isAssignableFrom(toType)) // no upcast
		) { 
			error(String.format("Cannot cast from %s to %s",
					fromType.getNameOfTypes, toType.canonicalName),
				param,
				null,
				ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
				IssueCodes::INVALID_CAST);
		}
		else if(toType.primitive && !(fromType.primitive || fromType.wrapper)) {
				error(String.format("Cannot cast from %s to %s",
					fromType.getNameOfTypes, toType.canonicalName
					),
					param,
					null, 
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					IssueCodes::INVALID_CAST);
		}
	}

	@Check
	def checkDefaultValueTypeCompatibleWithParameterType(ParameterizedFeature feature) {
		for(param : feature.params) {
			if (param.defaultValue!=null) {
				checkDefaultValueTypeCompatibleWithParameterType(param)
			}
		}
	}

	@Check
	def checkNoFeatureMultiDefinition(FeatureContainer featureContainer) {
		var Set<String> localFields = new TreeSet
		var Set<ActionKey> localFunctions = new TreeSet
		var ActionNameKey actionID
		var SignatureKey signatureID
		var String name
		
		var JvmIdentifiableElement container = null
		
		for(feature : featureContainer.features) {
			if (container===null) {
				container = logicalContainerProvider.getNearestLogicalContainer(feature)
			}
			if (feature instanceof Action) {
				var s = feature.signature as ActionSignature
				name = s.name
				actionID = sarlSignatureProvider.createFunctionID(container, name)
				signatureID = sarlSignatureProvider.createSignatureIDFromSarlModel(s.params)
			}
			else if (feature instanceof ActionSignature) {
				name = feature.name
				actionID = sarlSignatureProvider.createFunctionID(container, name)
				signatureID = sarlSignatureProvider.createSignatureIDFromSarlModel(feature.params)
			}
			else if (feature instanceof Constructor) {
				name = SARLKeywords.CONSTRUCTOR
				actionID = sarlSignatureProvider.createConstructorID(container)
				signatureID = sarlSignatureProvider.createSignatureIDFromSarlModel(feature.params)
			}
			else {
				name = null
				actionID = null
				signatureID = null
				if (feature instanceof Attribute) {
					if (!localFields.add(feature.name)) {
						error(
							String.format(
								"Cannot define many times the same feature in '%s': %s",
								featureContainer.name,
								feature.name
							), 
							feature,
							null,
							io.sarl.lang.validation.IssueCodes::FIELD_ALREADY_DEFINED)
					}
				}
			}
			if (actionID!==null && signatureID!==null) {
				var sig = sarlSignatureProvider.getSignatures(actionID, signatureID)
				if (sig!==null) {
					for(key : sig.signatureKeys) {
						if (!localFunctions.add(key.toActionKey(name))) {
							error(
								String.format(
									"Cannot define many times the same feature in '%s': %s",
									featureContainer.name,
									name+"("+sig.toString+")"
								), 
								feature,
								null,
								io.sarl.lang.validation.IssueCodes::ACTION_ALREADY_DEFINED)
						}
					}
				}
			}
		}
	}
		
	@Check
	def checkActionName(ActionSignature action) {
		if (isHiddenAction(action.name)) {
			error(
				String.format(
					"Invalid action name '%s'. You must not give to an action a name that is starting with '_handle_'. This prefix is reserved by the SARL compiler.",
					action.name
				), 
				action,
				null,
				io.sarl.lang.validation.IssueCodes::INVALID_ACTION_NAME)
		}
	}
	
	@Check
	def checkAttributeName(Attribute attribute) {
		if (attribute.name.isHiddenAttribute) {
			error(
				String.format(
					"Invalid attribute name '%s'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.",
					attribute.name
				), 
				attribute,
				null,
				io.sarl.lang.validation.IssueCodes::INVALID_ATTRIBUTE_NAME)
		}
	}
	
	protected def JvmGenericType getJvmGenericType(EObject element) {
		return element.getJvmGenericType(services.jvmModelAssociations)
	}
	
	@Check
	def dispatch checkFinalFieldInitialization(Event event) {
		var JvmGenericType type = event.jvmGenericType
		if (type!==null) {
			type.checkFinalFieldInitialization
		}
	}
	
	@Check
	def dispatch checkFinalFieldInitialization(Agent agent) {
		var JvmGenericType type = agent.jvmGenericType
		if (type!==null) {
			type.checkFinalFieldInitialization
		}
	}

	@Check
	def dispatch checkFinalFieldInitialization(Behavior behavior) {
		var JvmGenericType type = behavior.jvmGenericType
		if (type!==null) {
			type.checkFinalFieldInitialization
		}
	}

	@Check
	def dispatch checkFinalFieldInitialization(Skill skill) {
		var JvmGenericType type = skill.jvmGenericType
		if (type!==null) {
			type.checkFinalFieldInitialization
		}
	}

	// Override this function to ensure that the initialization default is reported.
	protected override reportUninitializedField(JvmField field) {
		error(
			String.format(
				"The blank final field '%s' may not have been initialized.",
				field.simpleName
			), 
			field,
			null,
			IssueCodes::MISSING_INITIALIZATION)
	}
		
	private def checkRedundantInterface(JvmGenericType jvmElement, JvmTypeReference interfaceReference, LightweightTypeReference lightweightInterfaceReference, Iterable<LightweightTypeReference> knownInterfaces) {
		if (jvmElement.extendedClass!==null) {
			var superType = jvmElement.extendedClass.toLightweightTypeReference
			if (memberOfTypeHierarchy(superType, lightweightInterfaceReference)) {
				warning(
					String.format(
						"The feature '%s' is already implemented by the super type '%s'.",
						canonicalName(lightweightInterfaceReference),
						canonicalName(superType)),
					interfaceReference,
					null,
					io.sarl.lang.validation.IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION)
					return;
			}
		}
		
		for(previousInterface : knownInterfaces) {
			if (memberOfTypeHierarchy(previousInterface, lightweightInterfaceReference)) {
				warning(
					String.format(
						"The feature '%s' is already implemented by the preceding interface '%s'.",
						canonicalName(lightweightInterfaceReference),
						canonicalName(previousInterface)),
					interfaceReference,
					null,
					io.sarl.lang.validation.IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION)
					return;
			}
		}
	}
	
	private def checkRedundantInterfaces(JvmGenericType jvmElement) {
		if (!isIgnored(io.sarl.lang.validation.IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION)) {
			var List<LightweightTypeReference> knownInterfaces = new ArrayList
			for(interface : jvmElement.extendedInterfaces) {
				var interfaceType = interface.toLightweightTypeReference
				checkRedundantInterface(jvmElement, interface, interfaceType, knownInterfaces)
				knownInterfaces.add(interfaceType)
			}
		}
	}

	@Check
	def checkInheritedFeatures(InheritingElement element) {
		var jvmElement = element.jvmGenericType
		if (jvmElement!==null) {
			val Map<ActionKey,JvmOperation> finalOperations = new TreeMap
			val Map<ActionKey,JvmOperation> overridableOperations = new TreeMap
			val Map<String,JvmField> inheritedFields = new TreeMap
			val Map<ActionKey,JvmOperation> operationsToImplement = new TreeMap

			jvmElement.populateInheritanceContext(
				finalOperations, overridableOperations,
				inheritedFields, operationsToImplement,
				sarlSignatureProvider
			)
						
			jvmElement.checkRedundantInterfaces
			
			if (!isIgnored(io.sarl.lang.validation.IssueCodes::FIELD_NAME_SHADOWING)) {
				for(feature : jvmElement.declaredFields) {
					val inheritedField = inheritedFields.get(feature.simpleName)
					if (inheritedField!==null) {
						warning(
							String.format(
								"The field '%s' in '%s' is hidding the inherited field '%s'.",
								feature.simpleName, jvmElement.qualifiedName,
								inheritedField.qualifiedName),
							feature,
							null,
							io.sarl.lang.validation.IssueCodes::FIELD_NAME_SHADOWING)
					}
				}
			}
		}
	}

}

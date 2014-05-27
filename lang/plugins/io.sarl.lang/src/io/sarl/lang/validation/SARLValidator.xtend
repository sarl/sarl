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
import org.eclipse.xtext.common.types.JvmConstructor
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmIdentifiableElement
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference

import static io.sarl.lang.util.ModelUtil.*

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
class SARLValidator extends AbstractSARLValidator {

	@Inject
	private ILogicalContainerProvider logicalContainerProvider;

	@Inject
	private ActionSignatureProvider sarlSignatureProvider;

	/**
	 * @param feature
	 */
	@Check
	public def checkNoDefaultValueForVariadicParameter(ParameterizedFeature feature) {
		if (feature.varargs) {
			var FormalParameter lastParam = feature.params.last()
			if (lastParam.defaultValue!==null) {
				error(
						String.format(
								"A default value cannot be declared for the variadic formal parameter '%s'.", //$NON-NLS-1$
								lastParam.name), 
						lastParam,
						null,
						IssueCodes.DEFAULT_VALUE_FOR_VARIADIC_PARAMETER)
			}
		}
	}

	private def checkDefaultValueTypeCompatibleWithParameterType(FormalParameter param) {
		var toType = toLightweightTypeReference(param.parameterType, true)
		var fromType = param.defaultValue.actualType
		if (!canCast(fromType, toType, true)) {
			error(String.format("Cannot cast from %s to %s", //$NON-NLS-1$
					fromType.nameOfTypes, toType.canonicalName),
					param,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					org.eclipse.xtext.xbase.validation.IssueCodes::INVALID_CAST)
		}
	}

	/**
	 * @param feature
	 */
	@Check
	public def checkDefaultValueTypeCompatibleWithParameterType(ParameterizedFeature feature) {
		for(param : feature.params) {
			if (param.defaultValue!==null) {
				param.checkDefaultValueTypeCompatibleWithParameterType
			}
		}
	}

	/**
	 * @param featureContainer
	 */
	@Check
	public def checkNoFeatureMultiDefinition(FeatureContainer featureContainer) {
		val Set<String> localFields = new TreeSet
		val Set<ActionKey> localFunctions = new TreeSet
		var ActionNameKey actionID
		var SignatureKey signatureID
		var String name

		var JvmIdentifiableElement container = null

		for(feature : featureContainer.getFeatures()) {
			if (container===null) {
				container = this.logicalContainerProvider.getNearestLogicalContainer(feature)
			}
			if (feature instanceof Action) {
				var ActionSignature s = feature.signature as ActionSignature
				name = s.name
				actionID = this.sarlSignatureProvider.createFunctionID(container, name)
				signatureID = this.sarlSignatureProvider.createSignatureIDFromSarlModel(s.varargs, s.params)
			}
			else if (feature instanceof ActionSignature) {
				name = feature.name
				actionID = this.sarlSignatureProvider.createFunctionID(container, name)
				signatureID = this.sarlSignatureProvider.createSignatureIDFromSarlModel(feature.varargs, feature.params)
			}
			else if (feature instanceof Constructor) {
				name = SARLKeywords.CONSTRUCTOR
				actionID = this.sarlSignatureProvider.createConstructorID(container)
				signatureID = this.sarlSignatureProvider.createSignatureIDFromSarlModel(feature.varargs, feature.params)
			}
			else {
				name = null
				actionID = null
				signatureID = null
				if (feature instanceof Attribute) {
					if (!localFields.add(feature.name)) {
						error(
								String.format(
										"Cannot define many times the same feature in '%s': %s", //$NON-NLS-1$
										featureContainer.name,
										feature.name
										), 
								feature,
								null,
								IssueCodes::FIELD_ALREADY_DEFINED)
					}
				}
			}
			if (actionID!==null && signatureID!==null) {
				var sig = this.sarlSignatureProvider.getSignatures(actionID, signatureID)
				if (sig!==null) {
					for(SignatureKey key : sig.signatureKeys()) {
						if (!localFunctions.add(key.toActionKey(name))) {
							error(
									String.format(
											"Cannot define many times the same feature in '%s': %s", //$NON-NLS-1$
											featureContainer.name,
											name+"("+sig.toString()+")" //$NON-NLS-1$ //$NON-NLS-2$
											), 
									feature,
									null,
									IssueCodes::ACTION_ALREADY_DEFINED)
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
	public def checkActionName(ActionSignature action) {
		if (isHiddenAction(action.getName())) {
			error(
					String.format(
							"Invalid action name '%s'. You must not give to an action a name that is starting with '_handle_'. This prefix is reserved by the SARL compiler.", //$NON-NLS-1$
							action.name
							), 
					action,
					null,
					IssueCodes::INVALID_ACTION_NAME)
		}
	}

	/**
	 * @param attribute
	 */
	@Check
	public def checkAttributeName(Attribute attribute) {
		if (isHiddenAttribute(attribute.getName())) {
			error(
					String.format(
							"Invalid attribute name '%s'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.", //$NON-NLS-1$
							attribute.name
							), 
					attribute,
					null,
					IssueCodes::INVALID_ATTRIBUTE_NAME)
		}
	}

	/** Replies the JVM generic type for the given element.
	 * 
	 * @param element
	 * @return the generic type of the given element.
	 */
	protected def JvmGenericType getJvmGenericType(EObject element) {
		return getJvmGenericType(element, services.jvmModelAssociations)
	}

	/**
	 * @param event
	 */
	@Check
	public def checkFinalFieldInitialization(Event event) {
		var type = event.jvmGenericType
		if (type!==null) {
			type.checkFinalFieldInitialization
		}
	}

	/**
	 * @param agent
	 */
	@Check
	public def checkFinalFieldInitialization(Agent agent) {
		var type = agent.jvmGenericType
		if (type!==null) {
			type.checkFinalFieldInitialization
		}
	}

	/**
	 * @param behavior
	 */
	@Check
	public def checkFinalFieldInitialization(Behavior behavior) {
		var type = behavior.jvmGenericType
		if (type!=null) {
			type.checkFinalFieldInitialization
		}
	}

	/**
	 * @param skill
	 */
	@Check
	public def checkFinalFieldInitialization(Skill skill) {
		var type = skill.jvmGenericType
		if (type!=null) {
			type.checkFinalFieldInitialization
		}
	}

	// Override this function to ensure that the initialization default is reported.
	protected override reportUninitializedField(JvmField field) {
		error(
				String.format(
						"The blank final field '%s' may not have been initialized.", //$NON-NLS-1$
						field.simpleName
						), 
				field,
				null,
				org.eclipse.xtext.xbase.validation.IssueCodes::MISSING_INITIALIZATION)
	}

	private def checkRedundantInterface(
			JvmGenericType jvmElement, JvmTypeReference interfaceReference, 
			LightweightTypeReference lightweightInterfaceReference, 
			Iterable<LightweightTypeReference> knownInterfaces) {
		if (jvmElement.getExtendedClass()!==null) {
			var superType = jvmElement.extendedClass.toLightweightTypeReference
			if (memberOfTypeHierarchy(superType, lightweightInterfaceReference)) {
				warning(
						String.format(
								"The feature '%s' is already implemented by the super type '%s'.", //$NON-NLS-1$
								lightweightInterfaceReference.canonicalName,
								superType.canonicalName),
						interfaceReference,
						null,
						IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION)
				return
			}
		}

		for(previousInterface : knownInterfaces) {
			if (memberOfTypeHierarchy(previousInterface, lightweightInterfaceReference)) {
				warning(
						String.format(
								"The feature '%s' is already implemented by the preceding interface '%s'.", //$NON-NLS-1$
								lightweightInterfaceReference.canonicalName,
								previousInterface.canonicalName),
						interfaceReference,
						null,
						IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION)
				return
			}
		}
	}

	private def checkRedundantInterfaces(JvmGenericType jvmElement) {
		if (!isIgnored(IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION)) {
			var List<LightweightTypeReference> knownInterfaces = new ArrayList
			for(inter : jvmElement.extendedInterfaces) {
				var interfaceType = inter.toLightweightTypeReference
				checkRedundantInterface(jvmElement, inter, interfaceType, knownInterfaces)
				knownInterfaces.add(interfaceType)
			}
		}
	}

	/**
	 * @param element
	 */
	@Check
	public def checkInheritedFeatures(InheritingElement element) {
		var jvmElement = element.jvmGenericType
		if (jvmElement!==null) {
			var Map<ActionKey,JvmOperation> finalOperations = new TreeMap
			var Map<ActionKey,JvmOperation> overridableOperations = new TreeMap
			var Map<String,JvmField> inheritedFields = new TreeMap
			var Map<ActionKey,JvmOperation> operationsToImplement = new TreeMap
			var Map<SignatureKey,JvmConstructor> superConstructors = new TreeMap

			populateInheritanceContext(
					jvmElement,
					finalOperations, overridableOperations,
					inheritedFields, operationsToImplement,
					superConstructors, this.sarlSignatureProvider)

			checkRedundantInterfaces(jvmElement)

			if (!IssueCodes::FIELD_NAME_SHADOWING.ignored) {
				for(feature : jvmElement.declaredFields) {
					if (!isHiddenAttribute(feature.simpleName)) {
						var inheritedField = inheritedFields.get(feature.simpleName)
						if (inheritedField!==null) {
							warning(
									String.format(
											"The field '%s' in '%s' is hidding the inherited field '%s'.", //$NON-NLS-1$
											feature.simpleName, jvmElement.qualifiedName,
											inheritedField.qualifiedName),
									feature,
									null,
									IssueCodes::FIELD_NAME_SHADOWING)
						}
					}
				}
			}

			for(feature : jvmElement.declaredOperations) {
				var sig = this.sarlSignatureProvider.createSignatureIDFromJvmModel(feature.varArgs, feature.parameters)
				var actionKey = this.sarlSignatureProvider.createActionID(feature.simpleName, sig)
				var implementedFunction = operationsToImplement.remove(actionKey)
				if (finalOperations.containsKey(actionKey)) {
					error(
							String.format(
									"Cannot override the operation %s, which is declared a final in the super type.", //$NON-NLS-1$
									actionKey.toString),
							feature,
							null,
							IssueCodes::OVERRIDE_FINAL_OPERATION)
				}
				else {
					if (implementedFunction!==null) {
						var currentReturnType = feature.returnType.toLightweightTypeReference
						var inheritedReturnType = implementedFunction.returnType.toLightweightTypeReference
						if (!canCast(currentReturnType, inheritedReturnType, false)) {
							error(
									String.format(
											"Incompatible return type between '%s' and '%s' for %s.", //$NON-NLS-1$
											currentReturnType.canonicalName,
											inheritedReturnType.canonicalName,
											actionKey.toString),
									feature,
									null,
									org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE)
						}
					}
					else {
						var superOperation = overridableOperations.get(actionKey)
						if (superOperation!==null) {
							var currentReturnType = feature.returnType.toLightweightTypeReference
							var inheritedReturnType = superOperation.returnType.toLightweightTypeReference
							if (!canCast(currentReturnType, inheritedReturnType, false)) {
								error(
										String.format(
												"Incompatible return type between '%s' and '%s' for %s.", //$NON-NLS-1$
												currentReturnType.canonicalName,
												inheritedReturnType.canonicalName,
												actionKey.toString),
										feature,
										null,
										org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE);
							}
						}
					}
				}
			}

			if (!jvmElement.abstract && !jvmElement.interface) {
				for(key : operationsToImplement.keySet()) {
					error(
							String.format(
									"The operation %s must be implemented.", //$NON-NLS-1$
									key.toString),
							element,
							null,
							IssueCodes::MISSING_ACTION_IMPLEMENTATION)
				}
			}
		}
	}

	/**
	 * @param element
	 */
	@Check
	public def checkNoFinalTypeExtension(InheritingElement element) {
		var jvmElement = element.jvmGenericType
		if (jvmElement!==null) {
			for(superType : jvmElement.superTypes) {
				var ref = superType.toLightweightTypeReference
				if (ref!==null && ref.final) {
					error(
							String.format(
									"Cannot extend the final type '%s'.", //$NON-NLS-1$
									superType.qualifiedName),
							element,
							null,
							IssueCodes::FINAL_TYPE_EXTENSION)
				}
			}
		}
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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
import io.sarl.lang.core.Capacity
import io.sarl.lang.sarl.Action
import io.sarl.lang.sarl.ActionSignature
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.Attribute
import io.sarl.lang.sarl.Behavior
import io.sarl.lang.sarl.BehaviorUnit
import io.sarl.lang.sarl.CapacityUses
import io.sarl.lang.sarl.Constructor
import io.sarl.lang.sarl.Event
import io.sarl.lang.sarl.FeatureContainer
import io.sarl.lang.sarl.FormalParameter
import io.sarl.lang.sarl.ImplementingElement
import io.sarl.lang.sarl.InheritingElement
import io.sarl.lang.sarl.NamedElement
import io.sarl.lang.sarl.ParameterizedFeature
import io.sarl.lang.sarl.RequiredCapacity
import io.sarl.lang.sarl.SarlPackage
import io.sarl.lang.sarl.SarlScript
import io.sarl.lang.sarl.Skill
import io.sarl.lang.signature.ActionNameKey
import io.sarl.lang.signature.ActionSignatureProvider
import io.sarl.lang.signature.SignatureKey
import io.sarl.lang.util.ModelUtil
import java.util.List
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.xtext.common.types.JvmConstructor
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmIdentifiableElement
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.validation.CheckType
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.eclipse.xtext.xbase.XBlockExpression
import org.eclipse.xtext.xbase.XBooleanLiteral
import org.eclipse.xtext.xbase.XConstructorCall
import org.eclipse.xtext.xbase.XFeatureCall
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference

import static io.sarl.lang.util.ModelUtil.*

/**
 * Validator for the SARL elements.
 * <p>
 * The check type may be one of:<ul>
 * <li>{@link CheckType#FAST}: is executed after a delay of 500ms after ANY editing action (type, enter, delete);</li>
 * <li>{@link CheckType#NORMAL}: is executed after a build (manual, or automatic);</li>
 * <li>{@link CheckType#EXPENSIVE}: is executed by right clicking ANYWHERE in the editor window and chooseing "Validate".</li>
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
	private ILogicalContainerProvider logicalContainerProvider

	@Inject
	private ActionSignatureProvider sarlSignatureProvider
	
	protected def canonicalTypeName(LightweightTypeReference typeRef) {
		if (typeRef===null) "void" else typeRef.getHumanReadableName()
	}

	@Check(CheckType.NORMAL)
	public def checkClassPath(SarlScript sarlScript) {
		var typeReferences = services.typeReferences;

		var version = System.getProperty("java.specification.version"); //$NON-NLS-1$
				
		if (version==null || version.empty ||
			compareVersions(version, "1.7")<0) {
			error(
				"Couldn't find a JDK 1.7 or higher on the project's classpath.",
				sarlScript,
				null,
				IssueCodes::JDK_NOT_ON_CLASSPATH);
		}

		if (typeReferences.findDeclaredType(ReassignFirstArgument, sarlScript) == null) {
			error(
				"Couldn't find the mandatory library 'org.eclipse.xtext.xbase.lib' 2.6.0 or higher on the project's classpath.",
				sarlScript,
				null,
				IssueCodes::XBASE_LIB_NOT_ON_CLASSPATH)
		}
	}

	/**
	 * @param script
	 */
	@Check(CheckType.NORMAL)
	public def checkDuplicateTopElements(SarlScript script) {
		var QualifiedName packageName
		if (script.name!==null && !script.name.empty) {
			packageName = QualifiedName::create(script.name.split("\\."))
		}
		else {
			packageName = QualifiedName.create()			
		}
		var names = newTreeSet(null)
		for(feature : script.elements) {
			if (feature instanceof NamedElement) {
				val QualifiedName featureName = packageName.append(feature.name)
				// Check in the local file
				if (names.contains(featureName)) {
					error(
							String.format(
									"Duplicate definition of the type '%s'",
									featureName.toString), 
							feature,
							SarlPackage.Literals::NAMED_ELEMENT__NAME,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes.DUPLICATE_TYPE_NAME,
							featureName.toString)
				}
				// Check in the rest of the class path
				else {
					names.add(featureName)
				}
			}
		}
	}

	/**
	 * @param feature
	 */
	@Check(CheckType.FAST)
	public def checkNoDefaultValueForVariadicParameter(ParameterizedFeature feature) {
		if (feature.varargs) {
			var FormalParameter lastParam = feature.params.last()
			if (lastParam.defaultValue!==null) {
				error(
						String.format(
								"A default value cannot be declared for the variadic formal parameter '%s'.",
								lastParam.name), 
						feature,
						SarlPackage.Literals::PARAMETERIZED_FEATURE__VARARGS,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						IssueCodes.INVALID_USE_OF_VAR_ARG)
			}
		}
	}

	private def checkDefaultValueTypeCompatibleWithParameterType(FormalParameter param) {
		var rawType = param.parameterType
		if (rawType!==null) {
			var toType = toLightweightTypeReference(rawType, true)
			var fromType = param.defaultValue.actualType
			if (!canCast(fromType, toType, true, false, true)) {
				error(String.format("Type mismatch: cannot convert from %s to %s",
						fromType.nameOfTypes, toType.canonicalName),
						param,
						SarlPackage.Literals::FORMAL_PARAMETER__DEFAULT_VALUE,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_TYPES,
						fromType.canonicalName,
						toType.canonicalName)
			}
		}
		else {
				error("Illegal syntax",
						param,
						SarlPackage.Literals::FORMAL_PARAMETER__DEFAULT_VALUE,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						org.eclipse.xtext.xbase.validation.IssueCodes::INVALID_USE_OF_TYPE)
		}
	}

	/**
	 * @param feature
	 */
	@Check(CheckType.FAST)
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
	@Check(CheckType.FAST)
	public def checkNoFeatureMultiDefinition(FeatureContainer featureContainer) {
		val localFields = newTreeSet(null)
		val localFunctions = newTreeSet(null)
		var ActionNameKey actionID
		var SignatureKey signatureID
		var String name
		var EStructuralFeature errorStructFeature
		var EObject errorFeature

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
				errorFeature = s
				errorStructFeature = SarlPackage.Literals::ACTION_SIGNATURE__NAME
			}
			else if (feature instanceof ActionSignature) {
				name = feature.name
				actionID = this.sarlSignatureProvider.createFunctionID(container, name)
				signatureID = this.sarlSignatureProvider.createSignatureIDFromSarlModel(feature.varargs, feature.params)
				errorFeature = feature
				errorStructFeature = SarlPackage.Literals::ACTION_SIGNATURE__NAME
			}
			else if (feature instanceof Constructor) {
				name = SARLKeywords.CONSTRUCTOR
				actionID = this.sarlSignatureProvider.createConstructorID(container)
				signatureID = this.sarlSignatureProvider.createSignatureIDFromSarlModel(feature.varargs, feature.params)
				errorFeature = feature
				errorStructFeature = null
			}
			else {
				name = null
				actionID = null
				signatureID = null
				errorFeature = null
				errorStructFeature = null
				if (feature instanceof Attribute) {
					if (!localFields.add(feature.name)) {
						error(
								String.format(
										"Duplicate field in '%s': %s",
										featureContainer.name,
										feature.name
										), 
								feature,
								SarlPackage.Literals::ATTRIBUTE__NAME,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								IssueCodes::DUPLICATE_FIELD,
								feature.name)
					}
				}
			}
			if (actionID!==null && signatureID!==null) {
				var sig = this.sarlSignatureProvider.getSignatures(actionID, signatureID)
				if (sig!==null) {
					for(SignatureKey key : sig.signatureKeys()) {
						if (!localFunctions.add(key.toActionKey(name))) {
							var funcName = name+"("+sig.toString()+")"
							error(
									String.format(
											"Duplicate action in '%s': %s",
											featureContainer.name,
											funcName
											), 
									errorFeature,
									errorStructFeature,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									IssueCodes::DUPLICATE_METHOD,
									funcName)
						}
					}
				}
			}
		}
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkActionName(ActionSignature action) {
		if (isHiddenAction(action.name)) {
			var validName = ModelUtil::fixHiddenAction(action.name)
			error(
					String.format(
							"Invalid action name '%s'. You must not give to an action a name that is starting with '_handle_'. This prefix is reserved by the SARL compiler.",
							action.name
							), 
					action,
					SarlPackage.Literals::ACTION_SIGNATURE__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					IssueCodes::INVALID_MEMBER_NAME,
					"action", action.name, validName)
		}
	}

	/**
	 * @param attribute
	 */
	@Check(CheckType.FAST)
	public def checkAttributeName(Attribute attribute) {
		if (isHiddenAttribute(attribute.name)) {
			var validName = ModelUtil::fixHiddenAttribute(attribute.name)
			error(
					String.format(
							"Invalid attribute name '%s'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.",
							attribute.name
							), 
					attribute,
					SarlPackage.Literals::ATTRIBUTE__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					IssueCodes::INVALID_MEMBER_NAME,
					"attribute", attribute.name, validName)
		}
	}

	/** Replies the JVM generic type for the given element.
	 * 
	 * @param element
	 * @return the generic type of the given element.
	 */
	protected def JvmGenericType getJvmGenericType(EObject element) {
		if (element instanceof JvmGenericType) return element
		for(obj : services.jvmModelAssociations.getJvmElements(element)) {
			if (obj instanceof JvmGenericType) {
				return obj
			}
		}
		return null;
	}

	/**
	 * @param event
	 */
	@Check(CheckType.FAST)
	public def checkFinalFieldInitialization(Event event) {
		var type = event.jvmGenericType
		if (type!==null) {
			type.checkFinalFieldInitialization
		}
	}

	/**
	 * @param agent
	 */
	@Check(CheckType.FAST)
	public def checkFinalFieldInitialization(Agent agent) {
		var type = agent.jvmGenericType
		if (type!==null) {
			type.checkFinalFieldInitialization
		}
	}

	/**
	 * @param behavior
	 */
	@Check(CheckType.FAST)
	public def checkFinalFieldInitialization(Behavior behavior) {
		var type = behavior.jvmGenericType
		if (type!=null) {
			type.checkFinalFieldInitialization
		}
	}

	/**
	 * @param skill
	 */
	@Check(CheckType.FAST)
	public def checkFinalFieldInitialization(Skill skill) {
		var type = skill.jvmGenericType
		if (type!=null) {
			type.checkFinalFieldInitialization
		}
	}

	// Override this function to ensure that the initialization default is reported.
	protected override reportUninitializedField(JvmField field) {
		var sarlElement = services.jvmModelAssociations.getPrimarySourceElement(field)
		error(
				String.format(
						"The blank final field '%s' may not have been initialized.",
						field.simpleName
						), 
				sarlElement,
				SarlPackage.Literals.ATTRIBUTE__NAME,
				ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
				org.eclipse.xtext.xbase.validation.IssueCodes::MISSING_INITIALIZATION)
	}

	private def boolean checkRedundantInterface(
			InheritingElement element,
			EReference structuralElement,
			JvmParameterizedTypeReference interfaceReference,
			LightweightTypeReference lightweightInterfaceReference, 
			List<LightweightTypeReference> knownInterfaces) {
		var index = 0
		for(previousInterface : knownInterfaces) {
			if (memberOfTypeHierarchy(previousInterface, lightweightInterfaceReference)) {
				error(
						String.format(
								"Duplicate implemented feature '%s'.",
								lightweightInterfaceReference.canonicalName),
						element,
						structuralElement,
						knownInterfaces.size, // The index of the element to highlight in the super-types
						IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
						lightweightInterfaceReference.canonicalName,
						"pre")
				return true
			}
			else if (memberOfTypeHierarchy(lightweightInterfaceReference, previousInterface)) {
				error(
						String.format(
								"Duplicate implemented feature '%s'.",
								previousInterface.canonicalName),
						element,
						structuralElement,
						index,
						IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
						previousInterface.canonicalName,
						"post")
			}
			index++
		}
		return false
	}

	private def checkRedundantInterfaces(
						InheritingElement element, 
						EReference structuralElement,
						Iterable<JvmParameterizedTypeReference> interfaces,
						Iterable<JvmParameterizedTypeReference> superTypes) {
		var knownInterfaces = newArrayList
		for(interface : interfaces) {
			var lightweightInterfaceReference = interface.toLightweightTypeReference
			// Check the interface against the other interfaces 
			if (!checkRedundantInterface(
				element, structuralElement,
				interface, lightweightInterfaceReference, 
				knownInterfaces)) {
				// Check the interface against the super-types
				if (superTypes!==null && !IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION.ignored) {
					for(superType : superTypes) {
						var lightweightSuperType = superType.toLightweightTypeReference
						if (memberOfTypeHierarchy(lightweightSuperType, lightweightInterfaceReference)) {
							addIssue(
									String.format(
											"The feature '%s' is already implemented by the super-type '%s'.",
											lightweightInterfaceReference.canonicalName,
											lightweightSuperType.canonicalName),
									element,
									structuralElement,
									knownInterfaces.size, // The index of the element to highlight in the super-types
									IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION,
									lightweightInterfaceReference.canonicalName,
									"unknow")
						}
					}
				}
			}
			// Prepare next loop
			knownInterfaces += lightweightInterfaceReference
		}
	}

	/**
	 * @param element
	 */
	@Check(CheckType.FAST)
	public def checkInheritedFeatures(InheritingElement element) {
		var jvmElement = element.jvmGenericType
		if (jvmElement!==null) {
			var finalOperations = newTreeMap(null)
			var overridableOperations = newTreeMap(null)
			var inheritedFields = newTreeMap(null)
			var operationsToImplement = newTreeMap(null)

			populateInheritanceContext(
					jvmElement,
					finalOperations, overridableOperations,
					inheritedFields, operationsToImplement,
					null, this.sarlSignatureProvider)

			if (jvmElement.interface) {
				checkRedundantInterfaces(
					element, SarlPackage.Literals.INHERITING_ELEMENT__SUPER_TYPES,
					element.superTypes,
					null
				)
			}
			else if (element instanceof ImplementingElement) {
				checkRedundantInterfaces(
					element, SarlPackage.Literals.IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES,
					element.implementedTypes,
					element.superTypes
				)
			}
			
			for(feature : element.features) {
				if (feature instanceof Attribute) {
					if (!org.eclipse.xtext.xbase.validation.IssueCodes::VARIABLE_NAME_SHADOWING.ignored) {
						if (!isHiddenAttribute(feature.name)) {
							var inheritedField = inheritedFields.get(feature.name)
							if (inheritedField!==null) {
								var nameIndex = 0
								var newName = feature.name+Integer::toString(nameIndex)
								while (inheritedFields.containsKey(newName)) {
									nameIndex++
									newName = feature.name+Integer::toString(nameIndex)
								}
								addIssue(
										String.format(
												"The field '%s' in '%s' is hidding the inherited field '%s'.",
												feature.name, jvmElement.qualifiedName,
												inheritedField.qualifiedName),
										feature,
										SarlPackage.Literals.ATTRIBUTE__NAME,
										ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
										org.eclipse.xtext.xbase.validation.IssueCodes::VARIABLE_NAME_SHADOWING,
										feature.name,
										newName)
							}
						}
					}
				}
				else if (feature instanceof Action || feature instanceof ActionSignature) {
					var signature = (if (feature instanceof Action) (feature.signature) else feature) as ActionSignature
					var sig = this.sarlSignatureProvider.createSignatureIDFromSarlModel(signature.varargs, signature.params)
					var actionKey = this.sarlSignatureProvider.createActionID(signature.name, sig)
					if (finalOperations.containsKey(actionKey)) {
						error(
								String.format(
										"Cannot override the operation %s, which is declared a final in the super type.",
										actionKey.toString),
								signature,
								SarlPackage.Literals.ACTION_SIGNATURE__NAME,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								IssueCodes::OVERRIDDEN_FINAL_OPERATION,
								actionKey.toString)
					}
					else {
						var implementableFunction = operationsToImplement.remove(actionKey)
						if (implementableFunction!==null) {
							var currentReturnType = signature.type?.toLightweightTypeReference
							var inheritedReturnType = implementableFunction.returnType?.toLightweightTypeReference
							if (!canCast(currentReturnType, inheritedReturnType, false, true, true)) {
								error(
										String.format(
												"Incompatible return type between '%s' and '%s' for %s.",
												currentReturnType.canonicalTypeName,
												inheritedReturnType.canonicalTypeName,
												actionKey.toString),
										signature,
										SarlPackage.Literals.ACTION_SIGNATURE__TYPE,
										ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
										org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
										currentReturnType.canonicalTypeName,
										inheritedReturnType.identifier)
							}
						}
						else {
							var superOperation = overridableOperations.get(actionKey)
							if (superOperation!==null) {
								var currentReturnType = signature.type?.toLightweightTypeReference
								var inheritedReturnType = superOperation.returnType?.toLightweightTypeReference
								if (!canCast(currentReturnType, inheritedReturnType, false, true, true)) {
									error(
											String.format(
													"Incompatible return type between '%s' and '%s' for %s.",
													currentReturnType.canonicalTypeName,
													inheritedReturnType.canonicalTypeName,
													actionKey.toString),
											signature,
											SarlPackage.Literals.ACTION_SIGNATURE__TYPE,
											ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
											org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
											currentReturnType.canonicalTypeName,
											inheritedReturnType.identifier)
								}
							}
						}
					}
				}
			}
			
			if (!jvmElement.abstract && !jvmElement.interface && !operationsToImplement.empty) {
				
				// The missed function may be generated on the Java side
				// (see generateMissedFunction function in SARLJvmModelInferrer).
				for(javaOp : jvmElement.declaredOperations) {
					var sig = this.sarlSignatureProvider.createSignatureIDFromJvmModel(javaOp.varArgs, javaOp.parameters)
					var actionKey = this.sarlSignatureProvider.createActionID(javaOp.simpleName, sig)
					operationsToImplement.remove(actionKey)
				}
				
				// Now, we are sure that there is missed operations
				if (!operationsToImplement.empty) {
					var data = newArrayList
					for(value : operationsToImplement.values()) {
						data += ModelUtil::toActionProtoptypeString(value)
						data += ModelUtil::getDefaultValueForType(value.returnType?.toLightweightTypeReference)
					}
					var first = true
					for(key : operationsToImplement.keySet()) {
						if (first) {
							first = false
							error(
									String.format(
											"The operation %s must be implemented.",
											key.toString),
									element,
									SarlPackage.Literals.NAMED_ELEMENT__NAME,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									IssueCodes::MISSING_METHOD_IMPLEMENTATION,
									data) // Provides the prototypes for the quick fixes
						}
						else {
							 // No prototypes, so no quick fix
							error(
									String.format(
											"The operation %s must be implemented.",
											key.toString),
									element,
									SarlPackage.Literals.NAMED_ELEMENT__NAME,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									IssueCodes::MISSING_METHOD_IMPLEMENTATION)
						}
					}
				}
			}
		}
	}
	
	private def checkImplicitConstructorCall(FeatureContainer container, SignatureKey[] defaultSignatures) {
		var jvmElement = container.jvmGenericType
		if (jvmElement!==null) {
			var superConstructors = newTreeMap(null)
			var supertype = jvmElement.extendedClass?.type
			if (supertype!==null) {
				var jvmSuperElement = supertype.jvmGenericType
				if (jvmSuperElement!==null) {
					for(superConstructor : jvmSuperElement.declaredConstructors) {
						var sig = this.sarlSignatureProvider.createSignatureIDFromJvmModel(superConstructor.varArgs, superConstructor.parameters)
						superConstructors.put(sig, superConstructor)
					}
				}
			}
			
			var voidKey = sarlSignatureProvider.createSignatureIDForVoid
			var hasDeclaredConstructor = false
			
			for(feature : container.features) {
				if (feature instanceof Constructor) {
					hasDeclaredConstructor = true
					var invokeDefaultConstructor = true
					var body = feature.body
					if (body instanceof XBlockExpression) {
						if (!body.expressions.empty) {
							var firstStatement = body.expressions.get(0)
							if (firstStatement instanceof XConstructorCall) {
								invokeDefaultConstructor = false
							}
							else if (firstStatement instanceof XFeatureCall) {
								var calledFeature = firstStatement.feature
								if (calledFeature instanceof JvmConstructor) {
									invokeDefaultConstructor = false
								}
							}
						}
					}
					else if (body instanceof XConstructorCall) {
						invokeDefaultConstructor = false
					}
					else if (body instanceof XFeatureCall) {
						var calledFeature = body.feature
						if (calledFeature instanceof JvmConstructor) {
							invokeDefaultConstructor = false
						}
					}
					if (invokeDefaultConstructor && !superConstructors.containsKey(voidKey)) {
						error(
								"Undefined default constructor in the super-type.",
								feature,
								SarlPackage.Literals.CONSTRUCTOR__BODY,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								IssueCodes::MISSING_CONSTRUCTOR)
					}
				}
			}
			
			if (!hasDeclaredConstructor) {
				for(defaultSignature : defaultSignatures) {
					if (!superConstructors.containsKey(defaultSignature)) {
						error(
								String::format("The constructor %s is undefined.",
									 this.sarlSignatureProvider.createActionID(
									 	supertype.simpleName,
									 	defaultSignature)
								),
								container,
								SarlPackage.Literals.NAMED_ELEMENT__NAME,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								IssueCodes::MISSING_CONSTRUCTOR)
					}
				}
			}
			
		}
	}

	/**
	 * @param event
	 */
	@Check(CheckType.FAST)
	public def checkImplicitConstructorCall(Event event) {
		checkImplicitConstructorCall(event,
			#[
				sarlSignatureProvider.createSignatureIDForVoid,
				sarlSignatureProvider.createSignatureIDFromString("io.sarl.lang.core.Address")
			]
		)
	}

	/**
	 * @param behavior
	 */
	@Check(CheckType.FAST)
	public def checkImplicitConstructorCall(Behavior behavior) {
		checkImplicitConstructorCall(behavior,
			#[
				sarlSignatureProvider.createSignatureIDFromString("io.sarl.lang.core.Agent")
			]
		)
	}

	/**
	 * @param behaviorUnit
	 */
	@Check(CheckType.FAST)
	public def checkBehaviorUnitGuardType(BehaviorUnit behaviorUnit) {
		var guard = behaviorUnit.guard
		if (guard!==null) {
			if (guard instanceof XBooleanLiteral) {
				if (guard.isTrue) {
					if (!IssueCodes::DISCOURAGED_BOOLEAN_EXPRESSION.ignored) {
						addIssue("Discouraged boolean value. The guard is always true.",
								guard,
								null,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								IssueCodes::DISCOURAGED_BOOLEAN_EXPRESSION)
					}
				}
				else {
					if (!IssueCodes::UNREACHABLE_BEHAVIOR_UNIT.ignored) {
						addIssue("Dead code. The guard is always false.",
								behaviorUnit,
								null,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								IssueCodes::UNREACHABLE_BEHAVIOR_UNIT,
								behaviorUnit.event.simpleName)
					}
				}
				return;
			}

			var fromType = guard.actualType
			if (!fromType.isAssignableFrom(Boolean::TYPE)) {
				error(String.format("Type mismatch: cannot convert from %s to %s",
						fromType.nameOfTypes, boolean.name),
						behaviorUnit.guard,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_TYPES)
			}
		}
	}

	/**
	 * @param element
	 */
	@Check(CheckType.FAST)
	public def checkCapacityTypeForUses(CapacityUses uses) {
		for(usedType : uses.capacitiesUsed) {
			var ref = usedType.toLightweightTypeReference
			if (ref!==null && !ref.isSubtypeOf(typeof(Capacity))) {
				error(
						String.format(
								"Invalid type: '%s'. Only capacities can be used after the keyword '%s'.",
								usedType.qualifiedName,
								SARLKeywords::USES),
						usedType,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						IssueCodes::INVALID_CAPACITY_TYPE,
						usedType.simpleName)
			}
		}
	}

	/**
	 * @param element
	 */
	@Check(CheckType.FAST)
	public def checkCapacityTypeForRequires(RequiredCapacity requires) {
		for(requiredType : requires.requiredCapacities) {
			var ref = requiredType.toLightweightTypeReference
			if (ref!==null && !ref.isSubtypeOf(typeof(Capacity))) {
				error(
						String.format(
								"Invalid type: '%s'. Only capacities can be used after the keyword '%s'.",
								requiredType.qualifiedName,
								SARLKeywords::REQUIRES),
						requiredType,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						IssueCodes::INVALID_CAPACITY_TYPE,
						requiredType.simpleName)
			}
		}
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkActionSignatureFires(ActionSignature action) {
		for(event : action.firedEvents) {
			var ref = event.toLightweightTypeReference
			if (ref!==null && !ref.isSubtypeOf(typeof(io.sarl.lang.core.Event))) {
				error(
						String.format(
								"Invalid type: '%s'. Only events can be used after the keyword '%s'.",
								event.qualifiedName,
								SARLKeywords::FIRES),
						event,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						IssueCodes::INVALID_FIRING_EVENT_TYPE,
						event.simpleName)
			}
		}
	}
	
	protected def int checkSuperTypes(InheritingElement element, Class<?> expectedType, boolean onlySubTypes) {
		var int nbSuperTypes = 0
		var inferredType = element.jvmGenericType
		if (inferredType != null) {
			var isInterface = expectedType.interface
			var jvmTypes = inferredType.superTypes.iterator
			for(superType : element.superTypes) {
				var success = true
				var JvmTypeReference jvmType
				if (jvmTypes.hasNext) {
					jvmType = jvmTypes.next 
					var ref = superType.toLightweightTypeReference
					if (ref!==null) {
						if (((ref.interfaceType!==isInterface) || !ref.isSubtypeOf(expectedType)
							|| (onlySubTypes && ref.isType(expectedType)))) {
							var String msg
							if (onlySubTypes) {
								msg = "Invalid super-type: '%s'. Only subtypes of '%s' are allowed for '%s'."
							}
							else {
								msg = "Invalid super-type: '%s'. Only the type '%s' and one of its subtypes are allowed for '%s'."
							}
							error(
									String.format(
											msg,
											superType.qualifiedName,
											expectedType.name,
											element.name),
									superType,
									null,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									IssueCodes::INVALID_EXTENDED_TYPE,
									superType.simpleName)
							success = false
						}
						else if (superType.qualifiedName==inferredType.qualifiedName) {
							error( String::format(
											"Inconsistent type hierarchy for '%s': cycle is detected.",
											inferredType.qualifiedName),
									SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
									IssueCodes::INCONSISTENT_TYPE_HIERARCHY)
							success = false
						}
						else if (superType.qualifiedName==inferredType.qualifiedName
								|| jvmType.qualifiedName!=superType.qualifiedName) {
							error( String::format(
											"Inconsistent type hierarchy for '%s': cycle is detected, or super-type not found.",
											inferredType.qualifiedName),
									SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
									IssueCodes::INCONSISTENT_TYPE_HIERARCHY)
							success = false
						}
					}
				}
				else {
					error( String::format("Inconsistent type hierarchy for '%s': type not found '%s'.",
									inferredType.qualifiedName,
									superType.qualifiedName),
							SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
							IssueCodes::INCONSISTENT_TYPE_HIERARCHY)
					success = false
				}
				
				if (success) {
					nbSuperTypes++
				}
			}
		}
		return nbSuperTypes
	}


	protected def boolean checkImplementedTypes(ImplementingElement element, Class<?> expectedType, int mandatoryNumberOfTypes, boolean onlySubTypes) {
		var success = true
		var nb = 0
		for(superType : element.implementedTypes) {
			var ref = superType.toLightweightTypeReference
			if (ref!==null &&
				(!ref.interfaceType || !ref.isSubtypeOf(expectedType)
				|| (onlySubTypes && ref.isType(expectedType)))) {
				var String msg
				if (onlySubTypes) {
					msg = "Invalid implemented type: '%s'. Only subtypes of '%s' are allowed for '%s'."
				}
				else {
					msg = "Invalid implemented type: '%s'. Only the type '%s' and one of its subtypes are allowed for '%s'."
				}
				error(
						String.format(
								msg,
								superType.qualifiedName,
								expectedType.name,
								element.name),
						superType,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						IssueCodes::INVALID_IMPLEMENTED_TYPE,
						superType.simpleName)
				success = false
			}
			else {
				nb++
			}
		}
		if (nb<mandatoryNumberOfTypes) {
				error(
						String.format(
								"Missing implemented type '%s' for '%s'.",
								expectedType.name,
								element.name),
						element,
						SarlPackage.Literals::IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						org.eclipse.xtext.xbase.validation.IssueCodes::MISSING_TYPE)
				success = false
		}
		return success
	}
	
	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkEventSuperType(Event event) {
		checkSuperTypes(event, typeof(io.sarl.lang.core.Event), false)
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkBehaviorSuperType(Behavior behavior) {
		checkSuperTypes(behavior, typeof(io.sarl.lang.core.Behavior), false)
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkAgentSuperType(Agent agent) {
		checkSuperTypes(agent, typeof(io.sarl.lang.core.Agent), false)
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkCapacitySuperType(io.sarl.lang.sarl.Capacity capacity) {
		checkSuperTypes(capacity, typeof(Capacity), false)
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkSkillSuperType(Skill skill) {
		var nbSuperTypes = checkSuperTypes(skill, typeof(io.sarl.lang.core.Skill), false)
		checkImplementedTypes(skill, typeof(Capacity),
			(if (nbSuperTypes>0) 0 else 1),
			true)
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkBehaviorUnitEventType(BehaviorUnit behaviorUnit) {
		var event = behaviorUnit.event
		var ref = event.toLightweightTypeReference
		if (ref===null || ref.interfaceType || !ref.isSubtypeOf(typeof(io.sarl.lang.core.Event))) {
			error(
					String.format(
							"Invalid type: '%s'. Only events are allowed after the keyword '%s'.",
							event.qualifiedName,
							SARLKeywords.ON),
					event,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH)
		}
	}

	/**
	 * @param capacity
	 */
	@Check(CheckType.FAST)
	public def checkCapacityFeatures(io.sarl.lang.sarl.Capacity capacity) {
		if (capacity.features.empty) {
			if (!IssueCodes::DISCOURAGED_CAPACITY_DEFINITION.ignored) {
				addIssue("Discouraged capacity definition. A capacity without actions defined inside is not useful since it cannot be called by an agent or a behavior.",
						capacity,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						IssueCodes::DISCOURAGED_CAPACITY_DEFINITION,
						capacity.name,
						"aFunction")
			}
		}
	}

}

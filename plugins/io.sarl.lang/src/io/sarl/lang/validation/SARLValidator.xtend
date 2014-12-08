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

import com.google.common.collect.Lists
import com.google.common.collect.Multimaps
import com.google.inject.Inject
import io.sarl.lang.SARLKeywords
import io.sarl.lang.SARLLangActivator
import io.sarl.lang.annotation.ImportedCapacityFeature
import io.sarl.lang.core.Capacity
import io.sarl.lang.core.Event
import io.sarl.lang.sarl.Action
import io.sarl.lang.sarl.ActionSignature
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.Attribute
import io.sarl.lang.sarl.Behavior
import io.sarl.lang.sarl.BehaviorUnit
import io.sarl.lang.sarl.CapacityUses
import io.sarl.lang.sarl.Constructor
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
import io.sarl.lang.signature.ActionKey
import io.sarl.lang.signature.ActionNameKey
import io.sarl.lang.signature.ActionSignatureProvider
import io.sarl.lang.signature.SignatureKey
import io.sarl.lang.util.ModelUtil
import java.text.MessageFormat
import java.util.List
import java.util.Map
import org.eclipse.emf.ecore.EAttribute
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EReference
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.xtext.common.types.JvmConstructor
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmIdentifiableElement
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.validation.CheckType
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.eclipse.xtext.xbase.XAbstractFeatureCall
import org.eclipse.xtext.xbase.XBlockExpression
import org.eclipse.xtext.xbase.XBooleanLiteral
import org.eclipse.xtext.xbase.XConstructorCall
import org.eclipse.xtext.xbase.XFeatureCall
import org.eclipse.xtext.xbase.XMemberFeatureCall
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator
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
	
	@Inject
	private JvmModelAssociator jvmModelAssociator
	
	protected def canonicalTypeName(LightweightTypeReference typeRef) {
		if (typeRef===null) "void" else typeRef.getHumanReadableName()
	}

	@Check(CheckType.NORMAL)
	public def checkClassPath(SarlScript sarlScript) {
		var typeReferences = services.typeReferences;

		var version = System.getProperty("java.specification.version"); //$NON-NLS-1$
		
		var sarlLangBundle = SARLLangActivator::activator
		var minJdkVersion = sarlLangBundle.minimalJdkVersion
		var minXtextVersion = sarlLangBundle.minimalXtextVersion
		
		if (version==null || version.empty ||
			compareVersions(version, minJdkVersion)<0) {
			error(
				MessageFormat::format(Messages::SARLValidator_0, minJdkVersion),
				sarlScript,
				null,
				IssueCodes::JDK_NOT_ON_CLASSPATH);
		}

		if (typeReferences.findDeclaredType(ReassignFirstArgument, sarlScript) == null) {
			error(
				MessageFormat::format(Messages::SARLValidator_1, minXtextVersion),
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
							MessageFormat::format(
									Messages::SARLValidator_2,
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
	
	private def checkForbiddenFeatureCall(XAbstractFeatureCall expression) {
		var id = expression.feature.qualifiedName
		if (id == "java.lang.System.exit") {
			error(
				Messages.SARLValidator_39, 
				expression,
				null,
				ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
				org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE)
		}
	}

	@Check(CheckType.FAST)
	public def checkForbiddenCalls(XMemberFeatureCall expression) {
		expression.checkForbiddenFeatureCall
	}

	@Check(CheckType.FAST)
	public def checkForbiddenCalls(XFeatureCall expression) {
		expression.checkForbiddenFeatureCall
	}
	
	@Check(CheckType.FAST)
	private def checkDiscouragedFeatureCall(XAbstractFeatureCall expression) {
		var id = expression.feature.qualifiedName
		switch (id) {
			case "java.lang.System.err",
			case "java.lang.System.out",
			case "java.lang.System.setErr",
			case "java.lang.System.setOut",
			case "java.lang.System.console",
			case "java.lang.System.inheritedChannel": {
				addIssue(
					Messages.SARLValidator_40,
					expression,
					org.eclipse.xtext.xbase.validation.IssueCodes.DISCOURAGED_REFERENCE)
			}
			default: {
				if (id.startsWith("org.eclipse.xtext.xbase.lib.InputOutput")) {
					addIssue(
						Messages.SARLValidator_41,
						expression,
						org.eclipse.xtext.xbase.validation.IssueCodes.DISCOURAGED_REFERENCE)
				}
			}
		}
	}

	@Check(CheckType.FAST)
	public def checkDiscouragedCalls(XMemberFeatureCall expression) {
		if (!isIgnored(org.eclipse.xtext.xbase.validation.IssueCodes::DISCOURAGED_REFERENCE)) {
			expression.checkDiscouragedFeatureCall
		}
	}

	@Check(CheckType.FAST)
	public def checkDiscouragedCalls(XFeatureCall expression) {
		if (!isIgnored(org.eclipse.xtext.xbase.validation.IssueCodes::DISCOURAGED_REFERENCE)) {
			expression.checkDiscouragedFeatureCall
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
						MessageFormat::format(
								Messages::SARLValidator_3,
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
				error(MessageFormat::format(
						Messages::SARLValidator_4,
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
				error(Messages::SARLValidator_5,
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
				name = feature.name
				actionID = this.sarlSignatureProvider.createFunctionID(container, name)
				signatureID = this.sarlSignatureProvider.createSignatureIDFromSarlModel(feature.varargs, feature.params)
				errorFeature = feature
				errorStructFeature = SarlPackage.Literals::ACTION__NAME
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
								MessageFormat::format(
										Messages::SARLValidator_6,
										Messages::SARLValidator_7,
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
									MessageFormat::format(
											Messages::SARLValidator_6,
											Messages::SARLValidator_8,
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
			var validName1 = ModelUtil::fixHiddenAction(action.name)
			var validName2 = ModelUtil::removeHiddenAction(action.name)
			error(
					MessageFormat::format(
							Messages::SARLValidator_9,
							action.name
							), 
					action,
					SarlPackage.Literals::ACTION_SIGNATURE__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					IssueCodes::INVALID_MEMBER_NAME,
					Messages::SARLValidator_8, action.name, validName1, validName2)
		}
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkActionName(Action action) {
		if (isHiddenAction(action.name)) {
			var validName1 = ModelUtil::fixHiddenAction(action.name)
			var validName2 = ModelUtil::removeHiddenAction(action.name)
			error(
					MessageFormat::format(
							Messages::SARLValidator_9,
							action.name
							), 
					action,
					SarlPackage.Literals::ACTION__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					IssueCodes::INVALID_MEMBER_NAME,
					Messages::SARLValidator_8, action.name, validName1, validName2)
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
					MessageFormat::format(
							Messages::SARLValidator_10,
							attribute.name
							), 
					attribute,
					SarlPackage.Literals::ATTRIBUTE__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					IssueCodes::INVALID_MEMBER_NAME,
					Messages::SARLValidator_11, attribute.name, validName)
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
	public def checkFinalFieldInitialization(io.sarl.lang.sarl.Event event) {
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
				MessageFormat::format(
						Messages::SARLValidator_12,
						field.simpleName
						), 
				sarlElement,
				SarlPackage.Literals.ATTRIBUTE__NAME,
				ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
				org.eclipse.xtext.xbase.validation.IssueCodes::MISSING_INITIALIZATION)
	}
	
	protected override reportUninitializedField(JvmField field, JvmConstructor constructor) {
		error(
				MessageFormat::format(
						Messages::SARLValidator_38,
						field.simpleName,
						constructor.toString), 
				constructor,
				null,
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
						MessageFormat::format(
								Messages::SARLValidator_13,
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
						MessageFormat::format(
								Messages::SARLValidator_13,
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
									MessageFormat::format(
											Messages::SARLValidator_14,
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
										MessageFormat::format(
												Messages::SARLValidator_15,
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
				} else if (feature instanceof Action) {
					checkInheritedActionElement(
							finalOperations,
							overridableOperations,
							operationsToImplement,
							feature,
							feature.name,
							feature,
							feature.type,
							SarlPackage.Literals::ACTION__NAME,
							SarlPackage.Literals::ACTION__TYPE)
				} else if (feature instanceof ActionSignature) {
					checkInheritedActionElement(
							finalOperations,
							overridableOperations,
							operationsToImplement,
							feature,
							feature.name,
							feature,
							feature.type,
							SarlPackage.Literals::ACTION_SIGNATURE__NAME,
							SarlPackage.Literals::ACTION_SIGNATURE__TYPE)
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
									MessageFormat::format(
											Messages::SARLValidator_18,
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
									MessageFormat::format(
											Messages::SARLValidator_18,
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
	
	private def checkInheritedActionElement(
					Map<ActionKey, JvmOperation> finalOperations,
					Map<ActionKey, JvmOperation> overridableOperations,
					Map<ActionKey, JvmOperation> operationsToImplement,
					EObject referenceObject,
					String name,
					ParameterizedFeature paramOwner,
					JvmTypeReference type,
					EAttribute nameAttribute,
					EReference typeAttribute) {
		var sig = this.sarlSignatureProvider.createSignatureIDFromSarlModel(paramOwner.varargs, paramOwner.params)
		var actionKey = this.sarlSignatureProvider.createActionID(name, sig)
		if (finalOperations.containsKey(actionKey)) {
			error(
					MessageFormat::format(
							Messages::SARLValidator_16,
							actionKey.toString),
					referenceObject,
					nameAttribute,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					IssueCodes::OVERRIDDEN_FINAL_OPERATION,
					actionKey.toString)
		}
		else {
			var implementableFunction = operationsToImplement.remove(actionKey)
			if (implementableFunction!==null) {
				var currentReturnType = type?.toLightweightTypeReference
				var inheritedReturnType = implementableFunction.returnType?.toLightweightTypeReference
				if (!canCast(currentReturnType, inheritedReturnType, false, true, true)) {
					error(
							MessageFormat::format(
									Messages::SARLValidator_17,
									currentReturnType.canonicalTypeName,
									inheritedReturnType.canonicalTypeName,
									actionKey.toString),
							referenceObject,
							typeAttribute,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
							currentReturnType.canonicalTypeName,
							inheritedReturnType.identifier)
				}
			}
			else {
				var superOperation = overridableOperations.get(actionKey)
				if (superOperation!==null) {
					var currentReturnType = type?.toLightweightTypeReference
					var inheritedReturnType = superOperation.returnType?.toLightweightTypeReference
					if (!canCast(currentReturnType, inheritedReturnType, false, true, true)) {
						error(
								MessageFormat::format(
										Messages::SARLValidator_17,
										currentReturnType.canonicalTypeName,
										inheritedReturnType.canonicalTypeName,
										actionKey.toString),
								referenceObject,
								typeAttribute,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE,
								currentReturnType.canonicalTypeName,
								inheritedReturnType.identifier)
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
								Messages::SARLValidator_19,
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
								MessageFormat::format(
									Messages::SARLValidator_20,
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
	public def checkImplicitConstructorCall(io.sarl.lang.sarl.Event event) {
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
						addIssue(Messages::SARLValidator_21,
								guard,
								null,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								IssueCodes::DISCOURAGED_BOOLEAN_EXPRESSION)
					}
				}
				else {
					if (!IssueCodes::UNREACHABLE_BEHAVIOR_UNIT.ignored) {
						addIssue(Messages::SARLValidator_22,
								behaviorUnit,
								null,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								IssueCodes::UNREACHABLE_BEHAVIOR_UNIT,
								behaviorUnit.name.simpleName)
					}
				}
				return;
			}

			var fromType = guard.actualType
			if (!fromType.isAssignableFrom(Boolean::TYPE)) {
				error(MessageFormat::format(Messages::SARLValidator_23,
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
						MessageFormat::format(
								Messages::SARLValidator_24,
								usedType.qualifiedName,
								Messages::SARLValidator_25,
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
						MessageFormat::format(
								Messages::SARLValidator_24,
								requiredType.qualifiedName,
								Messages::SARLValidator_25,
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
			if (ref!==null && !ref.isSubtypeOf(typeof(Event))) {
				error(
						MessageFormat::format(
								Messages::SARLValidator_24,
								event.qualifiedName,
								Messages::SARLValidator_26,
								SARLKeywords::FIRES),
						event,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						IssueCodes::INVALID_FIRING_EVENT_TYPE,
						event.simpleName)
			}
		}
	}
	
	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkActionFires(Action action) {
		for(event : action.firedEvents) {
			var ref = event.toLightweightTypeReference
			if (ref!==null && !ref.isSubtypeOf(typeof(Event))) {
				error(
						MessageFormat::format(
								Messages::SARLValidator_24,
								event.qualifiedName,
								Messages::SARLValidator_26,
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
			var inferredSuperTypes = Lists::newLinkedList
			inferredSuperTypes.addAll(inferredType.superTypes)
			var isExpectingInterface = expectedType.interface
			var superTypeIndex = 0
			for(superType : element.superTypes) {
				var success = true
				var jvmSuperType = superType?.type
				if (jvmSuperType !== null) {
					val inferredSuperType = if (inferredSuperTypes.empty) null else inferredSuperTypes.removeFirst
					var lighweightSuperType = superType.toLightweightTypeReference
					if (!(jvmSuperType instanceof JvmGenericType)
						|| (isExpectingInterface !== (jvmSuperType as JvmGenericType).interface)) {
						if (isExpectingInterface) {
							error(
								MessageFormat::format(Messages::SARLValidator_27, Messages::SARLValidator_28),
								SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
								superTypeIndex,
								IssueCodes::INVALID_EXTENDED_TYPE,
								inferredType.identifier,
								jvmSuperType.identifier)
						} else {
							error(
								MessageFormat::format(Messages::SARLValidator_27, Messages::SARLValidator_29),
								SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
								superTypeIndex,
								IssueCodes::INVALID_EXTENDED_TYPE,
								inferredType.identifier,
								jvmSuperType.identifier)
						}
						success = false
					} else if (lighweightSuperType.final) {
						error(Messages::SARLValidator_30,
							SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
							superTypeIndex,
							IssueCodes::OVERRIDDEN_FINAL_TYPE,
							inferredType.identifier,
							jvmSuperType.identifier)
						success = false
					} else if (!lighweightSuperType.isSubtypeOf(expectedType)
						|| ((onlySubTypes) && (lighweightSuperType.isType(expectedType)))) {
						if (onlySubTypes) {
							error(MessageFormat::format(Messages::SARLValidator_31, expectedType.name),
								SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
								superTypeIndex,
								IssueCodes::INVALID_EXTENDED_TYPE,
								inferredType.identifier,
								jvmSuperType.identifier)
						} else {
							error(MessageFormat::format(Messages::SARLValidator_32, expectedType.name),
								SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
								superTypeIndex,
								IssueCodes::INVALID_EXTENDED_TYPE,
								inferredType.identifier,
								jvmSuperType.identifier)
						}
						success = false
					} else if (inferredSuperType == null
							|| inferredSuperType.identifier != jvmSuperType.identifier
							|| inferredType.identifier == jvmSuperType.identifier) {
						error(MessageFormat::format(Messages::SARLValidator_33,
								inferredType.qualifiedName),
								SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
								superTypeIndex,
								IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
								inferredType.identifier,
								jvmSuperType.identifier)
						success = false
					}
				} else {
					error(MessageFormat::format(Messages::SARLValidator_33,
							inferredType.qualifiedName),
							SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
							superTypeIndex,
							IssueCodes::INCONSISTENT_TYPE_HIERARCHY,
							inferredType.identifier)
					success = false
				}
				superTypeIndex++
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
					msg = Messages::SARLValidator_34
				}
				else {
					msg = Messages::SARLValidator_35
				}
				error(
						MessageFormat::format(
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
						MessageFormat::format(
								Messages::SARLValidator_36,
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
	public def checkEventSuperType(io.sarl.lang.sarl.Event event) {
		checkSuperTypes(event, typeof(Event), false)
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
		var event = behaviorUnit.name
		var ref = event.toLightweightTypeReference
		if (ref===null || ref.interfaceType || !ref.isSubtypeOf(typeof(Event))) {
			error(
					MessageFormat::format(
							Messages::SARLValidator_24,
							event.qualifiedName,
							Messages::SARLValidator_26,
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
				addIssue(Messages::SARLValidator_37,
						capacity,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						IssueCodes::DISCOURAGED_CAPACITY_DEFINITION,
						capacity.name,
						"aFunction")
			}
		}
	}
	
	/**
	 * @param container
	 */
	@Check(CheckType.NORMAL)
	public def checkUnusedCapacities(CapacityUses element) {
		if(!isIgnored(IssueCodes::UNUSED_AGENT_CAPACITY)) {
			var jvmContainer = this.logicalContainerProvider.getNearestLogicalContainer(element)
			var container = this.jvmModelAssociator.getPrimarySourceElement(jvmContainer)

			val annotationId = typeof(ImportedCapacityFeature).name
			var importedFeatures = Multimaps::<String, JvmOperation>newMultimap(newHashMap) [ newArrayList ]
			for (method : jvmContainer.eContents.filter [
				if (it instanceof JvmOperation) {
					if (it.annotations.findFirst [it.annotation.identifier == annotationId] !== null) {
						return true
					}
				}
				return false
			]) {
				var m = method as JvmOperation
				var annotationValues = m.annotations.findFirst[it.annotation.identifier == annotationId].values
				var annotationType = ((annotationValues.get(0)) as JvmTypeAnnotationValue)
				var capacityType = annotationType.values.get(0)
				importedFeatures.put(capacityType.identifier, m)
			}

			var index = 0
			for (capacity : element.capacitiesUsed) {
				var operations = importedFeatures.get(capacity.identifier)
				if (!operations.empty) {
					var iterator = operations.iterator
					var notFound = true
					while (notFound && iterator.hasNext) {
						var operation = iterator.next
						if (operation.isLocallyUsed(container)) {
							notFound = false
						}
					}
					if (notFound) {
						addIssue(
							MessageFormat::format(
								Messages::SARLValidator_42,
								capacity.simpleName),
							element,
							SarlPackage.Literals.CAPACITY_USES__CAPACITIES_USED,
							index,
							IssueCodes::UNUSED_AGENT_CAPACITY,
							capacity.simpleName)
					}
				}
				index++
			}
		}
	}

}

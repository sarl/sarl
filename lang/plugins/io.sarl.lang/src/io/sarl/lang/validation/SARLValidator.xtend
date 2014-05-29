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
import io.sarl.lang.sarl.SarlScript
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
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.validation.CheckType
import org.eclipse.xtext.xbase.XBooleanLiteral
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
	private ILogicalContainerProvider logicalContainerProvider;

	@Inject
	private ActionSignatureProvider sarlSignatureProvider;

	private static def int compareVersions(String v1, String v2) {
		val t1 = v1.split("\\s*[.-]\\s*")
		val t2 = v2.split("\\s*[.-]\\s*")
		var String s1
		var String s2
		var int n1
		var int n2
		var int cmp
		for(var i=0; i<t1.length || i<t2.length; i++) {
			s1 = t1.get(i)
			s2 = t2.get(i)
			try {
				if (i<t1.length) {
					n1 = Integer.parseInt(s1)
				}
				else {
					n1 = 0
				}
				if (i<t2.length) {
					n2 = Integer.parseInt(s2)
				}
				else {
					n2 = 0
				}
				cmp = Integer.compare(n1, n2)
			}
			catch(Exception e) {
				// Treat the string elements
				cmp = s1.compareTo(s2)
			}
			if (cmp!=0) return cmp
		}
		return 0
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
	public def checkDuplicateTypes(SarlScript script) {
		var QualifiedName packageName
		if (script.name!==null && !script.name.empty) {
			packageName = QualifiedName::create(script.name.split("\\."))
		}
		else {
			packageName = QualifiedName.create()			
		}
		var names = new TreeSet<QualifiedName>
		for(feature : script.elements) {
			if (feature instanceof NamedElement) {
				val QualifiedName featureName = packageName.append(feature.name)
				// Check in the local file
				if (names.contains(featureName)) {
					error(
							String.format(
									"Duplicate definition of the type '%s' in the file '%s'",
									featureName.toString,
									script.eResource.URI), 
							feature,
							null,
							IssueCodes.DUPLICATE_TYPE_NAME)
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
						lastParam,
						null,
						IssueCodes.INVALID_USE_OF_VAR_ARG)
			}
		}
	}

	private def checkDefaultValueTypeCompatibleWithParameterType(FormalParameter param) {
		var toType = toLightweightTypeReference(param.parameterType, true)
		var fromType = param.defaultValue.actualType
		if (!canCast(fromType, toType, true)) {
			error(String.format("Type mismatch: cannot convert from %s to %s",
					fromType.nameOfTypes, toType.canonicalName),
					param,
					null,
					org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_TYPES)
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
										"Duplicate field in '%s': %s",
										featureContainer.name,
										feature.name
										), 
								feature,
								null,
								IssueCodes::DUPLICATE_FIELD)
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
											"Duplicate action in '%s': %s",
											featureContainer.name,
											name+"("+sig.toString()+")"
											), 
									feature,
									null,
									IssueCodes::DUPLICATE_METHOD)
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
		if (isHiddenAction(action.getName())) {
			error(
					String.format(
							"Invalid action name '%s'. You must not give to an action a name that is starting with '_handle_'. This prefix is reserved by the SARL compiler.",
							action.name
							), 
					action,
					null,
					IssueCodes::INVALID_MEMBER_NAME)
		}
	}

	/**
	 * @param attribute
	 */
	@Check(CheckType.FAST)
	public def checkAttributeName(Attribute attribute) {
		if (isHiddenAttribute(attribute.getName())) {
			error(
					String.format(
							"Invalid attribute name '%s'. You must not give to an attribute a name that is starting with '___FORMAL_PARAMETER_DEFAULT_VALUE_'. This prefix is reserved by the SARL compiler.",
							attribute.name
							), 
					attribute,
					null,
					IssueCodes::INVALID_MEMBER_NAME)
		}
	}

	/** Replies the JVM generic type for the given element.
	 * 
	 * @param element
	 * @return the generic type of the given element.
	 */
	protected def JvmGenericType getJvmGenericType(EObject element) {
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
		error(
				String.format(
						"The blank final field '%s' may not have been initialized.",
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
				addIssue(
					String.format(
							"The feature '%s' is already implemented by the super type '%s'.",
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
				addIssue(
						String.format(
								"The feature '%s' is already implemented by the preceding interface '%s'.",
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
	@Check(CheckType.FAST)
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

			if (!org.eclipse.xtext.xbase.validation.IssueCodes::VARIABLE_NAME_SHADOWING.ignored) {
				for(feature : jvmElement.declaredFields) {
					if (!isHiddenAttribute(feature.simpleName)) {
						var inheritedField = inheritedFields.get(feature.simpleName)
						if (inheritedField!==null) {
							warning(
									String.format(
											"The field '%s' in '%s' is hidding the inherited field '%s'.",
											feature.simpleName, jvmElement.qualifiedName,
											inheritedField.qualifiedName),
									feature,
									null,
									org.eclipse.xtext.xbase.validation.IssueCodes::VARIABLE_NAME_SHADOWING)
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
									"Cannot override the operation %s, which is declared a final in the super type.",
									actionKey.toString),
							feature,
							null,
							IssueCodes::OVERRIDDEN_FINAL)
				}
				else {
					if (implementedFunction!==null) {
						var currentReturnType = feature.returnType.toLightweightTypeReference
						var inheritedReturnType = implementedFunction.returnType.toLightweightTypeReference
						if (!canCast(currentReturnType, inheritedReturnType, false)) {
							error(
									String.format(
											"Incompatible return type between '%s' and '%s' for %s.",
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
												"Incompatible return type between '%s' and '%s' for %s.",
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
									"The operation %s must be implemented.",
									key.toString),
							element,
							null,
							IssueCodes::MISSING_METHOD_IMPLEMENTATION)
				}
			}
		}
	}

	/**
	 * @param element
	 */
	@Check(CheckType.FAST)
	public def checkNoFinalTypeExtension(InheritingElement element) {
		var jvmElement = element.jvmGenericType
		if (jvmElement!==null) {
			for(superType : jvmElement.superTypes) {
				var ref = superType.toLightweightTypeReference
				if (ref!==null && ref.final) {
					error(
							String.format(
									"Cannot extend the final type '%s'.",
									superType.qualifiedName),
							element,
							null,
							IssueCodes::OVERRIDDEN_FINAL)
				}
			}
		}
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
					if (!isIgnored(IssueCodes::DISCOURAGED_BOOLEAN_EXPRESSION)) {
						addIssue("Discouraged boolean value. The guard is always true.",
								guard,
								null,
								IssueCodes::DISCOURAGED_BOOLEAN_EXPRESSION)
					}
				}
				else {
					if (!isIgnored(org.eclipse.xtext.xbase.validation.IssueCodes::UNREACHABLE_CODE)) {
						addIssue("Dead code. The guard is always false.",
								behaviorUnit,
								null,
								org.eclipse.xtext.xbase.validation.IssueCodes::UNREACHABLE_CODE)
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
			if (ref!==null && !ref.isSubtypeOf(Capacity)) {
				error(
						String.format(
								"Invalid type: '%s'. Only capacities can be used after the keyword '%s'.",
								usedType.qualifiedName,
								SARLKeywords::USES),
						uses,
						null,
						org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH)
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
			if (ref!==null && !ref.isSubtypeOf(Capacity)) {
				error(
						String.format(
								"Invalid type: '%s'. Only capacities can be used after the keyword '%s'.",
								requiredType.qualifiedName,
								SARLKeywords::REQUIRES),
						requires,
						null,
						org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH)
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
			if (ref!==null && !ref.isSubtypeOf(Event)) {
				error(
						String.format(
								"Invalid type: '%s'. Only events can be used after the keyword '%s'.",
								event.qualifiedName,
								SARLKeywords::FIRES),
						action,
						null,
						org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH)
			}
		}
	}
	
	protected def checkSuperTypes(InheritingElement element, Class<?> expectedType, boolean onlySubTypes) {
		var isInterface = expectedType.interface
		for(superType : element.superTypes) {
			var ref = superType.toLightweightTypeReference
			if (ref!==null &&
				((ref.interfaceType!==isInterface) || !ref.isSubtypeOf(expectedType)
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
						element,
						null,
						org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH)
			}
		}
	}


	protected def checkImplementedTypes(ImplementingElement element, Class<?> expectedType, int mandatoryNumberOfTypes, boolean onlySubTypes) {
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
						element,
						null,
						org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH)
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
						null,
						org.eclipse.xtext.xbase.validation.IssueCodes::MISSING_TYPE)
		}
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkEventSuperType(io.sarl.lang.sarl.Event event) {
		checkSuperTypes(event, Event, false)
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkBehaviorSuperType(Behavior behavior) {
		checkSuperTypes(behavior, io.sarl.lang.core.Behavior, false)
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkAgentSuperType(Agent agent) {
		checkSuperTypes(agent, io.sarl.lang.core.Agent, false)
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkCapacitySuperType(io.sarl.lang.sarl.Capacity capacity) {
		checkSuperTypes(capacity, Capacity, false)
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkSkillSuperType(Skill skill) {
		checkSuperTypes(skill, io.sarl.lang.core.Skill, false)
		checkImplementedTypes(skill, Capacity, 1, true)
	}

	/**
	 * @param action
	 */
	@Check(CheckType.FAST)
	public def checkBehaviorUnitEventType(BehaviorUnit behaviorUnit) {
		var event = behaviorUnit.event
		var error = true
		if (event!==null) {
			var ref = event.toLightweightTypeReference
			if (ref!==null && !ref.interfaceType 
				&& ref.isSubtypeOf(Event)) {
				error = false
			}
		}
		if (error) {
			error(
					String.format(
							"Invalid type: '%s'. Only events are allowed after the keyword '%s'.",
							event.qualifiedName,
							SARLKeywords.ON),
					behaviorUnit,
					null,
					org.eclipse.xtext.xbase.validation.IssueCodes::TYPE_BOUNDS_MISMATCH)
		}
	}

}

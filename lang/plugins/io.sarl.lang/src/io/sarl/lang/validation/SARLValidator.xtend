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
import io.sarl.lang.sarl.Capacity
import io.sarl.lang.sarl.Constructor
import io.sarl.lang.sarl.FeatureContainer
import io.sarl.lang.sarl.FormalParameter
import io.sarl.lang.sarl.InheritingElement
import io.sarl.lang.sarl.ParameterizedFeature
import io.sarl.lang.sarl.Skill
import io.sarl.lang.signature.ActionKey
import io.sarl.lang.signature.ActionNameKey
import io.sarl.lang.signature.ActionSignatureProvider
import io.sarl.lang.signature.InferredActionSignature
import io.sarl.lang.signature.SignatureKey
import java.util.LinkedList
import java.util.List
import java.util.Map
import java.util.Set
import java.util.TreeMap
import java.util.TreeSet
import org.eclipse.emf.common.util.EList
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmIdentifiableElement
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.util.TypeReferences
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.eclipse.xtext.xbase.XBooleanLiteral
import org.eclipse.xtext.xbase.XExpression
import org.eclipse.xtext.xbase.XNullLiteral
import org.eclipse.xtext.xbase.XNumberLiteral
import org.eclipse.xtext.xbase.XStringLiteral
import org.eclipse.xtext.xbase.XTypeLiteral
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider
import org.eclipse.xtext.xbase.typesystem.computation.NumberLiterals
import org.eclipse.xtext.xbase.typesystem.legacy.StandardTypeReferenceOwner
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference
import org.eclipse.xtext.xbase.typesystem.references.OwnedConverter
import org.eclipse.xtext.xbase.validation.IssueCodes

/**
 * Custom validation rules. 
 *
 * see http://www.eclipse.org/Xtext/documentation.html#validation
 */
class SARLValidator extends AbstractSARLValidator {

	@Inject
	private ILogicalContainerProvider logicalContainerProvider;

	@Inject
	private var TypeReferences typeReferences
	
	@Inject
	private NumberLiterals numberLiterals
	
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
		
	private def boolean isInterface(LightweightTypeReference typeRef) {
		return typeRef.type.interface
	}
	
	private def boolean isClass(LightweightTypeReference typeRef) {
		var t = typeRef.type
		if (t instanceof JvmGenericType) {
			return !t.interface
		}
		return false
	}	
	
	private def LightweightTypeReference actualType(XExpression expr, EObject context, LightweightTypeReference targetType) {
		var type = expr.actualType
		if (type!==null) return type
		if (expr instanceof XNullLiteral) {
		}
		var JvmTypeReference jvmType = null
		if (expr instanceof XBooleanLiteral) {
			jvmType = typeReferences.getTypeForName(Boolean::TYPE, context);
		}
		else if (expr instanceof XStringLiteral) {
			if ((targetType.isType(Character::TYPE) || targetType.isType(Character))
				&& (expr.value!=null && expr.value.length<=1)) {
				jvmType = typeReferences.getTypeForName(Character::TYPE, context);
			}
			else {
				jvmType = typeReferences.getTypeForName(String, context);
			}
		}
		else if (expr instanceof XNumberLiteral) {
			var jType = numberLiterals.getJavaType(expr)
			jvmType = typeReferences.getTypeForName(jType, expr);
			
		}
		else if (expr instanceof XTypeLiteral) {
			jvmType = typeReferences.createTypeRef(expr.type)
		}
		if (jvmType!==null) {
			val OwnedConverter converter = new OwnedConverter(
				new StandardTypeReferenceOwner(getServices(), context),
				true
			)
			return converter.toLightweightReference(jvmType)
		}
		return null;
	}
	
	private def checkDefaultValueTypeCompatibleWithParameterType(FormalParameter param) {
		var toType = toLightweightTypeReference(param.parameterType, true)
		var fromType = param.defaultValue.actualType(param, toType)
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
		
		if ((fromType.getType() instanceof JvmDeclaredType || fromType.isPrimitive())
			&&
			(!isInterface(fromType) || isFinal(toType)) // if one of the types is an interface and the other is a non final class (or interface) there always can be a subtype
			&&
			(!isInterface(toType) || isFinal(fromType))
			&&
			(!toType.isAssignableFrom(fromType))
			&&
			(   isFinal(fromType) || isFinal(toType)
				|| isClass(fromType) && isClass(toType))
			&&
			(!fromType.isAssignableFrom(toType)) // no upcast
		) { 
			error(String.format("Cannot cast from %s to %s",
					getNameOfTypes(fromType), canonicalName(toType)),
				param,
				null,
				ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
				IssueCodes::INVALID_CAST);
		}
		else if(toType.isPrimitive() && !(fromType.isPrimitive() || fromType.isWrapper())) {
				error(String.format("Cannot cast from %s to %s",
					getNameOfTypes(fromType), canonicalName(toType)
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
	def checkNoActionCollision(FeatureContainer featureContainer) {
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
				signatureID = sarlSignatureProvider.createSignatureID(s.params)
			}
			else if (feature instanceof ActionSignature) {
				name = feature.name
				actionID = sarlSignatureProvider.createFunctionID(container, name)
				signatureID = sarlSignatureProvider.createSignatureID(feature.params)
			}
			else if (feature instanceof Constructor) {
				name = SARLKeywords.CONSTRUCTOR
				actionID = sarlSignatureProvider.createConstructorID(container)
				signatureID = sarlSignatureProvider.createSignatureID(feature.params)
			}
			else {
				actionID = null
				signatureID = null
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
								io.sarl.lang.validation.IssueCodes::ACTION_COLLISION)
							return;
						}
					}
				}
			}
		}
	}
	
	/*private def Map<ActionNameKey,EList<InferredActionSignature>> getCapacityActionsFromHierarchy(EList<InheritingElement> sources) {
		var Map<ActionNameKey,EList<InferredActionSignature>> actions = new TreeMap
		var Set<String> encounteredCapacities = new TreeSet
		var List<Capacity> capacities = new LinkedList
		for(p : sources) {
			if (p instanceof Capacity) {
				capacities.add(p)
			}
		}
		while (!capacities.empty) {
			var cap = capacities.remove(0);
			if (encounteredCapacities.add("")) {
				for(p : cap.superTypes) {
					if (p instanceof Capacity) {
						capacities.add(p)
					}
				}
				var JvmIdentifiableElement container = null
				for(feature : cap.features) {
					if (feature instanceof ActionSignature) {
						if (container===null) {
							container = logicalContainerProvider.getNearestLogicalContainer(feature)
						}
						var ank = sarlSignatureProvider.createFunctionID(container, feature.name)
						var sk = sarlSignatureProvider.createSignatureID(feature.params)
						var is = sarlSignatureProvider.getSignatures(ank, sk)
						actions.add(sk.toActionKey(feature.name))
					}
				}
			}
		}
		return actions
	}*/
	
	@Check
	def checkSkillActionImplementationPrototype(Skill skill) {
		/*var Set<ActionKey> actions = getCapacityActionsFromHierarchy(skill.implementedTypes)
		var JvmIdentifiableElement container = null
		for(feature : skill.features) {
			if (feature instanceof Action) {
				if (container===null) {
					container = logicalContainerProvider.getNearestLogicalContainer(feature)
				}
				var signature = feature.signature as ActionSignature
				var sk = sarlSignatureProvider.createSignatureID(signature.params)
				
			}
		}*/
	}
	
}

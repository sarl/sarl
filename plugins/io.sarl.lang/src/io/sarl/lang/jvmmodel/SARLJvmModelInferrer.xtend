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
package io.sarl.lang.jvmmodel

import com.google.inject.Inject
import io.sarl.lang.SARLKeywords
import io.sarl.lang.annotation.DefaultValue
import io.sarl.lang.annotation.DefaultValueSource
import io.sarl.lang.annotation.DefaultValueUse
import io.sarl.lang.annotation.Generated
import io.sarl.lang.core.Address
import io.sarl.lang.core.Percept
import io.sarl.lang.sarl.Action
import io.sarl.lang.sarl.ActionSignature
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.Attribute
import io.sarl.lang.sarl.Behavior
import io.sarl.lang.sarl.BehaviorUnit
import io.sarl.lang.sarl.Capacity
import io.sarl.lang.sarl.CapacityUses
import io.sarl.lang.sarl.Constructor
import io.sarl.lang.sarl.Event
import io.sarl.lang.sarl.FormalParameter
import io.sarl.lang.sarl.ImplementingElement
import io.sarl.lang.sarl.InheritingElement
import io.sarl.lang.sarl.RequiredCapacity
import io.sarl.lang.sarl.Skill
import io.sarl.lang.sarl.TopElement
import io.sarl.lang.signature.ActionKey
import io.sarl.lang.signature.ActionSignatureProvider
import io.sarl.lang.signature.InferredStandardParameter
import io.sarl.lang.signature.InferredValuedParameter
import io.sarl.lang.signature.SignatureKey
import java.text.MessageFormat
import java.util.List
import java.util.Map
import java.util.UUID
import java.util.logging.Logger
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.JvmConstructor
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmExecutable
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmFormalParameter
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.JvmVisibility
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.xbase.XBooleanLiteral
import org.eclipse.xtext.xbase.XExpression
import org.eclipse.xtext.xbase.compiler.XbaseCompiler
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeExtensions
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices
import org.eclipse.xtext.xbase.validation.ReadAndWriteTracking

import static io.sarl.lang.util.ModelUtil.*

/**
 * <p>Infers a JVM model from the source model.</p> 
 *
 * <p>The JVM model should contain all elements that would appear in the Java code 
 * which is generated from the source model. Other models link against 
 * the JVM model rather than the source model.</p>
 * 
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class SARLJvmModelInferrer extends AbstractModelInferrer {

	@Inject extension JvmTypesBuilder

	@Inject extension IQualifiedNameProvider
	
	@Inject private XbaseCompiler xbaseCompiler

	@Inject private JvmModelAssociator jvmModelAssociator

	@Inject private Logger log

	@Inject private ActionSignatureProvider sarlSignatureProvider

	@Inject	private ReadAndWriteTracking readAndWriteTracking

	@Inject	private CommonTypeComputationServices services
	
	@Inject private JvmTypeExtensions typeExtensions;

	/**
	 * The dispatch method {@code infer} is called for each instance of the
	 * given element's type that is contained in a resource.
	 * 
	 * @param event
	 *            the model to create one or more
	 *            {@link JvmDeclaredType declared
	 *            types} from.
	 * @param acceptor
	 *            each created
	 *            {@code type}
	 *            without a container should be passed to the acceptor in order
	 *            get attached to the current resource. The acceptor's
	 *            {@link IJvmDeclaredTypeAcceptor#accept(org.eclipse.xtext.common.types.JvmDeclaredType)
	 *            accept(..)} method takes the constructed empty type for the
	 *            pre-indexing phase.
	 * @param isPreIndexingPhase
	 *            whether the method is called in a pre-indexing phase, i.e.
	 *            when the global index is not yet fully updated. You must not
	 *            rely on linking using the index if isPreIndexingPhase is
	 *            <code>true</code>.
	 */
	def dispatch void infer(Event event, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		var qn = event.fullyQualifiedName
		if (qn===null) {
			return
		}
		acceptor.accept(event.toClass(qn))
			[
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				event.copyDocumentationTo(it)
				
				var long serial = 1L
				serial = serial + generateExtendedTypes(it, event, typeof(io.sarl.lang.core.Event))
				var JvmField jvmField
				var jvmFields = newArrayList
				var actionIndex = 0
				var hasConstructor = false

				for (feature : event.features) {
					if (feature!==null) {
						switch feature {
							Attribute: {
								jvmField = generateAttribute(feature, JvmVisibility::PUBLIC)
								if (jvmField!==null) {
									jvmFields.add(jvmField)
									members += jvmField
									serial = serial + feature.name.hashCode
								}
							}
							Constructor: {
								if (generateConstructor(event, feature, actionIndex) !== null) {
									serial = serial + event.fullyQualifiedName.hashCode
									actionIndex++
									hasConstructor = true
								}
							}
						}
					}
				}
				
				if (!hasConstructor) {
					var op = event.toConstructor [
						documentation = Messages::SARLJvmModelInferrer_0
						body = '''
							super();
						'''
					]
					op.annotations += annotationRef(typeof(Generated))
					typeExtensions.setSynthetic(op, true);
					members += op
					//
					val addrType = typeRef(typeof(Address))
					op = event.toConstructor [
						documentation = MessageFormat::format(Messages::SARLJvmModelInferrer_1, 'source')
						parameters += toParameter('source', addrType)
						body = '''
							super(source);
						'''
					]
					op.annotations += annotationRef(typeof(Generated))
					typeExtensions.setSynthetic(op, true);
					members += op
				}
								
				if (!jvmFields.isEmpty) {

					val JvmField[] tab = jvmFields // single translation to the array
 					var elementType = event.toClass(event.fullyQualifiedName)
 					
					var op = toEqualsMethod(event, elementType, tab)
 					if (op!==null) {
						op.annotations += annotationRef(typeof(Generated))
						typeExtensions.setSynthetic(op, true);
						members += op
					}
					
					op = toHashCodeMethod(event, tab)
					if (op!==null) {
						op.annotations += annotationRef(typeof(Generated))
						typeExtensions.setSynthetic(op, true);
						members += op
					}
					
					op = event.toMethod("attributesToString", typeRef(typeof(String)))[
						visibility = JvmVisibility::PROTECTED
						documentation = MessageFormat::format(Messages::SARLJvmModelInferrer_2, event.name)
						body = [
							append(
								'''
								StringBuilder result = new StringBuilder(super.attributesToString());
								«FOR attr : event.features.filter(typeof(Attribute))»
									result.append("«attr.name»  = ").append(this.«attr.name»);
								«ENDFOR»
								return result.toString();''')
						]
					]
					if (op!==null) {
						op.annotations += annotationRef(typeof(Generated))
						typeExtensions.setSynthetic(op, true);
						members += op
					}
				}

				val serialValue = serial
				val serialField = event.toField("serialVersionUID", typeRef(typeof(long))) [
					visibility = JvmVisibility::PRIVATE
					final = true
					static = true
					initializer = [append(serialValue+"L")]
				]
				serialField.annotations += annotationRef(typeof(Generated))
				typeExtensions.setSynthetic(serialField, true);
				members += serialField
				readAndWriteTracking.markInitialized(serialField, null)

			]
	}

	def dispatch void infer(Capacity capacity, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		var qn = capacity.fullyQualifiedName
		if (qn===null) {
			return
		}
		acceptor.accept(capacity.toInterface(qn.toString, null))
			[
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				capacity.copyDocumentationTo(it)
				generateExtendedTypes(it, capacity, typeof(io.sarl.lang.core.Capacity))
				
				var actionIndex = 0
				for (feature : capacity.features) {
					if (feature!==null) {
						if (generateAction(feature as ActionSignature, null, actionIndex) !== null) {
							actionIndex++
						}
					}
				}
			]
	}

	def dispatch void infer(Skill skill, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		var qn = skill.fullyQualifiedName
		if (qn===null) {
			return
		}
		acceptor.accept(skill.toClass(qn))
			[
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				skill.copyDocumentationTo(it)

				generateExtendedTypes(it, skill, typeof(io.sarl.lang.core.Skill))
				generateImplementedTypes(it, skill, typeof(io.sarl.lang.core.Capacity))

				val finalOperations = newTreeMap(null)
				val overridableOperations = newTreeMap(null)
				val operationsToImplement = newTreeMap(null)
				populateInheritanceContext(
						it,
						finalOperations, overridableOperations,
						null, operationsToImplement,
						null, this.sarlSignatureProvider)
				
				var actionIndex = 0
				var hasConstructor = false
				
				for (feature : skill.features) {
					if (feature!==null) {
						switch feature {
							Action: {
								var sig = feature.signature as ActionSignature
								if (it.generateAction(
									sig,
									feature.body,
									actionIndex, false,
									operationsToImplement,
									overridableOperations
								) [ return !finalOperations.containsKey(it)
											&& !overridableOperations.containsKey(it)
								] !== null) {
									actionIndex++
								}
							}
							Constructor: {
								if (it.generateConstructor(skill, feature, actionIndex) !== null) {
									actionIndex++
									hasConstructor = true
								}
							}
							Attribute: {
								it.generateAttribute(feature, JvmVisibility::PROTECTED)
							}
							CapacityUses: {
								for (used : feature.capacitiesUsed) {
									actionIndex = generateCapacityDelegatorMethods(skill, used, actionIndex, operationsToImplement, overridableOperations)
								}
							}
						}
					}
				}
				
				actionIndex = generateMissedFunction(skill, actionIndex, operationsToImplement, overridableOperations)
								
				if (!hasConstructor) {
					val aType = typeRef(typeof(io.sarl.lang.core.Agent))
					var op = skill.toConstructor [
						documentation = MessageFormat::format(Messages::SARLJvmModelInferrer_3, 'owner')
						parameters += skill.toParameter('owner', aType)
						body = '''
							super(owner);
						'''
					]
					op.annotations += annotationRef(typeof(Generated))
					it.members += op
					typeExtensions.setSynthetic(op, true)
					op = skill.toConstructor [
						documentation = Messages::SARLJvmModelInferrer_4
						body = '''
							super();
						'''
					]
					op.annotations += annotationRef(typeof(Generated))
					it.members += op
					typeExtensions.setSynthetic(op, true)
				}
			]
	}

	def dispatch void infer(Behavior behavior, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		var qn = behavior.fullyQualifiedName
		if (qn===null) {
			return
		}
		acceptor.accept(behavior.toClass(qn))
			[
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				behavior.copyDocumentationTo(it)
				generateExtendedTypes(it, behavior, typeof(io.sarl.lang.core.Behavior))
				
				var behaviorUnitIndex = 1
				var actionIndex = 1
				var hasConstructor = false
				
				for (feature : behavior.features) {
					if (feature!==null) {
						switch feature {
							RequiredCapacity: {
								//TODO Provide "requires" implementation
							}
							BehaviorUnit: {
								val bMethod = generateBehaviorUnit(feature, behaviorUnitIndex)
								if (bMethod !== null) {
									behaviorUnitIndex++						
									members += bMethod
								}
							}
							Action: {
								if (generateAction(feature.signature as ActionSignature, feature.body, actionIndex) !== null) {
									actionIndex++
								}
							}
							CapacityUses: {
								for (used : feature.capacitiesUsed) {
									actionIndex = generateCapacityDelegatorMethods(behavior, used, actionIndex, null, null)
								}
							}
							Constructor: {
								if (generateConstructor(behavior, feature, actionIndex) !== null) {
									actionIndex++
									hasConstructor = true
								}
							}
							Attribute: {
								generateAttribute(feature, JvmVisibility::PROTECTED)
							}
						}
					}
				}
				
				if (!hasConstructor) {
					val aType = typeRef(typeof(io.sarl.lang.core.Agent))
					var op = behavior.toConstructor [
						documentation = MessageFormat::format(Messages::SARLJvmModelInferrer_5, 'owner')
						parameters += toParameter('owner', aType)
						body = '''
							super(owner);
						'''
					]
					op.annotations += annotationRef(typeof(Generated))
					members +=  op
					typeExtensions.setSynthetic(op, true);
				}
			]
	}

	def dispatch void infer(Agent agent, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		var qn = agent.fullyQualifiedName
		if (qn===null) {
			return
		}
		acceptor.accept(agent.toClass(qn)) [
			// Reset the action registry
			sarlSignatureProvider.resetSignatures(it)
			
			agent.copyDocumentationTo(it)
			generateExtendedTypes(agent, typeof(io.sarl.lang.core.Agent))

			var cons1 = agent.toConstructor [
				documentation = MessageFormat::format(Messages::SARLJvmModelInferrer_6, 'parentID')
				parameters += agent.toParameter('parentID', typeRef(typeof(UUID)))
				body = '''
					super(parentID, null);
				'''
			]
			cons1.annotations += annotationRef(typeof(Generated))
			members += cons1
			typeExtensions.setSynthetic(cons1, true)
			
			var cons2 = agent.toConstructor [
				documentation = MessageFormat::format(
					Messages::SARLJvmModelInferrer_7,
					'parentID', 'agentID')
				parameters += agent.toParameter('parentID', typeRef(typeof(UUID)))
				parameters += agent.toParameter('agentID', typeRef(typeof(UUID)))
				body = '''
					super(parentID, agentID);
				'''
			]
			cons2.annotations += annotationRef(typeof(Generated))
			members += cons2
			typeExtensions.setSynthetic(cons2, true)

			var behaviorUnitIndex = 1
			var actionIndex = 1
			
			for (feature : agent.features) {
				if (feature!==null) {
					switch feature {
						BehaviorUnit: {
							val bMethod = generateBehaviorUnit(feature, behaviorUnitIndex)
							if (bMethod !== null) {
								behaviorUnitIndex++
								members += bMethod
							}
						}
						Action: {
							if (generateAction(feature.signature as ActionSignature, feature.body, actionIndex) !== null) {
								actionIndex++
							}
						}
						Attribute: {
							generateAttribute(feature, JvmVisibility::PROTECTED)
						}
						CapacityUses: {
							for (used : feature.capacitiesUsed) {
								actionIndex = generateCapacityDelegatorMethods(agent, used, actionIndex, null, null)
							}
						}
					}
				}
			}
		]
	}
	
	protected def int generateMissedFunction(
		JvmGenericType output,
		EObject owner,
		int actionIndex,
		Map<ActionKey,JvmOperation> operationsToImplement,
		Map<ActionKey,JvmOperation> overridableOperations) {
			
		var actIndex = actionIndex
		var String currentKeyStr = null
		var JvmOperation originalOperation = null
		var SignatureKey sigKey = null
		
		for(missedOperation : operationsToImplement.entrySet) {
			var originalSignature = annotationString(missedOperation.value, typeof(DefaultValueUse))
			if (originalSignature!==null) {
				if (originalSignature!=currentKeyStr) {
					currentKeyStr = originalSignature
					sigKey = this.sarlSignatureProvider.createSignatureIDFromString(originalSignature)
					var key = this.sarlSignatureProvider.createActionID(missedOperation.key.functionName, sigKey)
					originalOperation = overridableOperations.get(key);
				}
				if (originalOperation!==null) {
					var op = owner.toMethod(originalOperation.simpleName, originalOperation.returnType, null)
					op.varArgs = originalOperation.varArgs
					op.final = true
					var args = newArrayList
					
					var it1 = missedOperation.value.parameters.iterator
					var it2 = originalOperation.parameters.iterator
					var JvmFormalParameter oparam = null
					
					while (it2.hasNext) {
						var param = it2.next
						var vId = annotationString(param, typeof(DefaultValue))
						if (oparam==null && it1.hasNext) {
							oparam = it1.next
						}
						if (oparam!==null && oparam.simpleName==param.simpleName) {
							args += oparam.simpleName
							op.parameters += param.toParameter(oparam.simpleName, oparam.parameterType)
							oparam = null
						}
						else if (vId!==null && !vId.empty) {
							args.add(
								originalOperation.declaringType.qualifiedName
								+".___FORMAL_PARAMETER_DEFAULT_VALUE_"
								+vId)
						}
						else {
							throw new IllegalStateException(Messages::SARLJvmModelInferrer_8)
						}
					}
					
					{
						val tmpName = originalOperation.simpleName
						val tmpArgs = args
						op.body = [
							append(tmpName)
							append("(")
							append(IterableExtensions.join(tmpArgs, ", "))
							append(");")
						] //(new CallingFunctionGenerator(originalOperation.simpleName, args))
					}
					op.annotations += annotationRef(typeof(DefaultValueUse), originalSignature)
					output.members += op				
					actIndex++
				}
			}
		}
		return actIndex
	}

	protected def long generateExtendedTypes(JvmGenericType owner, InheritingElement element, Class<?> defaultType) {
		var serial = 0L
		var isInterface = owner.interface
		for(JvmParameterizedTypeReference superType : element.superTypes) {
			if (superType.type instanceof JvmGenericType) {	
				var reference = toLightweightTypeReference(superType, services)
				if (reference.interfaceType===isInterface && reference.isSubtypeOf(defaultType)) {
					owner.superTypes += cloneWithProxies(superType)
					serial = serial + superType.identifier.hashCode
				}
			}
		}
		if (owner.superTypes.empty) {
			var type = typeRef(defaultType)
			owner.superTypes += type
			serial = serial + type.identifier.hashCode
		}
		return serial
	}
	
	protected def long generateImplementedTypes(JvmGenericType owner, ImplementingElement element, Class<?> mandatoryType) {
		var serial = 0L
		for(JvmParameterizedTypeReference implementedType : element.implementedTypes) {
			if (implementedType.type instanceof JvmGenericType) {	
				var reference = toLightweightTypeReference(implementedType, services)
				if (reference.interfaceType && reference.isSubtypeOf(mandatoryType)) {
					owner.superTypes += cloneWithProxies(implementedType)
					serial = serial + implementedType.identifier.hashCode
				}
			}
		}
		return serial
	}

	protected def JvmField generateAttribute(JvmGenericType owner, Attribute attr, JvmVisibility attrVisibility) {
		var field = attr.toField(attr.name, attr.type) [
			visibility = attrVisibility
			attr.copyDocumentationTo(it)
			final = (!attr.writeable)
			static = false
			initializer = attr.initialValue
		]
		owner.members += field
		if (attr.initialValue!==null) {
			readAndWriteTracking.markInitialized(field, null)
		}
		return field
	}
	
	protected def int generateCapacityDelegatorMethods(
		JvmGenericType owner, InheritingElement context,
		JvmParameterizedTypeReference capacityType, int index,
		Map<ActionKey,JvmOperation> operationsToImplement,
		Map<ActionKey,JvmOperation> implementedOperations) {
		
		if (capacityType.type instanceof JvmGenericType) {	
			var reference = toLightweightTypeReference(capacityType, services)
			if (reference.isSubtypeOf(typeof(io.sarl.lang.core.Capacity))) {
				var actionIndex = index
				val capacityOperations = newTreeMap(null)
				
				populateInterfaceElements(
						capacityType.type as JvmGenericType,
						capacityOperations,
						null, this.sarlSignatureProvider)
		
				for(entry : capacityOperations.entrySet) {
					if (implementedOperations===null || !implementedOperations.containsKey(entry.key)) {
						var op = context.toMethod(entry.value.simpleName, entry.value.returnType) [
							visibility = JvmVisibility::PROTECTED
							val args = newArrayList
							for(param : entry.value.parameters) {
								var sarlElement = services.jvmModelAssociations.getPrimarySourceElement(param)
								parameters += sarlElement.toParameter(param.simpleName, param.parameterType)
								args += param.simpleName
							}
							varArgs = entry.value.varArgs
							body = [
								if (entry.value.returnType.identifier!='void') {
									append("return ")
								}
								append("getSkill(")
								append(entry.value.declaringType.qualifiedName)
								append(".class).")
								append(entry.value.simpleName)
								append("(")
								append(args.join(", "))
								append(");")
							]
						]
						op.annotations += annotationRef(typeof(Generated))
						owner.members += op
						// 
						if (operationsToImplement!==null) operationsToImplement.remove(entry.key)
						if (implementedOperations!==null) implementedOperations.put(entry.key, entry.value)
						actionIndex++
					}
				}
				return actionIndex
			}
		}
		return index
	}

	protected def JvmOperation generateBehaviorUnit(JvmGenericType owner, BehaviorUnit unit, int index) {
		if (unit.event!==null) {
			var isTrueGuard = false
			val guard = unit.guard
			
			if (guard == null) {
				isTrueGuard = true
			}
			else if (guard instanceof XBooleanLiteral) {
				if (guard.isTrue) {
					isTrueGuard = true
				}
				else {
					// The guard is always false => no need to generate the code
					return null
				}
			}

			val voidType = typeRef(Void::TYPE)
			val behName = "_handle_" + unit.event.simpleName + "_" + index
			
			val behaviorMethod = unit.toMethod(behName, voidType) [
				unit.copyDocumentationTo(it)
				annotations += annotationRef(typeof(Percept))
				parameters +=
					unit.event.toParameter(SARLKeywords::OCCURRENCE, unit.event)
			]
						
			if (isTrueGuard) {
				behaviorMethod.body = unit.body
			} else {
				val guardMethodName = behName + "_Guard"
				val guardMethod = guard.toMethod(guardMethodName, typeRef(Boolean::TYPE)) [
					documentation = MessageFormat::format(
						Messages::SARLJvmModelInferrer_9,
						behName, guard.toString)
					parameters += unit.event.toParameter(SARLKeywords::OCCURRENCE, unit.event)
					body = guard
				]
	
				jvmModelAssociator.associateLogicalContainer(unit.body, behaviorMethod)
	
				behaviorMethod.body = [
					it.append('''if ( «guardMethodName»(«SARLKeywords::OCCURRENCE»)) { ''')
					xbaseCompiler.compile(unit.body, it, voidType, null)
					it.append('}')
				]
	
				owner.members += guardMethod
			}
			return behaviorMethod
		}
		log.fine(Messages::SARLJvmModelInferrer_10)
		return null
	}
	
	protected def List<String> generateFormalParametersAndDefaultValueFields(
		JvmExecutable owner, JvmGenericType actionContainer, 
		EObject sourceElement, boolean varargs,
		List<FormalParameter> params, 
		boolean isForInterface,
		int actionIndex) {

		var parameterTypes = newArrayList
		var JvmFormalParameter lastParam = null
		var paramIndex = 0
		var hasDefaultValue = false
		for (param : params) {
			val paramName = param.name
			val paramType = param.parameterType
			
			if (paramName!==null && paramType!==null) {
				lastParam = param.toParameter(paramName, paramType)
	
				if (param.defaultValue!==null) {
					hasDefaultValue = true
					var namePostPart = actionIndex+"_"+paramIndex
					var name = "___FORMAL_PARAMETER_DEFAULT_VALUE_"+namePostPart
					// FIXME: Hide these attributes into an inner interface.
					var field = param.defaultValue.toField(name, paramType) [
						documentation = MessageFormat::format(Messages::SARLJvmModelInferrer_11, paramName)
						static = true
						final = true
						if (isForInterface) {
							visibility = JvmVisibility::PUBLIC
						}
						else {
							visibility = JvmVisibility::PRIVATE
						}
						initializer = param.defaultValue
					]
					field.annotations += annotationRef(typeof(Generated))
					actionContainer.members += field
					if (owner instanceof JvmConstructor) {
						readAndWriteTracking.markInitialized(field, owner)
					} else {
						readAndWriteTracking.markInitialized(field, null)
					}
					var annot = annotationRef(typeof(DefaultValue), namePostPart)
					lastParam.annotations += annot
				}
				
				owner.parameters += lastParam
				parameterTypes.add(paramType.identifier)
				
				paramIndex++
			}
		}
		if (varargs && lastParam !== null) {
			lastParam.parameterType = lastParam.parameterType.addArrayTypeDimension
		}
		if (hasDefaultValue) {
			owner.annotations += annotationRef(typeof(DefaultValueSource))
		}
		return parameterTypes
	}

	protected def List<String> generateFormalParametersWithDefaultValue(JvmExecutable owner, JvmGenericType actionContainer, boolean varargs, List<InferredStandardParameter> signature, int actionIndex) {
		var JvmFormalParameter lastParam = null
		val arguments = newArrayList
		var paramIndex = 0
		for(parameterSpec : signature) {
			if (parameterSpec instanceof InferredValuedParameter) {
				arguments.add("___FORMAL_PARAMETER_DEFAULT_VALUE_"+actionIndex+"_"+paramIndex)
			}
			else {
				val param = parameterSpec.parameter
				val paramName = param.name
				val paramType = param.parameterType
				if (paramName!==null && paramType!==null) {
					lastParam = param.toParameter(paramName, paramType)
					owner.parameters += lastParam
					arguments.add(paramName)
				}
			}
			paramIndex++
		}
		if (varargs && lastParam !== null) {
			lastParam.parameterType = lastParam.parameterType.addArrayTypeDimension
		}
		return arguments
	}

	protected final def JvmOperation generateAction(
		JvmGenericType owner, ActionSignature signature, 
		XExpression operationBody, int index) {
		return generateAction(owner, signature, operationBody,
			index, operationBody===null, null,
			null, null
		)		
	}

	protected def JvmOperation generateAction(
		JvmGenericType owner, ActionSignature signature,
		XExpression operationBody, int index, boolean isAbstract,
		Map<ActionKey,JvmOperation> operationsToImplement,
		Map<ActionKey,JvmOperation> implementedOperations,
		(ActionKey) => boolean inheritedOperation) {
			
		var returnType = signature.type
		if (returnType == null) {
			returnType = typeRef(Void::TYPE)
		}
		
		val actionKey = sarlSignatureProvider.createFunctionID(owner, signature.name)
				
		var mainOp = signature.toMethod(signature.name, returnType) [
			signature.copyDocumentationTo(it)
			varArgs = signature.varargs
			abstract = isAbstract
			generateFormalParametersAndDefaultValueFields(
				owner, signature, signature.varargs, signature.params, isAbstract, index
			)
			body = operationBody
		]
		owner.members += mainOp
		
		val otherSignatures = sarlSignatureProvider.createSignature(
			actionKey,
			signature.varargs, signature.params
		)

		var actSigKey = sarlSignatureProvider.createActionID(
					signature.name,
					otherSignatures.formalParameterKey
				)
		if (operationsToImplement!==null && actSigKey!==null) {
			var removedOp = operationsToImplement.remove(actSigKey)
			if (removedOp!==null && implementedOperations!==null) {
				implementedOperations.put(actSigKey, removedOp)
			}
		}
		
		for(otherSignature : otherSignatures.getInferredSignatures().entrySet) {
			var ak = sarlSignatureProvider.createActionID(
					signature.name,
					otherSignature.key
				)
			if (ak!==null &&
				(inheritedOperation==null || 
				inheritedOperation.apply(ak))) {
				var additionalOp = signature.toMethod(signature.name, returnType) [
					signature.copyDocumentationTo(it)
					varArgs = signature.varargs
					final = !isAbstract
					abstract = isAbstract
					val args = generateFormalParametersWithDefaultValue(
						owner, signature.varargs, otherSignature.value, index
					)
					if (!isAbstract) {
						body = [
							append(signature.name)
							append("(")
							append(args.join(", "))
							append(");")
						]
					}
					annotations += annotationRef(
						typeof(DefaultValueUse), 
						otherSignatures.formalParameterKey.toString
					)
				]
				owner.members += additionalOp
	
				if (operationsToImplement!==null) {
					var removedOp = operationsToImplement.remove(ak)
					if (removedOp!==null && implementedOperations!==null) {
						implementedOperations.put(ak, removedOp)
					}
				}
			}
		}

		return mainOp
	}

	protected def SignatureKey generateConstructor(JvmGenericType owner, TopElement context, Constructor constructor, int index) {
		val actionKey = sarlSignatureProvider.createConstructorID(owner)
		
		owner.members += constructor.toConstructor [
			constructor.copyDocumentationTo(it)
			varArgs = constructor.varargs
			generateFormalParametersAndDefaultValueFields(
				owner, constructor, constructor.varargs, constructor.params, false, index
			)
			body = constructor.body
		]

		val otherSignatures = sarlSignatureProvider.createSignature(
			actionKey,
			constructor.varargs, constructor.params
		)
		
		for(otherSignature : otherSignatures) {
			var op = constructor.toConstructor [
				constructor.copyDocumentationTo(it)
				varArgs = constructor.varargs
				val args = generateFormalParametersWithDefaultValue(
					owner, constructor.varargs, otherSignature, index
				)
				body = [
					append("this(")
					append(args.join(", "))
					append(");")
				]
				annotations += annotationRef(
					typeof(DefaultValueUse),
					otherSignatures.formalParameterKey.toString
				)
			]
			owner.members += op
		}
		
		return otherSignatures.formalParameterKey
	}

	/** Generate the "equals()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 */
	protected def JvmOperation toEqualsMethod(
									EObject sourceElement, 
									JvmDeclaredType declaredType,
									JvmField... jvmFields) {
		if (sourceElement === null || declaredType === null) {
			return null
		}
		var result = toMethod(sourceElement, "equals", typeRef(Boolean.TYPE), null)
		if (result === null) {
			return null
		}
		result.annotations += annotationRef(typeof(Override))
		result.parameters +=
				toParameter(sourceElement, "obj",
				typeRef(typeof(Object)))
		result.body = [
				append("if (this == obj)").increaseIndentation
				newLine.append("return true;").decreaseIndentation
				newLine.append("if (obj == null)").increaseIndentation
				newLine.append("return false;").decreaseIndentation
				newLine.append("if (getClass() != obj.getClass())").increaseIndentation
				newLine.append("return false;").decreaseIndentation
				newLine.append("if (!super.equals(obj))").increaseIndentation
				newLine.append("return false;").decreaseIndentation
				newLine.append(declaredType.simpleName + " other = (" + declaredType.simpleName + ") obj;")
				for (JvmField field : jvmFields) {
					var typeName = field.type.identifier
					switch(typeName) {
					case Boolean.TYPE.name,
					case Integer.TYPE.name,
					case Long.TYPE.name,
					case Character.TYPE.name,
					case Byte.TYPE.name,
					case Short.TYPE.name: {
						newLine.append("if (other." + field.simpleName +" != this." + field.simpleName + ")").increaseIndentation
						newLine.append("return false;").decreaseIndentation
					}
					case Double.TYPE.name: {
						newLine.append("if (Double.doubleToLongBits(other." + field.simpleName +") != Double.doubleToLongBits(this." + field.simpleName + "))").increaseIndentation
						newLine.append("return false;").decreaseIndentation
					}
					case Float.TYPE.name: {
						newLine.append("if (Float.floatToIntBits(other." + field.simpleName +") != Float.floatToIntBits(this." + field.simpleName + "))").increaseIndentation
						newLine.append("return false;").decreaseIndentation
					}
					default: {
						newLine.append("if (this." + field.simpleName +" == null) {").increaseIndentation
						newLine.append("if (other." + field.simpleName +" != null)").increaseIndentation
						newLine.append("return false;").decreaseIndentation
						decreaseIndentation
						newLine.append("} else if (!this."+ field.simpleName +".equals(other."+ field.simpleName +"))").increaseIndentation
						newLine.append("return false;").decreaseIndentation
					}
					}
				}
				newLine.append("return true;")
			]
		return result;
	}

	/** Generate the "hashCode()" operation.
	 * This function was deprecated in Xbase, and should be provided by DSL
	 * providers now.
	 */
	protected def JvmOperation toHashCodeMethod(
								EObject sourceElement,
								JvmField... jvmFields) {
		if (sourceElement === null) {
			return null
		}
		var result = toMethod(sourceElement, "hashCode", typeRef(Integer.TYPE), null)
		if (result === null) {
			return null
		}
		result.annotations += annotationRef(typeof(Override))
		result.body = [
				append("final int prime = 31;")
				newLine.append("int result = super.hashCode();")
				for (JvmField field : jvmFields) {
					var typeName = field.type.identifier
					switch(typeName) {
					case Boolean.TYPE.name: {
						newLine.append("result = prime * result + (this." + field.simpleName + " ? 1231 : 1237);")
					}
					case Integer.TYPE.name,
					case Character.TYPE.name,
					case Character.TYPE.name,
					case Short.TYPE.name: {
						newLine.append("result = prime * result + this." + field.simpleName + ";")
					}
					case Long.TYPE.name: {
						newLine.append("result = prime * result + (int) (this." + field.simpleName + " ^ (this." + field.simpleName + " >>> 32));")
					}
					case Float.TYPE.name: {
						newLine.append("result = prime * result + Float.floatToIntBits(this." + field.simpleName +");")
					}
					case Double.TYPE.name: {
						newLine.append("result = prime * result + (int) (Double.doubleToLongBits(this." + field.simpleName + ") ^ (Double.doubleToLongBits(this." + field.simpleName + ") >>> 32));")
					}
					default: {
						newLine.append("result = prime * result + ((this." + field.simpleName +"== null) ? 0 : this." + field.simpleName + ".hashCode());")
					}
					}
				}
				newLine.append("return result;")
			]
		return result
	}

}

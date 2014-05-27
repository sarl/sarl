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
package io.sarl.lang.jvmmodel

import com.google.inject.Inject
import io.sarl.lang.SARLKeywords
import io.sarl.lang.annotation.DefaultValue
import io.sarl.lang.annotation.DefaultValueSource
import io.sarl.lang.annotation.DefaultValueUse
import io.sarl.lang.annotation.Generated
import io.sarl.lang.bugfixes.XtendBug392440
import io.sarl.lang.bugfixes.XtendBug434912
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
import io.sarl.lang.sarl.InheritingElement
import io.sarl.lang.sarl.RequiredCapacity
import io.sarl.lang.sarl.Skill
import io.sarl.lang.sarl.TopElement
import io.sarl.lang.signature.ActionKey
import io.sarl.lang.signature.ActionSignatureProvider
import io.sarl.lang.signature.InferredStandardParameter
import io.sarl.lang.signature.InferredValuedParameter
import io.sarl.lang.signature.SignatureKey
import java.util.ArrayList
import java.util.List
import java.util.Map
import java.util.TreeMap
import java.util.UUID
import java.util.logging.Logger
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.JvmAnnotationTarget
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmExecutable
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmFormalParameter
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmStringAnnotationValue
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.JvmVisibility
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.xbase.XExpression
import org.eclipse.xtext.xbase.compiler.XbaseCompiler
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor.IPostIndexingInitializing
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import org.eclipse.xtext.xbase.typesystem.legacy.StandardTypeReferenceOwner
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference
import org.eclipse.xtext.xbase.typesystem.references.OwnedConverter
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

	private var XtendBug392440 hashCodeBugFix
	private var XtendBug434912 toEqualsBugFix
	
	@Inject
	def void setTypesBuilder(JvmTypesBuilder typesBuilder) {
		this.hashCodeBugFix = new XtendBug392440(typesBuilder)
		this.toEqualsBugFix = new XtendBug434912(typesBuilder)
	}

	/**
	 * The dispatch method {@code infer} is called for each instance of the
	 * given element's type that is contained in a resource.
	 * 
	 * @param element
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
	 *            pre-indexing phase. This one is further initialized in the
	 *            indexing phase using the closure you pass to the returned
	 *            {@link IPostIndexingInitializing#initializeLater(org.eclipse.xtext.xbase.lib.Procedures.Procedure1)
	 *            initializeLater(..)}.
	 * @param isPreIndexingPhase
	 *            whether the method is called in a pre-indexing phase, i.e.
	 *            when the global index is not yet fully updated. You must not
	 *            rely on linking using the index if isPreIndexingPhase is
	 *            <code>true</code>.
	 */
	def dispatch void infer(Event event, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(event.toClass(event.fullyQualifiedName)).initializeLater(
			[
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				event.copyDocumentationTo(it)
				
				var long serial = 1L
				serial = serial + generateSuperTypes(event, typeof(io.sarl.lang.core.Event))
				var JvmField jvmField
				var List<JvmField> jvmFields = new ArrayList()
				var actionIndex = 0
				var hasConstructor = false

				for (feature : event.features) {
					switch feature {
						Attribute: {
							jvmField = generateAttribute(feature, JvmVisibility::PUBLIC)
							jvmFields.add(jvmField)
							members += jvmField
							serial = serial + feature.name.hashCode
						}
						Constructor: {
							generateConstructor(event, feature, actionIndex)
							serial = serial + event.fullyQualifiedName.hashCode
							actionIndex++
							hasConstructor = true
						}
					}

				}
				
				if (!hasConstructor) {
					var op = event.toConstructor [
						documentation = '''
							Construct an event. The source of the event is unknown.
						'''
						body = '''
							super();
						'''
					]
					op.annotations += toAnnotation(typeof(Generated))	
					members += op
					op = event.toConstructor [
						documentation = '''
							Construct an event.
							@param source - address of the agent that is emitting this event.
						'''
						parameters += event.toParameter('source', newTypeRef(Address))
						body = '''
							super(source);
						'''
					]
					op.annotations += toAnnotation(typeof(Generated))	
					members += op
				}
								
				if (!jvmFields.isEmpty) {

					val JvmField[] tab = jvmFields // single translation to the array
 					var elementType = event.toClass(event.fullyQualifiedName)
 					
 					var op = toEqualsBugFix.toEqualsMethod(it, event, elementType, true, tab)
					op.annotations += toAnnotation(typeof(Generated))	
					members += op
					
					op = hashCodeBugFix.toHashCodeMethod(it, event, true, tab)
					op.annotations += toAnnotation(typeof(Generated))	
					members += op
					
					op = event.toMethod("attributesToString", newTypeRef(String))[
						visibility = JvmVisibility::PROTECTED
						documentation = '''Returns a String representation of the Event «event.name» attributes only.'''
						body = [
							append(
								'''
								StringBuilder result = new StringBuilder(super.attributesToString());
								«FOR attr : event.features.filter(Attribute)»
									result.append("«attr.name»  = ").append(this.«attr.name»);
								«ENDFOR»
								return result.toString();''')
						]
					]
					op.annotations += toAnnotation(typeof(Generated))	
					members += op
					
				}

				val serialValue = serial
				val serialField = event.toField("serialVersionUID", newTypeRef(long)) [
					visibility = JvmVisibility::PRIVATE
					final = true
					static = true
					initializer = [append(serialValue+"L")]
				]
				serialField.annotations += toAnnotation(typeof(Generated))	
				members += serialField
				readAndWriteTracking.markInitialized(serialField)

			])
	}

	def dispatch void infer(Capacity capacity, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(capacity.toInterface(capacity.fullyQualifiedName.toString, null)).initializeLater(
			[
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				capacity.copyDocumentationTo(it)
				generateSuperTypes(capacity, typeof(io.sarl.lang.core.Capacity))
				
				var actionIndex = 0
				for (feature : capacity.features) {
					generateAction(feature as ActionSignature, null, actionIndex)
					actionIndex++
				}
			])
	}

	def dispatch void infer(Skill skill, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(skill.toClass(skill.fullyQualifiedName)).initializeLater(
			[
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				skill.copyDocumentationTo(it)
				it.generateSuperTypes(skill, typeof(io.sarl.lang.core.Skill))

				for (cap : skill.implementedTypes) {
					if (cap!==null) {
						var capName = cap.fullyQualifiedName
						if (capName!==null) {
							it.superTypes += skill.newTypeRef(capName.toString)
						}
					}
				}

				val Map<ActionKey,JvmOperation> finalOperations = new TreeMap
				val Map<ActionKey,JvmOperation> overridableOperations = new TreeMap
				val Map<ActionKey,JvmOperation> operationsToImplement = new TreeMap
				populateInheritanceContext(
						it,
						finalOperations, overridableOperations,
						null, operationsToImplement,
						null, this.sarlSignatureProvider)
				
				var actionIndex = 0
				var hasConstructor = false
				
				for (feature : skill.features) {
					switch feature {
						Action: {
							var sig = feature.signature as ActionSignature
							it.generateAction(
								sig,
								feature.body,
								actionIndex, false,
								operationsToImplement,
								overridableOperations
							) [ return !finalOperations.containsKey(it)
										&& !overridableOperations.containsKey(it)
							]
							actionIndex++
						}
						Constructor: {
							it.generateConstructor(
								skill, feature, actionIndex
							)
							actionIndex++
							hasConstructor = true
						}
						Attribute: {
							it.generateAttribute(feature, JvmVisibility::PROTECTED)
						}
						CapacityUses: {
							for (used : feature.capacitiesUsed) {
								actionIndex = generateCapacityDelegatorMethods(skill, used, actionIndex)
							}
						}
					}
				}
				
				actionIndex = generateMissedFunction(skill, actionIndex, operationsToImplement, overridableOperations)
								
				if (!hasConstructor) {
					it.members += skill.toConstructor [
						documentation = '''
							Construct a skill.
							@param owner - agent that is owning this skill. 
						'''
						parameters += skill.toParameter('owner', newTypeRef(io.sarl.lang.core.Agent))
						body = '''
							super(owner);
						'''
					]
					it.members += skill.toConstructor [
						documentation = '''
							Construct a skill. The owning agent is unknown. 
						'''
						body = '''
							super();
						'''
					]
				}
			])
	}

	def dispatch void infer(Behavior behavior, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(behavior.toClass(behavior.fullyQualifiedName)).initializeLater(
			[
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				behavior.copyDocumentationTo(it)
				generateSuperTypes(behavior, typeof(io.sarl.lang.core.Behavior))
				
				var behaviorUnitIndex = 1
				var actionIndex = 1
				var hasConstructor = false
				
				for (feature : behavior.features) {
					switch feature {
						RequiredCapacity: {
							//TODO 
						}
						BehaviorUnit: {
							val bMethod = generateBehaviorUnit(feature, behaviorUnitIndex)
							if (bMethod !== null) {
								behaviorUnitIndex++						
								members += bMethod
							}
						}
						Action: {
							generateAction(feature.signature as ActionSignature, feature.body, actionIndex)
							actionIndex++
						}
						CapacityUses: {
							for (used : feature.capacitiesUsed) {
								actionIndex = generateCapacityDelegatorMethods(behavior, used, actionIndex)
							}
						}
						Constructor: {
							generateConstructor(behavior, feature, actionIndex)
							actionIndex++
							hasConstructor = true
						}
						Attribute: {
							generateAttribute(feature, JvmVisibility::PROTECTED)
						}
					}
				}
				
				if (!hasConstructor) {
					members += behavior.toConstructor [
						documentation = '''
							Construct a behavior.
							@param owner - reference to the agent that is owning this behavior.
						'''
						parameters += behavior.toParameter('owner', newTypeRef(io.sarl.lang.core.Agent))
						body = '''
							super(owner);
						'''
					]
				}
			])
	}

	def dispatch void infer(Agent agent, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(agent.toClass(agent.fullyQualifiedName)).initializeLater [
			// Reset the action registry
			sarlSignatureProvider.resetSignatures(it)
			
			agent.copyDocumentationTo(it)
			generateSuperTypes(agent, typeof(io.sarl.lang.core.Agent))
			var cons = agent.toConstructor [
				documentation = '''
					Construct an agent.
					@param parentID - identifier of the parent. It is the identifer
					of the parent agent and the enclosing contect, at the same time.
				'''
				parameters += agent.toParameter('parentID', newTypeRef(UUID))
				body = '''
					super(parentID);
				'''
			]
			cons.annotations += agent.toAnnotation(
					typeof(Generated)
				)
			members += cons
			
			var behaviorUnitIndex = 1
			var actionIndex = 1
			
			for (feature : agent.features) {
				switch feature {
					BehaviorUnit: {
						val bMethod = generateBehaviorUnit(feature, behaviorUnitIndex)
						if (bMethod !== null) {
							behaviorUnitIndex++
							members += bMethod
						}
					}
					Action: {
						generateAction(feature.signature as ActionSignature, feature.body, actionIndex)
						actionIndex++
					}
					Attribute: {
						generateAttribute(feature, JvmVisibility::PROTECTED)
					}
					CapacityUses: {
						for (used : feature.capacitiesUsed) {
							actionIndex = generateCapacityDelegatorMethods(agent, used, actionIndex)
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
			var originalSignature = missedOperation.value.annotationString(typeof(DefaultValueUse))
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
					var args = new ArrayList<String>
					
					var it1 = missedOperation.value.parameters.iterator
					var it2 = originalOperation.parameters.iterator
					var JvmFormalParameter oparam = null
					
					while (it2.hasNext) {
						var param = it2.next
						var vId = param.annotationString(typeof(DefaultValue))
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
							throw new IllegalStateException("Invalid generation of the default-valued formal parameters")
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
					op.annotations += owner.toAnnotation(typeof(DefaultValueUse), originalSignature)
					output.members += op				
					actIndex++
				}
			}
		}
		return actIndex
	}

	protected def long generateSuperTypes(JvmGenericType owner, InheritingElement element, Class<?> defaultType) {
		var serial = 0L
		if (!element.superTypes.empty) {
			for(InheritingElement superType : element.superTypes) {
				if (superType!==null && superType.fullyQualifiedName != null) {
					var type = element.newTypeRef(superType.fullyQualifiedName.toString)						
					owner.superTypes += type
					serial = serial + type.identifier.hashCode
				}
			}
		} else {
			var type = element.newTypeRef(defaultType)
			owner.superTypes += type
			serial = serial + type.identifier.hashCode
		}
		return serial
	}
	
	protected def JvmField generateAttribute(JvmGenericType owner, Attribute attr, JvmVisibility attrVisibility) {
		var field = attr.toField(attr.name, attr.type) [
			visibility = attrVisibility
			attr.copyDocumentationTo(it)
			final = (!attr.writeable)
			static = (!attr.writeable) && (attr.initialValue!==null)
			initializer = attr.initialValue
		]
		owner.members += field
		if (attr.initialValue!==null) {
			readAndWriteTracking.markInitialized(field)
		}
		return field
	}
	
	protected def void iterateOnActions(Capacity capacity, (Capacity, Collection<ActionSignature>)=>void func) {
		val caps = new LinkedList<InheritingElement>()
		caps.add(capacity)
		while (!caps.empty) {
			var cap = caps.removeFirst
			if (cap instanceof Capacity) {
				caps.addAll(cap.superTypes)
				var list = new ArrayList<ActionSignature>
				for(sig : cap.features) {
					list.add(sig as ActionSignature)
				}
				func.apply(cap, list)
			}
		}
	}
	
	private def String secureTypeName(NamedElement o) {
		var name = o.fullyQualifiedName
		if (name!==null) return name.toString
		var sname = o.name
		if (sname!==null) return sname
		log.finer("Cannot determine the fully qualified name of: "+o)
		return o.toString
	}

	protected def void extractCapacityActions(Capacity capacity, Set<ActionSignature> functions, Map<String,Collection<? extends ActionSignature>> functionsPerCapacity) {
		capacity.iterateOnActions [ c, l |
			if (functions!==null) functions.addAll(l)
			if (functionsPerCapacity!==null)
				functionsPerCapacity.put(c.secureTypeName,l)
		]
	}
	
	protected def int generateCapacityDelegatorMethods(JvmGenericType owner, InheritingElement context, Capacity capacity, int index) {
		//FIXME :the generation of the functions does not take the inherited context into account
		// Detect the needed actions by iterating on the capacity hierarchy
		val functions = new TreeSet(new ActionSignatureComparator)
		val functionsPerCapacity = new TreeMap<String,Collection<? extends ActionSignature>>
		capacity.extractCapacityActions(functions, functionsPerCapacity)
		// Go through inherited classes, and remove the functions that are provided by the super classes
		val classes = new LinkedList(context.superTypes)
		while (!classes.empty) {
			val superClass = classes.removeFirst
			classes.addAll(superClass.superTypes)
			for( feature : superClass.features) {
				if (feature instanceof ActionSignature) {
					functions.remove(feature)
				}
				else if (feature instanceof CapacityUses) {
					val caps = new LinkedList<Capacity>
					caps.addAll(feature.capacitiesUsed)
					while(!caps.empty) {
						val cap = caps.removeFirst
						for(s : cap.superTypes) {
							if (s instanceof Capacity) caps.add(s)
						}
						var list = functionsPerCapacity.get(cap.secureTypeName)
						if (list===null) {
							cap.iterateOnActions [ c, l |
								functionsPerCapacity.put(c.secureTypeName, l)
								functions.removeAll(l)
							]
						}
						else {
								functions.removeAll(list)
						}
					}
				}
			}
		}
		// Generate the missed actions
		var actionIndex = index
		for (signature : functions) {
			owner.generateAction(signature, null, actionIndex, false, null, null, null).setBody [
				if (signature.type != null) {
					append('''return ''')
				}
				append('''getSkill(''')
				append(context.newTypeRef(capacity.fullyQualifiedName.toString).type)
				append('''.class).«signature.name»(''')
				append(signature.params.join(', ')[name])
				append(');')
			]
			actionIndex++
		}
		
		return actionIndex
	}

	protected def JvmOperation generateBehaviorUnit(JvmGenericType owner, BehaviorUnit unit, int index) {
		val eventName = unit.event.name
		if (eventName!==null && !eventName.empty) {
			val behName = "_handle_" + unit.event.name + "_" + index
	
			val behaviorMethod = unit.toMethod(behName, unit.newTypeRef(Void::TYPE)) [
				unit.copyDocumentationTo(it)
				annotations += unit.toAnnotation(typeof(Percept))
				parameters +=
					unit.event.toParameter(SARLKeywords::OCCURRENCE, newTypeRef(unit.event, unit.event.fullyQualifiedName.toString))
			]
	
			if (unit.guard == null) {
				behaviorMethod.body = unit.body
			} else {
				val guard = unit.guard
				val guardMethodName = behName + "_Guard"
				val guardMethod = guard.toMethod(guardMethodName, guard.newTypeRef(Boolean::TYPE)) [
					documentation = "Ensures that the behavior " + behName + " is called only when the guard " +
						guard.toString + " is valid"
					parameters += unit.event.toParameter(SARLKeywords::OCCURRENCE,
						newTypeRef(unit.event, unit.event.fullyQualifiedName.toString))
				]
	
				guardMethod.body = guard
				jvmModelAssociator.associateLogicalContainer(unit.body, behaviorMethod)
	
				behaviorMethod.body = [
					it.append('''if ( «guardMethodName»(«SARLKeywords::OCCURRENCE»)) { ''')
					xbaseCompiler.compile(unit.body, it, 
						behaviorMethod.newTypeRef(Void::TYPE).toLightweightTypeReference
					)
					it.append('}')
				]
	
				owner.members += guardMethod
			}
			return behaviorMethod
		}
		log.fine("Unable to resolve the event for a behavior unit")
		return null
	}
	
	protected def List<String> generateFormalParametersAndDefaultValueFields(
		JvmExecutable owner, JvmGenericType actionContainer, 
		EObject sourceElement, boolean varargs,
		List<FormalParameter> params, 
		boolean isForInterface,
		int actionIndex) {

		var parameterTypes = new ArrayList
		var JvmFormalParameter lastParam = null
		var paramIndex = 0
		var hasDefaultValue = false
		for (param : params) {
			lastParam = param.toParameter(param.name, param.parameterType)

			if (param.defaultValue!==null) {
				hasDefaultValue = true
				var namePostPart = actionIndex+"_"+paramIndex
				var name = "___FORMAL_PARAMETER_DEFAULT_VALUE_"+namePostPart
				// FIXME: Hide these attributes into an inner interface.
				var field = param.defaultValue.toField(name, param.parameterType) [
					documentation = "Default value for the parameter "+param.name
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
				field.annotations += param.toAnnotation(typeof(Generated))
				actionContainer.members += field
				readAndWriteTracking.markInitialized(field)
				var annot = param.toAnnotation(typeof(DefaultValue), namePostPart)
				lastParam.annotations += annot
			}
			
			owner.parameters += lastParam
			parameterTypes.add(param.parameterType.identifier)
			
			paramIndex++
		}
		if (varargs && lastParam !== null) {
			lastParam.parameterType = lastParam.parameterType.addArrayTypeDimension
		}
		if (hasDefaultValue) {
			owner.annotations += sourceElement.toAnnotation(typeof(DefaultValueSource))
		}
		return parameterTypes
	}

	protected def List<String> generateFormalParametersWithDefaultValue(JvmExecutable owner, JvmGenericType actionContainer, boolean varargs, List<InferredStandardParameter> signature, int actionIndex) {
		var JvmFormalParameter lastParam = null
		val arguments = new ArrayList
		var paramIndex = 0
		for(parameterSpec : signature) {
			if (parameterSpec instanceof InferredValuedParameter) {
				arguments.add("___FORMAL_PARAMETER_DEFAULT_VALUE_"+actionIndex+"_"+paramIndex)
			}
			else {
				val param = parameterSpec.parameter
				lastParam = param.toParameter(param.name, param.parameterType)
				owner.parameters += lastParam
				arguments.add(param.name)
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
			returnType = signature.newTypeRef(Void::TYPE)
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
		if (operationsToImplement!==null) {
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
			if (inheritedOperation==null || 
				inheritedOperation.apply(ak)) {
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
					annotations += signature.toAnnotation(
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
		
		owner.members += context.toConstructor [
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
			var op = owner.toConstructor [
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
				annotations += owner.toAnnotation(
					typeof(DefaultValueUse),
					otherSignatures.formalParameterKey.toString
				)
			]
			owner.members += op
		}
		
		return otherSignatures.formalParameterKey
	}
	
	protected def LightweightTypeReference toLightweightTypeReference(JvmTypeReference typeRef) {
		return toLightweightTypeReference(typeRef, false);
	}
	
	protected def LightweightTypeReference toLightweightTypeReference(JvmTypeReference typeRef, boolean keepUnboundWildcardInformation) {
		var OwnedConverter converter = new OwnedConverter(new StandardTypeReferenceOwner(this.services, typeRef), keepUnboundWildcardInformation);
		var LightweightTypeReference reference = converter.toLightweightReference(typeRef);
		return reference;
	}
	
	protected def String annotationString(JvmAnnotationTarget op, Class<?> annotationType) {
		val n = annotationType.name
		for(aref : op.annotations) {
			var an = aref.annotation
			if (n==an.qualifiedName) {
				for(value : aref.values) {
					if (value instanceof JvmStringAnnotationValue) {
						for(sValue : value.values) {
							if (value!==null) return sValue;
						}
					}
				}
			}
		}
		return null;
	}
		
}

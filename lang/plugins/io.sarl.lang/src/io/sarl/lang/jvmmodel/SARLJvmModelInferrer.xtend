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
import io.sarl.lang.sarl.AbstractElement
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
import io.sarl.lang.sarl.RequiredCapacity
import io.sarl.lang.sarl.Skill
import java.util.logging.Logger
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.TypesFactory
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.xbase.XExpression
import org.eclipse.xtext.xbase.compiler.XbaseCompiler
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import io.sarl.lang.core.Percept
import java.util.UUID

/**
 * <p>Infers a JVM model from the source model.</p> 
 *
 * <p>The JVM model should contain all elements that would appear in the Java code 
 * which is generated from the source model. Other models link against the JVM model rather than the source model.</p>     
 */
class SARLJvmModelInferrer extends AbstractModelInferrer {

	public static final String KEYWORD_OCCURRENCE = "occurrence";

	/**
     * convenience API to build and initialize JVM types and their members.
     */
	@Inject extension JvmTypesBuilder

	@Inject extension IQualifiedNameProvider

	@Inject protected XbaseCompiler xbaseCompiler

	@Inject protected JvmModelAssociator jvmModelAssociator

	@Inject TypesFactory typesFactory

	@Inject Logger log

	private Iterable<Capacity> kCapacities = null

	/**
	 * The dispatch method {@code infer} is called for each instance of the
	 * given element's type that is contained in a resource.
	 * 
	 * @param element
	 *            the model to create one or more
	 *            {@link org.eclipse.xtext.common.types.JvmDeclaredType declared
	 *            types} from.
	 * @param acceptor
	 *            each created
	 *            {@link org.eclipse.xtext.common.types.JvmDeclaredType type}
	 *            without a container should be passed to the acceptor in order
	 *            get attached to the current resource. The acceptor's
	 *            {@link IJvmDeclaredTypeAcceptor#accept(org.eclipse.xtext.common.types.JvmDeclaredType)
	 *            accept(..)} method takes the constructed empty type for the
	 *            pre-indexing phase. This one is further initialized in the
	 *            indexing phase using the closure you pass to the returned
	 *            {@link org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor.IPostIndexingInitializing#initializeLater(org.eclipse.xtext.xbase.lib.Procedures.Procedure1)
	 *            initializeLater(..)}.
	 * @param isPreIndexingPhase
	 *            whether the method is called in a pre-indexing phase, i.e.
	 *            when the global index is not yet fully updated. You must not
	 *            rely on linking using the index if isPreIndexingPhase is
	 *            <code>true</code>.
	 */
	def dispatch void infer(Event element, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {

		acceptor.accept(element.toClass(element.fullyQualifiedName)).initializeLater(
			[
				documentation = element.documentation
				superTypes += newTypeRef(element, typeof(io.sarl.lang.core.Event))
				for (feature : element.features) {
					switch feature {
						Action: {
							generateAction(feature.signature, feature.body)
						}
						Attribute: {
							members += feature.toField(feature.name, feature.type) [
								final = !feature.writeable
								initializer = feature.initialValue
							]
							members += feature.toGetter(feature.name, feature.type)
							if (feature.writeable) {
								members += feature.toSetter(feature.name, feature.type)
							}

						}
						CapacityUses: {
							for (used : feature.capacitiesUsed) {
								generateCapactyDelegatorMethods(element, used)
							}
						}
						Constructor: {
							generateContructor(element, feature)
						}
					}

				}
				//String result = "Factorial [";
				//result = result + "number  = " + this.number;
				//result = result + "value  = " + this.value;
				//result = result + "]";
				//return result;
				members += element.toMethod('toString', newTypeRef(String)) [
					documentation = '''Returns a String representation of the Event «element.name»'''
					body = [
						append(
							'''
							StringBuilder result = new StringBuilder();
							result.append("«element.name»[");
							«FOR attr : element.features.filter(Attribute)»
								result.append("«attr.name»  = ").append(this.«attr.name»);
							«ENDFOR»
							result.append("]");
							return result.toString();''')
					]
				]
			])
	}

	def dispatch void infer(Capacity capacity, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {

		//capacity.generateAccessor(acceptor)
		acceptor.accept(capacity.toInterface(capacity.fullyQualifiedName.toString, null)).initializeLater(
			[
				documentation = capacity.documentation
				if (capacity.superType != null && capacity.superType.fullyQualifiedName != null) {
					superTypes += newTypeRef(capacity.superType.fullyQualifiedName.toString)
				} else {
					superTypes += newTypeRef(capacity, typeof(io.sarl.lang.core.Capacity))
				}
				for (feature : capacity.actions) {

					generateAction(feature, null)

				}
			])
	}

	def dispatch void infer(Skill element, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(element.toClass(element.fullyQualifiedName)).initializeLater(
			[
				documentation = element.documentation
				superTypes += newTypeRef(element, typeof(io.sarl.lang.core.Skill))
				for (cap : element.implementedCapacities) {
					if (cap.fullyQualifiedName != null) {
						superTypes += element.newTypeRef(cap.fullyQualifiedName.toString)
					} else {

						//log.fine("Null FQN for Capacity:" + cap.name)
						println("Null FQN for Capacity:" + cap.name)
					}
				}
				for (feature : element.features) {
					switch feature {
						Action: {
							generateAction(feature.signature, feature.body)
						}
						Attribute: {
							members += feature.toField(feature.name, feature.type) [
								documentation = feature.documentation
								final = !feature.writeable
								initializer = feature.initialValue
							]
						}
						CapacityUses: {
							for (used : feature.capacitiesUsed) {
								generateCapactyDelegatorMethods(element, used)
							}
						}
						Constructor: {
							generateContructor(element, feature)
						}
					}
				}
			])
	}

	def dispatch void infer(Behavior element, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(element.toClass(element.fullyQualifiedName)).initializeLater(
			[
				documentation = element.documentation
				var int counter = 0
				superTypes += newTypeRef(element, typeof(io.sarl.lang.core.Behavior))
				for (feature : element.features) {
					switch feature {
						RequiredCapacity: {
							//TODO 
						}
						BehaviorUnit: {
							members += generateBehaviorUnit(feature as BehaviorUnit, counter)
						}
						CapacityUses: {
							for (used : feature.capacitiesUsed) {
								generateCapactyDelegatorMethods(element, used)
							}
						}
						Constructor: {
							generateContructor(element, feature)
						}
						Attribute: {
							members += feature.toField(feature.name, feature.type) [
								documentation = feature.documentation
								final = !feature.writeable
								initializer = feature.initialValue
							]
						}
					}
				}
			])
	}

	def dispatch void infer(Agent agent, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {

		acceptor.accept(agent.toClass(agent.fullyQualifiedName)).initializeLater [
			documentation = agent.documentation
			superTypes += newTypeRef(agent, typeof(io.sarl.lang.core.Agent))
			members += agent.toConstructor [
				documentation = '''Creates a new Agent of type «agent.name»'''
				parameters += agent.toParameter('parentID', newTypeRef(UUID))
				body = '''
					super(parentID);
				'''
			]
			var int counter = 0
			for (feature : agent.features) {
				switch feature {
					BehaviorUnit: {
						counter = counter + 1

						val bMethod = generateBehaviorUnit(feature as BehaviorUnit, counter)

						members += bMethod
					}
					Action: {
						generateAction(feature.signature, feature.body)

					}
					Attribute: {
						members += feature.toField(feature.name, feature.type) [
							documentation = feature.documentation
							initializer = feature.initialValue
							final = !feature.writeable
						]
					}
					CapacityUses: {
						for (used : feature.capacitiesUsed) {
							generateCapactyDelegatorMethods(agent, used)
						}
					}
				}
			}
		]

	}

	def void generateCapactyDelegatorMethods(JvmGenericType owner, AbstractElement context, Capacity capacity) {
		for (signature : capacity.actions) {
			owner.generateAction(signature, null).setBody [
				if (signature.type != null) {
					append('''return ''')
				}
				append('''this.getSkill(''')
				append(context.newTypeRef(capacity.fullyQualifiedName.toString).type)
				append('''.class).«signature.name»(''')
				append(signature.params.join(', ')[name])
				append(');')
			]
		}

	}

	def JvmOperation generateBehaviorUnit(JvmGenericType owner, BehaviorUnit unit, int index) {
		val behName = "_handle_" + unit.event.name + "_" + index

		val behaviorMethod = unit.toMethod(behName, unit.newTypeRef(Void::TYPE)) [
			documentation = unit.documentation
			//TODO: Change subscribe annotations to decouple from guava
			annotations += unit.toAnnotation(typeof(Percept))
			parameters +=
				unit.event.toParameter(KEYWORD_OCCURRENCE, newTypeRef(unit.event, unit.event.fullyQualifiedName.toString))
		]

		if (unit.guard == null) {
			behaviorMethod.body = unit.body
		} else {
			val guard = unit.guard
			val guardMethodName = behName + "_Guard"
			val guardMethod = guard.toMethod(guardMethodName, guard.newTypeRef(Boolean::TYPE)) [
				documentation = "Ensures that the behavior " + behName + " is called only when the guard " +
					guard.toString + " is valid"
				parameters += unit.event.toParameter(KEYWORD_OCCURRENCE,
					newTypeRef(unit.event, unit.event.fullyQualifiedName.toString))
			]

			guardMethod.body = guard
			jvmModelAssociator.associateLogicalContainer(unit.body, behaviorMethod)

			behaviorMethod.body = [
				it.append('''if ( «guardMethodName»(«KEYWORD_OCCURRENCE»)) { ''')
				xbaseCompiler.compile(unit.body, it, behaviorMethod.newTypeRef(Void::TYPE))
				it.append('}')
			]

			owner.members += guardMethod
		}
		behaviorMethod
	}

	def JvmOperation generateAction(JvmGenericType owner, ActionSignature signature, XExpression operationBody) {
		var returnType = signature.type
		if (returnType == null) {
			returnType = signature.newTypeRef(Void::TYPE)
		}
		val op = owner.toMethod(signature.name, returnType) [
			documentation = signature.documentation
			for (p : signature.params) {
				parameters += p.toParameter(p.name, p.parameterType)
			}
			body = operationBody
		]

		owner.members += op
		return op
	}

	def void generateContructor(JvmGenericType owner, AbstractElement context, Constructor constructor) {
		owner.members += context.toConstructor [
			documentation = constructor.documentation
			for (p : constructor.params) {
				parameters += p.toParameter(p.name, p.parameterType)
			}
			body = constructor.body
		]

	}

}

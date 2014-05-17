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
import io.sarl.lang.core.Percept
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
import java.util.ArrayList
import java.util.List
import java.util.UUID
import java.util.logging.Logger
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmVisibility
import org.eclipse.xtext.common.types.TypesFactory
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.xbase.XExpression
import org.eclipse.xtext.xbase.compiler.XbaseCompiler
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder
import io.sarl.lang.SARLKeywords
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.JvmFormalParameter

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

	/**
     * convenience API to build and initialize JVM types and their members.
     */
	@Inject extension JvmTypesBuilder

	@Inject extension IQualifiedNameProvider
	
	@Inject protected XbaseCompiler xbaseCompiler

	@Inject protected JvmModelAssociator jvmModelAssociator

	@Inject private TypesFactory typesFactory

	@Inject private Logger log

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
	 *            {@link JvmDeclaredType type}
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
				
				var long serial = 1L

				var JvmTypeReference parentType
				if(element.superType != null){
					parentType = newTypeRef(element.superType.fullyQualifiedName.toString)
					serial = serial + element.superType.fullyQualifiedName.toString.hashCode
				} else {
					parentType = newTypeRef(element, typeof(io.sarl.lang.core.Event))
					serial = serial + "io.sarl.lang.core.Event".hashCode
				}
				superTypes += parentType

				var JvmField jvmField
				var List<JvmField> jvmFields = new ArrayList()

				for (feature : element.features) {
					switch feature {
						Attribute: {
							jvmField = feature.toField(feature.name, feature.type) [
								visibility = JvmVisibility::PUBLIC
								documentation = feature.documentation
								final = !feature.writeable
								initializer = feature.initialValue
							]
							jvmFields.add(jvmField)
							members += jvmField
							serial = serial + feature.name.hashCode
						}
						Constructor: {
							generateConstructor(element, feature)
							serial = serial + element.fullyQualifiedName.hashCode
						}
					}

				}
								
				if (!jvmFields.isEmpty) {

					val JvmField[] tab = jvmFields // single translation to the array
 					var elementType = element.toClass(element.fullyQualifiedName)
 					
 					members += toEqualsMethod_Bug434912(element, elementType, true, tab)
					
					members += toHashCodeMethod_Bug392440(element, true, tab)
					
					members += element.toMethod("attributesToString", newTypeRef(String))[
						visibility = JvmVisibility::PROTECTED
						documentation = '''Returns a String representation of the Event «element.name» attributes only.'''
						body = [
							append(
								'''
								StringBuilder result = new StringBuilder(super.attributesToString());
								«FOR attr : element.features.filter(Attribute)»
									result.append("«attr.name»  = ").append(this.«attr.name»);
								«ENDFOR»
								return result.toString();''')
						]
					]
					
				}

				val serialValue = serial
				members += element.toField("serialVersionUID", newTypeRef(long)) [
					visibility = JvmVisibility::PRIVATE
					final = true
					static = true
					initializer = [append(serialValue+"L")]
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
					if (cap.name!=null) {
						if (cap.fullyQualifiedName != null) {
							superTypes += element.newTypeRef(cap.fullyQualifiedName.toString)
						} else {
							log.fine("Unable to resolve the fully qualified name of the implemented capacity '"+cap.name+"' for the skill:" + element.name)
						}
					} else {
						log.fine("Unable to resolve an implemented capacity name for the skill:" + element.name)
					}
				}
				for (feature : element.features) {
					switch feature {
						Action: {
							generateAction(feature.signature, feature.body)
						}
						Attribute: {
							members += feature.toField(feature.name, feature.type) [
								visibility = JvmVisibility::PROTECTED
								documentation = feature.documentation
								final = !feature.writeable
								initializer = feature.initialValue
							]
						}
						CapacityUses: {
							for (used : feature.capacitiesUsed) {
								generateCapacityDelegatorMethods(element, used)
							}
						}
						Constructor: {
							generateConstructor(element, feature)
						}
					}
				}
			])
	}

	def dispatch void infer(Behavior element, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(element.toClass(element.fullyQualifiedName)).initializeLater(
			[
				documentation = element.documentation
				var int counter = 1
				if (element.superType != null && element.superType.fullyQualifiedName != null) {
					superTypes += newTypeRef(element.superType.fullyQualifiedName.toString)
				} else {
					superTypes += newTypeRef(element, typeof(io.sarl.lang.core.Behavior))
				}
				for (feature : element.features) {
					switch feature {
						RequiredCapacity: {
							//TODO 
						}
						BehaviorUnit: {
							val bMethod = generateBehaviorUnit(feature, counter)
							if (bMethod !== null) {
								counter = counter + 1						
								members += bMethod
							}
						}
						Action: {
							generateAction(feature.signature, feature.body)
						}
						CapacityUses: {
							for (used : feature.capacitiesUsed) {
								generateCapacityDelegatorMethods(element, used)
							}
						}
						Constructor: {
							generateConstructor(element, feature)
						}
						Attribute: {
							members += feature.toField(feature.name, feature.type) [
								visibility = JvmVisibility::PROTECTED
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
			if (agent.superType != null && agent.superType.fullyQualifiedName != null) {
				superTypes += newTypeRef(agent.superType.fullyQualifiedName.toString)
			} else {
				superTypes += newTypeRef(agent, typeof(io.sarl.lang.core.Agent))
			}
			members += agent.toConstructor [
				documentation = '''Creates a new Agent of type «agent.name»'''
				parameters += agent.toParameter('parentID', newTypeRef(UUID))
				body = '''
					super(parentID);
				'''
			]
			var int counter = 1
			for (feature : agent.features) {
				switch feature {
					BehaviorUnit: {
						val bMethod = generateBehaviorUnit(feature, counter)
						if (bMethod !== null) {
							counter = counter + 1						
							members += bMethod
						}
					}
					Action: {
						generateAction(feature.signature, feature.body)
					}
					Attribute: {
							members += feature.toField(feature.name, feature.type) [
								visibility = JvmVisibility::PROTECTED
								documentation = feature.documentation
								final = !feature.writeable
								initializer = feature.initialValue
							]
					}
					CapacityUses: {
						for (used : feature.capacitiesUsed) {
							generateCapacityDelegatorMethods(agent, used)
						}
					}
				}
			}
		]

	}

	def void generateCapacityDelegatorMethods(JvmGenericType owner, AbstractElement context, Capacity capacity) {
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
		val eventName = unit.event.name
		if (eventName!==null && !eventName.empty) {
			val behName = "_handle_" + unit.event.name + "_" + index
	
			val behaviorMethod = unit.toMethod(behName, unit.newTypeRef(Void::TYPE)) [
				documentation = unit.documentation
				annotations += unit.toAnnotation(typeof(Percept))
				parameters +=
					unit.event.toParameter(SARLKeywords.KEYWORD_OCCURRENCE, newTypeRef(unit.event, unit.event.fullyQualifiedName.toString))
			]
	
			if (unit.guard == null) {
				behaviorMethod.body = unit.body
			} else {
				val guard = unit.guard
				val guardMethodName = behName + "_Guard"
				val guardMethod = guard.toMethod(guardMethodName, guard.newTypeRef(Boolean::TYPE)) [
					documentation = "Ensures that the behavior " + behName + " is called only when the guard " +
						guard.toString + " is valid"
					parameters += unit.event.toParameter(SARLKeywords.KEYWORD_OCCURRENCE,
						newTypeRef(unit.event, unit.event.fullyQualifiedName.toString))
				]
	
				guardMethod.body = guard
				jvmModelAssociator.associateLogicalContainer(unit.body, behaviorMethod)
	
				behaviorMethod.body = [
					it.append('''if ( «guardMethodName»(«SARLKeywords.KEYWORD_OCCURRENCE»)) { ''')
					xbaseCompiler.compile(unit.body, it, behaviorMethod.newTypeRef(Void::TYPE))
					it.append('}')
				]
	
				owner.members += guardMethod
			}
			return behaviorMethod
		}
		log.fine("Unable to resolve the event for a behavior unit")
		return null
	}

	def JvmOperation generateAction(JvmGenericType owner, ActionSignature signature, XExpression operationBody) {
		var returnType = signature.type
		if (returnType == null) {
			returnType = signature.newTypeRef(Void::TYPE)
		}
		val op = owner.toMethod(signature.name, returnType) [
			documentation = signature.documentation
			varArgs = signature.varargs
			var JvmFormalParameter lastParam = null
			for (p : signature.params) {
				lastParam = p.toParameter(p.name, p.parameterType)
				parameters += lastParam
			}
			if (signature.varargs && lastParam !== null) {
				lastParam.parameterType = lastParam.parameterType.addArrayTypeDimension
			}
			body = operationBody
		]

		owner.members += op
		return op
	}

	def void generateConstructor(JvmGenericType owner, AbstractElement context, Constructor constructor) {
		owner.members += context.toConstructor [
			documentation = constructor.documentation
			varArgs = constructor.varargs
			var JvmFormalParameter lastParam = null
			for (p : constructor.params) {
				lastParam = p.toParameter(p.name, p.parameterType)
				parameters += lastParam
			}
			if (constructor.varargs && lastParam !== null) {
				lastParam.parameterType = lastParam.parameterType.addArrayTypeDimension
			}
			body = constructor.body
		]

	}
	
	/** 
	 * FIXME: Remove this function if it is fixed in Xtext: https://bugs.eclipse.org/bugs/show_bug.cgi?id=392440
	 * 
	 * Copied/pasted from {@link JvmTypesBuilder#toHashCodeMethod(EObject, boolean, JvmField...)}.
	 * Updated for fixing the issue {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=392440"}
	 *
	 * @param owner 
	 * @param sourceElement
	 * @param extendsSomethingWithProperHashCode
	 * @param jvmFields
	 * @return the operation.
	 */
	def JvmOperation toHashCodeMethod_Bug392440(JvmGenericType owner, EObject sourceElement, boolean extendsSomethingWithProperHashCode, JvmField ...jvmFields) {
		if (sourceElement === null) return null
		var JvmOperation result = toMethod(sourceElement, "hashCode", newTypeRef(sourceElement, Integer.TYPE), null)
		if (result === null) return null
		result.annotations.add(toAnnotation(sourceElement, Override))
		result.body = [
				append("final int prime = 31;")
				if (extendsSomethingWithProperHashCode) {
					newLine().append("int result = super.hashCode();")
				} else {
					newLine().append("int result = 1;")
				}
				for (JvmField field : jvmFields) {
					var String typeName = field.type.identifier
					if (Boolean.TYPE.name == typeName) {
						newLine().append("result = prime * result + (this." + field.getSimpleName() +" ? 1231 : 1237);")
					} else if (Integer.TYPE.name == typeName
							|| Character.TYPE.name == typeName
							|| Byte.TYPE.name == typeName
							|| Short.TYPE.name == typeName) {
						newLine().append("result = prime * result + this." + field.getSimpleName() +";")
					} else if (Long.TYPE.name == typeName) {
						newLine().append("result = prime * result + (int) (this." + field.getSimpleName() +" ^ (this." + field.getSimpleName() + " >>> 32));")
					} else if (Float.TYPE.name == typeName) {
						newLine().append("result = prime * result + Float.floatToIntBits(this." + field.getSimpleName() +");")
					} else if (Double.TYPE.name == typeName) {
						newLine().append("result = prime * result + (int) (Double.doubleToLongBits(this." + field.getSimpleName() +") ^ (Double.doubleToLongBits(this." + field.getSimpleName() + ") >>> 32));");
					} else {
						newLine().append("result = prime * result + ((this." + field.getSimpleName() +"== null) ? 0 : this."+field.getSimpleName()+".hashCode());");
					}
				}
				newLine().append("return result;");
		]
		return result
	}

	/** 
	 * FIXME: Remove this function if it is fixed in Xtext: https://bugs.eclipse.org/bugs/show_bug.cgi?id=434912
	 * 
	 * Copied/pasted from {@link JvmTypesBuilder#toEquals}.
	 * Updated for fixing the issue {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=434912"}
	 *
	 * @param owner 
	 * @param sourceElement
	 * @param declaredType
	 * @param isDelegateToSuperEquals
	 * @param jvmFields
	 * @return the operation.
	 */
	def JvmOperation toEqualsMethod_Bug434912(JvmGenericType owner, EObject sourceElement, JvmDeclaredType declaredType, boolean isDelegateToSuperEquals, JvmField... jvmFields) {
		var JvmOperation result = toMethod(sourceElement, "equals", newTypeRef(sourceElement, Boolean.TYPE), null)
		result.annotations.add(sourceElement.toAnnotation(Override))
		result.parameters.add( sourceElement.toParameter("obj", newTypeRef(sourceElement, Object)))
		result.body = [
					append("if (this == obj)").increaseIndentation()
					newLine().append("return true;").decreaseIndentation()
					newLine().append("if (obj == null)").increaseIndentation()
					newLine().append("return false;").decreaseIndentation()
					newLine().append("if (getClass() != obj.getClass())").increaseIndentation()
					newLine().append("return false;").decreaseIndentation()
					if (isDelegateToSuperEquals) {
						newLine().append("if (!super.equals(obj))").increaseIndentation()
						newLine().append("return false;").decreaseIndentation()
					}
					newLine().append(declaredType.getSimpleName()+" other = (" + declaredType.getSimpleName() + ") obj;")
					for (JvmField field : jvmFields) {
						var String typeName = field.type.identifier
						if (Boolean.TYPE.name == typeName 
								|| Integer.TYPE.name == typeName
								|| Long.TYPE.name == typeName
								|| Character.TYPE.name == typeName
								|| Byte.TYPE.name == typeName
								|| Short.TYPE.name == typeName) {
							newLine().append("if (other." + field.getSimpleName() +" != this." + field.getSimpleName() + ")").increaseIndentation()
							newLine().append("return false;").decreaseIndentation()
							
						} else if (Double.TYPE.name == typeName) {
							newLine().append("if (Double.doubleToLongBits(other." + field.getSimpleName() +") != Double.doubleToLongBits(this." + field.getSimpleName() + "))").increaseIndentation()
							newLine().append("return false;").decreaseIndentation()
						} else if (Float.TYPE.name == typeName) {
							newLine().append("if (Float.floatToIntBits(other." + field.getSimpleName() +") != Float.floatToIntBits(this." + field.getSimpleName() + "))").increaseIndentation()
							newLine().append("return false;").decreaseIndentation()
						} else {
							newLine().append("if (this." + field.getSimpleName() +" == null) {").increaseIndentation()
							newLine().append("if (other." + field.getSimpleName() +" != null)").increaseIndentation()
							newLine().append("return false;").decreaseIndentation()
							decreaseIndentation()
							newLine().append("} else if (!this."+ field.getSimpleName() +".equals(other."+ field.getSimpleName() +"))").increaseIndentation()
							newLine().append("return false;").decreaseIndentation()
						}
					}
					newLine().append("return true;")
			]
		return result
	}

}

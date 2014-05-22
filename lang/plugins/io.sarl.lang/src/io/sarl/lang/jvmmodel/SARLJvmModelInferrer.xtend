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
import io.sarl.lang.sarl.NamedElement
import io.sarl.lang.sarl.RequiredCapacity
import io.sarl.lang.sarl.Skill
import io.sarl.lang.sarl.TopElement
import io.sarl.lang.signature.ActionSignatureComparator
import io.sarl.lang.signature.ActionSignatureProvider
import io.sarl.lang.signature.InferredStandardParameter
import io.sarl.lang.signature.InferredValuedParameter
import java.util.ArrayList
import java.util.Collection
import java.util.LinkedList
import java.util.List
import java.util.Map
import java.util.Set
import java.util.TreeMap
import java.util.TreeSet
import java.util.UUID
import java.util.logging.Logger
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.JvmDeclaredType
import org.eclipse.xtext.common.types.JvmExecutable
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmFormalParameter
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmVisibility
import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.xbase.XExpression
import org.eclipse.xtext.xbase.XStringLiteral
import org.eclipse.xtext.xbase.compiler.XbaseCompiler
import org.eclipse.xtext.xbase.compiler.output.FakeTreeAppendable
import org.eclipse.xtext.xbase.jvmmodel.AbstractModelInferrer
import org.eclipse.xtext.xbase.jvmmodel.IJvmDeclaredTypeAcceptor
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder

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
	 *            {@code type}
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
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				documentation = element.documentation
				
				var long serial = 1L
				serial = serial + generateSuperTypes(element, typeof(io.sarl.lang.core.Event))
				var JvmField jvmField
				var List<JvmField> jvmFields = new ArrayList()

				for (feature : element.features) {
					switch feature {
						Attribute: {
							jvmField = generateAttribute(feature, JvmVisibility::PUBLIC)
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
		acceptor.accept(capacity.toInterface(capacity.fullyQualifiedName.toString, null)).initializeLater(
			[
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				documentation = capacity.documentation
				generateSuperTypes(capacity, typeof(io.sarl.lang.core.Capacity))
				for (feature : capacity.features) {
					generateAction(feature as ActionSignature, null)
				}
			])
	}

	def dispatch void infer(Skill element, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(element.toClass(element.fullyQualifiedName)).initializeLater(
			[
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				documentation = element.documentation
				superTypes += newTypeRef(element, typeof(io.sarl.lang.core.Skill))
				for (cap : element.implementedTypes) {
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
							generateAction(feature.signature as ActionSignature, feature.body)
						}
						Attribute: {
							generateAttribute(feature, JvmVisibility::PROTECTED)
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
				// Reset the action registry
				sarlSignatureProvider.resetSignatures(it)

				documentation = element.documentation
				generateSuperTypes(element, typeof(io.sarl.lang.core.Behavior))
				var int counter = 1
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
							generateAction(feature.signature as ActionSignature, feature.body)
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
							generateAttribute(feature, JvmVisibility::PROTECTED)
						}
					}
				}
			])
	}

	def dispatch void infer(Agent agent, IJvmDeclaredTypeAcceptor acceptor, boolean isPreIndexingPhase) {
		acceptor.accept(agent.toClass(agent.fullyQualifiedName)).initializeLater [
			// Reset the action registry
			sarlSignatureProvider.resetSignatures(it)
			
			documentation = agent.documentation
			generateSuperTypes(agent, typeof(io.sarl.lang.core.Agent))
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
						generateAction(feature.signature as ActionSignature, feature.body)
					}
					Attribute: {
						generateAttribute(feature, JvmVisibility::PROTECTED)
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
			documentation = attr.documentation
			final = (!attr.writeable)
			static = (!attr.writeable) && (attr.initialValue!==null)
			initializer = attr.initialValue
		]
		owner.members += field
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
	
	protected def void generateCapacityDelegatorMethods(JvmGenericType owner, InheritingElement context, Capacity capacity) {
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
		for (signature : functions) {
			owner.generateAction(signature, null).setBody [
				if (signature.type != null) {
					append('''return ''')
				}
				append('''getSkill(''')
				append(context.newTypeRef(capacity.fullyQualifiedName.toString).type)
				append('''.class).«signature.name»(''')
				append(signature.params.join(', ')[name])
				append(');')
			]
		}
	}

	protected def JvmOperation generateBehaviorUnit(JvmGenericType owner, BehaviorUnit unit, int index) {
		val eventName = unit.event.name
		if (eventName!==null && !eventName.empty) {
			val behName = "_handle_" + unit.event.name + "_" + index
	
			val behaviorMethod = unit.toMethod(behName, unit.newTypeRef(Void::TYPE)) [
				documentation = unit.documentation
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
	
	protected def List<String> generateFormalParametersWithoutDefaultValue(JvmExecutable owner, boolean varargs, List<FormalParameter> params) {
		var parameterTypes = new ArrayList
		var JvmFormalParameter lastParam = null
		for (param : params) {
			lastParam = param.toParameter(param.name, param.parameterType)
			owner.parameters += lastParam
			parameterTypes.add(param.parameterType.identifier)
		}
		if (varargs && lastParam !== null) {
			lastParam.parameterType = lastParam.parameterType.addArrayTypeDimension
		}
		return parameterTypes
	}

	protected def List<String> generateFormalParametersWithDefaultValue(JvmExecutable owner, boolean varargs, List<InferredStandardParameter> signature) {
		var JvmFormalParameter lastParam = null
		val arguments = new ArrayList
		for(parameterSpec : signature) {
			if (parameterSpec instanceof InferredValuedParameter) {
				// Special case: convert a String literal to a char
				var boolean treated = false;
				var expr = parameterSpec.expr
				if (expr instanceof XStringLiteral) {
					val id = parameterSpec.type.identifier
					if (id=='char' || id=='java.lang.Character') {
						treated = true
						var str = expr.value
						if (str.length>0)
							arguments.add("'"+str.charAt(0)+"'")
						else
							arguments.add("\\0")
					}
				}
				val jExpr = new FakeTreeAppendable
				xbaseCompiler.compileAsJavaExpression(
					parameterSpec.expr, jExpr, parameterSpec.type
				)
				if (!treated){
					arguments.add(jExpr.content)
				}
				associate(parameterSpec.expr, owner)
			}
			else {
				val param = parameterSpec.parameter
				lastParam = param.toParameter(param.name, param.parameterType)
				owner.parameters += lastParam
				arguments.add(param.name)
			}
		}
		if (varargs && lastParam !== null) {
			lastParam.parameterType = lastParam.parameterType.addArrayTypeDimension
		}
		return arguments
	}

	protected def JvmOperation generateAction(JvmGenericType owner, ActionSignature signature, XExpression operationBody) {
		var returnType = signature.type
		if (returnType == null) {
			returnType = signature.newTypeRef(Void::TYPE)
		}
				
		var op = owner.toMethod(signature.name, returnType) [
			documentation = signature.documentation
			varArgs = signature.varargs
			generateFormalParametersWithoutDefaultValue(signature.varargs, signature.params)
			body = operationBody
		]
		owner.members += op

		val otherSignatures = sarlSignatureProvider.createSignature(
			sarlSignatureProvider.createFunctionID(owner, signature.name),
			signature.varargs, signature.params
		)
		
		for(otherSignature : otherSignatures) {
			op = owner.toMethod(signature.name, returnType) [
				documentation = signature.documentation
				varArgs = signature.varargs
				final = true
				val args = generateFormalParametersWithDefaultValue(
					signature.varargs, otherSignature
				)
				body = [
					append(signature.name)
					append("(")
					append(args.join(", "))
					append(");")
				]
			]
			owner.members += op
		}

		return op
	}

	protected def void generateConstructor(JvmGenericType owner, TopElement context, Constructor constructor) {
		owner.members += context.toConstructor [
			documentation = constructor.documentation
			varArgs = constructor.varargs
			generateFormalParametersWithoutDefaultValue(constructor.varargs, constructor.params)
			body = constructor.body
		]

		val otherSignatures = sarlSignatureProvider.createSignature(
			sarlSignatureProvider.createConstructorID(owner),
			constructor.varargs, constructor.params
		)
		
		for(otherSignature : otherSignatures) {
			owner.members += owner.toConstructor [
				documentation = constructor.documentation
				varArgs = constructor.varargs
				val args = generateFormalParametersWithDefaultValue(
					constructor.varargs, otherSignature
				)
				body = [
					append("this(")
					append(args.join(", "))
					append(");")
				]
			]
		}
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
	protected def JvmOperation toHashCodeMethod_Bug392440(JvmGenericType owner, EObject sourceElement, boolean extendsSomethingWithProperHashCode, JvmField ...jvmFields) {
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
	protected def JvmOperation toEqualsMethod_Bug434912(JvmGenericType owner, EObject sourceElement, JvmDeclaredType declaredType, boolean isDelegateToSuperEquals, JvmField... jvmFields) {
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

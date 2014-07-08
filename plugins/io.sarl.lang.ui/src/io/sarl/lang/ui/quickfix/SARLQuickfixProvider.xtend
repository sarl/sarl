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
package io.sarl.lang.ui.quickfix

import com.google.inject.Inject
import io.sarl.lang.sarl.Action
import io.sarl.lang.sarl.Attribute
import io.sarl.lang.sarl.BehaviorUnit
import io.sarl.lang.sarl.Capacity
import io.sarl.lang.sarl.Feature
import io.sarl.lang.sarl.FeatureContainer
import io.sarl.lang.sarl.SarlScript
import io.sarl.lang.sarl.TopElement
import io.sarl.lang.validation.IssueCodes
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.nodemodel.ICompositeNode
import org.eclipse.xtext.nodemodel.util.NodeModelUtils
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.ui.editor.model.IXtextDocument
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext
import org.eclipse.xtext.ui.editor.model.edit.ISemanticModification
import org.eclipse.xtext.ui.editor.quickfix.Fix
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor
import org.eclipse.xtext.validation.Issue
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable
import org.eclipse.xtext.xbase.ui.quickfix.XbaseQuickfixProvider
import io.sarl.lang.sarl.ParameterizedFeature

/**
 * Custom quickfixes.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#quickfixes"
 */
class SARLQuickfixProvider extends XbaseQuickfixProvider {

	@Inject
	private ReplacingAppendable.Factory appendableFactory
	
	protected def void removeBackwardWithSpaces(Issue issue, IXtextDocument document) {
		// Skip spaces before the identifier until the coma
		var index = issue.offset - 1
		while (Character::isWhitespace(document.getChar(index))) {
			index--
		}
		
		var delta = issue.offset - index - 1
		document.replace(index + 1, issue.length + delta, "")
	}
	
	protected def boolean removeBackwardUntilSeparator(Issue issue, IXtextDocument document, String separator) {
		return removeBackwardUntilSeparator(issue.offset, issue.length, document, separator)
	}

	protected def boolean removeBackwardUntilSeparator(int offset, int length, IXtextDocument document, String separator) {
		// Skip spaces before the identifier until the coma
		var index = offset - 1
		while (Character::isWhitespace(document.getChar(index))) {
			index--
		}
		
		// Test if it previous non-space character is the separator
		var boolean foundSeparator = (document.getChar(index)==separator.charAt(0))
		if (foundSeparator) {
			index--
			// Skip the previous spaces
			while (Character::isWhitespace(document.getChar(index))) {
				index--
			}

			var delta = offset - index - 1
			document.replace(index + 1, length + delta, "")
		}
		
		return foundSeparator
	}

	protected def boolean removeForwardUntilSeparator(Issue issue, IXtextDocument document, String separator) {
		// Skip spaces after the identifier until the coma
		var index = issue.offset + issue.length
		while (Character::isWhitespace(document.getChar(index))) {
			index++
		}
		
		// Test if it next non-space character is the separator
		var boolean foundSeparator = (document.getChar(index)==separator.charAt(0))
		if (foundSeparator) {
			index++
			// Skip the previous spaces
			while (Character::isWhitespace(document.getChar(index))) {
				index++
			}

			var newLength = index - issue.offset
			document.replace(issue.offset, newLength, "")
		}

		return foundSeparator
	}
	
	private def removeBackwardWithInheritingKeyword(Issue issue, IXtextDocument document) {
		// Skip spaces before the identifier
		var index = issue.offset - 1
		while (Character::isWhitespace(document.getChar(index))) {
			index--
		}
		
		// Skip non-spaces before the identifier
		while (!Character::isWhitespace(document.getChar(index))) {
			index--
		}
		
		// Skip spaces before the previous keyword
		while (Character::isWhitespace(document.getChar(index))) {
			index--
		}
		
		var delta = issue.offset - index - 1
		document.replace(index + 1, issue.length + delta, "")
	}

	protected def boolean removeBetweenSeparators(Issue issue, IXtextDocument document, String beginSeparator, String endSeparator) {
		var offset = issue.offset
		var length = issue.length
		
		// Skip spaces before the identifier until the separator
		var index = offset - 1
		while (Character::isWhitespace(document.getChar(index))) {
			index--
		}
		
		// Test if it previous non-space character is the separator
		var boolean foundSeparator = (document.getChar(index)==beginSeparator.charAt(0))
		if (foundSeparator) {
			index--
			// Skip the previous spaces
			while (Character::isWhitespace(document.getChar(index))) {
				index--
			}

			length = length + (offset - index - 1)
			offset = index + 1
			
			// Skip spaces after the identifier until the separator
			index = offset + length
			while (Character::isWhitespace(document.getChar(index))) {
				index++
			}
			
			// Test if it next non-space character is the separator
			foundSeparator = (document.getChar(index)==endSeparator.charAt(0))
			if (foundSeparator) {
				index++
	
				length = index - offset
				document.replace(offset, length, "")
			}
		}
		
		return foundSeparator
	}

	protected def int getInsertOffset(FeatureContainer container) {
		if (container.features.empty) {
			val node = NodeModelUtils.findActualNodeFor(container)
			val openingBraceNode = node.leafNodes.findFirst[text == "{"]
			if (openingBraceNode != null)
				return openingBraceNode.offset + 1
			else
				return node.endOffset
		} 
		else {
			var lastFeature = IterableExtensions.last(container.features);
			val node = NodeModelUtils.findActualNodeFor(lastFeature)
			return node.endOffset
		}
	}

	@Fix(IssueCodes::WRONG_PACKAGE)
	def fixPackageName(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==1) {
			val expectedPackage = issue.data.get(0)
			var msg = String::format("Change package declaration to '%s'", expectedPackage)
			acceptor.accept(issue, msg, msg, null)
				[ element, context |
					(element as SarlScript).name = if (""==expectedPackage) null else expectedPackage
				]
		}
	}

	@Fix(IssueCodes::DUPLICATE_TYPE_NAME)
	def fixDuplicateTopElements(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==1) {
			val duplicateName = issue.data.get(0)
			var msg = String::format("Remove the duplicate definition of '%s'", duplicateName)
			acceptor.accept(issue, msg, msg, null)
				[ element, context |
					remove(element, TopElement, context)
				]
		}
	}

	@Fix(IssueCodes::DUPLICATE_FIELD)
	def fixDuplicateAttribute(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==1) {
			val duplicateName = issue.data.get(0)
			var msg = String::format("Remove the duplicate field '%s'", duplicateName)
			acceptor.accept(issue, msg, msg, null)
				[ element, context |
					remove(element, Attribute, context)
				]
		}
	}

	@Fix(IssueCodes::DUPLICATE_METHOD)
	def fixDuplicateMethod(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==1) {
			val duplicateName = issue.data.get(0)
			var msg = String::format("Remove '%s'", duplicateName)
			acceptor.accept(issue, msg, msg, null)
				[ element, context |
					removeExecutableFeature(element, context)
				]
		}
	}
	
	protected def removeExecutableFeature(EObject element, IModificationContext context) {
		var ICompositeNode node = null
		var action = EcoreUtil2.getContainerOfType(element, Action)
		if (action===null) {
			var feature = EcoreUtil2.getContainerOfType(element, Feature)
			node = NodeModelUtils.findActualNodeFor(feature)
		}
		else {
			node = NodeModelUtils.findActualNodeFor(action)
		}
		if (node!==null) {
			remove(context.xtextDocument, node)
		}		
	}

	@Fix(IssueCodes::INVALID_MEMBER_NAME)
	def fixActionName(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==3) {
			val type = issue.data.get(0)
			val invalidName = issue.data.get(1)
			val validName = issue.data.get(2)
			var msg = String::format("Rename the %s '%s' to '%s'", type, invalidName, validName)
			acceptor.accept(issue, msg, msg, null)
				[ context |
					context.xtextDocument.replace(issue.offset, issue.length, validName);
				]
			msg = String::format("Remove the %s '%s'", type, invalidName)
			if (type=="attribute") {
				acceptor.accept(issue, msg, msg, null)
					[ element, context |
						remove(element, Attribute, context)
					]
			}
			else {
				acceptor.accept(issue, msg, msg, null)
					[ element, context |
						removeExecutableFeature(element, context)
					]
			}
		}
	}
	
	@Fix(IssueCodes::REDUNDANT_INTERFACE_IMPLEMENTATION)
	def fixRedundantInterface(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==2) {
			val redundantName = issue.data.get(0)
			val mode = issue.data.get(1)
			var msg = String::format("Remove the redundant feature '%s'", redundantName)
			var ISemanticModification fct
			switch(mode) {
				case "pre": {
					fct = [ element, context |
						var document = context.xtextDocument
						removeBackwardUntilSeparator(issue, document, ',')
					]
				}
				case "post": {
					fct = [ element, context |
						var document = context.xtextDocument
						removeForwardUntilSeparator(issue, document, ',')
					]
				}
				default: {
					fct = [ element, context |
						var document = context.xtextDocument
						var sep = ','
						if (!removeBackwardUntilSeparator(issue, document, sep)) {
							if (!removeForwardUntilSeparator(issue, document, sep)) {
								removeBackwardWithInheritingKeyword(issue, document)
							}
						}
					]
				}
			}
			acceptor.accept(issue, msg, msg, null, fct)
		}
	}
	
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes::VARIABLE_NAME_SHADOWING)
	def fixVariableNameShadowing(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==2) {
			val redundantName = issue.data.get(0)
			val newName = issue.data.get(1)
			var msg = String::format("Remove the field '%s'", redundantName)
			acceptor.accept(issue, msg, msg, null) [ element, context |
				remove(element, Attribute, context)
			]
			msg = String::format("Rename the field '%s' to '%s'", redundantName, newName)
			acceptor.accept(issue, msg, msg, null) [ context |
				var document = context.xtextDocument
				document.replace(issue.offset, issue.length, newName)
			]
		}
	}

	@Fix(IssueCodes::OVERRIDDEN_FINAL_OPERATION)
	def fixOverriddenFinal(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==1) {
			val signature = issue.data.get(0)
			var msg = String::format("Remove the action '%s'", signature)
			acceptor.accept(issue, msg, msg, null) [ element, context |
				removeExecutableFeature(element, context)
			]
		}
	}

	@Fix(IssueCodes::DISCOURAGED_BOOLEAN_EXPRESSION)
	def fixDiscouragedBooleanExpression(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==0) {
			var msg = "Remove the guard"
			acceptor.accept(issue, msg, msg, null) [ context |
				var document = context.xtextDocument
				removeBetweenSeparators(issue, document, '[', ']')
			]
		}
	}

	@Fix(IssueCodes::UNREACHABLE_BEHAVIOR_UNIT)
	def fixUnreachableBehaviorUnit(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==1) {
			val eventName = issue.data.get(0)
			var msg = String::format("Remove the behavior unit on '%s'", eventName)
			acceptor.accept(issue, msg, msg, null) [ element, context |
				remove(element, BehaviorUnit, context)
			]
		}
	}

	@Fix(IssueCodes::INVALID_CAPACITY_TYPE)
	def fixInvalidCapacityType(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==1) {
			val typeName = issue.data.get(0)
			var msg = String::format("Remove the type '%s'", typeName)
			acceptor.accept(issue, msg, msg, null) [ element, context |
				var document = context.xtextDocument
				var sep = ','
				if (!removeBackwardUntilSeparator(issue, document, sep)) {
					if (!removeForwardUntilSeparator(issue, document, sep)) {
						removeBackwardWithInheritingKeyword(issue, document)
					}
				}
			]
		}
	}

	@Fix(IssueCodes::INVALID_FIRING_EVENT_TYPE)
	def fixInvalidFiringEventType(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==1) {
			val typeName = issue.data.get(0)
			var msg = String::format("Remove the type '%s'", typeName)
			acceptor.accept(issue, msg, msg, null) [ element, context |
				var document = context.xtextDocument
				var sep = ','
				if (!removeBackwardUntilSeparator(issue, document, sep)) {
					if (!removeForwardUntilSeparator(issue, document, sep)) {
						removeBackwardWithInheritingKeyword(issue, document)
					}
				}
			]
		}
	}

	@Fix(IssueCodes::INVALID_IMPLEMENTED_TYPE)
	def fixInvalidImplementedCapacityType(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==1) {
			val typeName = issue.data.get(0)
			var msg = String::format("Remove the type '%s'", typeName)
			acceptor.accept(issue, msg, msg, null) [ element, context |
				var document = context.xtextDocument
				var sep = ','
				if (!removeBackwardUntilSeparator(issue, document, sep)) {
					if (!removeForwardUntilSeparator(issue, document, sep)) {
						removeBackwardWithInheritingKeyword(issue, document)
					}
				}
			]
		}
	}

	@Fix(IssueCodes::INVALID_EXTENDED_TYPE)
	def fixInvalidExtendedCapacityType(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==1) {
			val typeName = issue.data.get(0)
			var msg = String::format("Remove the type '%s'", typeName)
			acceptor.accept(issue, msg, msg, null) [ element, context |
				var document = context.xtextDocument
				var sep = ','
				if (!removeBackwardUntilSeparator(issue, document, sep)) {
					if (!removeForwardUntilSeparator(issue, document, sep)) {
						removeBackwardWithInheritingKeyword(issue, document)
					}
				}
			]
		}
	}

	@Fix(IssueCodes::DISCOURAGED_CAPACITY_DEFINITION)
	def fixDiscouragedCapacityDefinition(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==2) {
			val capacityName = issue.data.get(0)
			val defaultActionName = issue.data.get(1)
			var msg = String::format("Remove the capacity '%s'", capacityName)
			acceptor.accept(issue, msg, msg, null) [ element, context |
				remove(element, Capacity, context)
			]
			msg = String::format("Add the action '%s'", defaultActionName)
			acceptor.accept(issue, msg, msg, null) [ element, context |
				var container = EcoreUtil2.getContainerOfType(element, FeatureContainer)
				if (container!==null) {
					var insertOffset = getInsertOffset(container)
					var document = context.getXtextDocument()
					var appendable = appendableFactory.create(document, element.eResource() as XtextResource, insertOffset, 0)
					if (container.features.empty) {
						appendable.increaseIndentation()
					}
					appendable.newLine().append("def ").append(defaultActionName)
					appendable.commitChanges()
				}
			]
		}
	}
	
	@Fix(IssueCodes::MISSING_METHOD_IMPLEMENTATION)
	def fixMissingMethodImplementation(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length>=1) {
			var lines = ""
			val methods = newTreeMap(null)
			for(var i=0; i<issue.data.size; i+=2) {
				var meth = issue.data.get(i)
				lines += String::format("- %s\n", meth)
				methods.put(meth, issue.data.get(i+1))
			}
			val description = String::format("Add the following unimplemented actions:\n%s", lines)
			acceptor.accept(issue, "Add unimplemented actions", description, null) [ element, context |
				var container = EcoreUtil2.getContainerOfType(element, FeatureContainer)
				if (container!==null) {
					var insertOffset = getInsertOffset(container)
					var document = context.getXtextDocument()
					var appendable = appendableFactory.create(document, element.eResource() as XtextResource, insertOffset, 0)
					if (container.features.empty) {
						appendable.increaseIndentation()
					}
					for(meth : methods.entrySet) {
						appendable.newLine().append(meth.key).append(" {")
						appendable.increaseIndentation()
						appendable.newLine().append("// TODO: Auto-generated action.")
						var value = meth.value
						if (value!==null && value!="") {
							appendable.newLine().append(value)
						}
						appendable.decreaseIndentation()
						appendable.newLine().append("}")
					}
					appendable.commitChanges()
				}
			]
		}
	}

	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes::INCOMPATIBLE_RETURN_TYPE)
	def fixIncompatibleReturnType(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.data!=null && issue.data.length==2) {
			val expectedType = issue.data.get(1)
			var msg = String::format("Replace the type by '%s'", expectedType)
			acceptor.accept(issue, msg, msg, null) [ element, context |
				var document = context.xtextDocument
				document.replace(issue.offset, issue.length, expectedType)
			]
		}
	}

	@Fix(IssueCodes.INVALID_USE_OF_VAR_ARG)
	def fixNoDefaultValueForVariadicParameter(Issue issue, IssueResolutionAcceptor acceptor) {
		var String msg
		msg = "Remove the variadic parameter"
		acceptor.accept(issue, msg, msg, null) [ element, context |
			var document = context.xtextDocument
			removeBackwardWithSpaces(issue, document)
		]
		msg = "Remove the default value"
		acceptor.accept(issue, msg, msg, null) [ element, context |
			var container = EcoreUtil2.getContainerOfType(element, ParameterizedFeature)
			if (container!==null && !container.params.empty) {
				var formalParameter = container.params.last
				if (formalParameter.defaultValue!==null) {
					val node = NodeModelUtils.findActualNodeFor(formalParameter.defaultValue)
					if (node!==null) {
						var document = context.xtextDocument
						removeBackwardUntilSeparator(node.offset, node.length, document, "=")
					}
				}
			}
		]
	}
	
}

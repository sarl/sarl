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
package io.sarl.lang.ui.labeling

import com.google.inject.Inject
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
import io.sarl.lang.sarl.SarlScript
import io.sarl.lang.sarl.Skill
import io.sarl.lang.ui.images.SARLImages
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.jface.viewers.StyledString
import org.eclipse.xtext.common.types.JvmConstructor
import org.eclipse.xtext.common.types.JvmExecutable
import org.eclipse.xtext.common.types.JvmField
import org.eclipse.xtext.common.types.JvmOperation
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.common.types.JvmVisibility
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider
import org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices
import org.eclipse.xtext.xbase.ui.labeling.XbaseLabelProvider
import org.eclipse.xtext.xbase.validation.UIStrings

/**
 * Provides labels for a EObjects.
 * 
 * see http://www.eclipse.org/Xtext/documentation.html#labelProvider
 */
class SARLLabelProvider extends XbaseLabelProvider {

	@Inject UIStrings uiStrings
	@Inject OperatorMapping operatorMapping
	@Inject	CommonTypeComputationServices services
	@Inject ILogicalContainerProvider logicalContainerProvider;
	@Inject SARLImages images
	
	@Inject
	new(AdapterFactoryLabelProvider delegate) {
		super(delegate);
	}
	
	protected def <T> T jvmElement(EObject element, Class<T> type) {
		for(obj : services.jvmModelAssociations.getJvmElements(element)) {
			if (type.isInstance(obj)) {
				return type.cast(obj)
			}
		}
		return null;
	}

	// Descriptors
	
	protected def dispatch ImageDescriptor imageDescriptor(SarlScript element) {
		images.forFile
	}
	
	protected def dispatch ImageDescriptor imageDescriptor(Agent element) {
		images.forAgent
	}
	
	protected def dispatch ImageDescriptor imageDescriptor(Event element) {
		images.forEvent
	}

	protected def dispatch ImageDescriptor imageDescriptor(Capacity element) {
		images.forCapacity
	}

	protected def dispatch ImageDescriptor imageDescriptor(Skill element) {
		images.forSkill
	}

	protected def dispatch ImageDescriptor imageDescriptor(Behavior element) {
		images.forBehavior
	}

	protected def dispatch ImageDescriptor imageDescriptor(Attribute element) {
		images.forAttribute(element.writeable)
	}

	protected def dispatch ImageDescriptor imageDescriptor(Constructor element) {
		images.forConstructor(JvmVisibility::PUBLIC, 0)
	}

	protected def dispatch ImageDescriptor imageDescriptor(Action element) {
		images.forAction
	}

	protected def dispatch ImageDescriptor imageDescriptor(ActionSignature element) {
		images.forActionSignature
	}

	protected def dispatch ImageDescriptor imageDescriptor(CapacityUses element) {
		images.forCapacityUses
	}

	protected def dispatch ImageDescriptor imageDescriptor(RequiredCapacity element) {
		images.forCapacityRequirements
	}

	protected def dispatch ImageDescriptor imageDescriptor(BehaviorUnit element) {
		images.forBehaviorUnit
	}

	// Texts
	
	protected def text(JvmParameterizedTypeReference element) {
		var s = element.simpleName.convertToStyledString
		if (element.arguments!==null && !element.arguments.empty) {
			s.append("<")
			var boolean b = false
			for(t : element.arguments) {
				if (b) s.append(", ")
				else b = true
				s.append(text(t))
			}
			s.append(">")
		}
		return s
	}

	protected def text(JvmTypeReference element) {
		uiStrings.referenceToString(element, element.simpleName)
	}

	protected def text(SarlScript element) {
		element.eResource.URI.trimFileExtension.lastSegment
	}
	
	protected def text(Agent element) {
		element.name
	}
	
	protected def text(Event element) {
		element.name
	}

	protected def text(Capacity element) {
		element.name
	}

	protected def text(Skill element) {
		element.name
	}

	protected def text(Behavior element) {
		element.name
	}

	protected def text(Attribute element) {
		var jvmElement = element.jvmElement(JvmField)
		if (jvmElement !== null) {
			element.name.convertToStyledString
				.append(" : " + jvmElement.type.simpleName, StyledString::COUNTER_STYLER)
		}
		else if (element.type !== null) {
			element.name.convertToStyledString
				.append(" : " + element.type.simpleName, StyledString::COUNTER_STYLER)
		}
		else {
			element.name.convertToStyledString
		}
	}
	
	protected def StyledString signatureWithoutReturnType(StyledString simpleName, JvmExecutable element) {
		simpleName.convertToStyledString.append(
			uiStrings.parameters(element)
		)
	}

	protected def text(Constructor element) {
		var jvmElement = element.jvmElement(JvmConstructor)
		if (jvmElement!==null) {
			var container = logicalContainerProvider.getNearestLogicalContainer(element)
			if (container!==null) {
				signatureWithoutReturnType(
					container.simpleName.convertToStyledString,
					jvmElement)
			}
			else {
				signatureWithoutReturnType(
					new StyledString("new", StyledString::DECORATIONS_STYLER),
					jvmElement)
			}
		}
	}

	protected def text(Action element) {
		if (element.signature!==null)
			(element.signature as ActionSignature).toText
		else
			"" 
	}
	
	private def toText(ActionSignature element) {
		val simpleName = element.name
		if (simpleName != null) {
			val qnName = QualifiedName.create(simpleName)
			val operator = operatorMapping.getOperator(qnName)
			if (operator != null) {
				val result = signature(operator.firstSegment, element.jvmElement(JvmExecutable))
				result.append(' (' + simpleName + ')', StyledString::COUNTER_STYLER)
				return result
			}
		}
		return signature(element.name, element.jvmElement(JvmOperation))
	}

	protected def text(ActionSignature element) {
		element.toText
	}

	protected def text(CapacityUses element) {
		"capacity uses".convertToString
	}

	protected def text(RequiredCapacity element) {
		"required capacities".convertToString
	}

	protected def text(BehaviorUnit element) {
		var s = new StyledString("on ", StyledString::DECORATIONS_STYLER)
		s.append(element.event.simpleName)
		if (element.guard!==null) {
			s.append(" [guarded]", StyledString::DECORATIONS_STYLER)
		}
		s
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.ui.contentassist;

/** Provides proposal for the content assist mechanism.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#contentAssist"
 */
public class SARLProposalProvider extends AbstractSARLProposalProvider {

// FIXME: The following code is commented out due to the issue #123

//	@Inject extension IQualifiedNameProvider
//
//	@Inject private var IJvmTypeProvider.Factory jvmTypeProviderFactory
//    @Inject private var ITypesProposalProvider typeProposalProvider
//	@Inject private ILogicalContainerProvider logicalContainerProvider
//
//	override completeEvent_SuperTypes(
//		EObject model, Assignment assignment,
//		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
//		if (model instanceof Event) {
//			model.buildProposalsWithout(
//				model.fullyQualifiedName.toString,
//				"io.sarl.lang.core.Event", context,
//				acceptor, SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
//				IJavaSearchConstants::CLASS
//			)
//		}
//	}
//
//	override completeCapacity_SuperTypes(
//		EObject model, Assignment assignment,
//		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
//		if (model instanceof Capacity) {
//			model.buildProposalsWithout(
//				model.fullyQualifiedName.toString,
//				"io.sarl.lang.core.Capacity", context,
//				acceptor, SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
//				IJavaSearchConstants::INTERFACE
//			)
//		}
//	}
//
//	override completeSkill_SuperTypes(
//		EObject model, Assignment assignment,
//		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
//		if (model instanceof Skill) {
//			model.buildProposalsWithout(
//				model.fullyQualifiedName.toString,
//				"io.sarl.lang.core.Skill", context,
//				acceptor, SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
//				IJavaSearchConstants::CLASS
//			)
//		}
//	}
//
//	override completeBehavior_SuperTypes(
//		EObject model, Assignment assignment,
//		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
//		if (model instanceof Behavior) {
//			model.buildProposalsWithout(
//				model.fullyQualifiedName.toString,
//				"io.sarl.lang.core.Behavior", context,
//				acceptor, SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
//				IJavaSearchConstants::CLASS
//			)
//		}
//	}
//
//	override completeAgent_SuperTypes(
//		EObject model, Assignment assignment,
//		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
//		if (model instanceof Agent) {
//			model.buildProposalsWithout(
//				model.fullyQualifiedName.toString,
//				"io.sarl.lang.core.Agent", context,
//				acceptor, SarlPackage.Literals::INHERITING_ELEMENT__SUPER_TYPES,
//				IJavaSearchConstants::CLASS
//			)
//		}
//	}
//
//	override completeSkill_ImplementedTypes(
//		EObject model, Assignment assignment,
//		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
//		if (model instanceof Skill) {
//			model.buildProposalsWithout(
//				model.fullyQualifiedName.toString,
//				"io.sarl.lang.core.Capacity", context,
//				acceptor, SarlPackage.Literals::IMPLEMENTING_ELEMENT__IMPLEMENTED_TYPES,
//				IJavaSearchConstants::INTERFACE
//			)
//		}
//	}
//
//	override completeBehaviorUnit_Event(
//		EObject model, Assignment assignment,
//		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
//		if (model instanceof BehaviorUnit) {
//			var container = logicalContainerProvider.getLogicalContainer(model)
//			if (container!==null) {
//				model.buildProposalsWith(
//					container.fullyQualifiedName.toString,
//					"io.sarl.lang.core.Event", context,
//					acceptor, SarlPackage.Literals::BEHAVIOR_UNIT__EVENT,
//					IJavaSearchConstants::CLASS
//				)
//			}
//		}
//	}
//
//	override completeCapacityUses_CapacitiesUsed(
//		EObject model, Assignment assignment,
//		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
//		if (model instanceof CapacityUses) {
//			var container = logicalContainerProvider.getLogicalContainer(model)
//			if (container!==null) {
//				model.buildProposalsWith(
//					container.fullyQualifiedName.toString,
//					"io.sarl.lang.core.Capacity", context,
//					acceptor, SarlPackage.Literals::CAPACITY_USES__CAPACITIES_USED,
//					IJavaSearchConstants::INTERFACE
//				)
//			}
//		}
//	}
//
//	override completeRequiredCapacity_RequiredCapacities(
//		EObject model, Assignment assignment,
//		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
//		if (model instanceof RequiredCapacity) {
//			var container = logicalContainerProvider.getLogicalContainer(model)
//			if (container!==null) {
//				model.buildProposalsWith(
//					container.fullyQualifiedName.toString,
//					"io.sarl.lang.core.Capacity", context,
//					acceptor, SarlPackage.Literals::REQUIRED_CAPACITY__REQUIRED_CAPACITIES,
//					IJavaSearchConstants::INTERFACE
//				)
//			}
//		}
//	}
//
//	override completeActionSignature_FiredEvents(
//		EObject model, Assignment assignment,
//		ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
//		if (model instanceof ActionSignature) {
//			var container = logicalContainerProvider.getLogicalContainer(model)
//			if (container!==null) {
//				model.buildProposalsWith(
//					container.fullyQualifiedName.toString,
//					"io.sarl.lang.core.Event", context,
//					acceptor, SarlPackage.Literals::ACTION_SIGNATURE__FIRED_EVENTS,
//					IJavaSearchConstants::CLASS
//				)
//			}
//		}
//	}
//
//	//----------------------------
//
//	private def buildProposalsWithout(EObject event, String containerFullyQualifiedName, String superTypeName,
//			ContentAssistContext context, ICompletionProposalAcceptor acceptor, EReference reference, int expectedType) {
//		val jvmTypeProvider = jvmTypeProviderFactory.createTypeProvider(event.eResource.resourceSet)
//		val interfaceToImplement = jvmTypeProvider.findTypeByName(superTypeName)
//		typeProposalProvider.createSubTypeProposals(
//			interfaceToImplement,
//			this,
//			context,
//			reference,
//			new SARLProposalProvider.TypeFilter(
//				expectedType,
//				superTypeName,
//				containerFullyQualifiedName),
//			acceptor
//		);
//	}
//
//	private def buildProposalsWith(EObject event, String containerFullyQualifiedName, String superTypeName,
//			ContentAssistContext context, ICompletionProposalAcceptor acceptor, EReference reference, int expectedType) {
//		val jvmTypeProvider = jvmTypeProviderFactory.createTypeProvider(event.eResource.resourceSet)
//		val interfaceToImplement = jvmTypeProvider.findTypeByName(superTypeName)
//		typeProposalProvider.createSubTypeProposals(
//			interfaceToImplement,
//			this,
//			context,
//			reference,
//			new SARLProposalProvider.TypeFilter(
//				expectedType,
//				containerFullyQualifiedName),
//			acceptor
//		);
//	}
//
//	private static class TypeFilter implements ITypesProposalProvider.Filter {
//
//		private val int searchFor
//		private val String[] typeNames
//
//		new (int searchFor, String... typeNames) {
//			this.searchFor = searchFor
//			this.typeNames = typeNames
//		}
//
//		override accept(int modifiers,
//			char[] packageName, char[] simpleTypeName,
//			char[][] enclosingTypeNames, String path) {
//			if (Flags::isFinal(modifiers)
//				|| !Flags::isPublic(modifiers)
//				|| (searchFor==IJavaSearchConstants::INTERFACE && !Flags::isInterface(modifiers))
//				|| (searchFor==IJavaSearchConstants::CLASS &&
//						(Flags::isInterface(modifiers) || Flags::isEnum(modifiers))
//						 || Flags::isAnnotation(modifiers))) {
//				return false
//			}
//			var fqName = new StringBuilder(packageName.length + simpleTypeName.length + 1)
//			if (packageName.length != 0) {
//				fqName.append(packageName)
//				fqName.append('.')
//			}
//			for(char[] enclosingType: enclosingTypeNames) {
//				fqName.append(enclosingType);
//				fqName.append('.')
//			}
//			fqName.append(simpleTypeName)
//			var str = fqName.toString
//			for(exception : this.typeNames) {
//				if (str == exception) return false
//			}
//			return true
//		}
//
//		override getSearchFor() {
//			this.searchFor
//		}
//
//	}

}

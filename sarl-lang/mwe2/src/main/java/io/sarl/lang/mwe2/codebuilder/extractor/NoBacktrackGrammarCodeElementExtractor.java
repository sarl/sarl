/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.mwe2.codebuilder.extractor;

import java.util.Collections;
import java.util.HashSet;

import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Functions.Function4;
import org.eclipse.xtext.xbase.lib.IterableExtensions;

import com.google.common.collect.Iterables;
import com.google.inject.Singleton;

import io.sarl.lang.mwe2.codebuilder.config.CodeBuilderConfig;

/** Extract elements from the no-bracktracking grammar.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class NoBacktrackGrammarCodeElementExtractor extends AbstractCodeElementExtractor {

	@Override
	public Iterable<ElementDescription> getTopElements(Grammar grammar, CodeBuilderConfig config) {
		final var topRule = GrammarUtil.findRuleForName(grammar, config.getTopElementRuleName());
		if (topRule != null) {
			final var commonType = getGeneratedTypeFor(topRule);
			final var filteredElements = Iterables.filter(GrammarUtil.containedActions(topRule),
				it -> !Strings.isEmpty(it.getFeature()));
			return Iterables.transform(filteredElements, it -> {
				final var classifier = it.getType().getClassifier();
				final var name = classifier.getName();
				final var container = GrammarUtil.containingGroup(it);
				return newElementDescription(name, container, classifier, commonType);
			});
		}
		return Collections.emptyList();
	}

	private <T> T visitMembers(EObject grammarContainer, EObject container,
			Function4<? super CodeElementExtractor, ? super EObject, ? super EObject, ? super EClassifier, ? extends T> memberCallback) {
		final var treatedMembers = new HashSet<String>();
		for (final var nameAssignment : IterableExtensions.filter(
				GrammarUtil.containedAssignments(container), passignment -> Boolean.valueOf(getCodeBuilderConfig()
				.getMemberNameExtensionGrammarName().equals(passignment.getFeature())))) {
			// Get the container of the name assignment
			final var assignmentContainer = getContainerInRule(grammarContainer, nameAssignment);
			if (assignmentContainer != null) {
				final var classifier = getGeneratedTypeFor(assignmentContainer);
				if (!treatedMembers.contains(classifier.getName())) {
					treatedMembers.add(classifier.getName());
					final var retVal = memberCallback.apply(this, grammarContainer, assignmentContainer, classifier);
					if (retVal != null) {
						return retVal;
					}
				}
			}
		}
		return null;
	}

	private <T> T visitTypeReferencingMembers(EObject grammarContainer, EObject container,
			Function4<? super CodeElementExtractor, ? super EObject, ? super EObject, ? super EClassifier, ? extends T> memberCallback) {
		final var treatedMembers = new HashSet<String>();
		for (final var nameAssignment : IterableExtensions.filter(
				GrammarUtil.containedAssignments(container), passignment -> Boolean.valueOf(getCodeBuilderConfig()
				.getJvmTypeNamedMemberExtensionGrammarNames().contains(passignment.getFeature())))) {
			// Get the container of the name assignment
			final var assignmentContainer = getContainerInRule(grammarContainer, nameAssignment);
			if (assignmentContainer != null) {
				final var classifier = getGeneratedTypeFor(assignmentContainer);
				if (!treatedMembers.contains(classifier.getName())) {
					treatedMembers.add(classifier.getName());
					final var retVal = memberCallback.apply(this, grammarContainer, assignmentContainer, classifier);
					if (retVal != null) {
						return retVal;
					}
				}
			}
		}
		return null;
	}

	private <T> T visitIndirectlyNamedMembers(EObject grammarContainer, EObject container,
			Function4<? super CodeElementExtractor, ? super EObject, ? super EObject, ? super EClassifier, ? extends T> memberCallback) {
		final var treatedMembers = new HashSet<String>();
		for (final var nameAssignment : IterableExtensions.filter(
				GrammarUtil.containedAssignments(container), passignment -> Boolean.valueOf(getCodeBuilderConfig()
				.getIndirectlyNamedMemberExtensionGrammarNames().contains(passignment.getFeature())))) {
			// Get the container of the name assignment
			final var assignmentContainer = getContainerInRule(grammarContainer, nameAssignment);
			if (assignmentContainer != null) {
				final var classifier = getGeneratedTypeFor(assignmentContainer);
				if (!treatedMembers.contains(classifier.getName())) {
					treatedMembers.add(classifier.getName());
					final var retVal = memberCallback.apply(this, grammarContainer, assignmentContainer, classifier);
					if (retVal != null) {
						return retVal;
					}
				}
			}
		}
		return null;
	}

	private <T> T visitConstructors(EObject grammarContainer, EObject container,
			Function4<? super CodeElementExtractor, ? super EObject, ? super EObject, ? super EClassifier, ? extends T> callback) {
		final var treatedConstructors = new HashSet<String>();
		for (final var expressionAssignment : IterableExtensions.filter(
				GrammarUtil.containedAssignments(container), passignment -> Boolean.valueOf(getCodeBuilderConfig()
				.getMemberBlockExpressionExtensionGrammarName().equals(passignment.getFeature())))) {
			// Get the container of the name assignment
			final var consContainer = getContainerInRule(grammarContainer, expressionAssignment);
			if (consContainer != null
				&& !IterableExtensions.exists(GrammarUtil.containedAssignments(consContainer),
					it -> Boolean.valueOf(getCodeBuilderConfig()
					.getMemberNameExtensionGrammarName().equals(it.getFeature())))) {
				final var classifier = getGeneratedTypeFor(consContainer);
				if (!treatedConstructors.contains(classifier.getName())) {
					treatedConstructors.add(classifier.getName());
					final var retVal = callback.apply(this, grammarContainer, consContainer, classifier);
					if (retVal != null) {
						return retVal;
					}
				}
			}
		}
		return null;
	}

	@Override
	public <T> T visitMemberElements(
			ElementDescription element,
			EObject grammarContainer,
			Function4<? super CodeElementExtractor, ? super EObject, ? super EObject, ? super EClassifier, ? extends T> constructorCallback,
			Function4<? super CodeElementExtractor, ? super EObject, ? super EObject, ? super EClassifier, ? extends T> namedMemberCallback,
			Function4<? super CodeElementExtractor, ? super EObject, ? super EObject,
					? super EClassifier, ? extends T> typeReferencingMemberCallback,
			Function4<? super CodeElementExtractor, ? super EObject, ? super EObject,
					? super EClassifier, ? extends T> indirectlyNamedMemberCallback) {
		// Treat the standard members
		if (namedMemberCallback != null) {
			final var retVal = visitMembers(grammarContainer,  grammarContainer, namedMemberCallback);
			if (retVal != null) {
				return retVal;
			}
		}
		// Treat the members that are referencing types.
		if (typeReferencingMemberCallback != null) {
			final var retVal = visitTypeReferencingMembers(grammarContainer,  grammarContainer, typeReferencingMemberCallback);
			if (retVal != null) {
				return retVal;
			}
		}
		// Treat the named members that are indirectly named.
		if (indirectlyNamedMemberCallback != null) {
			final var retVal = visitIndirectlyNamedMembers(grammarContainer,  grammarContainer, indirectlyNamedMemberCallback);
			if (retVal != null) {
				return retVal;
			}
		}
		// Treat the constructors
		if (constructorCallback != null
			&& !getCodeBuilderConfig().getConstructorFreeTypes().contains(element.name())) {
			final var retVal = visitConstructors(grammarContainer, grammarContainer, constructorCallback);
			if (retVal != null) {
				return retVal;
			}
		}
		return null;
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

import java.util.NoSuchElementException;
import java.util.Objects;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Action;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.XtextGeneratorNaming;
import org.eclipse.xtext.xtext.generator.model.TypeReference;
import org.eclipse.xtext.xtext.generator.util.GenModelUtil2;

import com.google.common.collect.Iterables;
import com.google.inject.Inject;

import io.sarl.lang.mwe2.codebuilder.config.CodeBuilderConfig;

/** Abstract implementation for extracting elements from the backtracking grammar.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractCodeElementExtractor implements CodeElementExtractor {

	private static final String ANNOTATION_INFO_FIELD_NAME = "annotationInfo"; //$NON-NLS-1$

	@Inject
	private XtextGeneratorNaming naming;

	@Inject
	private CodeBuilderConfig configuration;
	
	private Grammar grammar;

	/** Replies the code builder configuration.
	 *
	 * @return the configuration.
	 */
	protected CodeBuilderConfig getCodeBuilderConfig() {
		return this.configuration;
	}

	@Override
	public TypeReference newTypeReference(EClassifier classifier) {
		if (classifier == null) {
			return new TypeReference(Object.class);
		}
		final var name = GenModelUtil2.getJavaTypeName(classifier, classifier.eResource().getResourceSet());
		if (Strings.isEmpty(name)) {
			return new TypeReference(Object.class);
		}
		return new TypeReference(name);
	}

	@Override
	public TypeReference newTypeReference(Class<?> type) {
		if (type == null) {
			return new TypeReference(Object.class);
		}
		return new TypeReference(type);
	}

	@Override
	public ElementDescription newElementDescription(String name, EObject grammarComponent, EClassifier elementType,
			TypeReference commonType) {
		final var interfaceType = getElementBuilderInterface(name);
		final var implementationType = getElementBuilderImpl(name);
		final var customImplementationType = getElementBuilderImplCustom(name);
		final var appenderType = getElementAppenderImpl(name);
		final var isAnnotationInfo = findAction(grammarComponent, getAnnotationInfoFieldName()) != null;
		return new ElementDescription(
				Strings.toFirstUpper(name), grammarComponent,
				newTypeReference(elementType), commonType,
				interfaceType, implementationType, customImplementationType, appenderType,
				isAnnotationInfo);
	}

	/** Replies the name of the Xtend annotation info field.
	 *
	 * @return the name of the annotation info field.
	 */
	@SuppressWarnings("static-method")
	public String getAnnotationInfoFieldName() {
		return ANNOTATION_INFO_FIELD_NAME;
	}

	/** Replies the assignment component with the given nazme in the given grammar component.
	 *
	 * @param grammarComponent the component to explore.
	 * @param assignmentName the name of the assignment to search for.
	 * @return the assignment component.
	 */
	protected static Action findAction(EObject grammarComponent, String assignmentName) {
		for (final var action : GrammarUtil.containedActions(grammarComponent)) {
			if (GrammarUtil.isAssignedAction(action)) {
				if (Objects.equals(assignmentName, action.getFeature())) {
					return action;
				}
			}
		}
		return null;
	}

	@Override
	public EClassifier getGeneratedTypeFor(EObject grammarElement) {
		if (grammarElement == null) {
			return null;
		}
		if (grammarElement instanceof AbstractRule cvalue) {
			return getGeneratedTypeFor(cvalue);
		}
		try {
			return Iterables.find(GrammarUtil.containedActions(grammarElement),
				it -> !Strings.isEmpty(it.getFeature())).getType().getClassifier();
		} catch (NoSuchElementException e) {
			return null;
		}
	}

	@Override
	public EClassifier getGeneratedTypeFor(AbstractRule rule) {
		final var actions = GrammarUtil.containedActions(rule);
		final EClassifier classifier;
		if (actions.isEmpty()) {
			classifier = rule.getType().getClassifier();
		} else {
			classifier = actions.get(0).getType().getClassifier();
		}
		return classifier;
	}

	/** Replies the container in the grammar rule for the given content element.
	 *
	 * @param root the biggest enclosing element to consider in the grammar.
	 * @param content the grammar element to search for the container.
	 * @return the container of the content.
	 */
	protected EObject getContainerInRule(EObject root, EObject content) {
		var container = content;
		do {
			final var classifier = getGeneratedTypeFor(container);
			if (classifier != null) {
				return container;
			}
			container = container.eContainer();
		} while (container != root);
		final var classifier = getGeneratedTypeFor(root);
		if (classifier != null) {
			return root;
		}
		return null;
	}

	@Override
	public void initialize(Grammar grammar) {
		this.grammar = grammar;
	}

	/** Replies the grammar.
	 *
	 * @return the grammar.
	 */
	protected Grammar getGrammar() {
		return this.grammar;
	}

	/** Replies the naming conventions.
	 *
	 * @return the naming conventions.
	 */
	@Pure
	protected XtextGeneratorNaming getNaming() {
		return this.naming;
	}

	@Pure
	@Override
	public String getLanguageBasePackage() {
		final var grammar = getGrammar();
		final var basePackage = getNaming().getRuntimeBasePackage(grammar);
		final var ecorePackage = basePackage + "." //$NON-NLS-1$
				+ GrammarUtil.getSimpleName(grammar).toLowerCase();
		return ecorePackage;
	}

	@Pure
	@Override
	public TypeReference getLanguageScriptInterface() {
		final var rule = GrammarUtil.findRuleForName(getGrammar(), this.configuration.getScriptRuleName());
		final var type = getGeneratedTypeFor(rule);
		return newTypeReference(type);
	}

	@Pure
	@Override
	public String getBasePackage() {
		final var grammar = getGrammar();
		final var basePackage = getNaming().getRuntimeBasePackage(grammar);
		return basePackage + ".codebuilder"; //$NON-NLS-1$
	}

	@Pure
	@Override
	public String getAppenderPackage() {
		return getBasePackage() + ".appenders"; //$NON-NLS-1$
	}

	@Pure
	@Override
	public String getDocumentationPackage() {
		final var grammar = getGrammar();
		final var basePackage = getNaming().getRuntimeBasePackage(grammar);
		return basePackage + ".documentation"; //$NON-NLS-1$
	}

	@Pure
	@Override
	public TypeReference getInnerBlockDocumentationAdapter() {
		return new TypeReference(getDocumentationPackage()
				+ ".InnerBlockDocumentationAdapter"); //$NON-NLS-1$
	}

	@Pure
	@Override
	public String getSerializerPackage() {
		final var grammar = getGrammar();
		final var basePackage = getNaming().getRuntimeBasePackage(grammar);
		return basePackage + ".serializer"; //$NON-NLS-1$
	}

	@Pure
	@Override
	public TypeReference getElementAppenderImpl(String elementName) {
		return new TypeReference(getAppenderPackage() + "." //$NON-NLS-1$
				+ Strings.toFirstUpper(elementName) + "SourceAppender"); //$NON-NLS-1$
	}

	@Pure
	@Override
	public TypeReference getElementAppenderImplCustom(String elementName) {
		return new TypeReference(getAppenderPackage() + "." //$NON-NLS-1$
				+ Strings.toFirstUpper(elementName) + "SourceAppenderCustom"); //$NON-NLS-1$
	}

	@Pure
	@Override
	public TypeReference getAbstractAppenderImpl() {
		return new TypeReference(getAppenderPackage() + ".AbstractSourceAppender"); //$NON-NLS-1$
	}

	@Pure
	@Override
	public String getBuilderPackage() {
		return getBasePackage() + ".builders"; //$NON-NLS-1$
	}

	@Pure
	@Override
	public TypeReference getElementBuilderInterface(String elementName) {
		return new TypeReference(getBuilderPackage() + ".I" //$NON-NLS-1$
				+ Strings.toFirstUpper(elementName) + "Builder"); //$NON-NLS-1$
	}

	@Pure
	@Override
	public TypeReference getElementBuilderImpl(String elementName) {
		return new TypeReference(getBuilderPackage() + "." //$NON-NLS-1$
				+ Strings.toFirstUpper(elementName) + "BuilderImpl"); //$NON-NLS-1$
	}

	@Pure
	@Override
	public TypeReference getElementBuilderImplCustom(String elementName) {
		return new TypeReference(getBuilderPackage() + "." //$NON-NLS-1$
				+ Strings.toFirstUpper(elementName) + "BuilderImplCustom"); //$NON-NLS-1$
	}

	@Pure
	@Override
	public TypeReference getLanguageTopElementType() {
		final Grammar grammar = getGrammar();
		final AbstractRule rule = GrammarUtil.findRuleForName(grammar, getCodeBuilderConfig().getTopElementRuleName());
		return newTypeReference(rule.getType().getClassifier());
	}

	@Override
	public TypeReference getLanguageKeywordAccessor() {
		final var grammar = getGrammar();
		final var basePackage = this.naming.getRuntimeBasePackage(grammar);
		return new TypeReference(basePackage + ".services." //$NON-NLS-1$
				+ GrammarUtil.getSimpleName(grammar).toUpperCase() + "GrammarKeywordAccess"); //$NON-NLS-1$
	}

	@Pure
	@Override
	public TypeReference getSerializerScopeProvider() {
		final var grammar = getGrammar();
		return new TypeReference(getSerializerScopeProviderPackage() + "." //$NON-NLS-1$
				+ GrammarUtil.getSimpleName(grammar).toUpperCase() + "SerializerScopeProvider"); //$NON-NLS-1$
	}

	@Pure
	@Override
	public TypeReference getSerializerTypeParameterScope() {
		final var grammar = getGrammar();
		return new TypeReference(getSerializerScopeProviderPackage() + "." //$NON-NLS-1$
				+ GrammarUtil.getSimpleName(grammar).toUpperCase() + "TypeParameterScope"); //$NON-NLS-1$
	}
	
	@Override
	public TypeReference getFormalParameterContainerType() {
		return new TypeReference(getCodeBuilderConfig().getFormalParameterContainerType());
	}
	
	@Override
	public TypeReference getFormalParameterSuperType(ResourceSet set) {
		return new TypeReference(getFormalParameterSuperEClass(), set);
	}

	@Override
	public EClass getFormalParameterSuperEClass() {
		return getCodeBuilderConfig().getFormalParameterSuperType();
	}

	@Override
	public ElementDescription getFormalParameter() {
		final var rule = GrammarUtil.findRuleForName(getGrammar(), getCodeBuilderConfig().getFormalParameterRuleName());
		final var classifier = getGeneratedTypeFor(rule);
		return newElementDescription(classifier.getName(), rule, classifier, classifier);
	}

	@Override
	public ElementDescription getTypeParameter() {
		final var rule = GrammarUtil.findRuleForName(getGrammar(), getCodeBuilderConfig().getTypeParameterRuleName());
		final var classifier = getGeneratedTypeFor(rule);
		return newElementDescription(classifier.getName(), rule, classifier, classifier);
	}

}

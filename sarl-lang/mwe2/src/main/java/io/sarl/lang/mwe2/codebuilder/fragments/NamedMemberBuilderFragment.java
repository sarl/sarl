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

package io.sarl.lang.mwe2.codebuilder.fragments;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.google.inject.Inject;

import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.Functions.Function4;

import io.sarl.lang.mwe2.codebuilder.extractor.CodeElementExtractor;

/** Generator of the builder for named members.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version mwe2 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid mwe2
 */
public class NamedMemberBuilderFragment extends AbstractMemberBuilderFragment {

	@Inject
	private BuilderFactoryContributions builderFactoryContributions;

	private Map<String, MemberDescription> members;

	@Override
	protected Iterable<MemberDescription> getMembers() {
		if (this.members == null) {
			this.members = new HashMap<>();
			final var topElements = new ArrayList<CodeElementExtractor.ElementDescription>();
			for (final var topElement : getCodeElementExtractor().getTopElements(
					getGrammar(), getCodeBuilderConfig())) {
				topElements.add(topElement);
			}
			for (final var containerDescription : topElements) {
				final var rule = getMemberRule(containerDescription);
				if (rule != null) {
					final var commonSuperType = getCodeElementExtractor().getGeneratedTypeFor(rule);
					final Function4<? super CodeElementExtractor, ? super EObject, ? super EObject, ? super EClassifier, ? extends Object> callback =
							(it, grammarContainer, memberContainer, classifier) -> {
								var memberName = Strings.toFirstUpper(classifier.getName());
								var memberDescription = NamedMemberBuilderFragment.this.members.get(memberName);
								if (memberDescription == null) {
									final var elementDescription =
											it.newElementDescription(classifier.getName(), memberContainer,
													classifier, commonSuperType);
									final var modifiers = getCodeBuilderConfig().getModifiers()
											.get(elementDescription.elementType().getSimpleName());
									memberDescription = new MemberDescription(
											elementDescription,
											containerDescription,
											topElements.contains(elementDescription),
											elementDescription.annotationInfo(),
											modifiers);
									NamedMemberBuilderFragment.this.members.put(memberName, memberDescription);
								}
								final var containerName = memberDescription.getContainerDescription().elementType().getSimpleName();
								var isNoBody = getCodeBuilderConfig().getNoActionBodyTypes()
										.contains(containerName);
								Set<String> containers;
								if (isNoBody) {
									containers = memberDescription.getNoBodyContainers();
								} else {
									containers = memberDescription.getStandardContainers();
								}
								containers.add(containerName);
								return null;
							};
					getCodeElementExtractor().visitMemberElements(containerDescription, rule, null, callback, null, callback);
				}
			}
		}
		return this.members.values();
	}

	@Override
	public void generate() {
		super.generate();
		final var members = getMembers();
		for (final var description : members) {
			generateBuilderFactoryContributions(description);
		}
	}

	/** Generate the contributions for the BuildFactory.
	 *
	 * @param description the description of the member.
	 */
	protected void generateBuilderFactoryContributions(MemberDescription description) {
		if (description.isTopElement()) {
			return;
		}
		if (description.getStandardContainers().isEmpty() && description.getNoBodyContainers().isEmpty()) {
			return;
		}
		var modifiers = description.getModifiers();
		if (modifiers.size() <= 1) {
			// No need to create a function with the modifier's label in the name.
			modifiers = Collections.singletonList(""); //$NON-NLS-1$
		}
		final var enableAppenders = getCodeBuilderConfig().isISourceAppendableEnable();
		for (final var modifier : modifiers) {
			final var createFunctionName = "create" //$NON-NLS-1$
					+ Strings.toFirstUpper(modifier)
					+ Strings.toFirstUpper(description.getElementDescription().name());

			String container = null;
			if (!description.getStandardContainers().isEmpty()) {
				container = description.getStandardContainers().iterator().next();
			} else {
				container = description.getNoBodyContainers().iterator().next();
			}

			final var createContainerFunctionName = "add" //$NON-NLS-1$
					+ Strings.toFirstUpper(container);
			final var containerBuilder = description.getContainerDescription().builderInterfaceType();
			this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create the factory for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
							+ description.getElementDescription().elementType().getSimpleName() + "."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the " //$NON-NLS-1$
							+ description.getElementDescription().name());
					it.newLine();
					it.append("\t * @param resourceSet the set of the resources that must be used for"); //$NON-NLS-1$
					it.newLine();
					it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the factory."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(description.getElementDescription().builderInterfaceType());
					it.append(" "); //$NON-NLS-1$
					it.append(createFunctionName);
					it.append("(String name, "); //$NON-NLS-1$
					it.append(ResourceSet.class);
					it.append(" resourceSet) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn "); //$NON-NLS-1$
					it.append(createFunctionName);
					it.append("(name, createResource(resourceSet));"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Create the factory for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
							+ description.getElementDescription().elementType().getSimpleName() + "."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the " //$NON-NLS-1$
							+ description.getElementDescription().name());
					it.newLine();
					it.append("\t * @param resource the resource that must be used for"); //$NON-NLS-1$
					it.newLine();
					it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the factory."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(description.getElementDescription().builderInterfaceType());
					it.append(" "); //$NON-NLS-1$
					it.append(createFunctionName);
					it.append("(String name, "); //$NON-NLS-1$
					it.append(Resource.class);
					it.append(" resource) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(getScriptBuilderInterface());
					it.append(" scriptBuilder = createScript(getFooPackageName(), resource);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(containerBuilder);
					it.append(" containerBuilder = scriptBuilder."); //$NON-NLS-1$
					it.append(createContainerFunctionName);
					it.append("(getFooTypeName());"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn containerBuilder.add"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(modifier));
					it.append(Strings.toFirstUpper(description.getElementDescription().name()));
					it.append("(name);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			});
			if (enableAppenders) {
				final var buildFunctionName = "build" //$NON-NLS-1$
						+ Strings.toFirstUpper(modifier)
						+ Strings.toFirstUpper(description.getElementDescription().name());
				final var appender = getCodeElementExtractor().getElementAppenderImpl(
						description.getElementDescription().name());
				this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
					@Override
					protected void appendTo(TargetStringConcatenation it) {
						it.append("\t/** Create the appender for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
								+ description.getElementDescription().elementType().getSimpleName() + "."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param name the name of the " //$NON-NLS-1$
								+ description.getElementDescription().name());
						it.newLine();
						it.append("\t * @param resourceSet the set of the resources that must be used for"); //$NON-NLS-1$
						it.newLine();
						it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @return the appender."); //$NON-NLS-1$
						it.newLine();
						appendFileLineComment(it);
						it.append("\t */"); //$NON-NLS-1$
						it.newLine();
						it.append("\t@"); //$NON-NLS-1$
						it.append(Pure.class);
						it.newLine();
						it.append("\tpublic "); //$NON-NLS-1$
						it.append(appender);
						it.append(" "); //$NON-NLS-1$
						it.append(buildFunctionName);
						it.append("(String name, "); //$NON-NLS-1$
						it.append(ResourceSet.class);
						it.append(" resourceSet) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(appender);
						it.append(" a = new "); //$NON-NLS-1$
						it.append(appender);
						it.append("("); //$NON-NLS-1$
						it.append(createFunctionName);
						it.append("(name, resourceSet));"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tgetInjector().injectMembers(a);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn a;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
						it.append("\t/** Create the appender for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
								+ description.getElementDescription().elementType().getSimpleName() + "."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param name the name of the " //$NON-NLS-1$
								+ description.getElementDescription().name());
						it.newLine();
						it.append("\t * @param resource the resource that must be used for"); //$NON-NLS-1$
						it.newLine();
						it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @return the appender."); //$NON-NLS-1$
						it.newLine();
						appendFileLineComment(it);
						it.append("\t */"); //$NON-NLS-1$
						it.newLine();
						it.append("\t@"); //$NON-NLS-1$
						it.append(Pure.class);
						it.newLine();
						it.append("\tpublic "); //$NON-NLS-1$
						it.append(appender);
						it.append(" "); //$NON-NLS-1$
						it.append(buildFunctionName);
						it.append("(String name, "); //$NON-NLS-1$
						it.append(Resource.class);
						it.append(" resource) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(appender);
						it.append(" a = new "); //$NON-NLS-1$
						it.append(appender);
						it.append("("); //$NON-NLS-1$
						it.append(createFunctionName);
						it.append("(name, resource));"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tgetInjector().injectMembers(a);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn a;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
					}
				});
			}
		}
	}

}

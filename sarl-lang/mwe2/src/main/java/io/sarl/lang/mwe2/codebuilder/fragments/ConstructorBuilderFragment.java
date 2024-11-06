/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import java.util.Collections;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;

/** Generator of the builder for constructors.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version mwe2 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid mwe2
 */
public class ConstructorBuilderFragment extends AbstractMemberBuilderFragment {

	@Inject
	private BuilderFactoryContributions builderFactoryContributions;

	private MemberDescription constructor;

	@Override
	protected Iterable<MemberDescription> getMembers() {
		if (this.constructor == null) {
			for (final var containerDescription : getCodeElementExtractor().getTopElements(
					getGrammar(), getCodeBuilderConfig())) {
				final var rule = getMemberRule(containerDescription);
				if (rule != null) {
					final var commonSuperType = getCodeElementExtractor().getGeneratedTypeFor(rule);
					this.constructor = getCodeElementExtractor().visitMemberElements(containerDescription, rule,
						(it, grammarContainer, memberContainer, classifier) -> {
							final var memberDescription = it.newElementDescription(
									classifier.getName(), memberContainer, classifier, commonSuperType);
							return new MemberDescription(memberDescription, containerDescription, false,
									memberDescription.annotationInfo(), null);
						},
						null, null);
					if (this.constructor != null) {
						break;
					}
				}
			}
			if (this.constructor == null) {
				throw new IllegalStateException("No grammar elements for a constructor"); //$NON-NLS-1$
			}
		}
		return Collections.singletonList(this.constructor);
	}

	@Override
	public void generate() {
		super.generate();
		generateBuilderFactoryContributions();
	}

	/** Generate the contributions for the BuildFactory.
	 */
	protected void generateBuilderFactoryContributions() {
		// Get a container
		final var createFunctionName = "create" //$NON-NLS-1$
				+ Strings.toFirstUpper(this.constructor.getElementDescription().name());
		final var createContainerFunctionName = "add" //$NON-NLS-1$
				+ Strings.toFirstUpper(this.constructor.getContainerDescription().name());
		final var containerBuilder = this.constructor.getContainerDescription().builderInterfaceType();
		// Generate the contribution.
		this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("\t/** Create the factory for a " + getLanguageName() + " constructor."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t * @param resourceSet the set of the resources that must be used for"); //$NON-NLS-1$
				it.newLine();
				it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(ConstructorBuilderFragment.this.constructor.getElementDescription().builderInterfaceType());
				it.append(" "); //$NON-NLS-1$
				it.append(createFunctionName);
				it.append("("); //$NON-NLS-1$
				it.append(ResourceSet.class);
				it.append(" resourceSet) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn "); //$NON-NLS-1$
				it.append(createFunctionName);
				it.append("(createResource(resourceSet));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Create the factory for a " + getLanguageName() + " constructor."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append("\t * @param resource the resource that must be used for"); //$NON-NLS-1$
				it.newLine();
				it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(ConstructorBuilderFragment.this.constructor.getElementDescription().builderInterfaceType());
				it.append(" "); //$NON-NLS-1$
				it.append(createFunctionName);
				it.append("("); //$NON-NLS-1$
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
				it.append(ConstructorBuilderFragment.this.constructor.getElementDescription().name());
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		});
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			final var buildFunctionName = "build" //$NON-NLS-1$
					+ Strings.toFirstUpper(this.constructor.getElementDescription().name());
			final var appender = getCodeElementExtractor().getElementAppenderImpl(
					this.constructor.getElementDescription().name());
			this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create the appender for a " + getLanguageName() //$NON-NLS-1$
							+ " constructor."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param resourceSet the set of the resources that must be used for"); //$NON-NLS-1$
					it.newLine();
					it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the appender."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(appender);
					it.append(" "); //$NON-NLS-1$
					it.append(buildFunctionName);
					it.append("("); //$NON-NLS-1$
					it.append(ResourceSet.class);
					it.append(" resourceSet) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(appender);
					it.append(" a = new "); //$NON-NLS-1$
					it.append(appender);
					it.append("("); //$NON-NLS-1$
					it.append(createFunctionName);
					it.append("(resourceSet));"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tgetInjector().injectMembers(a);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn a;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Create the appender for a " + getLanguageName() //$NON-NLS-1$
							+ " constructor."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param resource the resource that must be used for"); //$NON-NLS-1$
					it.newLine();
					it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the appender."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(appender);
					it.append(" "); //$NON-NLS-1$
					it.append(buildFunctionName);
					it.append("("); //$NON-NLS-1$
					it.append(Resource.class);
					it.append(" resource) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(appender);
					it.append(" a = new "); //$NON-NLS-1$
					it.append(appender);
					it.append("("); //$NON-NLS-1$
					it.append(createFunctionName);
					it.append("(resource));"); //$NON-NLS-1$
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

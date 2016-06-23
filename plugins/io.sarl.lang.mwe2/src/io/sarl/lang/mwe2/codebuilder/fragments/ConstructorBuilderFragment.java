/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.inject.Inject;

import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/** Generator of the builder for constructors.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ConstructorBuilderFragment extends AbstractMemberBuilderFragment {
	
	@Inject
	private BuilderFactoryContributions builderFactoryContributions;

	private MemberDescription constructor;
	
	@Override
	protected Iterable<MemberDescription> getMembers() {
		if (this.constructor == null) {
			AbstractRule rule = getConstructorRule();
			String simpleName = Strings.toFirstUpper(rule.getName());
			TypeReference builderInterface = getElementBuilderInterface(simpleName);
			TypeReference builderImpl = getElementBuilderImpl(simpleName);
			TypeReference builderImplCustom = getElementBuilderImplCustom(simpleName);
			EClassifier classifier = getGeneratedTypeFor(rule);
			TypeReference generatedType = newTypeReference(classifier);
			this.constructor = new MemberDescription(
					rule.getName(), simpleName, builderInterface, builderImpl,
					builderImplCustom, generatedType, null);		
		}
		return Collections.singletonList(this.constructor);
	}
	
	/** Get the constructor rule from the grammar.
	 *
	 * @return the top elements.
	 */
	protected AbstractRule getConstructorRule() {
		Grammar grammar = getGrammar();
		Pattern pattern = Pattern.compile(getCodeBuilderConfig().getConstructorGrammarPattern());
		for (AbstractRule rule : GrammarUtil.allRules(grammar)) {
			Matcher matcher = pattern.matcher(rule.getName());
			if (matcher.find()) {
				return rule;
			}
		}
		throw new IllegalStateException("Constructor rule not found"); //$NON-NLS-1$
	}
	
	@Override
	public void generate() {
		super.generate();
		generateBuilderFactoryContributions();
	}

	/** Replies the rule of a container of constructor.
	 *
	 * @return the rule for a constructor's container.
	 */
	private AbstractRule getConstructorContainerRule() {
		AbstractRule topElementRule = GrammarUtil.findRuleForName(getGrammar(), getCodeBuilderConfig().getTopElementRuleName());
		for (RuleCall ruleCall : GrammarUtil.containedRuleCalls(topElementRule)) {
			AbstractRule memberRule = null;
			for (Assignment assignment : GrammarUtil.containedAssignments(ruleCall.getRule())) {
				if (Objects.equals(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName(), assignment.getFeature())) {
					if (assignment.getTerminal() instanceof RuleCall) {
						memberRule = ((RuleCall) assignment.getTerminal()).getRule();
						break;
					}
				}
			}
			if (memberRule != null) {
				Set<String> treatedRules = getTopElementRules();
				AbstractRule foundRule = visitMemberElements(ruleCall.getRule(), memberRule, treatedRules,
						new Functions.Function2<AbstractRule, AbstractRule, AbstractRule>() {
							@Override
							public AbstractRule apply(AbstractRule containerRule, AbstractRule constructorRule) {
								return containerRule;
							}
						}, null);
				if (foundRule != null) {
					return foundRule;
				}
			}
		}
		return null;
	}
	
	/** Generate the contributions for the BuildFactory.
	 */
	protected void generateBuilderFactoryContributions() {
		// Get a container
		AbstractRule containerRule = getConstructorContainerRule();
		final String createFunctionName = "create" //$NON-NLS-1$
				+ Strings.toFirstUpper(this.constructor.getSimpleName());
		final String createContainerFunctionName = "add" //$NON-NLS-1$
				+ Strings.toFirstUpper(containerRule.getName());
		final TypeReference containerBuilder = getElementBuilderInterface(containerRule.getName());
		// Generate the contribution.
		this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
			@SuppressWarnings("synthetic-access")
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
				it.append(ConstructorBuilderFragment.this.constructor.getBuilderInterface());
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
				it.append(ConstructorBuilderFragment.this.constructor.getBuilderInterface());
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
				it.append("(\"FooType\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn containerBuilder.add"); //$NON-NLS-1$
				it.append(ConstructorBuilderFragment.this.constructor.getSimpleName());
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		});
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			final String buildFunctionName = "build" //$NON-NLS-1$
					+ Strings.toFirstUpper(this.constructor.getSimpleName());
			final TypeReference appender = getElementAppenderImpl(this.constructor.getSimpleName());
			this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create the appender for a " + getLanguageName() + " constructor."); //$NON-NLS-1$ //$NON-NLS-2$
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
					it.append("\t\treturn new "); //$NON-NLS-1$
					it.append(appender);
					it.append("("); //$NON-NLS-1$
					it.append(createFunctionName);
					it.append("(resourceSet));"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Create the appender for a " + getLanguageName() + " constructor."); //$NON-NLS-1$ //$NON-NLS-2$
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
					it.append("\t\treturn new "); //$NON-NLS-1$
					it.append(appender);
					it.append("("); //$NON-NLS-1$
					it.append(createFunctionName);
					it.append("(resource));"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			});
		}
	}

}

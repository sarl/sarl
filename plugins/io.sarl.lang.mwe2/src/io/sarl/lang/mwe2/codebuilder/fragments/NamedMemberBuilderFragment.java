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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

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

/** Generator of the builder for named members.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NamedMemberBuilderFragment extends AbstractMemberBuilderFragment {
	
	@Inject
	private BuilderFactoryContributions builderFactoryContributions;

	private Map<String, MemberDescription> members = null;
	
	@Override
	protected Iterable<MemberDescription> getMembers() {
		if (this.members == null) {
			this.members = new HashMap<>();
			Grammar grammar = getGrammar();
			AbstractRule rule = GrammarUtil.findRuleForName(grammar, getCodeBuilderConfig().getTopElementRuleName());
			if (rule != null) {
				for (RuleCall ruleCall : GrammarUtil.containedRuleCalls(rule)) {
					AbstractRule topMemberRule = null;
					for (Assignment assignment : GrammarUtil.containedAssignments(ruleCall.getRule())) {
						if (Objects.equals(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName(), assignment.getFeature())) {
							if (topMemberRule == null && assignment.getTerminal() instanceof RuleCall) {
								topMemberRule = ((RuleCall) assignment.getTerminal()).getRule();
							}
						}
					}
					if (topMemberRule != null) {
						visitMemberElements(ruleCall.getRule(), topMemberRule, getTopElementRules(),
								null,
								new Functions.Function3<AbstractRule, AbstractRule, String, Object>() {
									@SuppressWarnings("synthetic-access")
									@Override
									public Object apply(AbstractRule containerRule, AbstractRule memberRule, String name) {
										MemberDescription memberDescription = NamedMemberBuilderFragment.this.members.get(
												memberRule.getName());
										String memberName = Strings.toFirstUpper(memberRule.getName());
										EClassifier classifier = getGeneratedTypeFor(memberRule);
										TypeReference generatedType = newTypeReference(classifier);
										if (memberDescription == null) {
											TypeReference interfaceType = getElementBuilderInterface(memberName);
											TypeReference implementationType = getElementBuilderImpl(memberName);
											TypeReference customImplementationType = getElementBuilderImplCustom(memberName);
											List<String> modifiers = getCodeBuilderConfig().getModifiers()
													.get(generatedType.getSimpleName());
											memberDescription = new MemberDescription(
													memberRule.getName(),
													memberName,
													interfaceType,
													implementationType,
													customImplementationType,
													generatedType,
													modifiers);
											NamedMemberBuilderFragment.this.members.put(memberRule.getName(),
													memberDescription);
										}
										boolean isNoBody = getCodeBuilderConfig().getNoActionBodyTypes()
												.contains(generatedType.getSimpleName());
										Set<String> containers;
										if (isNoBody) {
											containers = memberDescription.getNoBodyContainers();
										} else {
											containers = memberDescription.getStandardContainers();
										}
										containers.add(Strings.toFirstUpper(containerRule.getName()));
										return null;
									}
								});
					}
				}
			}
		}
		return this.members.values();
	}
		
	@Override
	public void generate() {
		super.generate();
		Iterable<MemberDescription> members = getMembers();
		for (MemberDescription description : members) {
			generateBuilderFactoryContributions(description);
		}
	}
	
	/** Generate the contributions for the BuildFactory.
	 *
	 * @param description the description of the member.
	 */
	protected void generateBuilderFactoryContributions(MemberDescription description) {
		if (description.getStandardContainers().isEmpty() && description.getNoBodyContainers().isEmpty()) {
			return;
		}
		List<String> modifiers = description.getModifiers();
		if (modifiers.size() <= 1) {
			// No need to create a function with the modifier's label in the name.
			modifiers = Collections.singletonList(""); //$NON-NLS-1$
		}
		boolean enableAppenders = getCodeBuilderConfig().isISourceAppendableEnable();
		for (String modifier : modifiers) {
			final String createFunctionName = "create" //$NON-NLS-1$
					+ Strings.toFirstUpper(modifier)
					+ Strings.toFirstUpper(description.getSimpleName());
			
			String container = null;
			if (!description.getStandardContainers().isEmpty()) {
				container = description.getStandardContainers().iterator().next();
			} else {
				container = description.getNoBodyContainers().iterator().next();
			}

			final String createContainerFunctionName = "add" //$NON-NLS-1$
					+ Strings.toFirstUpper(container);
			final TypeReference containerBuilder = getElementBuilderInterface(container);
			this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create the factory for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
							+ description.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the " + description.getSimpleName()); //$NON-NLS-1$
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
					it.append(description.getBuilderInterface());
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
							+ description.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the " + description.getSimpleName()); //$NON-NLS-1$
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
					it.append(description.getBuilderInterface());
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
					it.append("(\"FooType\");"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn containerBuilder.add"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(modifier));
					it.append(description.getSimpleName());
					it.append("(name);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			});
			if (enableAppenders) {
				final String buildFunctionName = "build" //$NON-NLS-1$
						+ Strings.toFirstUpper(modifier)
						+ Strings.toFirstUpper(description.getSimpleName());
				final TypeReference appender = getElementAppenderImpl(description.getSimpleName());
				this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
					@Override
					protected void appendTo(TargetStringConcatenation it) {
						it.append("\t/** Create the appender for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
								+ description.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param name the name of the " + description.getSimpleName()); //$NON-NLS-1$
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
						it.append("(String name, "); //$NON-NLS-1$
						it.append(ResourceSet.class);
						it.append(" resourceSet) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn new "); //$NON-NLS-1$
						it.append(appender);
						it.append("("); //$NON-NLS-1$
						it.append(createFunctionName);
						it.append("(name, resourceSet));"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
						it.append("\t/** Create the appender for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
								+ description.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param name the name of the " + description.getSimpleName()); //$NON-NLS-1$
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
						it.append("(String name, "); //$NON-NLS-1$
						it.append(Resource.class);
						it.append(" resource) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn new "); //$NON-NLS-1$
						it.append(appender);
						it.append("("); //$NON-NLS-1$
						it.append(createFunctionName);
						it.append("(name, resource));"); //$NON-NLS-1$
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

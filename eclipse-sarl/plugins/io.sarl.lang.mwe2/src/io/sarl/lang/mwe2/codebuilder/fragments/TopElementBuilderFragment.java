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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.inject.Inject;
import javax.inject.Provider;

import com.google.inject.Injector;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsFactory;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/** Generator of the builder for top element types.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class TopElementBuilderFragment extends AbstractSubCodeBuilderFragment {

	@Inject
	private BuilderFactoryContributions builderFactoryContributions;

	/** Replies the accessor to the generated type.
	 *
	 * @param generatedType the name of the type.
	 * @return the accessor name.
	 */
	@SuppressWarnings("static-method")
	@Pure
	protected String getGeneratedTypeAccessor(TypeReference generatedType) {
		return "get" //$NON-NLS-1$
				+ Strings.toFirstUpper(generatedType.getSimpleName())
				+ "()"; //$NON-NLS-1$
	}

	@Override
	protected Collection<AbstractSubCodeBuilderFragment> initializeSubGenerators(Injector injector) {
		return Arrays.asList(
				injector.getInstance(ConstructorBuilderFragment.class),
				injector.getInstance(NamedMemberBuilderFragment.class));
	}

	@Override
	public void generate() {
		generateIElementBuilder();
		generateElementBuilderImplementation();
		generateBuilderFactoryContributions();
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			generateElementSourceAppender();
		}
		super.generate();
	}

	@Override
	public void generateBindings(BindingFactory factory) {
		super.generateBindings(factory);
		final IFileSystemAccess2 fileSystem = getSrc();
		TypeReference type;
		for (final TopElementDescription description : generateTopElements(false, false)) {
			type = getElementBuilderImplCustom(description.getSimpleName());
			if ((!fileSystem.isFile(type.getJavaPath()))
					&& (!fileSystem.isFile(type.getXtendPath()))) {
				type = description.getBuilderImplementation();
			}
			factory.addfinalTypeToType(description.getBuilderInterface(), type);
		}
	}

	/** Generate the contributions for the BuildFactory.
	 */
	protected void generateBuilderFactoryContributions() {
		final List<TopElementDescription> topElements = generateTopElements(true, false);
		final boolean enableAppenders = getCodeBuilderConfig().isISourceAppendableEnable();
		for (final TopElementDescription element : topElements) {
			final String createFunctionName = "create" + Strings.toFirstUpper(element.getSimpleName()); //$NON-NLS-1$
			this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create the factory for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
							+ element.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the " + element.getSimpleName()); //$NON-NLS-1$
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
					it.append(element.getBuilderInterface());
					it.append(" "); //$NON-NLS-1$
					it.append(createFunctionName);
					it.append("(String name, "); //$NON-NLS-1$
					it.append(ResourceSet.class);
					it.append(" resourceSet) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t return "); //$NON-NLS-1$
					it.append(createFunctionName);
					it.append("(name, createResource(resourceSet));"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Create the factory for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
							+ element.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the " + element.getSimpleName()); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param resource the resource that must be used for"); //$NON-NLS-1$
					it.newLine();
					it.append("\t *    containing the generated element, and resolving types from names."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the factory."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(element.getBuilderInterface());
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
					it.append("\t\treturn scriptBuilder.add"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(element.getSimpleName()));
					it.append("(name);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			});
			if (enableAppenders) {
				final String buildFunctionName = "build" + Strings.toFirstUpper(element.getSimpleName()); //$NON-NLS-1$
				final TypeReference appender = getElementAppenderImpl(element.getSimpleName());
				this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
					@Override
					protected void appendTo(TargetStringConcatenation it) {
						it.append("\t/** Create the appender for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
								+ element.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param name the name of the " + element.getSimpleName()); //$NON-NLS-1$
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
						it.append(getElementAppenderImpl(element.getSimpleName()));
						it.append(" "); //$NON-NLS-1$
						it.append(buildFunctionName);
						it.append("(String name, "); //$NON-NLS-1$
						it.append(ResourceSet.class);
						it.append(" resourceSet) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn new "); //$NON-NLS-1$
						it.append(appender);
						it.append("("); //$NON-NLS-1$
						it.append(buildFunctionName);
						it.append("(name, resourceSet));"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
						it.append("\t/** Create the appender for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
								+ element.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param name the name of the " + element.getSimpleName()); //$NON-NLS-1$
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
						it.append(getElementAppenderImpl(element.getSimpleName()));
						it.append(" "); //$NON-NLS-1$
						it.append(buildFunctionName);
						it.append("(String name, "); //$NON-NLS-1$
						it.append(Resource.class);
						it.append(" resource) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn new "); //$NON-NLS-1$
						it.append(appender);
						it.append("("); //$NON-NLS-1$
						it.append(buildFunctionName);
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

	/** Generate the element builder interface.
	 */
	protected void generateIElementBuilder() {
		final List<TopElementDescription> topElements = generateTopElements(true, false);
		for (final TopElementDescription element : topElements) {
			final StringConcatenationClient content = new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
							+ " " + element.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$//$NON-NLS-2$
					it.newLine();
					it.append(" */"); //$NON-NLS-1$
					it.newLine();
					it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
					it.newLine();
					it.append("public interface "); //$NON-NLS-1$
					it.append(element.getBuilderInterface().getSimpleName());
					it.append(" {"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append(element.getContent());
					for (final StringConcatenationClient cons : generateMembers(element.getConstructors(),
							element, true, false)) {
						it.append(cons);
					}
					for (final StringConcatenationClient mbr : generateMembers(element.getNamedMembers(), element, true, false)) {
						it.append(mbr);
					}
					it.append("}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			};
			final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(element.getBuilderInterface(), content);
			javaFile.writeTo(getSrcGen());
		}
	}

	/** Generate the element builder interface.
	 */
	protected void generateElementSourceAppender() {
		final List<TopElementDescription> topElements = generateTopElements(false, true);
		for (final TopElementDescription element : topElements) {
			final TypeReference appender = getElementAppenderImpl(element.getSimpleName());
			final StringConcatenationClient content = new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("/** Source adapter of a " + getLanguageName() //$NON-NLS-1$
							+ " " + element.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$//$NON-NLS-2$
					it.newLine();
					it.append(" */"); //$NON-NLS-1$
					it.newLine();
					it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
					it.newLine();
					it.append("public class "); //$NON-NLS-1$
					it.append(appender);
					it.append(" extends "); //$NON-NLS-1$
					it.append(getAbstractAppenderImpl());
					it.append(" implements "); //$NON-NLS-1$
					it.append(element.getBuilderInterface());
					it.append(" {"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append(generateAppenderMembers(appender.getSimpleName(), element.getBuilderInterface(),
							getGeneratedTypeAccessor(element.getGeneratedType())));
					it.append(element.getContent());
					for (final StringConcatenationClient cons : generateMembers(element.getConstructors(), element,
							false, true)) {
						it.append(cons);
					}
					for (final StringConcatenationClient mbr : generateMembers(element.getNamedMembers(), element,
							false, true)) {
						it.append(mbr);
					}
					it.append("}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			};
			final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(appender, content);
			javaFile.writeTo(getSrcGen());
		}
	}

	/** Generate the element builder implementation.
	 */
	protected void generateElementBuilderImplementation() {
		final List<TopElementDescription> topElements = generateTopElements(false, false);
		for (final TopElementDescription element : topElements) {
			final StringConcatenationClient content = new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
							+ " " + element.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$//$NON-NLS-2$
					it.newLine();
					it.append(" */"); //$NON-NLS-1$
					it.newLine();
					it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
					it.newLine();
					it.append("public class "); //$NON-NLS-1$
					it.append(element.getBuilderImplementation().getSimpleName());
					it.append(" extends "); //$NON-NLS-1$
					it.append(getAbstractBuilderImpl());
					it.append(" implements "); //$NON-NLS-1$
					it.append(element.getBuilderInterface().getSimpleName());
					it.append(" {"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append(element.getContent());
					for (final StringConcatenationClient cons : generateMembers(element.getConstructors(), element,
							false, false)) {
						it.append(cons);
					}
					for (final StringConcatenationClient mbr : generateMembers(element.getNamedMembers(), element,
							false, false)) {
						it.append(mbr);
					}
					it.append("}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			};
			final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(element.getBuilderImplementation(), content);
			javaFile.writeTo(getSrcGen());
		}
	}

	/** Generate the creators of members.
	 *
	 * @param rules the rules from which the members' definitions could be retreived.
	 * @param description the description of the top element.
	 * @param forInterface <code>true</code> if the generation is for an interface.
	 * @param forAppender <code>true</code> if the generation is for the ISourceAppender.
	 * @return the code.
	 */
	protected List<StringConcatenationClient> generateMembers(Set<String> rules,
			TopElementDescription description, boolean forInterface, boolean forAppender) {
		final List<StringConcatenationClient> clients = new ArrayList<>();
		for (final String ruleName : rules) {
			final AbstractRule rule = GrammarUtil.findRuleForName(getGrammar(), ruleName);
			clients.addAll(generateMember(rule, description, forInterface, forAppender));
		}
		return clients;
	}

	/** Generate a member from the grammar.
	 *
	 * @param rule the rules to extract from.
	 * @param description the description of the top element.
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @param forAppender <code>true</code> if the generation is for the ISourceAppender.
	 * @return the member functions.
	 */
	@SuppressWarnings("checkstyle:all")
	protected List<StringConcatenationClient> generateMember(AbstractRule rule, TopElementDescription description,
			boolean forInterface, boolean forAppender) {
		final String memberName = Strings.toFirstUpper(rule.getName());
		EClassifier classifier = getGeneratedTypeFor(rule);
		final AtomicBoolean hasName = new AtomicBoolean(false);
		final AtomicBoolean hasTypeName = new AtomicBoolean(false);
		for (Assignment assignment : GrammarUtil.containedAssignments(rule)) {
			if (Objects.equals(getCodeBuilderConfig().getMemberNameExtensionGrammarName(), assignment.getFeature())) {
				hasName.set(true);
				if (nameMatches(assignment.getTerminal(), getCodeBuilderConfig().getTypeReferenceGrammarPattern())) {
					hasTypeName.set(true);
				}
			}
		}
		List<StringConcatenationClient> clients = new ArrayList<>();
		List<String> tmpModifiers = getCodeBuilderConfig().getModifiers().get(classifier.getName());
		if (tmpModifiers == null || tmpModifiers.isEmpty()) {
			tmpModifiers = Collections.singletonList(""); //$NON-NLS-1$
		}
		final List<String> modifiers = tmpModifiers;
		TypeReference builderType = getElementBuilderInterface(memberName);
		if (!forInterface && !forAppender) {
			clients.add(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t@"); //$NON-NLS-1$
					it.append(Inject.class);
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(Provider.class);
					it.append("<"); //$NON-NLS-1$
					it.append(builderType);
					it.append("> "); //$NON-NLS-1$
					it.append(Strings.toFirstLower(builderType.getSimpleName()));
					it.append("Provider;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			});
		}
		for (String modifier : modifiers) {
			String functionName;
			if (modifiers.size() > 1) {
				functionName = "add" + Strings.toFirstUpper(modifier) + memberName; //$NON-NLS-1$
			} else {
				functionName = "add" + memberName; //$NON-NLS-1$
			}
			clients.add(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create " + getAorAnArticle(memberName) //$NON-NLS-1$
					+ " " + memberName + "."); //$NON-NLS-1$//$NON-NLS-2$
					it.newLine();
					if (hasName.get()) {
						it.append("\t * @param name - the "); //$NON-NLS-1$
						if (hasTypeName.get()) {
							it.append("type"); //$NON-NLS-1$
						} {
							it.append("name"); //$NON-NLS-1$
						}
						it.append(" of the " + memberName + "."); //$NON-NLS-1$ //$NON-NLS-2$
						it.newLine();
					}
					it.append("\t * @return the builder."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(builderType);
					it.append(" "); //$NON-NLS-1$
					it.append(functionName);
					it.append("("); //$NON-NLS-1$
					if (hasName.get()) {
						it.append("String name"); //$NON-NLS-1$
					}
					it.append(")"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						if (forAppender) {
							it.append("return this.builder."); //$NON-NLS-1$
							it.append(functionName);
							it.append("("); //$NON-NLS-1$
							if (hasName.get()) {
								it.append("name"); //$NON-NLS-1$
							}
							it.append(");"); //$NON-NLS-1$
						} else {
							it.append(builderType);
							it.append(" builder = this."); //$NON-NLS-1$
							it.append(Strings.toFirstLower(builderType.getSimpleName()));
							it.append("Provider.get();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tbuilder.eInit("); //$NON-NLS-1$
							it.append(getGeneratedTypeAccessor(description.getGeneratedType()));
							if (hasName.get()) {
								it.append(", name"); //$NON-NLS-1$
							}
							if (!Strings.isEmpty(modifier) && modifiers.size() > 1) {
								it.append(", \""); //$NON-NLS-1$
								it.append(Strings.convertToJavaString(modifier));
								it.append("\""); //$NON-NLS-1$
							}
							it.append(");"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\treturn builder;"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
			});
		}
		if (modifiers.size() > 1) {
			String firstModifier = Strings.toFirstUpper(modifiers.get(0));
			String functionName = "add" + memberName; //$NON-NLS-1$
			String callFunctionName = "add" + firstModifier + memberName; //$NON-NLS-1$
			clients.add(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create " + getAorAnArticle(memberName) //$NON-NLS-1$
						+ " " + memberName + "."); //$NON-NLS-1$//$NON-NLS-2$
					it.append("\t *"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * <p>This function is equivalent to {@link #"); //$NON-NLS-1$
					it.append(callFunctionName);
					it.append("}."); //$NON-NLS-1$
					it.newLine();
					if (hasName.get()) {
						it.append("\t * @param name - the "); //$NON-NLS-1$
						if (hasTypeName.get()) {
							it.append("type"); //$NON-NLS-1$
						} {
							it.append("name"); //$NON-NLS-1$
						}
						it.append(" of the " + memberName + "."); //$NON-NLS-1$ //$NON-NLS-2$
						it.newLine();
					}
					it.append("\t * @return the builder."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(builderType);
					it.append(" "); //$NON-NLS-1$
					it.append(functionName);
					it.append("("); //$NON-NLS-1$
					if (hasName.get()) {
						it.append("String name"); //$NON-NLS-1$
					}
					it.append(")"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn "); //$NON-NLS-1$
						if (forAppender) {
							it.append("this.builder."); //$NON-NLS-1$
							it.append(functionName);
							it.append("("); //$NON-NLS-1$
							if (hasName.get()) {
								it.append("name"); //$NON-NLS-1$
							}
							it.append(");"); //$NON-NLS-1$
						} else {
							it.append("this."); //$NON-NLS-1$
							it.append(callFunctionName);
							it.append("("); //$NON-NLS-1$
							if (hasName.get()) {
								it. append("name"); //$NON-NLS-1$
							}
							it.append(");"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
			});
		}
		return clients;
	}

	/** Generate top elements from the grammar.
	 *
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @param forAppender <code>true</code> if the generation is for the ISourceAppender.
	 * @return the top elements.
	 */
	protected List<TopElementDescription> generateTopElements(boolean forInterface, boolean forAppender) {
		final Set<String> topElementNames = new TreeSet<>();
		final Grammar grammar = getGrammar();
		final List<TopElementDescription> topElements = new ArrayList<>();
		final AbstractRule rule = GrammarUtil.findRuleForName(grammar, getCodeBuilderConfig().getTopElementRuleName());
		if (rule != null) {
			for (final RuleCall ruleCall : GrammarUtil.containedRuleCalls(rule)) {
				final String elementName = Strings.toFirstUpper(ruleCall.getRule().getName());
				final TypeReference interfaceType = getElementBuilderInterface(elementName);
				final TypeReference implementationType = getElementBuilderImpl(elementName);
				final EClassifier classifier = getGeneratedTypeFor(ruleCall.getRule());
				final TypeReference generatedType = newTypeReference(classifier);
				final TopElementDescription description = new TopElementDescription(
						ruleCall.getRule().getName(),
						elementName, interfaceType,
						implementationType, generatedType);
				generateTopElement(ruleCall.getRule(), forInterface, forAppender, description);
				topElements.add(description);
				topElementNames.add(elementName);
			}
		}
		// Remove the top elements as members.
		for (final TopElementDescription description : topElements) {
			description.getNamedMembers().removeAll(topElementNames);
		}
		return topElements;
	}

	/** Generatea top element from the grammar.
	 *
	 * @param rule the rule to extract.
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @param forAppender <code>true</code> if the generation is for the ISourceAppender.
	 * @param description the description of the top element.
	 */
	@SuppressWarnings("checkstyle:all")
	protected void generateTopElement(AbstractRule rule, boolean forInterface,
			boolean forAppender, TopElementDescription description) {
		final AtomicBoolean isExtendKeywordFound = new AtomicBoolean(false);
		final AtomicBoolean isExtendsKeywordFound = new AtomicBoolean(false);
		final AtomicBoolean isImplementKeywordFound = new AtomicBoolean(false);
		final AtomicBoolean isImplementsKeywordFound = new AtomicBoolean(false);
		final AtomicBoolean isAnnotated = new AtomicBoolean(false);
		final AtomicBoolean hasModifiers = new AtomicBoolean(false);
		AbstractRule memberRule = null;

		for (Assignment assignment : GrammarUtil.containedAssignments(rule)) {
			if (Objects.equals(getCodeBuilderConfig().getModifierListGrammarName(), assignment.getFeature())) {
				hasModifiers.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getAnnotationListGrammarName(), assignment.getFeature())) {
				isAnnotated.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getTypeExtensionGrammarName(), assignment.getFeature())) {
				if (Objects.equals("*", assignment.getCardinality()) //$NON-NLS-1$
						|| Objects.equals("+=", assignment.getCardinality()) //$NON-NLS-1$
						|| isExtendKeywordFound.get()) {
					isExtendsKeywordFound.set(true);
				}
				isExtendKeywordFound.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getTypeImplementationGrammarName(), assignment.getFeature())) {
				if (Objects.equals("*", assignment.getCardinality()) //$NON-NLS-1$
						|| Objects.equals("+=", assignment.getCardinality()) //$NON-NLS-1$
						|| isImplementKeywordFound.get()) {
					isImplementsKeywordFound.set(true);
				}
				isImplementKeywordFound.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName(), assignment.getFeature())) {
				if (memberRule == null && assignment.getTerminal() instanceof RuleCall) {
					memberRule = ((RuleCall) assignment.getTerminal()).getRule();
				}
			}
		}

		if (memberRule != null) {
			visitMemberElements(rule, memberRule, getTopElementRules(),
					new Functions.Function2<AbstractRule, AbstractRule, Boolean>() {
						@Override
						public Boolean apply(AbstractRule containerRule, AbstractRule constructorRule) {
							description.getConstructors().add(constructorRule.getName());
							return null;
						}
					},
					new Functions.Function3<AbstractRule, AbstractRule, String, Boolean>() {
						@Override
						public Boolean apply(AbstractRule containerRule, AbstractRule memberRule,
								String memberAssignementName) {
							description.getNamedMembers().add(memberRule.getName());
							return null;
						}
					});
		}

		String generatedObjectFieldName = Strings.toFirstLower(description.getGeneratedType().getSimpleName());

		description.setContent(new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				if (!forInterface) {
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(description.getGeneratedType());
					it.append(" "); //$NON-NLS-1$
					it.append(generatedObjectFieldName);
					it.append(";"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				it.append("\t/** Initialize the Ecore element."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(getLanguageScriptInterface());
				it.append(" script, String name)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(script, name);"); //$NON-NLS-1$
					} else {
						it.append("\t\tif (this."); //$NON-NLS-1$
						it.append(generatedObjectFieldName);
						it.append(" == null) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis."); //$NON-NLS-1$
						it.append(generatedObjectFieldName);
						it.append(" = "); //$NON-NLS-1$
						it.append(getXFactoryFor(description.getGeneratedType()));
						it.append(".eINSTANCE.create"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(description.getGeneratedType().getSimpleName()));
						it.append("();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tscript."); //$NON-NLS-1$
						it.append(getLanguageScriptMemberGetter());
						it.append("().add(this."); //$NON-NLS-1$
						it.append(generatedObjectFieldName);
						it.append(");"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tif (!"); //$NON-NLS-1$
						it.append(Strings.class);
						it.append(".isEmpty(name)) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tthis."); //$NON-NLS-1$
						it.append(generatedObjectFieldName);
						it.append(".setName(name);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the generated " //$NON-NLS-1$
						+ description.getGeneratedType().getSimpleName() + "."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(description.getGeneratedType());
				it.append(" "); //$NON-NLS-1$
				it.append(getGeneratedTypeAccessor(description.getGeneratedType()));
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder."); //$NON-NLS-1$
						it.append(getGeneratedTypeAccessor(description.getGeneratedType()));
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn this."); //$NON-NLS-1$
						it.append(generatedObjectFieldName);
						it.append(";"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the resource to which the " //$NON-NLS-1$
						+ description.getGeneratedType().getSimpleName() + " is attached."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(Resource.class);
				it.append(" eResource()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn "); //$NON-NLS-1$
					it.append(getGeneratedTypeAccessor(description.getGeneratedType()));
					it.append(".eResource();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateStandardCommentFunctions(forInterface, forAppender,
						getGeneratedTypeAccessor(description.getGeneratedType())));
				if (isExtendKeywordFound.get()) {
					String defaultType = getCodeBuilderConfig().getDefaultSupers().get(
							description.getGeneratedType().getSimpleName());
					if (isExtendsKeywordFound.get()) {
						it.append("\t/** Add the super type."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param superType - the qualified name of the super type."); //$NON-NLS-1$
						it.newLine();
						it.append("\t */"); //$NON-NLS-1$
						it.newLine();
						it.append("\t"); //$NON-NLS-1$
						if (!forInterface) {
							it.append("public "); //$NON-NLS-1$
						}
						it.append("void addExtends(String superType)"); //$NON-NLS-1$
					} else {
						it.append("\t/** Change the super type."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param superType - the qualified name of the super type,"); //$NON-NLS-1$
						it.newLine();
						it.append("\t *     or <code>null</code> if the default type."); //$NON-NLS-1$
						it.newLine();
						it.append("\t */"); //$NON-NLS-1$
						it.newLine();
						it.append("\t"); //$NON-NLS-1$
						if (!forInterface) {
							it.append("public "); //$NON-NLS-1$
						}
						it.append("void setExtends(String superType)"); //$NON-NLS-1$
					}
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							if (isExtendsKeywordFound.get()) {
								it.append("\t\tthis.builder.addExtends(superType);"); //$NON-NLS-1$
							} else {
								it.append("\t\tthis.builder.setExtends(superType);"); //$NON-NLS-1$
							}
						} else {
							it.append("\t\tif (!"); //$NON-NLS-1$
							it.append(Strings.class);
							it.append(".isEmpty(superType)"); //$NON-NLS-1$
							if (!Strings.isEmpty(defaultType)) {
								it.newLine();
								it.append("\t\t\t\t&& !"); //$NON-NLS-1$
								it.append(new TypeReference(defaultType));
								it.append(".class.getName().equals(superType)"); //$NON-NLS-1$
							}
							it.append(") {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(JvmParameterizedTypeReference.class);
							it.append(" superTypeRef = newTypeRef(this."); //$NON-NLS-1$
							it.append(generatedObjectFieldName);
							it.append(", superType);"); //$NON-NLS-1$
							it.newLine();
							if (!Strings.isEmpty(defaultType)) {
								it.append("\t\t\t"); //$NON-NLS-1$
								it.append(JvmTypeReference.class);
								it.append(" baseTypeRef = newTypeRef(this."); //$NON-NLS-1$
								it.append(generatedObjectFieldName);
								it.append(", "); //$NON-NLS-1$
								it.append(new TypeReference(defaultType));
								it.append(".class);"); //$NON-NLS-1$
								it.newLine();
								it.append("\t\t\tif (isSubTypeOf(this."); //$NON-NLS-1$
								it.append(generatedObjectFieldName);
								it.append(", superTypeRef, baseTypeRef)) {"); //$NON-NLS-1$
								it.newLine();
								it.append("\t\t\t\t"); //$NON-NLS-1$
							} else {
								it.append("\t\t\t"); //$NON-NLS-1$
							}
							it.append("this."); //$NON-NLS-1$
							it.append(generatedObjectFieldName);
							if (isExtendsKeywordFound.get()) {
								it.append(".getExtends().add(superTypeRef);"); //$NON-NLS-1$
							} else {
								it.append(".setExtends(superTypeRef);"); //$NON-NLS-1$
							}
							it.newLine();
							if (!Strings.isEmpty(defaultType) && !isExtendsKeywordFound.get()) {
								it.append("\t\t\t\treturn;"); //$NON-NLS-1$
								it.newLine();
							}
							if (!Strings.isEmpty(defaultType)) {
								it.append("\t\t\t}"); //$NON-NLS-1$
								it.newLine();
							}
							it.append("\t\t}"); //$NON-NLS-1$
							it.newLine();
							if (!isExtendsKeywordFound.get()) {
								it.append("\t\tthis."); //$NON-NLS-1$
								it.append(generatedObjectFieldName);
								it.append(".setExtends(null);"); //$NON-NLS-1$
								it.newLine();
							}
						}
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (isImplementKeywordFound.get()) {
					if (isImplementsKeywordFound.get()) {
						it.append("\t/** Add an implemented type."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param type - the qualified name of the implemented type."); //$NON-NLS-1$
						it.newLine();
						it.append("\t */"); //$NON-NLS-1$
						it.newLine();
						it.append("\t"); //$NON-NLS-1$
						if (!forInterface) {
							it.append("public "); //$NON-NLS-1$
						}
						it.append("void addImplements(String type)"); //$NON-NLS-1$
						if (forInterface) {
							it.append(";"); //$NON-NLS-1$
						} else {
							it.append(" {"); //$NON-NLS-1$
							it.newLine();
							if (forAppender) {
								it.append("\t\tthis.builder.addImplements(type);"); //$NON-NLS-1$
							} else {
								it.append("\t\tif (!"); //$NON-NLS-1$
								it.append(Strings.class);
								it.append(".isEmpty(type)) {"); //$NON-NLS-1$
								it.newLine();
								it.append("\t\t\tthis."); //$NON-NLS-1$
								it.append(generatedObjectFieldName);
								it.append(".getImplements().add(newTypeRef("); //$NON-NLS-1$
								it.append(generatedObjectFieldName);
								it.append(", type));"); //$NON-NLS-1$
								it.newLine();
								it.append("\t\t}"); //$NON-NLS-1$
							}
							it.newLine();
							it.append("\t}"); //$NON-NLS-1$
						}
						it.newLineIfNotEmpty();
						it.newLine();
					} else {
						it.append("\t/** Change the implemented type."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param type - the qualified name of the implemented type,"); //$NON-NLS-1$
						it.newLine();
						it.append("\t *     or <code>null</code> for nothing."); //$NON-NLS-1$
						it.newLine();
						it.append("\t */"); //$NON-NLS-1$
						it.newLine();
						it.append("\t"); //$NON-NLS-1$
						if (!forInterface) {
							it.append("public "); //$NON-NLS-1$
						}
						it.append("void setImplements(String type)"); //$NON-NLS-1$
						if (forInterface) {
							it.append(";"); //$NON-NLS-1$
						} else {
							it.append(" {"); //$NON-NLS-1$
							it.newLine();
							if (forAppender) {
								it.append("\t\tthis.builder.setImplements(type);"); //$NON-NLS-1$
							} else {
								it.append("\t\tthis."); //$NON-NLS-1$
								it.append(generatedObjectFieldName);
								it.append(".setImplements("); //$NON-NLS-1$
								it.newLine();
								it.append("\t\t\t("); //$NON-NLS-1$
								it.append(Strings.class);
								it.append(".isEmpty(type)) ? null : newTypeRef("); //$NON-NLS-1$
								it.append(generatedObjectFieldName);
								it.append(", type));"); //$NON-NLS-1$
							}
							it.newLine();
							it.append("\t}"); //$NON-NLS-1$
						}
						it.newLineIfNotEmpty();
						it.newLine();
					}
				}
				if (isAnnotated.get()) {
					it.append("\t/** Add an annotation."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type - the qualified name of the annotation."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void addAnnotation(String type)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\tthis.builder.addAnnotation(type);"); //$NON-NLS-1$
						} else {
							it.append("\t\tif (!"); //$NON-NLS-1$
							it.append(Strings.class);
							it.append(".isEmpty(type)) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(XAnnotation.class);
							it.append(" annotation = "); //$NON-NLS-1$
							it.append(XAnnotationsFactory.class);
							it.append(".eINSTANCE.createXAnnotation();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tannotation.setAnnotationType(newTypeRef("); //$NON-NLS-1$
							it.append(generatedObjectFieldName);
							it.append(", type).getType());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedObjectFieldName);
							it.append(".getAnnotations().add(annotation);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t}"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (hasModifiers.get()) {
					it.append("\t/** Add a modifier."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param modifier - the modifier to add."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void addModifier(String modifier)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\tthis.builder.addModifier(modifier);"); //$NON-NLS-1$
						} else {
							it.append("\t\tif (!"); //$NON-NLS-1$
							it.append(Strings.class);
							it.append(".isEmpty(modifier)) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedObjectFieldName);
							it.append(".getModifiers().add(modifier);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t}"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
			}
		});
	}

	/** Description of a top element.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class TopElementDescription {

		private final String ruleName;

		private final String simpleName;

		private final TypeReference interfaceType;

		private final TypeReference implementationType;

		private final TypeReference generatedType;

		private final Set<String> namedMemberCandidates = new TreeSet<>();

		private final Set<String> constructorCandidates = new TreeSet<>();

		private StringConcatenationClient content;

		/** Constructor.
		 *
		 * @param ruleName the name of the rule.
		 * @param simpleName the simple name.
		 * @param interfaceType the type of the builder interface.
		 * @param implementationType the type of the builder implementation.
		 * @param generatedType the type of the generated type.
		 */
		public TopElementDescription(String ruleName, String simpleName,
				TypeReference interfaceType, TypeReference implementationType,
				TypeReference generatedType) {
			this.ruleName = ruleName;
			this.simpleName = simpleName;
			this.interfaceType = interfaceType;
			this.implementationType = implementationType;
			this.generatedType = generatedType;
		}

		@Override
		public String toString() {
			return this.simpleName;
		}

		/** Replies the rule name.
		 *
		 * @return the rule name.
		 */
		@Pure
		public String getRuleName() {
			return this.ruleName;
		}

		/** Replies the simple name.
		 *
		 * @return the simple name.
		 */
		@Pure
		public String getSimpleName() {
			return this.simpleName;
		}

		/** Replies the named members.
		 *
		 * @return the named members.
		 */
		@Pure
		public Set<String> getNamedMembers() {
			return this.namedMemberCandidates;
		}

		/** Replies the constructors.
		 *
		 * @return the constructors.
		 */
		@Pure
		public Set<String> getConstructors() {
			return this.constructorCandidates;
		}

		/** Replies the type of the builder interface.
		 *
		 * @return the type.
		 */
		@Pure
		public TypeReference getBuilderInterface() {
			return this.interfaceType;
		}

		/** Replies the type of the builder implementation.
		 *
		 * @return the type.
		 */
		@Pure
		public TypeReference getBuilderImplementation() {
			return this.implementationType;
		}

		/** Replies the type of the generated type.
		 *
		 * @return the type.
		 */
		@Pure
		public TypeReference getGeneratedType() {
			return this.generatedType;
		}

		/** Replies the content of the builder.
		 *
		 * @return the content.
		 */
		@Pure
		public StringConcatenationClient getContent() {
			return this.content;
		}

		/** Change the content of the builder.
		 *
		 * @param content the content.
		 */
		public void setContent(StringConcatenationClient content) {
			this.content = content;
		}

	}

}

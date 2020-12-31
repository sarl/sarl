/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.inject.Inject;
import javax.inject.Provider;

import com.google.common.collect.Iterables;
import com.google.inject.Injector;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.util.EmfFormatter;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsFactory;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

import io.sarl.lang.mwe2.codebuilder.extractor.CodeElementExtractor;

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
		final ConstructorBuilderFragment fg1 = injector.getInstance(ConstructorBuilderFragment.class);
		final NamedMemberBuilderFragment fg2 = injector.getInstance(NamedMemberBuilderFragment.class);
		return Arrays.asList(fg1, fg2);
	}

	@Override
	public void generate() {
		generateIElementBuilder();
		generateElementBuilderImplementation();
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			generateElementSourceAppender();
		}
		generateBuilderFactoryContributions();
		super.generate();
	}

	@Override
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		for (final TopElementDescription description : generateTopElements(false, false)) {
			bindElementDescription(factory, description.getElementDescription());
		}
	}

	/** Generate the contributions for the BuildFactory.
	 */
	protected void generateBuilderFactoryContributions() {
		final List<TopElementDescription> topElements = generateTopElements(true, false);
		final boolean enableAppenders = getCodeBuilderConfig().isISourceAppendableEnable();
		for (final TopElementDescription element : topElements) {
			final String createFunctionName = "create" //$NON-NLS-1$
					+ Strings.toFirstUpper(element.getElementDescription().getName());
			this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create the factory for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
							+ element.getElementDescription().getElementType().getSimpleName() + "."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the " + element.getElementDescription().getName()); //$NON-NLS-1$
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
					it.append(element.getElementDescription().getBuilderInterfaceType());
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
							+ element.getElementDescription().getElementType().getSimpleName() + "."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the " + element.getElementDescription().getName()); //$NON-NLS-1$
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
					it.append(element.getElementDescription().getBuilderInterfaceType());
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
					it.append(Strings.toFirstUpper(element.getElementDescription().getName()));
					it.append("(name);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			});
			if (enableAppenders) {
				final String buildFunctionName = "build" + Strings.toFirstUpper(//$NON-NLS-1$
						element.getElementDescription().getName());
				final TypeReference appender = getCodeElementExtractor().getElementAppenderImpl(
						element.getElementDescription().getName());
				this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
					@Override
					protected void appendTo(TargetStringConcatenation it) {
						it.append("\t/** Create the appender for a " + getLanguageName() + " " //$NON-NLS-1$ //$NON-NLS-2$
								+ element.getElementDescription().getElementType().getSimpleName() + "."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param name the name of the " + element.getElementDescription().getName()); //$NON-NLS-1$
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
								+ element.getElementDescription().getElementType().getSimpleName() + "."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param name the name of the " + element.getElementDescription().getName()); //$NON-NLS-1$
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

	/** Generate the element builder interface.
	 */
	protected void generateIElementBuilder() {
		final List<TopElementDescription> topElements = generateTopElements(true, false);
		for (final TopElementDescription element : topElements) {
			final StringConcatenationClient content = new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
							+ " " + element.getElementDescription().getElementType().getSimpleName() //$NON-NLS-1$
							+ "."); //$NON-NLS-1$
					it.newLine();
					it.append(" */"); //$NON-NLS-1$
					it.newLine();
					it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
					it.newLine();
					it.append("public interface "); //$NON-NLS-1$
					it.append(element.getElementDescription().getBuilderInterfaceType().getSimpleName());
					it.append(" {"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append(element.getContent());
					for (final StringConcatenationClient cons : generateMembers(element.getConstructors(),
							element, true, false, true)) {
						it.append(cons);
					}
					for (final StringConcatenationClient mbr : generateMembers(element.getNamedMembers(),
							element, true, false, true)) {
						it.append(mbr);
					}
					for (final StringConcatenationClient mbr : generateMembers(element.getUnnamedMembers(),
							element, true, false, false)) {
						it.append(mbr);
					}
					it.append("}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			};
			final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(
					element.getElementDescription().getBuilderInterfaceType(), content);
			javaFile.writeTo(getSrcGen());
		}
	}

	/** Generate the element builder interface.
	 */
	protected void generateElementSourceAppender() {
		final List<TopElementDescription> topElements = generateTopElements(false, true);
		for (final TopElementDescription element : topElements) {
			final TypeReference appender = element.getElementDescription().getAppenderType();
			final StringConcatenationClient content = new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("/** Source adapter of a " + getLanguageName() //$NON-NLS-1$
							+ " " + element.getElementDescription().getElementType().getSimpleName() //$NON-NLS-1$
							+ "."); //$NON-NLS-1$
					it.newLine();
					it.append(" */"); //$NON-NLS-1$
					it.newLine();
					it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
					it.newLine();
					it.append("public class "); //$NON-NLS-1$
					it.append(appender.getSimpleName());
					it.append(" extends "); //$NON-NLS-1$
					it.append(getCodeElementExtractor().getAbstractAppenderImpl());
					it.append(" implements "); //$NON-NLS-1$
					it.append(element.getElementDescription().getBuilderInterfaceType());
					it.append(" {"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append(generateAppenderMembers(appender.getSimpleName(),
							element.getElementDescription().getBuilderInterfaceType(),
							getGeneratedTypeAccessor(element.getElementDescription().getElementType())));
					it.append(element.getContent());
					for (final StringConcatenationClient cons : generateMembers(element.getConstructors(), element,
							false, true, true)) {
						it.append(cons);
					}
					for (final StringConcatenationClient mbr : generateMembers(element.getNamedMembers(), element,
							false, true, true)) {
						it.append(mbr);
					}
					for (final StringConcatenationClient mbr : generateMembers(element.getUnnamedMembers(), element,
							false, true, false)) {
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
							+ " " + element.getElementDescription().getElementType().getSimpleName() //$NON-NLS-1$
							+ "."); //$NON-NLS-1$
					it.newLine();
					it.append(" */"); //$NON-NLS-1$
					it.newLine();
					it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
					it.newLine();
					it.append("public class "); //$NON-NLS-1$
					it.append(element.getElementDescription().getBuilderImplementationType().getSimpleName());
					it.append(" extends "); //$NON-NLS-1$
					it.append(getAbstractBuilderImpl());
					it.append(" implements "); //$NON-NLS-1$
					it.append(element.getElementDescription().getBuilderInterfaceType());
					it.append(" {"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append(element.getContent());
					for (final StringConcatenationClient cons : generateMembers(element.getConstructors(), element,
							false, false, true)) {
						it.append(cons);
					}
					for (final StringConcatenationClient mbr : generateMembers(element.getNamedMembers(), element,
							false, false, true)) {
						it.append(mbr);
					}
					for (final StringConcatenationClient mbr : generateMembers(element.getUnnamedMembers(), element,
							false, false, false)) {
						it.append(mbr);
					}
					it.append("}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
			};
			final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(
					element.getElementDescription().getBuilderImplementationType(), content);
			javaFile.writeTo(getSrcGen());
		}
	}

	/** Generate the creators of members.
	 *
	 * @param grammarContainers the containers from which the members' definitions could be retreived.
	 * @param description the description of the top element.
	 * @param forInterface <code>true</code> if the generation is for an interface.
	 * @param forAppender <code>true</code> if the generation is for the ISourceAppender.
	 * @param namedMembers <code>true</code> if the members have name.
	 * @return the code.
	 */
	protected List<StringConcatenationClient> generateMembers(
			Collection<CodeElementExtractor.ElementDescription> grammarContainers,
			TopElementDescription description, boolean forInterface, boolean forAppender, boolean namedMembers) {
		final List<StringConcatenationClient> clients = new ArrayList<>();
		for (final CodeElementExtractor.ElementDescription elementDescription : grammarContainers) {
			clients.addAll(generateMember(elementDescription, description, forInterface, forAppender, namedMembers));
		}
		return clients;
	}

	/** Generate a member from the grammar.
	 *
	 * @param memberDescription the description of the member.
	 * @param topElementDescription the description of the top element.
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @param forAppender <code>true</code> if the generation is for the ISourceAppender.
	 * @param namedMember <code>true</code> if the member has name.
	 * @return the member functions.
	 */
	protected List<StringConcatenationClient> generateMember(CodeElementExtractor.ElementDescription memberDescription,
			TopElementDescription topElementDescription, boolean forInterface, boolean forAppender, boolean namedMember) {
		if (namedMember) {
			return generateNamedMember(memberDescription, topElementDescription, forInterface, forAppender);
		}
		return generateUnnamedMember(memberDescription, topElementDescription, forInterface, forAppender);
	}

	@SuppressWarnings("checkstyle:all")
	private List<StringConcatenationClient> generateNamedMember(CodeElementExtractor.ElementDescription memberDescription,
			TopElementDescription topElementDescription, boolean forInterface, boolean forAppender) {
		final String memberName = Strings.toFirstUpper(memberDescription.getName());
		final TypeReference classifier = memberDescription.getElementType();
		final List<StringConcatenationClient> clients = new ArrayList<>();
		final AtomicBoolean hasName = new AtomicBoolean(false);
		final AtomicBoolean hasTypeName = new AtomicBoolean(false);
		for (final Assignment assignment : GrammarUtil.containedAssignments(memberDescription.getGrammarComponent())) {
			if (Objects.equals(getCodeBuilderConfig().getMemberNameExtensionGrammarName(), assignment.getFeature())) {
				hasName.set(true);
				if (nameMatches(assignment.getTerminal(), getCodeBuilderConfig().getTypeReferenceGrammarPattern())) {
					hasTypeName.set(true);
				}
			}
		}
		List<String> tmpModifiers = getCodeBuilderConfig().getModifiers().get(classifier.getSimpleName());
		if (tmpModifiers == null || tmpModifiers.isEmpty()) {
			tmpModifiers = Collections.singletonList(""); //$NON-NLS-1$
		}
		final List<String> modifiers = tmpModifiers;
		final TypeReference builderType = memberDescription.getBuilderInterfaceType();
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
		for (final String modifier : modifiers) {
			final String functionName;
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
						it.append("\t * @param name the "); //$NON-NLS-1$
						if (hasTypeName.get()) {
							it.append("type"); //$NON-NLS-1$
						} else {
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
							it.append(getGeneratedTypeAccessor(topElementDescription.getElementDescription().getElementType()));
							if (hasName.get()) {
								it.append(", name"); //$NON-NLS-1$
							}
							if (!Strings.isEmpty(modifier) && modifiers.size() > 1) {
								it.append(", \""); //$NON-NLS-1$
								it.append(Strings.convertToJavaString(modifier));
								it.append("\""); //$NON-NLS-1$
							}
							it.append(", getTypeResolutionContext());"); //$NON-NLS-1$
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
			final String firstModifier = Strings.toFirstUpper(modifiers.get(0));
			final String functionName = "add" + memberName; //$NON-NLS-1$
			final String callFunctionName = "add" + firstModifier + memberName; //$NON-NLS-1$
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
						it.append("\t * @param name the "); //$NON-NLS-1$
						if (hasTypeName.get()) {
							it.append("type"); //$NON-NLS-1$
						} else {
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
								it.append("name"); //$NON-NLS-1$
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

	@SuppressWarnings("checkstyle:all")
	private List<StringConcatenationClient> generateUnnamedMember(CodeElementExtractor.ElementDescription memberDescription,
			TopElementDescription topElementDescription, boolean forInterface, boolean forAppender) {
		final String generatedObjectFieldName = Strings.toFirstLower(
				topElementDescription.getElementDescription().getElementType().getSimpleName());
		final String memberName = Strings.toFirstUpper(memberDescription.getName());
		final TypeReference classifier = memberDescription.getElementType();
		final List<StringConcatenationClient> clients = new ArrayList<>();
		List<String> tmpModifiers = getCodeBuilderConfig().getModifiers().get(classifier.getSimpleName());
		if (tmpModifiers == null || tmpModifiers.isEmpty()) {
			tmpModifiers = Collections.singletonList(""); //$NON-NLS-1$
		}
		final List<String> modifiers = tmpModifiers;
		for (final String modifier : modifiers) {
			final String functionName;
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
					it.append("\t * @param name the types referenced by the " + memberName + "."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void "); //$NON-NLS-1$
					it.append(functionName);
					it.append("(String... name)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						if (forAppender) {
							it.append("this.builder."); //$NON-NLS-1$
							it.append(functionName);
							it.append("(name);"); //$NON-NLS-1$
						} else {
							it.append("if (name != null && name.length > 0) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(classifier);
							it.append(" member = "); //$NON-NLS-1$
							it.append(getXFactoryFor(topElementDescription.getElementDescription().getElementType()));
							it.append(".eINSTANCE.create"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(memberDescription.getElementType().getSimpleName()));
							it.append("();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedObjectFieldName);
							it.append(".getMembers().add(member);"); //$NON-NLS-1$
							it.newLine();
							if (memberDescription.isAnnotationInfo()) {
								final TypeReference commonType = memberDescription.getCommonSuperType();
								it.append("\t\t\tmember.setAnnotationInfo("); //$NON-NLS-1$
								it.append(getXFactoryFor(commonType));
								it.append(".eINSTANCE.create"); //$NON-NLS-1$
								it.append(Strings.toFirstUpper(commonType.getSimpleName()));
								it.append("());"); //$NON-NLS-1$
								it.newLine();
							}
							String field = Iterables.find(
									getCodeBuilderConfig().getUnnamedMemberExtensionGrammarNames(),
									(it2) -> findAssignmentFromFeatureName(memberDescription.getGrammarComponent(),
											it2) != null,
									null);
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(Collection.class);
							it.append("<"); //$NON-NLS-1$
							it.append(JvmParameterizedTypeReference.class);
							it.append("> thecollection = member.get"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(field));
							it.append("();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tfor (final String aname : name) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\tif (!"); //$NON-NLS-1$
							it.append(Strings.class);
							it.append(".isEmpty(aname)) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t\tthecollection.add(newTypeRef(this."); //$NON-NLS-1$
							it.append(generatedObjectFieldName);
							it.append(", aname));"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t}"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t}"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t}"); //$NON-NLS-1$
							it.newLine();
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
			final String firstModifier = Strings.toFirstUpper(modifiers.get(0));
			final String functionName = "add" + memberName; //$NON-NLS-1$
			final String callFunctionName = "add" + firstModifier + memberName; //$NON-NLS-1$
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
					it.append("\t * @param name the type referenced by the " + memberName + "."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void "); //$NON-NLS-1$
					it.append(functionName);
					it.append("(String name)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("this.builder."); //$NON-NLS-1$
							it.append(functionName);
							it.append("(name);"); //$NON-NLS-1$
						} else {
							it.append("this."); //$NON-NLS-1$
							it.append(callFunctionName);
							it.append("(name);"); //$NON-NLS-1$
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

	private Set<String> determineMemberElements() {
		final Set<String> memberElements = new HashSet<>();
		final Set<String> topElements = new HashSet<>();
		for (final CodeElementExtractor.ElementDescription containerDescription : getCodeElementExtractor().getTopElements(
				getGrammar(), getCodeBuilderConfig())) {
			topElements.add(containerDescription.getElementType().getName());
			final AbstractRule rule = getMemberRule(containerDescription);
			if (rule != null) {
				getCodeElementExtractor().visitMemberElements(containerDescription, rule, null,
					(it, grammarContainer, memberContainer, classifier) -> {
						memberElements.add(getCodeElementExtractor().newTypeReference(classifier).getName());
						return null;
					},
					(it, grammarContainer, memberContainer, classifier) -> {
						memberElements.add(getCodeElementExtractor().newTypeReference(classifier).getName());
						return null;
					});
			}
		}
		memberElements.retainAll(topElements);
		return memberElements;
	}

	/** Generate top elements from the grammar.
	 *
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @param forAppender <code>true</code> if the generation is for the ISourceAppender.
	 * @return the top elements.
	 */
	@SuppressWarnings("unlikely-arg-type")
	protected List<TopElementDescription> generateTopElements(boolean forInterface, boolean forAppender) {
		final Set<String> memberElements = determineMemberElements();
		final Collection<EObject> topElementContainers = new ArrayList<>();
		final List<TopElementDescription> topElements = new ArrayList<>();
		for (final CodeElementExtractor.ElementDescription description : getCodeElementExtractor().getTopElements(
				getGrammar(), getCodeBuilderConfig())) {
			final TopElementDescription topElementDescription = new TopElementDescription(
					description,
					memberElements.contains(description.getElementType().getName()),
					getCodeBuilderConfig().isXtendSupportEnabled() && description.isAnnotationInfo());
			generateTopElement(topElementDescription, forInterface, forAppender);
			topElements.add(topElementDescription);
			topElementContainers.add(topElementDescription.getElementDescription().getGrammarComponent());
		}
		// Remove the top elements as members.
		for (final TopElementDescription description : topElements) {
			description.getNamedMembers().removeAll(topElementContainers);
		}
		return topElements;
	}

	/** Generatea top element from the grammar.
	 *
	 * @param description the description of the top element.
	 * @param forInterface indicates if the generated code is for interfaces.
	 * @param forAppender <code>true</code> if the generation is for the ISourceAppender.
	 */
	@SuppressWarnings("checkstyle:all")
	protected void generateTopElement(TopElementDescription description,
			boolean forInterface, boolean forAppender) {
		final AtomicBoolean isExtendKeywordFound = new AtomicBoolean(false);
		final AtomicBoolean isExtendsKeywordFound = new AtomicBoolean(false);
		final AtomicBoolean isImplementKeywordFound = new AtomicBoolean(false);
		final AtomicBoolean isImplementsKeywordFound = new AtomicBoolean(false);
		final AtomicBoolean isAnnotated = new AtomicBoolean(false);
		final AtomicBoolean hasModifiers = new AtomicBoolean(false);
		final AtomicBoolean hasTypeParameters = new AtomicBoolean(false);
		AbstractRule memberRule = null;

		for (Assignment assignment : GrammarUtil.containedAssignments(description.getElementDescription().getGrammarComponent())) {
			if (Objects.equals(getCodeBuilderConfig().getModifierListGrammarName(), assignment.getFeature())) {
				hasModifiers.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getTypeParameterListGrammarName(), assignment.getFeature())) {
					hasTypeParameters.set(true);
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
			final EClassifier commonSuperClassifier = getCodeElementExtractor().getGeneratedTypeFor(memberRule);
			getCodeElementExtractor().visitMemberElements(description.getElementDescription(), memberRule,
					(it, grammarContainer, memberContainer, classifier) -> {
						description.getConstructors().add(it.newElementDescription(
								classifier.getName(), memberContainer, classifier, commonSuperClassifier));
						return null;
					},
					(it, grammarContainer, memberContainer, classifier) -> {
						description.getNamedMembers().add(it.newElementDescription(
								classifier.getName(), memberContainer, classifier, commonSuperClassifier));
						return null;
					},
					(it, grammarContainer, memberContainer, classifier) -> {
						description.getUnnamedMembers().add(it.newElementDescription(
								classifier.getName(), memberContainer, classifier, commonSuperClassifier));
						return null;
					});
		}

		String generatedObjectFieldName = Strings.toFirstLower(description.getElementDescription().getElementType().getSimpleName());

		description.setContent(new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				if (!forInterface && !forAppender) {
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(description.getElementDescription().getElementType());
					it.append(" "); //$NON-NLS-1$
					it.append(generatedObjectFieldName);
					it.append(";"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					if (description.isMemberElement()) {
						it.append("\tprivate "); //$NON-NLS-1$
						it.append(EObject.class);
						it.append(" container;"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
					}
				} else {
					it.append("\t/** Find the reference to the type with the given name."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param typeName the fully qualified name of the type"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the type reference."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(JvmParameterizedTypeReference.class);
					it.append(" newTypeRef(String typeName)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn this.builder.newTypeRef(typeName);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Find the reference to the type with the given name."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param context the context for the type reference use"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param typeName the fully qualified name of the type"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the type reference."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(JvmParameterizedTypeReference.class);
					it.append(" newTypeRef("); //$NON-NLS-1$
					it.append(Notifier.class);
					it.append(" context, String typeName)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn this.builder.newTypeRef(context, typeName);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (forInterface) {
					it.append("\t/** Replies the context for type resolution."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the context or {@code null} if the Ecore object is the context."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					it.append(IJvmTypeProvider.class);
					it.append(" getTypeResolutionContext();"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				} else if (forAppender) {
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(IJvmTypeProvider.class);
					it.append(" getTypeResolutionContext() {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn this.builder.getTypeResolutionContext();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (forInterface) {
					it.append("\t/** Dispose the resource."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\tvoid dispose();"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				} else if (forAppender) {
					it.append("\t/** Dispose the resource."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\tpublic void dispose() {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tthis.builder.dispose();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (!forInterface) {
					it.append("\t@"); //$NON-NLS-1$
					it.append(Override.class);
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tpublic "); //$NON-NLS-1$
					it.append(String.class);
					it.append(" toString() {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn "); //$NON-NLS-1$
					if (forAppender) {
						it.append("this.builder.toString();"); //$NON-NLS-1$
					} else {
						it.append(EmfFormatter.class);
						it.append(".objToStr("); //$NON-NLS-1$
						it.append(getGeneratedTypeAccessor(description.getElementDescription().getElementType()));
						it.append(");"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				it.append("\t/** Initialize the Ecore element when inside a script."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(getCodeElementExtractor().getLanguageScriptInterface());
				it.append(" script, String name, "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" context)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(script, name, context);"); //$NON-NLS-1$
					} else {
						it.append("\t\tsetTypeResolutionContext(context);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (this."); //$NON-NLS-1$
						it.append(generatedObjectFieldName);
						it.append(" == null) {"); //$NON-NLS-1$
						it.newLine();
						if (description.isMemberElement()) {
							it.append("\t\t\tthis.container = script;"); //$NON-NLS-1$
							it.newLine();
						}
						it.append("\t\t\tthis."); //$NON-NLS-1$
						it.append(generatedObjectFieldName);
						it.append(" = "); //$NON-NLS-1$
						it.append(getXFactoryFor(description.getElementDescription().getElementType()));
						it.append(".eINSTANCE.create"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(description.getElementDescription().getElementType().getSimpleName()));
						it.append("();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tscript."); //$NON-NLS-1$
						it.append(getLanguageScriptMemberGetter());
						it.append("().add(this."); //$NON-NLS-1$
						it.append(generatedObjectFieldName);
						it.append(");"); //$NON-NLS-1$
						it.newLine();
						if (description.isAnnotationInfo()) {
							final TypeReference commonType = description.getElementDescription().getCommonSuperType();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedObjectFieldName);
							it.append(".setAnnotationInfo("); //$NON-NLS-1$
							it.append(getXFactoryFor(commonType));
							it.append(".eINSTANCE.create"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(commonType.getSimpleName()));
							it.append("());"); //$NON-NLS-1$
							it.newLine();
						}
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
				if (description.isMemberElement()) {
					it.append("\t/** Initialize the Ecore element when inner type declaration."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void eInit("); //$NON-NLS-1$
					it.append(getCodeElementExtractor().getLanguageTopElementType());
					it.append(" container, String name, "); //$NON-NLS-1$
					it.append(IJvmTypeProvider.class);
					it.append(" context)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\tthis.builder.eInit(container, name, context);"); //$NON-NLS-1$
						} else {
							it.append("\t\tif (this."); //$NON-NLS-1$
							it.append(generatedObjectFieldName);
							it.append(" == null) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis.container = container;"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedObjectFieldName);
							it.append(" = "); //$NON-NLS-1$
							it.append(getXFactoryFor(description.getElementDescription().getElementType()));
							it.append(".eINSTANCE.create"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(description.getElementDescription().getElementType().getSimpleName()));
							it.append("();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tcontainer."); //$NON-NLS-1$
							it.append(getLanguageContainerMemberGetter());
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
				}
				it.append("\t/** Replies the generated " //$NON-NLS-1$
						+ description.getElementDescription().getElementType().getSimpleName() + "."); //$NON-NLS-1$
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
				it.append(description.getElementDescription().getElementType());
				it.append(" "); //$NON-NLS-1$
				it.append(getGeneratedTypeAccessor(description.getElementDescription().getElementType()));
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder."); //$NON-NLS-1$
						it.append(getGeneratedTypeAccessor(description.getElementDescription().getElementType()));
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
						+ description.getElementDescription().getElementType().getSimpleName() + " is attached."); //$NON-NLS-1$
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
					it.append(getGeneratedTypeAccessor(description.getElementDescription().getElementType()));
					it.append(".eResource();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateStandardCommentFunctions(forInterface, forAppender,
						getGeneratedTypeAccessor(description.getElementDescription().getElementType())));
				if (isExtendKeywordFound.get()) {
					String defaultType = getCodeBuilderConfig().getDefaultSupers().get(
							description.getElementDescription().getElementType().getSimpleName());
					if (isExtendsKeywordFound.get()) {
						it.append("\t/** Add the super type."); //$NON-NLS-1$
						it.newLine();
						it.append("\t * @param superType the qualified name of the super type."); //$NON-NLS-1$
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
						it.append("\t * @param superType the qualified name of the super type,"); //$NON-NLS-1$
						it.newLine();
						it.append("\t *     or {@code null} if the default type."); //$NON-NLS-1$
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
							if (description.isMemberElement()) {
								it.append("container"); //$NON-NLS-1$
							} else {
								it.append(generatedObjectFieldName);
							}
							it.append(", superType);"); //$NON-NLS-1$
							it.newLine();
							if (!Strings.isEmpty(defaultType)) {
								it.append("\t\t\t"); //$NON-NLS-1$
								it.append(JvmTypeReference.class);
								it.append(" baseTypeRef = findType(this."); //$NON-NLS-1$
								if (description.isMemberElement()) {
									it.append("container"); //$NON-NLS-1$
								} else {
									it.append(generatedObjectFieldName);
								}
								it.append(", "); //$NON-NLS-1$
								it.append(new TypeReference(defaultType));
								it.append(".class.getCanonicalName());"); //$NON-NLS-1$
								it.newLine();
								it.append("\t\t\tif (isSubTypeOf(this."); //$NON-NLS-1$
								if (description.isMemberElement()) {
									it.append("container"); //$NON-NLS-1$
								} else {
									it.append(generatedObjectFieldName);
								}
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
							if (!Strings.isEmpty(defaultType)) {
								it.append("\t\t\t\treturn;"); //$NON-NLS-1$
							} else {
								it.append("\t\t\treturn;"); //$NON-NLS-1$
							}
							it.newLine();
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
						it.append("\t * @param type the qualified name of the implemented type."); //$NON-NLS-1$
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
								it.append(".getImplements().add(newTypeRef(this."); //$NON-NLS-1$
								if (description.isMemberElement()) {
									it.append("container"); //$NON-NLS-1$
								} else {
									it.append(generatedObjectFieldName);
								}
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
						it.append("\t * @param type the qualified name of the implemented type,"); //$NON-NLS-1$
						it.newLine();
						it.append("\t *     or {@code null} for nothing."); //$NON-NLS-1$
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
								it.append(".isEmpty(type)) ? null : newTypeRef(this."); //$NON-NLS-1$
								if (description.isMemberElement()) {
									it.append("container"); //$NON-NLS-1$
								} else {
									it.append(generatedObjectFieldName);
								}
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
					it.append("\t * @param type the qualified name of the annotation."); //$NON-NLS-1$
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
							it.append("\t\t\tannotation.setAnnotationType(newTypeRef(this."); //$NON-NLS-1$
							if (description.isMemberElement()) {
								it.append("container"); //$NON-NLS-1$
							} else {
								it.append(generatedObjectFieldName);
							}
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
					it.append("\t * @param modifier the modifier to add."); //$NON-NLS-1$
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
				if (hasTypeParameters.get()) {
					if (!forInterface && !forAppender) {
						it.append("\t@"); //$NON-NLS-1$
						it.append(Inject.class);
						it.newLine();
						it.append("\tprivate "); //$NON-NLS-1$
						it.append(Provider.class);
						it.append("<"); //$NON-NLS-1$
						it.append(getTypeParameterBuilderInterface());
						it.append("> iTypeParameterBuilderProvider;"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
					}
					it.append("\t/** Add a type parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the simple name of the type parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the builder of type parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(getTypeParameterBuilderInterface());
					it.append(" addTypeParameter(String name)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\treturn this.builder.addTypeParameter(name);"); //$NON-NLS-1$
						} else {
							it.append("\t\t"); //$NON-NLS-1$
							it.append(getTypeParameterBuilderInterface());
							it.append(" builder = this.iTypeParameterBuilderProvider.get();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tfinal "); //$NON-NLS-1$
							it.append(description.getElementDescription().getElementType());
							it.append(" object = "); //$NON-NLS-1$
							it.append(getGeneratedTypeAccessor(description.getElementDescription().getElementType()));
							it.append(";"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tbuilder.eInit(object, name, getTypeResolutionContext());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tobject.getTypeParameters().add(builder.getJvmTypeParameter());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\treturn builder;"); //$NON-NLS-1$
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

		private final CodeElementExtractor.ElementDescription element;

		private final Collection<CodeElementExtractor.ElementDescription> namedMemberCandidates = new ArrayList<>();

		private final Collection<CodeElementExtractor.ElementDescription> unnamedMemberCandidates = new ArrayList<>();

		private final Collection<CodeElementExtractor.ElementDescription> constructorCandidates = new ArrayList<>();

		private StringConcatenationClient content;

		private final boolean isMemberElement;

		private final boolean isAnnotationInfo;

		/** Constructor.
		 *
		 * @param element the description of the element.
		 * @param isMemberElement indicates if this top element is also a member element.
		 * @param isAnnotationInfo indicates if the top element has annotation info.
		 */
		public TopElementDescription(CodeElementExtractor.ElementDescription element, boolean isMemberElement,
				boolean isAnnotationInfo) {
			this.element = element;
			this.isMemberElement = isMemberElement;
			this.isAnnotationInfo = isAnnotationInfo;
		}

		@Override
		public String toString() {
			return this.element.getName();
		}

		/** Replies the element description embedded in this top element description.
		 *
		 * @return the element description.
		 */
		public CodeElementExtractor.ElementDescription getElementDescription() {
			return this.element;
		}

		/** Replies if this top element is a member element too.
		 *
		 * @return <code>true</code> if the top element is also a member element.
		 */
		public boolean isMemberElement() {
			return this.isMemberElement;
		}

		/** Replies if this top element has annotation info.
		 *
		 * @return <code>true</code> if the top element has annotation info.
		 */
		public boolean isAnnotationInfo() {
			return this.isAnnotationInfo;
		}

		/** Replies the named members.
		 *
		 * @return the named members.
		 */
		@Pure
		public Collection<CodeElementExtractor.ElementDescription> getNamedMembers() {
			return this.namedMemberCandidates;
		}

		/** Replies the unnamed members.
		 *
		 * @return the unnamed members.
		 */
		@Pure
		public Collection<CodeElementExtractor.ElementDescription> getUnnamedMembers() {
			return this.unnamedMemberCandidates;
		}

		/** Replies the constructors.
		 *
		 * @return the constructors.
		 */
		@Pure
		public Collection<CodeElementExtractor.ElementDescription> getConstructors() {
			return this.constructorCandidates;
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

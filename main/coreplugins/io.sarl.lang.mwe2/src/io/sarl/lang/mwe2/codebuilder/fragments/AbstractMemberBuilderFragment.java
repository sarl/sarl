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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.inject.Inject;
import javax.inject.Provider;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.util.EmfFormatter;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsFactory;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

import io.sarl.lang.mwe2.codebuilder.extractor.CodeElementExtractor;

/** Generator of the builder for members.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractMemberBuilderFragment extends AbstractSubCodeBuilderFragment {

	/** Replies the members to generate.
	 *
	 * @return the members.
	 */
	protected abstract Iterable<MemberDescription> getMembers();

	@Override
	public void generate() {
		final Iterable<MemberDescription> members = getMembers();
		for (final MemberDescription description : members) {
			generateIMemberBuilder(description);
		}
		final boolean enableAppenders = getCodeBuilderConfig().isISourceAppendableEnable();
		for (final MemberDescription description : members) {
			generateMemberBuilderImpl(description);
			if (enableAppenders) {
				generateMemberAppender(description);
			}
		}
		super.generate();
	}

	@Override
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		for (final MemberDescription description : getMembers()) {
			if (!description.isTopElement()) {
				bindElementDescription(factory, description.getElementDescription());
			}
		}
	}

	/** Generate the member builder interface.
	 *
	 * @param description the description of the member.
	 */
	protected void generateIMemberBuilder(MemberDescription description) {
		if (description.isTopElement()) {
			return;
		}
		final TypeReference builder = description.getElementDescription().getBuilderInterfaceType();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " " + description.getElementDescription().getName() + "."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public interface "); //$NON-NLS-1$
				it.append(builder.getSimpleName());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateMembers(description, true, false));
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the memberbuilder implementation.
	 *
	 * @param description the description of the member.
	 */
	protected void generateMemberBuilderImpl(MemberDescription description) {
		if (description.isTopElement()) {
			return;
		}
		final TypeReference builderInterface = description.getElementDescription().getBuilderInterfaceType();
		final TypeReference builder = description.getElementDescription().getBuilderImplementationType();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " " + description.getElementDescription().getName() + "."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(builder.getSimpleName());
				it.append(" extends "); //$NON-NLS-1$
				it.append(getAbstractBuilderImpl());
				it.append(" implements "); //$NON-NLS-1$
				it.append(builderInterface);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateMembers(description, false, false));
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the member appender.
	 *
	 * @param description the description of the member.
	 */
	protected void generateMemberAppender(MemberDescription description) {
		if (description.isTopElement()) {
			return;
		}
		final TypeReference appender = description.getElementDescription().getAppenderType();
		final String generatedFieldAccessor = getGeneratedMemberAccessor(description);
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Source appender of a " + getLanguageName() //$NON-NLS-1$
						+ " " + description.getElementDescription().getName() + "."); //$NON-NLS-1$ //$NON-NLS-2$
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
				it.append(description.getElementDescription().getBuilderInterfaceType());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateAppenderMembers(appender.getSimpleName(),
						description.getElementDescription().getBuilderInterfaceType(), generatedFieldAccessor));
				it.append(generateMembers(description, false, true));
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(appender, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Replies the name of the accessor that replies the generated member.
	 *
	 * @param description the description of the member to generate.
	 * @return the accessor name.
	 */
	@SuppressWarnings("static-method")
	protected String getGeneratedMemberAccessor(MemberDescription description) {
		return "get" //$NON-NLS-1$
				+ Strings.toFirstUpper(description.getElementDescription().getElementType().getSimpleName()) + "()"; //$NON-NLS-1$
	}

	/** Generate the members of the builder.
	 *
	 * @param description the description of the member.
	 * @param forInterface <code>true</code> if the code must be generated for an interface.
	 * @param forAppender <code>true</code> if the code must be generated for an appender.
	 * @return the code.
	 */
	@SuppressWarnings("checkstyle:all")
	protected StringConcatenationClient generateMembers(MemberDescription description, boolean forInterface,
			boolean forAppender) {
		final TypeReference generatedType = description.getElementDescription().getElementType();
		final String generatedFieldName = Strings.toFirstLower(generatedType.getSimpleName());
		final String generatedFieldAccessor = getGeneratedMemberAccessor(description);

		final AtomicBoolean hasName = new AtomicBoolean(false);
		final AtomicBoolean hasTypeName = new AtomicBoolean(false);
		final AtomicBoolean hasType = new AtomicBoolean(false);
		final AtomicBoolean hasParameters = new AtomicBoolean(false);
		final AtomicBoolean hasReturnType = new AtomicBoolean(false);
		final AtomicBoolean hasThrows = new AtomicBoolean(false);
		final AtomicBoolean hasFires = new AtomicBoolean(false);
		final AtomicBoolean hasBlock = new AtomicBoolean(false);
		final AtomicBoolean isAnnotated = new AtomicBoolean(false);
		final AtomicBoolean hasModifiers = new AtomicBoolean(false);
		final AtomicBoolean hasTypeParameters = new AtomicBoolean(false);
		final List<String> expressions = new ArrayList<>();
		for (Assignment assignment : GrammarUtil.containedAssignments(description.getElementDescription().getGrammarComponent())) {
			if (Objects.equals(getCodeBuilderConfig().getModifierListGrammarName(), assignment.getFeature())) {
				hasModifiers.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getTypeParameterListGrammarName(), assignment.getFeature())) {
				hasTypeParameters.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getAnnotationListGrammarName(), assignment.getFeature())) {
				isAnnotated.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getMemberNameExtensionGrammarName(), assignment.getFeature())) {
				hasName.set(true);
				if (nameMatches(assignment.getTerminal(), getCodeBuilderConfig().getTypeReferenceGrammarPattern())) {
					hasTypeName.set(true);
				}
			} else if (Objects.equals(getCodeBuilderConfig().getMemberTypeExtensionGrammarName(),
					assignment.getFeature())) {
				hasType.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getParameterListGrammarName(),
					assignment.getFeature())) {
				hasParameters.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getMemberThrowsExtensionGrammarName(),
					assignment.getFeature())) {
				hasThrows.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getMemberFiresExtensionGrammarName(),
					assignment.getFeature())) {
				hasFires.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getMemberReturnTypeExtensionGrammarName(),
					assignment.getFeature())) {
				hasReturnType.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getMemberBlockExpressionExtensionGrammarName(),
					assignment.getFeature())) {
				if (nameMatches(assignment.getTerminal(), getCodeBuilderConfig().getExpression().getBlockExpressionGrammarPattern())) {
					hasBlock.set(true);
				}
			} else if (nameMatches(assignment.getTerminal(), getCodeBuilderConfig().getExpression().getExpressionGrammarPattern())) {
				expressions.add(assignment.getFeature());
			}
		}

		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				if (!forInterface && !forAppender) {
					it.append("\t@"); //$NON-NLS-1$
					it.append(Inject.class);
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(Provider.class);
					it.append("<"); //$NON-NLS-1$
					it.append(getFormalParameterBuilderInterface());
					it.append("> parameterProvider;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Inject.class);
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(Provider.class);
					it.append("<"); //$NON-NLS-1$
					it.append(getBlockExpressionBuilderInterface());
					it.append("> blockExpressionProvider;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Inject.class);
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(Provider.class);
					it.append("<"); //$NON-NLS-1$
					it.append(getExpressionBuilderInterface());
					it.append("> expressionProvider;"); //$NON-NLS-1$
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(EObject.class);
					it.append(" container;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(generatedType);
					it.append(" "); //$NON-NLS-1$
					it.append(generatedFieldName);
					it.append(";"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
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
				it.append("\t/** Initialize the Ecore element."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param container the container of the " //$NON-NLS-1$
						+ description.getElementDescription().getName() + "."); //$NON-NLS-1$
				it.newLine();
				if (hasName.get()) {
					it.append("\t * @param name the "); //$NON-NLS-1$
					if (hasTypeName.get()) {
						it.append("type"); //$NON-NLS-1$
					} else {
						it.append("name"); //$NON-NLS-1$
					}
					it.append(" of the " + description.getElementDescription().getName() + "."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
				}
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(getCodeElementExtractor().getLanguageTopElementType());
				it.append(" container, "); //$NON-NLS-1$
				if (hasName.get()) {
					it.append("String name, "); //$NON-NLS-1$
				}
				if (description.getModifiers().size() > 1) {
					it.append("String modifier, "); //$NON-NLS-1$
				}
				it.append(IJvmTypeProvider.class);
				it.append(" context)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(container, "); //$NON-NLS-1$
						if (hasName.get()) {
							it.append("name, "); //$NON-NLS-1$
						}
						if (description.getModifiers().size() > 1) {
							it.append("modifier, "); //$NON-NLS-1$
						}
						it.append("context);"); //$NON-NLS-1$
					} else {
						it.append("\t\tsetTypeResolutionContext(context);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (this."); //$NON-NLS-1$
						it.append(generatedFieldName);
						it.append(" == null) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis.container = container;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis."); //$NON-NLS-1$
						it.append(generatedFieldName);
						it.append(" = "); //$NON-NLS-1$
						it.append(getXFactoryFor(generatedType));
						it.append(".eINSTANCE.create"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(generatedType.getSimpleName()));
						it.append("();"); //$NON-NLS-1$
						it.newLine();
						if (description.isAnnotationInfo()) {
							final TypeReference commonSuperType = description.getElementDescription().getCommonSuperType();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".setAnnotationInfo("); //$NON-NLS-1$
							it.append(getXFactoryFor(commonSuperType));
							it.append(".eINSTANCE.create"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(commonSuperType.getSimpleName()));
							it.append("());"); //$NON-NLS-1$
							it.newLine();
						}
						if (hasName.get()) {
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".setName("); //$NON-NLS-1$
							if (hasTypeName.get()) {
								it.append("newTypeRef(container, name)"); //$NON-NLS-1$
							} else {
								it.append("name"); //$NON-NLS-1$
							}
							it.append(");"); //$NON-NLS-1$
							it.newLine();
						}
						if (description.getModifiers().size() > 1) {
							it.append("\t\t\tif ("); //$NON-NLS-1$
							boolean first = true;
							for (String mod : description.getModifiers()) {
								if (first) {
									first = false;
								} else {
									it.newLine();
									it.append("\t\t\t\t|| "); //$NON-NLS-1$
								}
								it.append(Strings.class);
								it.append(".equal(modifier, \""); //$NON-NLS-1$
								it.append(Strings.convertToJavaString(mod));
								it.append("\")"); //$NON-NLS-1$
							}
							it .append(") {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".getModifiers().add(modifier);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t} else {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\tthrow new IllegalStateException(\"Invalid modifier\");"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t}"); //$NON-NLS-1$
							it.newLine();
						} else if (description.getModifiers().size() == 1) {
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".getModifiers().add(\""); //$NON-NLS-1$
							it.append(Strings.convertToJavaString(description.getModifiers().iterator().next()));
							it.append("\");"); //$NON-NLS-1$
							it.newLine();
						}
						it.append("\t\t\tcontainer.get"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName()));
						it.append("().add(this."); //$NON-NLS-1$
						it.append(generatedFieldName);
						it.append(");"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the generated element."); //$NON-NLS-1$
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
				it.append(generatedType);
				it.append(" "); //$NON-NLS-1$
				it.append(generatedFieldAccessor);
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder."); //$NON-NLS-1$
						it.append(generatedFieldAccessor);
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn this."); //$NON-NLS-1$
						it.append(generatedFieldName);
						it.append(";"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the resource."); //$NON-NLS-1$
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
					it.append(generatedFieldAccessor);
					it.append(".eResource();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateStandardCommentFunctions(forInterface, forAppender, generatedFieldAccessor));
				if (hasType.get()) {
					it.append("\t/** Change the type."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the type of the member."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void set"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberTypeExtensionGrammarName()));
					it.append("(String type)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\tthis.builder.set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberTypeExtensionGrammarName()));
							it.append("(type);"); //$NON-NLS-1$
						} else {
							it.append("\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberTypeExtensionGrammarName()));
							it.append("(newTypeRef(this.container, type));"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (hasParameters.get()) {
					it.append("\t/** Add a formal parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the formal parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(getFormalParameterBuilderInterface());
					it.append(" add"); //$NON-NLS-1$
					it.append(toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getParameterListGrammarName())));
					it.append("(String name)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\treturn this.builder.add"); //$NON-NLS-1$
							it.append(toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getParameterListGrammarName())));
							it.append("(name);"); //$NON-NLS-1$
						} else {
							it.append("\t\t"); //$NON-NLS-1$
							it.append(getFormalParameterBuilderInterface());
							it.append(" builder = this.parameterProvider.get();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tbuilder.eInit(this."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(", name, getTypeResolutionContext());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\treturn builder;"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (hasThrows.get()) {
					it.append("\t/** Add a throwable exception."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the fully qualified name of the exception."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void add"); //$NON-NLS-1$
					it.append(toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getMemberThrowsExtensionGrammarName())));
					it.append("(String type)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\tthis.builder.add"); //$NON-NLS-1$
							it.append(toSingular(Strings.toFirstUpper(getCodeBuilderConfig()
									.getMemberThrowsExtensionGrammarName())));
							it.append("(type);"); //$NON-NLS-1$
						} else {
							it.append("\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".get"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberThrowsExtensionGrammarName()));
							it.append("().add(newTypeRef(this.container, type));"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (hasFires.get()) {
					it.append("\t/** Add a fired exception."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the fully qualified name of the event."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void add"); //$NON-NLS-1$
					it.append(toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getMemberFiresExtensionGrammarName())));
					it.append("(String type)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\tthis.builder.add"); //$NON-NLS-1$
							it.append(toSingular(Strings.toFirstUpper(getCodeBuilderConfig()
									.getMemberFiresExtensionGrammarName())));
							it.append("(type);"); //$NON-NLS-1$
						} else {
							it.append("\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".get"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberFiresExtensionGrammarName()));
							it.append("().add(newTypeRef(this.container, type));"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (hasReturnType.get()) {
					it.append("\t/** Change the return type."); //$NON-NLS-1$
					it.newLine();
					it.append("\t @param type the return type of the member."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void set"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberReturnTypeExtensionGrammarName()));
					it.append("(String type)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\tthis.builder.set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberReturnTypeExtensionGrammarName()));
							it.append("(type);"); //$NON-NLS-1$
						} else {
							it.append("\t\tif (!"); //$NON-NLS-1$
							it.append(Strings.class);
							it.append(".isEmpty(type)"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t&& !"); //$NON-NLS-1$
							it.append(Objects.class);
							it.append(".equals(\"void\", type)"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t&& !"); //$NON-NLS-1$
							it.append(Objects.class);
							it.append(".equals(Void.class.getName(), type)) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberReturnTypeExtensionGrammarName()));
							it.append("(newTypeRef(container, type));"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t} else {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberReturnTypeExtensionGrammarName()));
							it.append("(null);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t}"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				for (String expressionName : expressions) {
					it.append("\t/** Change the " + expressionName + "."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
					it.append("\t * @param value the value of the "); //$NON-NLS-1$
					it.append(expressionName);
					it.append(". It may be {@code null}."); //$NON-NLS-1$
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
					it.append(getExpressionBuilderInterface());
					it.append(" get"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(expressionName));
					it.append("()"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\treturn this.builder.get"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(expressionName));
							it.append("();"); //$NON-NLS-1$
						} else {
							it.append("\t\t"); //$NON-NLS-1$
							it.append(getExpressionBuilderInterface());
							it.append(" exprBuilder = this.expressionProvider.get();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\texprBuilder.eInit("); //$NON-NLS-1$
							it.append(generatedFieldAccessor);
							it.append(", new "); //$NON-NLS-1$
							it.append(Procedures.class);
							it.append(".Procedure1<"); //$NON-NLS-1$
							it.append(XExpression.class);
							it.append(">() {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\tpublic void apply("); //$NON-NLS-1$
							it.append(XExpression.class);
							it.append(" expr) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t\t"); //$NON-NLS-1$
							it.append(generatedFieldAccessor);
							it.append(".set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(expressionName));
							it.append("(expr);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t}"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t}, getTypeResolutionContext());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\treturn exprBuilder;"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (hasBlock.get()) {
					it.append("\t/** Create the block of code."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the block builder."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(getBlockExpressionBuilderInterface());
					it.append(" get"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig()
							.getMemberBlockExpressionExtensionGrammarName()));
					it.append("()"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\treturn this.builder.get"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig()
									.getMemberBlockExpressionExtensionGrammarName()));
							it.append("();"); //$NON-NLS-1$
						} else {
							it.append("\t\t"); //$NON-NLS-1$
							it.append(getBlockExpressionBuilderInterface());
							it.append(" block = this.blockExpressionProvider.get();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tblock.eInit(getTypeResolutionContext());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t"); //$NON-NLS-1$
							it.append(XBlockExpression.class);
							it.append(" expr = block.getXBlockExpression();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig()
									.getMemberBlockExpressionExtensionGrammarName()));
							it.append("(expr);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\treturn block;"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (isAnnotated.get()) {
					it.append("\t/** Add an annotation."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the qualified name of the annotation"); //$NON-NLS-1$
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
							it.append(generatedFieldAccessor);
							it.append(", type).getType());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(generatedFieldAccessor);
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
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(generatedFieldAccessor);
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
						it.append(generatedFieldAccessor);
						it.append(");"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
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
							it.append(generatedType);
							it.append(" object = "); //$NON-NLS-1$
							it.append(generatedFieldAccessor);
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
		};
	}

	/** Description of a member.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class MemberDescription {

		private final CodeElementExtractor.ElementDescription element;

		private final CodeElementExtractor.ElementDescription container;

		private final List<String> modifiers;

		private final Set<String> containers = new HashSet<>();

		private final Set<String> noBodyContainers = new HashSet<>();

		private final boolean isTopElement;

		private final boolean isAnnotationInfo;

		/** Constructor.
		 *
		 * @param element general description of the element.
		 * @param container description of the container.
		 * @param isTopElement indicates if the element is a top element too.
		 * @param annotationInfo indicates if the annotationInfo field is declared in the element.
		 * @param modifiers the modifiers.
		 */
		public MemberDescription(CodeElementExtractor.ElementDescription element,
				CodeElementExtractor.ElementDescription container,
				boolean isTopElement,
				boolean annotationInfo,
				List<String> modifiers) {
			this.element = element;
			this.container = container;
			this.isTopElement = isTopElement;
			this.isAnnotationInfo = annotationInfo;
			this.modifiers = modifiers;
		}

		/** Replies the element description embedded in this member element description.
		 *
		 * @return the element description.
		 */
		public CodeElementExtractor.ElementDescription getElementDescription() {
			return this.element;
		}

		/** Replies the container description embedded in this member element description.
		 *
		 * @return the element description.
		 */
		public CodeElementExtractor.ElementDescription getContainerDescription() {
			return this.container;
		}

		@Override
		public String toString() {
			return this.element.getName();
		}

		/** Replies if this element is a top element too.
		 *
		 * @return <code>true</code> if the element is a top element.
		 */
		public boolean isTopElement() {
			return this.isTopElement;
		}

		/** Replies if this element has the annotationInfo field.
		 *
		 * @return <code>true</code> if the field was declared.
		 */
		public boolean isAnnotationInfo() {
			return this.isAnnotationInfo;
		}

		/** Replies the modifiers.
		 *
		 * @return the modifiers.
		 */
		@Pure
		public List<String> getModifiers() {
			if (this.modifiers == null) {
				return Collections.emptyList();
			}
			return this.modifiers;
		}

		/** Replies the standard containers.
		 *
		 * @return the standard containers.
		 */
		@Pure
		public Set<String> getStandardContainers() {
			return this.containers;
		}

		/** Replies the no-action-body containers.
		 *
		 * @return the no-action-body containers.
		 */
		@Pure
		public Set<String> getNoBodyContainers() {
			return this.noBodyContainers;
		}

	}

}

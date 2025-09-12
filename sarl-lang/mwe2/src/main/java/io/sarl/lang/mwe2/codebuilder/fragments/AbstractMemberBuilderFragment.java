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

package io.sarl.lang.mwe2.codebuilder.fragments;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.TypeRef;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
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
import org.eclipse.xtext.xtext.generator.model.TypeReference;

import com.google.inject.Inject;
import com.google.inject.Provider;

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
		final var members = getMembers();
		for (final var description : members) {
			generateIMemberBuilder(description);
		}
		final var enableAppenders = getCodeBuilderConfig().isISourceAppendableEnable();
		for (final var description : members) {
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
		for (final var description : getMembers()) {
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
		final var builder = description.getElementDescription().builderInterfaceType();
		final var content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " " + description.getElementDescription().name() + "."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				appendFileLineComment(it);
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
		final var javaFile = getFileAccessFactory().createJavaFile(builder, content);
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
		final var builderInterface = description.getElementDescription().builderInterfaceType();
		final var builder = description.getElementDescription().builderImplementationType();
		final var content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " " + description.getElementDescription().name() + "."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				it.append(" * "); //$NON-NLS-1$
				it.append(getFileAndLineNumber(0));
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
		final var javaFile = getFileAccessFactory().createJavaFile(builder, content);
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
		final var appender = description.getElementDescription().appenderType();
		final var generatedFieldAccessor = getGeneratedMemberAccessor(description);
		final var content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Source appender of a " + getLanguageName() //$NON-NLS-1$
						+ " " + description.getElementDescription().name() + "."); //$NON-NLS-1$ //$NON-NLS-2$
				it.newLine();
				appendFileLineComment(it);
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(appender.getSimpleName());
				it.append(" extends "); //$NON-NLS-1$
				it.append(getCodeElementExtractor().getAbstractAppenderImpl());
				it.append(" implements "); //$NON-NLS-1$
				it.append(description.getElementDescription().builderInterfaceType());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateAppenderMembers(appender.getSimpleName(),
						description.getElementDescription().builderInterfaceType(), generatedFieldAccessor));
				it.append(generateMembers(description, false, true));
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
		final var javaFile = getFileAccessFactory().createJavaFile(appender, content);
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
				+ Strings.toFirstUpper(description.getElementDescription().elementType().getSimpleName()) + "()"; //$NON-NLS-1$
	}
	
	/** Replies if the given type corresponds to a {@code parameters} feature for a formal parameter.
	 *
	 * @param referenceType the type that should be corresponds to the feature.
	 * @return {@code true} if the given reference type corresponds to a formal parameter.
	 */
	protected boolean isFormalParameterFeature(TypeRef referenceType) {
		if (referenceType.getClassifier() instanceof EClass clazz) {
			final var superType = getCodeElementExtractor().getFormalParameterSuperEClass();
			return isAssignableFrom(superType, clazz);
		}
		return false;
	}

	/** Generate the members of the builder.
	 *
	 * @param description the description of the member.
	 * @param forInterface {@code true} if the code must be generated for an interface.
	 * @param forAppender {@code true} if the code must be generated for an appender.
	 * @return the code.
	 */
	protected StringConcatenationClient generateMembers(MemberDescription description, boolean forInterface,
			boolean forAppender) {
		final var builderType = description.getElementDescription().builderInterfaceType();
		final var generatedType = description.getElementDescription().elementType();
		final var generatedFieldName = Strings.toFirstLower(generatedType.getSimpleName());
		final var generatedFieldAccessor = getGeneratedMemberAccessor(description);

		final var hasName = new AtomicBoolean(false);
		final var hasTypeName = new AtomicBoolean(false);
		final var hasType = new AtomicBoolean(false);
		final var hasFormalParameters = new AtomicBoolean(false);
		final var hasReturnType = new AtomicBoolean(false);
		final var hasThrows = new AtomicBoolean(false);
		final var hasFires = new AtomicBoolean(false);
		final var hasBlock = new AtomicBoolean(false);
		final var isAnnotated = new AtomicBoolean(false);
		final var hasModifiers = new AtomicBoolean(false);
		final var hasTypeParameters = new AtomicBoolean(false);
		final var complexParameters = new HashSet<TypeReference>();
		final var expressions = new ArrayList<String>();
		for (final var assignment : GrammarUtil.containedAssignments(description.getElementDescription().grammarComponent())) {
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
			} else if (getCodeBuilderConfig().getIndirectlyNamedMemberExtensionGrammarNames().contains(assignment.getFeature())) {
				hasName.set(true);
				hasTypeName.set(false);
			} else if (Objects.equals(getCodeBuilderConfig().getMemberTypeExtensionGrammarName(),
					assignment.getFeature())) {
				hasType.set(true);
			} else if (Objects.equals(getCodeBuilderConfig().getParameterListGrammarName(),
					assignment.getFeature())) {
				if (assignment.getTerminal() instanceof RuleCall ruleCall) {
					final var type = ruleCall.getRule().getType();
					if (isFormalParameterFeature(type)) {
						hasFormalParameters.set(true);
					} else {
						complexParameters.add(getCodeElementExtractor().newTypeReference(type.getClassifier()));
					}
				}
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
					if (hasFormalParameters.get()) {
						appendEmptyComment(it);
						it.append("\t@"); //$NON-NLS-1$
						it.append(Inject.class);
						it.newLine();
						it.append("\tprivate "); //$NON-NLS-1$
						it.append(Provider.class);
						it.append("<"); //$NON-NLS-1$
						it.append(getFormalParameterBuilderInterface());
						it.append("> parameterProvider;"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
					}
					if (hasBlock.get()) {
						appendEmptyComment(it);
						it.append("\t@"); //$NON-NLS-1$
						it.append(Inject.class);
						it.newLine();
						it.append("\tprivate "); //$NON-NLS-1$
						it.append(Provider.class);
						it.append("<"); //$NON-NLS-1$
						it.append(getBlockExpressionBuilderInterface());
						it.append("> blockExpressionProvider;"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
					}
					if (!expressions.isEmpty()) {
						appendEmptyComment(it);
						it.append("\t@"); //$NON-NLS-1$
						it.append(Inject.class);
						it.newLine();
						it.append("\tprivate "); //$NON-NLS-1$
						it.append(Provider.class);
						it.append("<"); //$NON-NLS-1$
						it.append(getExpressionBuilderInterface());
						it.append("> expressionProvider;"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
					}
					appendEmptyComment(it);
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(EObject.class);
					it.append(" container;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					appendEmptyComment(it);
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(generatedType);
					it.append(" "); //$NON-NLS-1$
					it.append(generatedFieldName);
					it.append(";"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					if (hasBlock.get()) {
						appendEmptyComment(it);
						it.append("\tprivate "); //$NON-NLS-1$
						it.append(getBlockExpressionBuilderInterface());
						it.append(" internalBlockExpression"); //$NON-NLS-1$
						it.append(";"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
					}
					for (final var complexParameter : complexParameters) {
						appendEmptyComment(it);
						it.append("\t@"); //$NON-NLS-1$
						it.append(getCodeBuilderConfig().getInjectionAPI().getInjectType());
						it.newLine();
						it.append("\tprivate "); //$NON-NLS-1$
						it.append(Provider.class);
						it.append("<"); //$NON-NLS-1$
						it.append(getCodeElementExtractor().getComplexParameterBuilderInterface(complexParameter));
						it.append("> "); //$NON-NLS-1$
						it.append(Strings.toFirstLower(complexParameter.getSimpleName()));
						it.append("BuilderProvider;"); //$NON-NLS-1$
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
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(JvmTypeReference.class);
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
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(JvmTypeReference.class);
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
					it.append("\t/** Find the reference to the type and type parameters."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the type to reference"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param args the type arguments to put in the reference to the given type"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the type reference."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(JvmTypeReference.class);
					it.append(" newTypeRef("); //$NON-NLS-1$
					it.append(JvmType.class);
					it.append(" type, "); //$NON-NLS-1$
					it.append(JvmTypeReference.class);
					it.append("... args)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn this.builder.newTypeRef(type, args);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Find the reference to the type and type parameters."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the type to reference"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param args the type arguments to put in the reference to the given type"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the type reference."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(JvmTypeReference.class);
					it.append(" newTypeRef("); //$NON-NLS-1$
					it.append(Class.class);
					it.append(" type, "); //$NON-NLS-1$
					it.append(JvmTypeReference.class);
					it.append("... args)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn this.builder.newTypeRef(type, args);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Find the reference to the type and type parameters."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param context the context in which the type is defined"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the type to reference"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param args the type arguments to put in the reference to the given type"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the type reference."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(JvmTypeReference.class);
					it.append(" newTypeRef("); //$NON-NLS-1$
					it.append(Notifier.class);
					it.append(" context, "); //$NON-NLS-1$
					it.append(Class.class);
					it.append(" type, "); //$NON-NLS-1$
					it.append(JvmTypeReference.class);
					it.append("... args)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn this.builder.newTypeRef(context, type, args);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (forInterface) {
					it.append("\t/** Dispose the resource."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\tvoid dispose();"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				} else if (forAppender) {
					it.append("\t/** Dispose the resource."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
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
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					it.append(IJvmTypeProvider.class);
					it.append(" getTypeResolutionContext();"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				} else if (forAppender) {
					it.append("\t/** Replies the context for type resolution."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the context or {@code null} if the Ecore object is the context."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
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
						+ description.getElementDescription().name() + "."); //$NON-NLS-1$
				it.newLine();
				if (hasName.get()) {
					it.append("\t * @param name the "); //$NON-NLS-1$
					if (hasTypeName.get()) {
						it.append("type"); //$NON-NLS-1$
					} else {
						it.append("name"); //$NON-NLS-1$
					}
					it.append(" of the " + description.getElementDescription().name() + "."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
				}
				if (description.getModifiers().size() > 1) {
					it.append("\t * @param modifier the major/default modifier to be associated to the member."); //$NON-NLS-1$
					it.newLine();
				}
				it.append("\t * @param context the context in which type resolution must be applied."); //$NON-NLS-1$
				it.newLine();
				appendFileLineComment(it);
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(EObject.class);
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
					} else if (hasName.get() && hasTypeName.get()) {
						it.append("\t\t// Generator code: "); //$NON-NLS-1$
						it.append(getFileAndLineNumber(0));
						it.newLine();
						it.append("\t\tif (this."); //$NON-NLS-1$
						it.append(generatedFieldName);
						it.append(" == null) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis."); //$NON-NLS-1$
						it.append(generatedFieldName);
						it.append(" = "); //$NON-NLS-1$
						it.append(getXFactoryFor(generatedType));
						it.append(".eINSTANCE.create"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(generatedType.getSimpleName()));
						it.append("();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tinternalEInit(container, context);"); //$NON-NLS-1$
						it.newLine();
						if (hasName.get()) {
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(JvmTypeReference.class);
							it.append(" ref = newTypeRef(container, name);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tif (ref instanceof "); //$NON-NLS-1$
							it.append(JvmParameterizedTypeReference.class);
							it.append(" pref) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".setName(pref);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t} else {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\tthrow new "); //$NON-NLS-1$
							it.append(IllegalArgumentException.class);
							it.append("(name);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t}"); //$NON-NLS-1$
							it.newLine();
						}
						if (description.getModifiers().size() > 1) {
							it.append("\t\t\tif ("); //$NON-NLS-1$
							var first = true;
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
						it.append("\t\t}"); //$NON-NLS-1$
					} else {
						it.append("\t\t// Generator code: "); //$NON-NLS-1$
						it.append(getFileAndLineNumber(0));
						it.newLine();
						it.append("\t\tif (this."); //$NON-NLS-1$
						it.append(generatedFieldName);
						it.append(" == null) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis."); //$NON-NLS-1$
						it.append(generatedFieldName);
						it.append(" = "); //$NON-NLS-1$
						it.append(getXFactoryFor(generatedType));
						it.append(".eINSTANCE.create"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(generatedType.getSimpleName()));
						it.append("();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tinternalEInit(container, context);"); //$NON-NLS-1$
						it.newLine();
						if (hasName.get()) {
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".setName(name);"); //$NON-NLS-1$
							it.newLine();
						}
						if (description.getModifiers().size() > 1) {
							it.append("\t\t\tif ("); //$NON-NLS-1$
							var first = true;
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
						it.append("\t\t}"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				if (hasName.get() && hasTypeName.get()) {
					it.append("\t/** Initialize the Ecore element."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param container the container of the " //$NON-NLS-1$
							+ description.getElementDescription().name() + "."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the type of the " + description.getElementDescription().name() + "."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
					it.append("\t * @param context the context in which type resolution must be applied."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void eInit("); //$NON-NLS-1$
					it.append(EObject.class);
					it.append(" container, "); //$NON-NLS-1$
					it.append(JvmParameterizedTypeReference.class);
					it.append(" name, "); //$NON-NLS-1$
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
							it.append("\t\tthis.builder.eInit(container, name, "); //$NON-NLS-1$
							if (description.getModifiers().size() > 1) {
								it.append("modifier, "); //$NON-NLS-1$
							}
							it.append("context);"); //$NON-NLS-1$
						} else {
							it.append("\t\t// Generator code: "); //$NON-NLS-1$
							it.append(getFileAndLineNumber(0));
							it.newLine();
							it.append("\t\tif (this."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(" == null) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(" = "); //$NON-NLS-1$
							it.append(getXFactoryFor(generatedType));
							it.append(".eINSTANCE.create"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(generatedType.getSimpleName()));
							it.append("();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tinternalEInit(container, context);"); //$NON-NLS-1$
							it.newLine();
							if (hasName.get()) {
								it.append("\t\t\tthis."); //$NON-NLS-1$
								it.append(generatedFieldName);
								it.append(".setName(name);"); //$NON-NLS-1$
								it.newLine();
							}
							if (description.getModifiers().size() > 1) {
								it.append("\t\t\tif ("); //$NON-NLS-1$
								var first = true;
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
							it.append("\t\t}"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (!forAppender && !forInterface) {
					it.append("\tprivate void internalEInit("); //$NON-NLS-1$
					it.append(EObject.class);
					it.append(" container, "); //$NON-NLS-1$
					it.append(IJvmTypeProvider.class);
					it.append(" context) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t// Generator code: "); //$NON-NLS-1$
					it.append(getFileAndLineNumber(0));
					it.newLine();
					it.append("\t\tassert this."); //$NON-NLS-1$
					it.append(generatedFieldName);
					it.append(" != null;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tsetTypeResolutionContext(context);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tthis.container = container;"); //$NON-NLS-1$
					it.newLine();
					if (description.isAnnotationInfo()) {
						final var commonSuperType = description.getElementDescription().commonSuperType();
						it.append("\t\tthis."); //$NON-NLS-1$
						it.append(generatedFieldName);
						it.append(".setAnnotationInfo("); //$NON-NLS-1$
						it.append(getXFactoryFor(commonSuperType));
						it.append(".eINSTANCE.create"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(commonSuperType.getSimpleName()));
						it.append("());"); //$NON-NLS-1$
						it.newLine();
					}
					it.append("\t\tif (container instanceof "); //$NON-NLS-1$
					it.append(XtendTypeDeclaration.class);
					it.append(" typeDeclaration) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\ttypeDeclaration.get"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName()));
					it.append("().add(this."); //$NON-NLS-1$
					it.append(generatedFieldName);
					it.append(");"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tthis."); //$NON-NLS-1$
					it.append(generatedFieldName);
					it.append(".setDeclaringType(typeDeclaration);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				it.append("\t/** Replies the generated element."); //$NON-NLS-1$
				it.newLine();
				appendFileLineComment(it);
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
				appendFileLineComment(it);
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
				it.append(generateStandardCommentFunctions(forInterface, forAppender, generatedFieldAccessor,
						description.getElementDescription().builderInterfaceType()));
				if (hasType.get()) {
					it.append("\t/** Change the type."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the type of the member."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return {@code this}"); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(description.getElementDescription().builderInterfaceType());
					it.append(" set"); //$NON-NLS-1$
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
							it.append("\t\t"); //$NON-NLS-1$
							it.append(JvmTypeReference.class);
							it.append(" ref = newTypeRef(this.container, type);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tset"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberTypeExtensionGrammarName()));
							it.append("(ref);"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t\treturn this;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Change the type."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the type of the member."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return {@code this}"); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(description.getElementDescription().builderInterfaceType());
					it.append(" set"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberTypeExtensionGrammarName()));
					it.append("("); //$NON-NLS-1$
					it.append(JvmTypeReference.class);
					it.append(" type)"); //$NON-NLS-1$
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
							it.append("(type);"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t\treturn this;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (hasFormalParameters.get()) {
					it.append("\t/** Add a formal parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the formal parameter."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
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
					it.append("\t * @return {@code this}"); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(builderType);
					it.append(" add"); //$NON-NLS-1$
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
							it.append("\t\t"); //$NON-NLS-1$
							it.append(JvmTypeReference.class);
							it.append(" ref = newTypeRef(this.container, type);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tadd"); //$NON-NLS-1$
							it.append(toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getMemberThrowsExtensionGrammarName())));
							it.append("(ref);"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t\treturn this;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Add a throwable exception."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the exception."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return {@code this}"); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(builderType);
					it.append(" add"); //$NON-NLS-1$
					it.append(toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getMemberThrowsExtensionGrammarName())));
					it.append("("); //$NON-NLS-1$
					it.append(JvmTypeReference.class);
					it.append(" type)"); //$NON-NLS-1$
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
							it.append("().add(type);"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t\treturn this;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (hasFires.get()) {
					it.append("\t/** Add a fired event."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the fully qualified name of the event."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return {@code this}"); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(builderType);
					it.append(" add"); //$NON-NLS-1$
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
							it.append("\t\t"); //$NON-NLS-1$
							it.append(JvmTypeReference.class);
							it.append(" ref = newTypeRef(this.container, type);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tadd"); //$NON-NLS-1$
							it.append(toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getMemberFiresExtensionGrammarName())));
							it.append("(ref);"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t\treturn this;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Add a fired event."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the event."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void add"); //$NON-NLS-1$
					it.append(toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getMemberFiresExtensionGrammarName())));
					it.append("("); //$NON-NLS-1$
					it.append(JvmTypeReference.class);
					it.append(" type)"); //$NON-NLS-1$
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
							it.append("().add(type);"); //$NON-NLS-1$
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
					it.append("\t * @return {@code this}"); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(builderType);
					it.append(" set"); //$NON-NLS-1$
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
							it.append(".isEmpty(type)) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(JvmTypeReference.class);
							it.append(" ref = newTypeRef(container, type);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tset"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberReturnTypeExtensionGrammarName()));
							it.append("(ref);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t} else {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tset"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberReturnTypeExtensionGrammarName()));
							it.append("(("); //$NON-NLS-1$
							it.append(JvmTypeReference.class);
							it.append(") null);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t}"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t\treturn this;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Change the return type."); //$NON-NLS-1$
					it.newLine();
					it.append("\t @param type the return type of the member."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return {@code this}"); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(builderType);
					it.append(" set"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberReturnTypeExtensionGrammarName()));
					it.append("("); //$NON-NLS-1$
					it.append(JvmTypeReference.class);
					it.append(" type)"); //$NON-NLS-1$
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
							it.append("\t\tif (type != null && !"); //$NON-NLS-1$
							it.append(Objects.class);
							it.append(".equals(\"void\", type.getType().getIdentifier())"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t&& !"); //$NON-NLS-1$
							it.append(Objects.class);
							it.append(".equals(Void.class.getName(), type.getType().getIdentifier())) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberReturnTypeExtensionGrammarName()));
							it.append("(type);"); //$NON-NLS-1$
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
						it.append("\t\treturn this;"); //$NON-NLS-1$
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
					appendFileLineComment(it);
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
							it.append(" block = this.internalBlockExpression;"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tif (block == null) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tblock = this.blockExpressionProvider.get();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tblock.eInit(getTypeResolutionContext());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(XBlockExpression.class);
							it.append(" expr = block.getXBlockExpression();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig()
									.getMemberBlockExpressionExtensionGrammarName()));
							it.append("(expr);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis.internalBlockExpression = block;"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t}"); //$NON-NLS-1$
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
					it.append("\t * @return {@code this}"); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(builderType);
					it.append(" addAnnotation(String type)"); //$NON-NLS-1$
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
							it.append(JvmTypeReference.class);
							it.append(" ref = newTypeRef("); //$NON-NLS-1$
							it.append(generatedFieldAccessor);
							it.append(", type);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\taddAnnotation(ref);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t}"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t\treturn this;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Add an annotation."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the annotation type"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return {@code this}"); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(builderType);
					it.append(" addAnnotation("); //$NON-NLS-1$
					it.append(JvmTypeReference.class);
					it.append(" type)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\tthis.builder.addAnnotation(type);"); //$NON-NLS-1$
						} else {
							it.append("\t\tif (type != null) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(XAnnotation.class);
							it.append(" annotation = "); //$NON-NLS-1$
							it.append(XAnnotationsFactory.class);
							it.append(".eINSTANCE.createXAnnotation();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tannotation.setAnnotationType(type.getType());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t"); //$NON-NLS-1$
							it.append(generatedFieldAccessor);
							it.append(".getAnnotations().add(annotation);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t}"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t\treturn this;"); //$NON-NLS-1$
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
					it.append("\t * @return {@code this}"); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(builderType);
					it.append(" addModifier(String modifier)"); //$NON-NLS-1$
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
						it.append("\t\treturn this;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (!forInterface) {
					appendEmptyComment(it);
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
						appendEmptyComment(it);
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
					appendFileLineComment(it);
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
							it.append("\t\tbuilder.eInit(object, getExecutableReferenceFor(object), name, getTypeResolutionContext());"); //$NON-NLS-1$
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
				for (final var expressionName : expressions) {
					it.append("\t/** Replies the " + expressionName + "."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
					it.append("\t * @return the value of the "); //$NON-NLS-1$
					it.append(expressionName);
					it.append(". It may be {@code null}."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
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
				for (final var complexParameter : complexParameters) {
					it.append("\t/** Add a parameter of type {@code " + complexParameter.getName() + "}."); //$NON-NLS-1$ //$NON-NLS-2$
					it.newLine();
					it.append("\t * @param name the name of the parameter"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the builder for the parameter, never {@code null}."); //$NON-NLS-1$
					it.newLine();
					appendFileLineComment(it);
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append(getCodeElementExtractor().getComplexParameterBuilderInterface(complexParameter));
					it.append(" addParameter("); //$NON-NLS-1$
					it.append(String.class);
					it.append(" name)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\treturn this.builder.addParameter(name);"); //$NON-NLS-1$
						} else {
							it.append("\t\t"); //$NON-NLS-1$
							it.append(getCodeElementExtractor().getComplexParameterBuilderInterface(complexParameter));
							it.append(" builder = this."); //$NON-NLS-1$
							it.append(Strings.toFirstLower(complexParameter.getSimpleName()));
							it.append("BuilderProvider.get();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tbuilder.eInit(this."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(", name, getTypeResolutionContext());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\tif (!(this."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(" instanceof "); //$NON-NLS-1$
							it.append(XtendTypeDeclaration.class);
							it.append(")) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis."); //$NON-NLS-1$
							it.append(generatedFieldName);
							it.append(".get"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterListGrammarName()));
							it.append("().add(builder.get"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(complexParameter.getSimpleName()));
							it.append("());"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t}"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\treturn builder;"); //$NON-NLS-1$
							it.newLine();
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
			return this.element.name();
		}

		/** Replies if this element is a top element too.
		 *
		 * @return {@code true} if the element is a top element.
		 */
		public boolean isTopElement() {
			return this.isTopElement;
		}

		/** Replies if this element has the annotationInfo field.
		 *
		 * @return {@code true} if the field was declared.
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

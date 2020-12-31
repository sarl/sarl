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

import java.util.Iterator;
import java.util.List;
import javax.inject.Inject;
import javax.inject.Provider;

import com.google.common.collect.Iterators;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.tasks.ITaskTagProvider;
import org.eclipse.xtext.tasks.TaskTags;
import org.eclipse.xtext.util.EmfFormatter;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

import io.sarl.lang.mwe2.codebuilder.extractor.CodeElementExtractor;

/** Generator of the builder for XBlockExpression.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BlockExpressionBuilderFragment extends AbstractSubCodeBuilderFragment {

	@Inject
	private BuilderFactoryContributions builderFactoryContributions;

	/** Replies the implementation for the expression builder.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getBlockExpressionBuilderImpl() {
		return getCodeElementExtractor().getElementBuilderImpl("BlockExpression"); //$NON-NLS-1$
	}

	/** Replies the custom implementation for the expression builder.
	 *
	 * @return the custom implementation.
	 */
	@Pure
	public TypeReference getBlockExpressionBuilderImplCustom() {
		return getCodeElementExtractor().getElementBuilderImplCustom("BlockExpression"); //$NON-NLS-1$
	}

	@Override
	public void generate() {
		generateIBlockExpressionBuilder();
		generateBlockExpressionBuilderImpl();
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			generateBlockExpressionAppender();
		}
		generateBuilderFactoryContributions();
		super.generate();
	}

	@Override
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		bindTypeReferences(factory,
				getBlockExpressionBuilderInterface(),
				getBlockExpressionBuilderImpl(),
				getBlockExpressionBuilderImplCustom());
	}

	/** Generate the expression builder interface.
	 */
	protected void generateIBlockExpressionBuilder() {
		final TypeReference builder = getBlockExpressionBuilderInterface();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " XBlockExpression."); //$NON-NLS-1$
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
				it.append(generateMembers(true, false));
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the expression builder implementation.
	 */
	protected void generateBlockExpressionBuilderImpl() {
		final TypeReference builderInterface = getBlockExpressionBuilderInterface();
		final TypeReference builder = getBlockExpressionBuilderImpl();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " XBlockExpression."); //$NON-NLS-1$
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
				it.append(generateMembers(false, false));
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the expression appender.
	 */
	protected void generateBlockExpressionAppender() {
		final TypeReference builderInterface = getBlockExpressionBuilderInterface();
		final TypeReference appender = getCodeElementExtractor().getElementAppenderImpl("BlockExpression"); //$NON-NLS-1$
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Appender of a " + getLanguageName() //$NON-NLS-1$
						+ " XBlockExpression."); //$NON-NLS-1$
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
				it.append(builderInterface);
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateAppenderMembers(appender.getSimpleName(), builderInterface,
						"getXBlockExpression()")); //$NON-NLS-1$
				it.append(generateMembers(false, true));
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(appender, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the members of the builder.
	 *
	 * @param forInterface <code>true</code> if the code must be generated for an interface.
	 * @param forAppender <code>true</code> if the code must be generated for an appender.
	 * @return the code.
	 */
	@SuppressWarnings("checkstyle:all")
	protected StringConcatenationClient generateMembers(boolean forInterface, boolean forAppender) {
		final TypeReference adapter = getCodeElementExtractor().getInnerBlockDocumentationAdapter();
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				if (!forInterface && !forAppender) {
					it.append("\t@"); //$NON-NLS-1$
					it.append(Inject.class);
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(ITaskTagProvider.class);
					it.append(" taskTagProvider;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
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
					it.append(XBlockExpression.class);
					it.append(" block;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Replies the provider of task tags."); //$NON-NLS-1$
					it.newLine();
					it.append("\t *"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the provider."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\tprotected "); //$NON-NLS-1$
					it.append(ITaskTagProvider.class);
					it.append(" getTaskTagProvider() {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn this.taskTagProvider;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
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
				it.append("\t/** Create the XBlockExpression."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" context)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(context);"); //$NON-NLS-1$
					} else {
						it.append("\t\tsetTypeResolutionContext(context);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (this.block == null) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis.block = "); //$NON-NLS-1$
						it.append(XbaseFactory.class);
						it.append(".eINSTANCE.createXBlockExpression();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the string for \"auto-generated\" comments."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the comment text."); //$NON-NLS-1$
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
				it.append("String getAutoGeneratedActionString()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.getAutoGeneratedActionString();"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn getAutoGeneratedActionString(getXBlockExpression().eResource());"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the string for \"auto-generated\" comments."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resource the resource for which the comment must be determined."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the comment text."); //$NON-NLS-1$
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
				it.append("String getAutoGeneratedActionString("); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.getAutoGeneratedActionString(resource);"); //$NON-NLS-1$
					} else {
						it.append("\t\t"); //$NON-NLS-1$
						it.append(TaskTags.class);
						it.append(" tags = getTaskTagProvider().getTaskTags(resource);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tString taskTag;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (tags != null && tags.getTaskTags() != null && !tags.getTaskTags().isEmpty()) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\ttaskTag = tags.getTaskTags().get(0).getName();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t} else {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\ttaskTag = \"TODO\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn taskTag + \" "); //$NON-NLS-1$
						it.append(Strings.convertToJavaString(getCodeBuilderConfig().getAutoGeneratedComment()));
						it.append("\";"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** An empty block expression."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the block expression."); //$NON-NLS-1$
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
				it.append(XBlockExpression.class);
				it.append(" getXBlockExpression()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.getXBlockExpression();"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn this.block;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the resource to which the XBlockExpression is attached."); //$NON-NLS-1$
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
					it.append("\t\treturn getXBlockExpression().eResource();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append(generateCommentFunction(forInterface, false, "getXBlockExpression()", //$NON-NLS-1$
						"setInnerDocumentation", //$NON-NLS-1$
						"getXBlockExpression()", //$NON-NLS-1$
						adapter));
				it.append("\t/** Add an expression inside the block."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the expression builder."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(getExpressionBuilderInterface());
				it.append(" addExpression()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.addExpression();"); //$NON-NLS-1$
					} else {
						it.append("\t\tfinal "); //$NON-NLS-1$
						it.append(getExpressionBuilderInterface());
						it.append(" builder = this.expressionProvider.get();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tbuilder.eInit(getXBlockExpression(), new "); //$NON-NLS-1$
						it.append(Procedures.class);
						it.append(".Procedure1<"); //$NON-NLS-1$
						it.append(XExpression.class);
						it.append(">() {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\tprivate int index = -1;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\tpublic void apply("); //$NON-NLS-1$
						it.append(XExpression.class);
						it.append(" it) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\t\tif (this.index >= 0) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\t\t\tgetXBlockExpression().getExpressions().set(index, it);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\t\t} else {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\t\t\tgetXBlockExpression().getExpressions().add(it);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\t\t\tthis.index = getXBlockExpression().getExpressions().size() - 1;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t}, getTypeResolutionContext());"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn builder;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Fill the block with the standard \"auto-generated\" content."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * <p>Any previously added content is removed."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type the expected type of the block (the last instruction), or"); //$NON-NLS-1$
				it.newLine();
				it.append("\t    {@code null} for no type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void setDefaultAutoGeneratedContent(String type)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.setDefaultAutoGeneratedContent(type);"); //$NON-NLS-1$
					} else {
						it.append("\t\tgetXBlockExpression().getExpressions().clear();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif ("); //$NON-NLS-1$
						it.append(Strings.class);
						it.append(".isEmpty(type)) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tsetInnerDocumentation(getAutoGeneratedActionString());"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t} else {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t"); //$NON-NLS-1$
						it.append(getExpressionBuilderInterface());
						it.append(" expr = addExpression();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tString defaultValue = expr.getDefaultValueForType(type);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tif ("); //$NON-NLS-1$
						it.append(Strings.class);
						it.append(".isEmpty(defaultValue)) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tsetInnerDocumentation(getAutoGeneratedActionString());"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t} else {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\texpr.setExpression(defaultValue);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\texpr.setDocumentation(getAutoGeneratedActionString());"); //$NON-NLS-1$
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
						it.append(".objToStr(getXBlockExpression());"); //$NON-NLS-1$
					}
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
			}
		};
	}

	/** Generate the contributions for the BuildFactory.
	 */
	protected void generateBuilderFactoryContributions() {
		final BlockExpressionContextDescription blockContext = getBlockExpressionContextDescription();
		final String createFunctionName = "createXBlockExpression"; //$NON-NLS-1$
		this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("\t/** Create the factory for a " + getLanguageName() //$NON-NLS-1$
						+ " block expression."); //$NON-NLS-1$
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
				it.append(getBlockExpressionBuilderInterface());
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
				it.append("\t/** Create the factory for a " + getLanguageName() //$NON-NLS-1$
						+ " block expression."); //$NON-NLS-1$
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
				it.append(getBlockExpressionBuilderInterface());
				it.append(" "); //$NON-NLS-1$
				it.append(createFunctionName);
				it.append("("); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" script = createScript(getFooPackageName(), resource);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(blockContext.getContainerDescription().getBuilderInterfaceType());
				it.append(" topElement = script.add"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(blockContext.getContainerDescription().getElementType().getSimpleName()));
				it.append("(getFooTypeName());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(blockContext.getMemberDescription().getBuilderInterfaceType());
				it.append(" memberElement = topElement.add"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(blockContext.getMemberDescription().getElementType().getSimpleName()));
				it.append("(getFooMemberName());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn memberElement.get"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(blockContext.getExpressionAssignment().getFeature()));
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		});
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			final String buildFunctionName = "buildXBlockExpression"; //$NON-NLS-1$
			final TypeReference appender = getCodeElementExtractor().getElementAppenderImpl("BlockExpression"); //$NON-NLS-1$
			this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create the appender for a " + getLanguageName() //$NON-NLS-1$
							+ " block expression."); //$NON-NLS-1$
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
					it.append("\t/** Create the appender for a " + getLanguageName() //$NON-NLS-1$
							+ " block expression."); //$NON-NLS-1$
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

	/** Replies a keyword for declaring a container.
	 *
	 * @param grammarContainer the container description.
	 * @return the keyword, never {@code null} nor an empty string.
	 */
	protected String ensureContainerKeyword(EObject grammarContainer) {
		final Iterator<Keyword> iterator = Iterators.filter(grammarContainer.eContents().iterator(), Keyword.class);
		if (iterator.hasNext()) {
			return iterator.next().getValue();
		}
		return getExpressionConfig().getBlockMemberContainerDeclarationKeyword();
	}

	/** Replies a keyword for declaring a member.
	 *
	 * @param memberDescription the member description.
	 * @return the keyword, never {@code null} nor an empty string.
	 */
	protected String ensureMemberDeclarationKeyword(CodeElementExtractor.ElementDescription memberDescription) {
		final List<String> modifiers = getCodeBuilderConfig().getModifiers().get(memberDescription.getName());
		if (modifiers != null && !modifiers.isEmpty()) {
			return modifiers.get(0);
		}
		return null;
	}

	/** Replies the description of the block expression context.
	 *
	 * @return the description.
	 */
	protected BlockExpressionContextDescription getBlockExpressionContextDescription() {
		for (final CodeElementExtractor.ElementDescription containerDescription : getCodeElementExtractor().getTopElements(
				getGrammar(), getCodeBuilderConfig())) {
			if (!getCodeBuilderConfig().getNoActionBodyTypes().contains(containerDescription.getName())) {
				final AbstractRule rule = getMemberRule(containerDescription);
				if (rule != null) {
					final BlockExpressionContextDescription description =
							getCodeElementExtractor().visitMemberElements(containerDescription, rule, null,
								(it, grammarContainer, memberContainer, classifier) -> {
									final Assignment expressionAssignment = findAssignmentFromTerminalPattern(
											memberContainer,
											getExpressionConfig().getBlockExpressionGrammarPattern());
									final CodeElementExtractor.ElementDescription memberDescription =
											it.newElementDescription(
													classifier.getName(), memberContainer,
													classifier, XExpression.class);
									final String keyword = ensureMemberDeclarationKeyword(memberDescription);
									if (expressionAssignment != null && keyword != null) {
										return new BlockExpressionContextDescription(
												containerDescription,
												memberDescription,
												ensureContainerKeyword(containerDescription.getGrammarComponent()),
												keyword,
												expressionAssignment);
									}
									return null;
								},
						null);
					if (description != null) {
						return description;
					}
				}
			}
		}
		return null;
	}

	/** Description of the block expression context.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class BlockExpressionContextDescription {

		private final CodeElementExtractor.ElementDescription member;

		private final CodeElementExtractor.ElementDescription container;

		private final String containerKeyword;

		private final String fieldDeclarationKeyword;

		private final Assignment expressionAssignment;

		/** Constructor.
		 *
		 * @param container the container of the element that contains the expression.
		 * @param member the description of the expression container.
		 * @param containerKeyword the keyword for declaring a type.
		 * @param fieldDeclarationKeyword the keyword for declaring a field.
		 * @param expressionAssignment the assignment that contains the expression in the element.
		 */
		public BlockExpressionContextDescription(
				CodeElementExtractor.ElementDescription container,
				CodeElementExtractor.ElementDescription member,
				String containerKeyword,
				String fieldDeclarationKeyword,
				Assignment expressionAssignment) {
			this.container = container;
			this.member = member;
			this.containerKeyword = containerKeyword;
			this.fieldDeclarationKeyword = fieldDeclarationKeyword;
			this.expressionAssignment = expressionAssignment;
		}

		/** Replies the container description.
		 *
		 * @return the container description.
		 */
		public CodeElementExtractor.ElementDescription getContainerDescription() {
			return this.container;
		}

		/** Replies the member description.
		 *
		 * @return the member description.
		 */
		public CodeElementExtractor.ElementDescription getMemberDescription() {
			return this.member;
		}

		/** Replies the assignment that contains the expression.
		 *
		 * @return the grammar assignment.
		 */
		public Assignment getExpressionAssignment() {
			return this.expressionAssignment;
		}

		/** Replies the first keyword associated to the container.
		 *
		 * @return the keyword.
		 */
		public String getContainerKeyword() {
			return this.containerKeyword;
		}

		/** Replies the keyword for declaring a field.
		 *
		 * @return the keyword.
		 */
		public String getFieldDeclarationKeyword() {
			return this.fieldDeclarationKeyword;
		}

	}

}

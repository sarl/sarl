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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.inject.Inject;
import javax.inject.Provider;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.tasks.ITaskTagProvider;
import org.eclipse.xtext.tasks.TaskTags;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

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
		return getElementBuilderImpl("BlockExpression"); //$NON-NLS-1$
	}

	/** Replies the custom implementation for the expression builder.
	 *
	 * @return the custom implementation.
	 */
	@Pure
	public TypeReference getBlockExpressionBuilderImplCustom() {
		return getElementBuilderImplCustom("BlockExpression"); //$NON-NLS-1$
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
	public void generateBindings(BindingFactory factory) {
		super.generateBindings(factory);
		IFileSystemAccess2 fileSystem = getSrc();

		final TypeReference builderInterface = getBlockExpressionBuilderInterface();
		final TypeReference builderImpl = getBlockExpressionBuilderImpl();
		final TypeReference builderImplCustom = getBlockExpressionBuilderImplCustom();

		TypeReference type;
		if ((fileSystem.isFile(builderImplCustom.getJavaPath()))
				|| (fileSystem.isFile(builderImplCustom.getXtendPath()))) {
			type = builderImplCustom;
		} else {
			type = builderImpl;
		}
		factory.addfinalTypeToType(builderInterface, type);
	}

	/** Generate the expression builder interface.
	 */
	protected void generateIBlockExpressionBuilder() {
		final TypeReference builder = getBlockExpressionBuilderInterface();
		StringConcatenationClient content = new StringConcatenationClient() {
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
		JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the expression builder implementation.
	 */
	protected void generateBlockExpressionBuilderImpl() {
		final TypeReference builderInterface = getBlockExpressionBuilderInterface();
		final TypeReference builder = getBlockExpressionBuilderImpl();
		StringConcatenationClient content = new StringConcatenationClient() {
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
		JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the expression appender.
	 */
	protected void generateBlockExpressionAppender() {
		final TypeReference builderInterface = getBlockExpressionBuilderInterface();
		final TypeReference appender = getElementAppenderImpl("BlockExpression"); //$NON-NLS-1$
		StringConcatenationClient content = new StringConcatenationClient() {
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
				it.append(getAbstractAppenderImpl());
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
		JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(appender, content);
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
		final TypeReference adapter = getInnerBlockDocumentationAdapter();
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
				}
				it.append("\t/** Create the XBlockExpression."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit();"); //$NON-NLS-1$
					} else {
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
						it.append("\t\t\t\t});"); //$NON-NLS-1$
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
				it.append("\t * @param type - the expected type of the block (the last instruction), or"); //$NON-NLS-1$
				it.newLine();
				it.append("\t    <code>null</code> for no type."); //$NON-NLS-1$
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
					if (forAppender) {
						it.append("\t\tthis.builder.setDefaultAutoGeneratedContent(type);"); //$NON-NLS-1$
					} else {
						it.newLine();
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
		};
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

	/** Generate the contributions for the BuildFactory.
	 */
	protected void generateBuilderFactoryContributions() {
		final String createFunctionName = "createXBlockExpression"; //$NON-NLS-1$
		AbstractRule constructorRule = getConstructorRule();
		final String containerCreationName = "create" //$NON-NLS-1$
				+ Strings.toFirstUpper(constructorRule.getName());
		Assignment exprAssignment = null;
		for (Assignment assignement : GrammarUtil.containedAssignments(constructorRule)) {
			if (nameMatches(assignement.getTerminal(), getCodeBuilderConfig().getBlockExpressionGrammarPattern())) {
				exprAssignment = assignement;
				break;
			}
		}
		if (exprAssignment == null) {
			return;
		}
		final String containerFunctionName = "get" //$NON-NLS-1$
				+ toSingular(Strings.toFirstUpper(exprAssignment.getFeature()));
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
				it.append(containerCreationName);
				it.append("(resourceSet)."); //$NON-NLS-1$
				it.append(containerFunctionName);
				it.append("();"); //$NON-NLS-1$
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
				it.append("\t\treturn "); //$NON-NLS-1$
				it.append(containerCreationName);
				it.append("(resource)."); //$NON-NLS-1$
				it.append(containerFunctionName);
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		});
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			final String buildFunctionName = "buildXBlockExpression"; //$NON-NLS-1$
			final TypeReference appender = getElementAppenderImpl("BlockExpression"); //$NON-NLS-1$
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

}

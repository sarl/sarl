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

import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/** Generator of the builder for formal parameters.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class FormalParameterBuilderFragment extends AbstractSubCodeBuilderFragment {
	
	@Inject
	private BuilderFactoryContributions builderFactoryContributions;

	/** Replies the implementation for the formal parameter builder.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getFormalParameterBuilderImpl() {
		return getElementBuilderImpl("FormalParameter"); //$NON-NLS-1$
	}

	/** Replies the custom implementation for the formal parameter builder.
	 *
	 * @return the custom implementation.
	 */
	@Pure
	public TypeReference getFormalParameterBuilderImplCustom() {
		return getElementBuilderImplCustom("FormalParameter"); //$NON-NLS-1$
	}

	@Override
	public void generate() {
		generateIFormalParameterBuilder();
		generateFormalParameterBuilderImpl();
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			generateFormalParameterAppender();
		}
		generateBuilderFactoryContributions();
		super.generate();
	}
	
	@Override
	public void generateBindings(BindingFactory factory) {
		super.generateBindings(factory);
		IFileSystemAccess2 fileSystem = getSrc();
		
		final TypeReference builderInterface = getFormalParameterBuilderInterface();
		final TypeReference builderImpl = getFormalParameterBuilderImpl();
		final TypeReference builderImplCustom = getFormalParameterBuilderImplCustom();

		TypeReference type;
		if ((fileSystem.isFile(builderImplCustom.getJavaPath()))
				|| (fileSystem.isFile(builderImplCustom.getXtendPath()))) {
			type = builderImplCustom;
		} else {
			type = builderImpl;
		}
		factory.addfinalTypeToType(builderInterface, type);
	}

	/** Generate the formal parameter builder interface.
	 */
	protected void generateIFormalParameterBuilder() {
		final TypeReference builder = getFormalParameterBuilderInterface();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " formal parameter."); //$NON-NLS-1$
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
	
	/** Generate the formal parameter builder implementation.
	 */
	protected void generateFormalParameterBuilderImpl() {
		final TypeReference builderInterface = getFormalParameterBuilderInterface();
		final TypeReference builder = getFormalParameterBuilderImpl();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " formal parameter."); //$NON-NLS-1$
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

	/** Generate the formal parameter appender.
	 */
	protected void generateFormalParameterAppender() {
		final AbstractRule rule = GrammarUtil.findRuleForName(getGrammar(), getCodeBuilderConfig().getFormalParameterRuleName());
		EClassifier classifier = getGeneratedTypeFor(rule);
		final TypeReference parameterType = newTypeReference(classifier);
		final TypeReference builderInterface = getFormalParameterBuilderInterface();
		final TypeReference appender = getElementAppenderImpl("FormalParameter"); //$NON-NLS-1$
		final String accessor = "get" //$NON-NLS-1$
				+ Strings.toFirstUpper(parameterType.getSimpleName()) + "()"; //$NON-NLS-1$
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Appender of a " + getLanguageName() //$NON-NLS-1$
						+ " formal parameter."); //$NON-NLS-1$
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
				it.append(generateAppenderMembers(appender.getSimpleName(), builderInterface, accessor));
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
	protected StringConcatenationClient generateMembers(boolean forInterface, boolean forAppender) {
		final AbstractRule rule = GrammarUtil.findRuleForName(getGrammar(), getCodeBuilderConfig().getFormalParameterRuleName());
		EClassifier classifier = getGeneratedTypeFor(rule);
		final TypeReference parameterType = newTypeReference(classifier);
		final TypeReference parameterContainerType = new TypeReference(
				getCodeBuilderConfig().getFormalParameterContainerType());
		final Assignment defaultValueAssignment = findAssignmentFromFeatureName(rule,
				getCodeBuilderConfig().getParameterDefaultValueGrammarName());
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
					it.append(getExpressionBuilderInterface());
					it.append("> expressionProvider;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(parameterContainerType);
					it.append(" context;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(parameterType);
					it.append(" parameter;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(getExpressionBuilderInterface());
					it.append(" defaultValue;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				it.append("\t/** Initialize the formal parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context - the context of the formal parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param name - the name of the formal parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(parameterContainerType);
				it.append(" context, String name)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(context, name);"); //$NON-NLS-1$
					} else {
						it.append("\t\tthis.context = context;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis.parameter = "); //$NON-NLS-1$
						it.append(getXFactoryFor(parameterType));
						it.append(".eINSTANCE.create"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(parameterType.getSimpleName()));
						it.append("();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis.parameter.set"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterNameGrammarName()));
						it.append("(name);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis.parameter.set"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterTypeGrammarName()));
						it.append("(newTypeRef(this.context, Object.class.getName()));"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis.context.get"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterListGrammarName()));
						it.append("().add(this.parameter);"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the created parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the parameter."); //$NON-NLS-1$
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
				it.append(parameterType);
				it.append(" get"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(parameterType.getSimpleName()));
				it.append("()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.get"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(parameterType.getSimpleName()));
						it.append("();"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn this.parameter;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the resource to which the formal parameter is attached."); //$NON-NLS-1$
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
					it.append("\t\treturn get"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(parameterType.getSimpleName()));
					it.append("().eResource();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Change the type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type the formal parameter type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void set"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterTypeGrammarName()));
				it.append("(String type)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.set"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterTypeGrammarName()));
						it.append("(type);"); //$NON-NLS-1$
					} else {
						it.append("\t\tString typeName;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif ("); //$NON-NLS-1$
						it.append(Strings.class);
						it.append(".isEmpty(type)) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\ttypeName = Object.class.getName();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t} else {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\ttypeName = type;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tthis.parameter.set"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterTypeGrammarName()));
						it.append("(newTypeRef(this.context, typeName));"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				if (!Strings.isEmpty(getCodeBuilderConfig().getParameterVarArgGrammarName())) {
					it.append("\t/** Change the variadic property of the parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t *"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param isVariadic indicates if the parameter is variadic."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t"); //$NON-NLS-1$
					if (!forInterface) {
						it.append("public "); //$NON-NLS-1$
					}
					it.append("void set"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterVarArgGrammarName()));
					it.append("(boolean isVariadic)"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\tthis.builder.set"); //$NON-NLS-1$
						} else {
							it.append("\t\tthis.parameter.set"); //$NON-NLS-1$
						}
						it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterVarArgGrammarName()));
						it.append("(isVariadic);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
					}
					it.newLineIfNotEmpty();
					it.newLine();
				}
				if (defaultValueAssignment != null) {
					it.append("\t/** Replies the default value of the parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the default value builder."); //$NON-NLS-1$
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
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterDefaultValueGrammarName()));
					it.append("()"); //$NON-NLS-1$
					if (forInterface) {
						it.append(";"); //$NON-NLS-1$
					} else {
						it.append(" {"); //$NON-NLS-1$
						it.newLine();
						if (forAppender) {
							it.append("\t\treturn this.builder.get"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterDefaultValueGrammarName()));
							it.append("();"); //$NON-NLS-1$
						} else {
							it.append("\t\tif (this.defaultValue == null) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis.defaultValue = this.expressionProvider.get();"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\tthis.defaultValue.eInit(this.parameter, new "); //$NON-NLS-1$
							it.append(Procedures.class);
							it.append(".Procedure1<"); //$NON-NLS-1$
							it.append(XExpression.class);
							it.append(">() {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t\tpublic void apply("); //$NON-NLS-1$
							it.append(XExpression.class);
							it.append(" it) {"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t\t\tget"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(parameterType.getSimpleName()));
							it.append("().set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterDefaultValueGrammarName()));
							it.append("(it);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t\t}"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t});"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t}"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\treturn this.defaultValue;"); //$NON-NLS-1$
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
		final String createFunctionName = "create" //$NON-NLS-1$
				+ toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getParameterListGrammarName()));
		final String containerFunctionName = "add" //$NON-NLS-1$
				+ toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getParameterListGrammarName()));
		final String containerCreationName = "create" //$NON-NLS-1$
				+ Strings.toFirstUpper(getConstructorRule().getName());
		this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("\t/** Create the factory for a " + getLanguageName() //$NON-NLS-1$
						+ " formal parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param name the name of the parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resourceSet the set of the resources that must be used for"); //$NON-NLS-1$
				it.newLine();
				it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(getFormalParameterBuilderInterface());
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
				it.append("\t/** Create the factory for a " + getLanguageName() //$NON-NLS-1$
						+ " formal parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param name the name of the parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resource the resource that must be used for"); //$NON-NLS-1$
				it.newLine();
				it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(getFormalParameterBuilderInterface());
				it.append(" "); //$NON-NLS-1$
				it.append(createFunctionName);
				it.append("(String name, "); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append("return "); //$NON-NLS-1$
				it.append(containerCreationName);
				it.append("(resource)."); //$NON-NLS-1$
				it.append(containerFunctionName);
				it.append("(name);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		});
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			final String buildFunctionName = "build" //$NON-NLS-1$
					+ toSingular(Strings.toFirstUpper(getCodeBuilderConfig().getParameterListGrammarName()));
			final TypeReference appender = getElementAppenderImpl("FormalParameter"); //$NON-NLS-1$
			this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create the appender for a " + getLanguageName() //$NON-NLS-1$
							+ " formal parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param resourceSet the set of the resources that must be used for"); //$NON-NLS-1$
					it.newLine();
					it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the appender."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
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
					it.append("\t/** Create the appender for a " + getLanguageName() //$NON-NLS-1$
							+ " formal parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param name the name of the parameter."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param resource the resource that must be used for"); //$NON-NLS-1$
					it.newLine();
					it.append("\t *    containing the generated resource, and resolving types from names."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the appender."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
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

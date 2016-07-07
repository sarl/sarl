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

import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.inject.Inject;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/** Generator of the builder for XExpressions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ExpressionBuilderFragment extends AbstractSubCodeBuilderFragment {

	@Inject
	private BuilderFactoryContributions builderFactoryContributions;

	/** Replies the implementation for the expression builder.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getExpressionBuilderImpl() {
		return getElementBuilderImpl("Expression"); //$NON-NLS-1$
	}

	/** Replies the custom implementation for the expression builder.
	 *
	 * @return the custom implementation.
	 */
	@Pure
	public TypeReference getExpressionBuilderImplCustom() {
		return getElementBuilderImplCustom("Expression"); //$NON-NLS-1$
	}

	@Override
	public void generate() {
		generateIExpressionBuilder();
		generateExpressionBuilderImpl();
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			generateExpressionAppender();
		}
		generateBuilderFactoryContributions();
		super.generate();
	}

	@Override
	public void generateBindings(BindingFactory factory) {
		super.generateBindings(factory);
		final IFileSystemAccess2 fileSystem = getSrc();

		final TypeReference builderInterface = getExpressionBuilderInterface();
		final TypeReference builderImpl = getExpressionBuilderImpl();
		final TypeReference builderImplCustom = getExpressionBuilderImplCustom();

		final TypeReference type;
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
	protected void generateIExpressionBuilder() {
		final TypeReference builder = getExpressionBuilderInterface();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " XExpression."); //$NON-NLS-1$
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
	protected void generateExpressionBuilderImpl() {
		final TypeReference builderInterface = getExpressionBuilderInterface();
		final TypeReference builder = getExpressionBuilderImpl();
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " XExpression."); //$NON-NLS-1$
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
	protected void generateExpressionAppender() {
		final TypeReference builderInterface = getExpressionBuilderInterface();
		final TypeReference appender = getElementAppenderImpl("Expression"); //$NON-NLS-1$
		final StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
						+ " XExpression."); //$NON-NLS-1$
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
				it.append(generateAppenderMembers(appender.getSimpleName(),
						builderInterface, "getXExpression()")); //$NON-NLS-1$
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
		Pair<AbstractRule, AbstractRule> expressionContext = extractExpressionContext();
		TypeReference scriptType = getLanguageScriptInterface();
		EClassifier classifier = getGeneratedTypeFor(expressionContext.getKey());
		TypeReference topElementType = newTypeReference(classifier);
		classifier = getGeneratedTypeFor(expressionContext.getValue());
		TypeReference memberType = newTypeReference(classifier);
		Assignment expressionAssignment = findAssignmentFromTerminalPattern(expressionContext.getValue(),
				getCodeBuilderConfig().getExpressionGrammarPattern());
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				if (!forInterface && !forAppender) {
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(EObject.class);
					it.append(" context;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(Procedures.Procedure1.class);
					it.append("<"); //$NON-NLS-1$
					it.append(XExpression.class);
					it.append("> setter;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(XExpression.class);
					it.append(" expr;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				it.append("\t/** Initialize the expression."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context - the context of the expressions."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param setter - the object that permits to assign the expression to the context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(Procedures.Procedure1.class);
				it.append("<"); //$NON-NLS-1$
				it.append(XExpression.class);
				it.append("> setter)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(context, setter);"); //$NON-NLS-1$
					} else {
						it.append("\t\tthis.context = context;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tthis.setter = setter;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tthis.expr = null;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the last created expression."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the last created expression."); //$NON-NLS-1$
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
				it.append(XExpression.class);
				it.append(" getXExpression()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.getXExpression();"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn this.expr;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the resource to which the XExpression is attached."); //$NON-NLS-1$
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
					it.append("\t\treturn getXExpression().eResource();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Change the expression in the container."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param expression - the textual representation of the expression."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void setExpression(String expression)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.setExpression(expression);"); //$NON-NLS-1$
					} else {
						it.append("\t\tthis.expr = fromString(expression);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tthis.setter.apply(this.expr);"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Change the expression in the container."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param expression - the expression."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void setXExpression("); //$NON-NLS-1$
				it.append(XExpression.class);
				it.append(" expression)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.setXExpression(expression);"); //$NON-NLS-1$
					} else {
						it.append("\t\tthis.expr = expression;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tthis.setter.apply(this.expr);"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				if (!forInterface && !forAppender) {
					it.append("\t/** Generate a piece of "); //$NON-NLS-1$
					it.append(getLanguageName());
					it.append(" code that permits to compile an XExpression."); //$NON-NLS-1$
					it.newLine();
					it.append("\t *"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param expression the expression to compile."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the "); //$NON-NLS-1$
					it.append(getLanguageName());
					it.append(" code."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\tprotected String generateExpressionCode(String expression) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn \""); //$NON-NLS-1$
					it.append(GrammarUtil.containedKeywords(expressionContext.getKey()).get(0).getValue());
					it.append(" ____synthesis { "); //$NON-NLS-1$
					it.append(getCodeBuilderConfig().getModifiers().get(memberType.getSimpleName()).get(0));
					it.append(" ____fakefield = \" + expression + \" }\";"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t/** Create an expression but does not change the container."); //$NON-NLS-1$
					it.newLine();
					it.append("\t *"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param expression - the textual representation of the expression."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @return the expression."); //$NON-NLS-1$
					it.newLine();
					it.append("\t */"); //$NON-NLS-1$
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Pure.class);
					it.newLine();
					it.append("\tprotected "); //$NON-NLS-1$
					it.append(XExpression.class);
					it.append(" fromString(String expression)"); //$NON-NLS-1$
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tif (!"); //$NON-NLS-1$
					it.append(Strings.class);
					it.append(".isEmpty(expression)) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t"); //$NON-NLS-1$
					it.append(ResourceSet.class);
					it.append(" resourceSet = this.context.eResource().getResourceSet();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t"); //$NON-NLS-1$
					it.append(URI.class);
					it.append(" uri = computeUnusedUri(resourceSet);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t"); //$NON-NLS-1$
					it.append(Resource.class);
					it.append(" resource = getResourceFactory().createResource(uri);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tresourceSet.getResources().add(resource);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\ttry ("); //$NON-NLS-1$
					it.append(StringInputStream.class);
					it.append(" is = new "); //$NON-NLS-1$
					it.append(StringInputStream.class);
					it.append("(generateExpressionCode(expression))) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\tresource.load(is, null);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t"); //$NON-NLS-1$
					it.append(scriptType);
					it.append(" script = resource.getContents().isEmpty() ? null : ("); //$NON-NLS-1$
					it.append(scriptType);
					it.append(") resource.getContents().get(0);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t"); //$NON-NLS-1$
					it.append(topElementType);
					it.append(" topElement = ("); //$NON-NLS-1$
					it.append(topElementType);
					it.append(") script."); //$NON-NLS-1$
					it.append(getLanguageScriptMemberGetter());
					it.append("().get(0);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t"); //$NON-NLS-1$
					it.append(memberType);
					it.append(" member = ("); //$NON-NLS-1$
					it.append(memberType);
					it.append(") topElement.get"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName()));
					it.append("().get(0);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\treturn member.get"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(expressionAssignment.getFeature()));
					it.append("();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t} catch (Throwable exception) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\tthrow new RuntimeException(exception);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t} finally {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\tresourceSet.getResources().remove(resource);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tthrow new IllegalArgumentException(\"not a valid expression\");"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the XExpression for the default value associated to the given type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type - the type for which the default value should be determined."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the default value."); //$NON-NLS-1$
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
				it.append(XExpression.class);
				it.append(" getDefaultXExpressionForType(String type)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.getDefaultXExpressionForType(type);"); //$NON-NLS-1$
					} else {
						it.append("\t\t//TODO: Check if a similar function exists in the Xbase library."); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(XExpression.class);
						it.append(" expr = null;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (type != null && !\"void\".equals(type) && !Void.class.getName().equals(type)) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tswitch (type) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"boolean\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"java.lang.Boolean\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t"); //$NON-NLS-1$
						it.append(XBooleanLiteral.class);
						it.append(" booleanLiteral = "); //$NON-NLS-1$
						it.append(XbaseFactory.class);
						it.append(".eINSTANCE.createXBooleanLiteral();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbooleanLiteral.setIsTrue(false);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\texpr = booleanLiteral;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"float\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"java.lang.Float\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t"); //$NON-NLS-1$
						it.append(XNumberLiteral.class);
						it.append(" numberLiteral = "); //$NON-NLS-1$
						it.append(XbaseFactory.class);
						it.append(".eINSTANCE.createXNumberLiteral();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tnumberLiteral.setValue(\"0.0f\");"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\texpr = numberLiteral;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"double\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"java.lang.Double\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"java.lang.BigDecimal\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tnumberLiteral = "); //$NON-NLS-1$
						it.append(XbaseFactory.class);
						it.append(".eINSTANCE.createXNumberLiteral();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tnumberLiteral.setValue(\"0.0\");"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\texpr = numberLiteral;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"int\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"long\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"java.lang.Integer\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"java.lang.Long\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"java.lang.BigInteger\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tnumberLiteral = "); //$NON-NLS-1$
						it.append(XbaseFactory.class);
						it.append(".eINSTANCE.createXNumberLiteral();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tnumberLiteral.setValue(\"0\");"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\texpr = numberLiteral;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"byte\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"short\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"char\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"java.lang.Byte\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"java.lang.Short\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"java.lang.Character\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tnumberLiteral = "); //$NON-NLS-1$
						it.append(XbaseFactory.class);
						it.append(".eINSTANCE.createXNumberLiteral();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tnumberLiteral.setValue(\"0\");"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\t"); //$NON-NLS-1$
						it.append(XCastedExpression.class);
						it.append(" castExpression = "); //$NON-NLS-1$
						it.append(XbaseFactory.class);
						it.append(".eINSTANCE.createXCastedExpression();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tcastExpression.setTarget(numberLiteral);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tcastExpression.setType(newTypeRef(this.context, type));"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\texpr = numberLiteral;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tdefault:"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\texpr = "); //$NON-NLS-1$
						it.append(XbaseFactory.class);
						it.append(".eINSTANCE.createXNullLiteral();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn expr;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the default value for the given type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type - the type for which the default value should be determined."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the default value."); //$NON-NLS-1$
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
				it.append("String getDefaultValueForType(String type)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.getDefaultValueForType(type);"); //$NON-NLS-1$
					} else {
						it.append("\t\t//TODO: Check if a similar function exists in the Xbase library."); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tString defaultValue = \"\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (!"); //$NON-NLS-1$
						it.append(Strings.class);
						it.append(".isEmpty(type) && !\"void\".equals(type)) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tswitch (type) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"boolean\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tdefaultValue = \"true\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"double\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tdefaultValue = \"0.0\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"float\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tdefaultValue = \"0.0f\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"int\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tdefaultValue = \"0\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"long\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tdefaultValue = \"0\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"byte\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tdefaultValue = \"(0 as byte)\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"short\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tdefaultValue = \"(0 as short)\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tcase \"char\":"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tdefaultValue = \"(0 as char)\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tdefault:"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tdefaultValue = \"null\";"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tbreak;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn defaultValue;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
	}

	/** Generate the contributions for the BuildFactory.
	 */
	protected void generateBuilderFactoryContributions() {
		final Pair<AbstractRule, AbstractRule> context = extractExpressionContext();
		final Assignment expressionAssignment = findAssignmentFromTerminalPattern(context.getValue(),
				getCodeBuilderConfig().getExpressionGrammarPattern());
		final String createFunctionName = "createXExpression"; //$NON-NLS-1$
		this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("\t/** Create the factory for a " + getLanguageName() //$NON-NLS-1$
						+ " XExpression."); //$NON-NLS-1$
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
				it.append(getExpressionBuilderInterface());
				it.append(" "); //$NON-NLS-1$
				it.append(createFunctionName);
				it.append("("); //$NON-NLS-1$
				it.append(ResourceSet.class);
				it.append(" resourceSet) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn "); //$NON-NLS-1$
				it.append(createFunctionName);
				it.append("(createResource(resourceSet));"); //$NON-NLS-1$
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Create the factory for a " + getLanguageName() //$NON-NLS-1$
						+ " XExpression."); //$NON-NLS-1$
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
				it.append(getExpressionBuilderInterface());
				it.append(" "); //$NON-NLS-1$
				it.append(createFunctionName);
				it.append("("); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn createScript(getFooPackageName(), resource)"); //$NON-NLS-1$
				it.append(".add"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(context.getKey().getName()));
				it.append("(\"Foo\").add"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(context.getValue().getName()));
				it.append("(\"foo\").get"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(expressionAssignment.getFeature()));
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		});
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			final String buildFunctionName = "buildXExpression"; //$NON-NLS-1$
			final TypeReference appender = getElementAppenderImpl("Expression"); //$NON-NLS-1$
			this.builderFactoryContributions.addContribution(new StringConcatenationClient() {
				@Override
				protected void appendTo(TargetStringConcatenation it) {
					it.append("\t/** Create the appender for a " + getLanguageName() //$NON-NLS-1$
							+ " XExpression."); //$NON-NLS-1$
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
							+ " XExpression."); //$NON-NLS-1$
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

	@SuppressWarnings("checkstyle:all")
	private Pair<AbstractRule, AbstractRule> extractExpressionContext() {
		AbstractRule topElementRule = GrammarUtil.findRuleForName(getGrammar(), getCodeBuilderConfig().getTopElementRuleName());
		for (RuleCall ruleCall : GrammarUtil.containedRuleCalls(topElementRule)) {
			AbstractRule topMemberRule = null;
			for (Assignment assignment : GrammarUtil.containedAssignments(ruleCall.getRule())) {
				if (Objects.equals(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName(), assignment.getFeature())) {
					if (assignment.getTerminal() instanceof RuleCall) {
						topMemberRule = ((RuleCall) assignment.getTerminal()).getRule();
						break;
					}
				}
			}
			if (topMemberRule != null) {
				Set<String> treatedRules = getTopElementRules();
				Pattern fieldPattern = Pattern.compile(getCodeBuilderConfig().getFieldGrammarPattern());
				Pair<AbstractRule, AbstractRule> pair = visitMemberElements(ruleCall.getRule(), topMemberRule, treatedRules,
						null,
						new Functions.Function3<AbstractRule, AbstractRule, String,
						Pair<AbstractRule, AbstractRule>>() {
					@Override
					public Pair<AbstractRule, AbstractRule> apply(AbstractRule containerRule,
							AbstractRule memberRule, String name) {
						Matcher matcher = fieldPattern.matcher(memberRule.getName());
						if (matcher.find()) {
							return new Pair<>(containerRule, memberRule);
						}
						return null;
					}
				});
				if (pair != null) {
					return pair;
				}
			}
		}
		throw new IllegalStateException("no expression context found."); //$NON-NLS-1$
	}

}

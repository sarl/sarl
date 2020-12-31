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
import java.util.regex.Pattern;
import javax.inject.Inject;

import com.google.common.collect.Iterators;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.util.EmfFormatter;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

import io.sarl.lang.mwe2.codebuilder.extractor.CodeElementExtractor;

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

	/** Replies the custom implementation for the expression builder.
	 *
	 * @return the custom implementation.
	 */
	@Pure
	public TypeReference getExpressionBuilderImplCustom() {
		return getCodeElementExtractor().getElementBuilderImplCustom("Expression"); //$NON-NLS-1$
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
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		bindTypeReferences(factory,
				getExpressionBuilderInterface(),
				getExpressionBuilderImpl(),
				getExpressionBuilderImplCustom());
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
		final TypeReference appender = getCodeElementExtractor().getElementAppenderImpl("Expression"); //$NON-NLS-1$
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
				it.append(getCodeElementExtractor().getAbstractAppenderImpl());
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
		final ExpressionContextDescription expressionContext = getExpressionContextDescription();
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
					it.append("<? super "); //$NON-NLS-1$
					it.append(XExpression.class);
					it.append("> setter;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(XExpression.class);
					it.append(" expr;"); //$NON-NLS-1$
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
				it.append("\t/** Initialize the expression."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context the context of the expressions."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param setter the object that permits to assign the expression to the context."); //$NON-NLS-1$
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
				it.append("<? super "); //$NON-NLS-1$
				it.append(XExpression.class);
				it.append("> setter, "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" typeContext)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(context, setter, typeContext);"); //$NON-NLS-1$
					} else {
						it.append("\t\tsetTypeResolutionContext(typeContext);"); //$NON-NLS-1$
						it.newLine();
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
				it.append("\t * @param expression the textual representation of the expression."); //$NON-NLS-1$
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
				it.append("\t * @param expression the expression."); //$NON-NLS-1$
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
					it.append("\tstatic String generateExpressionCode(String expression) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn \""); //$NON-NLS-1$
					it.append(expressionContext.getContainerKeyword());
					it.append(" ____synthesis { "); //$NON-NLS-1$
					it.append(expressionContext.getFieldDeclarationKeyword());
					it.append(" ____fakefield = \" + expression + \" }\";"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tstatic String generateTypenameCode(String typeName) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\treturn \""); //$NON-NLS-1$
					it.append(expressionContext.getContainerKeyword());
					it.append(" ____synthesis { "); //$NON-NLS-1$
					it.append(expressionContext.getFieldDeclarationKeyword());
					it.append(" ____fakefield : \" + typeName + \" }\";"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tstatic "); //$NON-NLS-1$
					it.append(JvmParameterizedTypeReference.class);
					it.append(" parseType("); //$NON-NLS-1$
					it.append(Notifier.class);
					it.append(" context, String typeName, "); //$NON-NLS-1$
					it.append(getAbstractBuilderImpl());
					it.append(" caller) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(ResourceSet.class);
					it.append(" resourceSet = toResource(context).getResourceSet();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(URI.class);
					it.append(" uri = caller.computeUnusedUri(resourceSet);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					it.append(Resource.class);
					it.append(" resource = caller.getResourceFactory().createResource(uri);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tresourceSet.getResources().add(resource);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\ttry ("); //$NON-NLS-1$
					it.append(StringInputStream.class);
					it.append(" is = new "); //$NON-NLS-1$
					it.append(StringInputStream.class);
					it.append("(generateTypenameCode(typeName))) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tresource.load(is, null);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t"); //$NON-NLS-1$
					it.append(getCodeElementExtractor().getLanguageScriptInterface());
					it.append(" script = resource.getContents().isEmpty() ? null : ("); //$NON-NLS-1$
					it.append(getCodeElementExtractor().getLanguageScriptInterface());
					it.append(") resource.getContents().get(0);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t"); //$NON-NLS-1$
					it.append(expressionContext.getContainerDescription().getElementType());
					it.append(" topElement = ("); //$NON-NLS-1$
					it.append(expressionContext.getContainerDescription().getElementType());
					it.append(") script."); //$NON-NLS-1$
					it.append(getLanguageScriptMemberGetter());
					it.append("().get(0);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t"); //$NON-NLS-1$
					it.append(expressionContext.getMemberDescription().getElementType());
					it.append(" member = ("); //$NON-NLS-1$
					it.append(expressionContext.getMemberDescription().getElementType());
					it.append(") topElement.get"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName()));
					it.append("().get(0);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t"); //$NON-NLS-1$
					it.append(JvmTypeReference.class);
					it.append(" reference = member.getType();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tif (reference instanceof "); //$NON-NLS-1$
					it.append(JvmParameterizedTypeReference.class);
					it.append(") {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\tfinal "); //$NON-NLS-1$
					it.append(JvmParameterizedTypeReference.class);
					it.append(" pref = ("); //$NON-NLS-1$
					it.append(JvmParameterizedTypeReference.class);
					it.append(") reference;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\tif (!pref.getArguments().isEmpty()) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t\t"); //$NON-NLS-1$
					it.append(EcoreUtil2.class);
					it.append(".resolveAll(resource);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t\treturn pref;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t} catch ("); //$NON-NLS-1$
					it.append(Exception.class);
					it.append(" exception) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tthrow new "); //$NON-NLS-1$
					it.append(TypeNotPresentException.class);
					it.append("(typeName, exception);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t} finally {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tresourceSet.getResources().remove(resource);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tthrow new "); //$NON-NLS-1$
					it.append(TypeNotPresentException.class);
					it.append("(typeName, null);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();					
					it.append("\t/** Create an expression but does not change the container."); //$NON-NLS-1$
					it.newLine();
					it.append("\t *"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param expression the textual representation of the expression."); //$NON-NLS-1$
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
					it.append(getCodeElementExtractor().getLanguageScriptInterface());
					it.append(" script = resource.getContents().isEmpty() ? null : ("); //$NON-NLS-1$
					it.append(getCodeElementExtractor().getLanguageScriptInterface());
					it.append(") resource.getContents().get(0);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t"); //$NON-NLS-1$
					it.append(expressionContext.getContainerDescription().getElementType());
					it.append(" topElement = ("); //$NON-NLS-1$
					it.append(expressionContext.getContainerDescription().getElementType());
					it.append(") script."); //$NON-NLS-1$
					it.append(getLanguageScriptMemberGetter());
					it.append("().get(0);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\t"); //$NON-NLS-1$
					it.append(expressionContext.getMemberDescription().getElementType());
					it.append(" member = ("); //$NON-NLS-1$
					it.append(expressionContext.getMemberDescription().getElementType());
					it.append(") topElement.get"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getMemberCollectionExtensionGrammarName()));
					it.append("().get(0);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\t\treturn member.get"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(expressionContext.getExpressionAssignment().getFeature()));
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
				it.append("\t * @param type the type for which the default value should be determined."); //$NON-NLS-1$
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
				it.append("\t * @param type the type for which the default value should be determined."); //$NON-NLS-1$
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
				it.append(generateStandardCommentFunctions(forInterface, forAppender, "getXExpression()")); //$NON-NLS-1$
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
						it.append(".objToStr(getXExpression());"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
				}
				it.append("\t/** Create a reference to \"this\" object or to the current type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the reference."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(XFeatureCall.class);
				it.append(" createReferenceToThis()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					if (forAppender) {
						it.append("return this.builder.createReferenceToThis();"); //$NON-NLS-1$
					} else {
						it.append("final "); //$NON-NLS-1$
						it.append(XExpression.class);
						it.append(" expr = getXExpression();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(XtendTypeDeclaration.class);
						it.append(" type = "); //$NON-NLS-1$
						it.append(EcoreUtil2.class);
						it.append(".getContainerOfType(expr, "); //$NON-NLS-1$
						it.append(XtendTypeDeclaration.class);
						it.append(".class);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(JvmType.class);
						it.append(" jvmObject = getAssociatedElement("); //$NON-NLS-1$
						it.append(JvmType.class);
						it.append(".class, type, expr.eResource());"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tfinal "); //$NON-NLS-1$
						it.append(XFeatureCall.class);
						it.append(" thisFeature = "); //$NON-NLS-1$
						it.append(XbaseFactory.class);
						it.append(".eINSTANCE.createXFeatureCall();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tthisFeature.setFeature(jvmObject);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn thisFeature;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();

				it.append("\t/** Create a reference to \"super\" object or to the super type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the reference."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append(XFeatureCall.class);
				it.append(" createReferenceToSuper()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					if (forAppender) {
						it.append("return this.builder.createReferenceToSuper();"); //$NON-NLS-1$
					} else {
						it.append("final "); //$NON-NLS-1$
						it.append(XExpression.class);
						it.append(" expr = getXExpression();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(XtendTypeDeclaration.class);
						it.append(" type = "); //$NON-NLS-1$
						it.append(EcoreUtil2.class);
						it.append(".getContainerOfType(expr, "); //$NON-NLS-1$
						it.append(XtendTypeDeclaration.class);
						it.append(".class);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(JvmType.class);
						it.append(" jvmObject = getAssociatedElement("); //$NON-NLS-1$
						it.append(JvmType.class);
						it.append(".class, type, expr.eResource());"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tfinal "); //$NON-NLS-1$
						it.append(XFeatureCall.class);
						it.append(" superFeature = "); //$NON-NLS-1$
						it.append(XbaseFactory.class);
						it.append(".eINSTANCE.createXFeatureCall();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t"); //$NON-NLS-1$
						it.append(JvmIdentifiableElement.class);
						it.append(" feature;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (jvmObject instanceof "); //$NON-NLS-1$
						it.append(JvmDeclaredType.class);
						it.append(") {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tfeature = (("); //$NON-NLS-1$
						it.append(JvmDeclaredType.class);
						it.append(") jvmObject).getExtendedClass().getType();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t} else {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tfeature = findType(expr, getQualifiedName(type)).getType();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tif (feature instanceof "); //$NON-NLS-1$
						it.append(JvmDeclaredType.class);
						it.append(") {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tfeature = (("); //$NON-NLS-1$
						it.append(JvmDeclaredType.class);
						it.append(") feature).getExtendedClass().getType();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t} else {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t\tfeature = null;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (feature == null) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\treturn null;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tsuperFeature.setFeature(feature);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\treturn superFeature;"); //$NON-NLS-1$
					}
					it.newLine();
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
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
		return getExpressionConfig().getFieldContainerDeclarationKeyword();
	}

	/** Replies a keyword for declaring a field.
	 *
	 * @param memberDescription the member description.
	 * @return the keyword, never {@code null} nor an empty string.
	 */
	protected String ensureFieldDeclarationKeyword(CodeElementExtractor.ElementDescription memberDescription) {
		final List<String> modifiers = getCodeBuilderConfig().getModifiers().get(memberDescription.getName());
		if (modifiers != null && !modifiers.isEmpty()) {
			return modifiers.get(0);
		}
		return getExpressionConfig().getFieldDeclarationKeyword();
	}

	/** Replies the description of the expression context.
	 *
	 * @return the description.
	 */
	protected ExpressionContextDescription getExpressionContextDescription() {
		for (final CodeElementExtractor.ElementDescription containerDescription : getCodeElementExtractor().getTopElements(
				getGrammar(), getCodeBuilderConfig())) {
			final AbstractRule rule = getMemberRule(containerDescription);
			if (rule != null) {
				final Pattern fieldTypePattern = Pattern.compile(getExpressionConfig().getExpressionFieldTypenamePattern());
				final ExpressionContextDescription description = getCodeElementExtractor().visitMemberElements(
						containerDescription, rule, null,
					(it, grammarContainer, memberContainer, classifier) -> {
						if (fieldTypePattern.matcher(classifier.getName()).find()) {
							final Assignment expressionAssignment = findAssignmentFromTerminalPattern(
									memberContainer,
									getExpressionConfig().getExpressionGrammarPattern());
							final CodeElementExtractor.ElementDescription memberDescription =
									it.newElementDescription(classifier.getName(), memberContainer,
											classifier, XExpression.class);
							return new ExpressionContextDescription(
									containerDescription,
									memberDescription,
									ensureContainerKeyword(containerDescription.getGrammarComponent()),
									ensureFieldDeclarationKeyword(memberDescription),
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
		return null;
	}

	/** Generate the contributions for the BuildFactory.
	 */
	protected void generateBuilderFactoryContributions() {
		final ExpressionContextDescription expressionContext = getExpressionContextDescription();
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
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(getScriptBuilderInterface());
				it.append(" script = createScript(getFooPackageName(), resource);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(expressionContext.getContainerDescription().getBuilderInterfaceType());
				it.append(" topElement = script.add"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(expressionContext.getContainerDescription().getElementType().getSimpleName()));
				it.append("(getFooTypeName());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(expressionContext.getMemberDescription().getBuilderInterfaceType());
				it.append(" memberElement = topElement.add"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(expressionContext.getMemberDescription().getElementType().getSimpleName()));
				it.append("(getFooMemberName());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn memberElement.get"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(expressionContext.getExpressionAssignment().getFeature()));
				it.append("();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		});
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			final String buildFunctionName = "buildXExpression"; //$NON-NLS-1$
			final TypeReference appender = getCodeElementExtractor().getElementAppenderImpl("Expression"); //$NON-NLS-1$
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

	/** Description of the expression context.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ExpressionContextDescription {

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
		public ExpressionContextDescription(
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

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

import javax.inject.Inject;
import javax.inject.Provider;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmVoid;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.resource.IFragmentProvider;
import org.eclipse.xtext.util.EmfFormatter;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

import io.sarl.lang.mwe2.codebuilder.extractor.CodeElementExtractor;

/** Generator of the builder for formal parameters.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class FormalParameterBuilderFragment extends AbstractSubCodeBuilderFragment {

	/** Replies the implementation for the formal parameter builder.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getFormalParameterBuilderImpl() {
		return getCodeElementExtractor().getElementBuilderImpl("FormalParameter"); //$NON-NLS-1$
	}

	/** Replies the custom implementation for the formal parameter builder.
	 *
	 * @return the custom implementation.
	 */
	@Pure
	public TypeReference getFormalParameterBuilderImplCustom() {
		return getCodeElementExtractor().getElementBuilderImplCustom("FormalParameter"); //$NON-NLS-1$
	}

	@Override
	public void generate() {
		generateIFormalParameterBuilder();
		generateFormalParameterBuilderImpl();
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			generateFormalParameterAppender();
		}
		super.generate();
	}

	@Override
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		bindTypeReferences(factory,
				getFormalParameterBuilderInterface(),
				getFormalParameterBuilderImpl(),
				getFormalParameterBuilderImplCustom());
	}

	/** Generate the formal parameter builder interface.
	 */
	protected void generateIFormalParameterBuilder() {
		final TypeReference builder = getFormalParameterBuilderInterface();
		final StringConcatenationClient content = new StringConcatenationClient() {
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
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the formal parameter builder implementation.
	 */
	protected void generateFormalParameterBuilderImpl() {
		final TypeReference builderInterface = getFormalParameterBuilderInterface();
		final TypeReference builder = getFormalParameterBuilderImpl();
		final StringConcatenationClient content = new StringConcatenationClient() {
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
		final JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the formal parameter appender.
	 */
	protected void generateFormalParameterAppender() {
		final CodeElementExtractor.ElementDescription parameter = getCodeElementExtractor().getFormalParameter();
		final String accessor = "get" //$NON-NLS-1$
				+ Strings.toFirstUpper(parameter.getElementType().getSimpleName()) + "()"; //$NON-NLS-1$
		final TypeReference builderInterface = getFormalParameterBuilderInterface();
		final TypeReference appender = getCodeElementExtractor().getElementAppenderImpl("FormalParameter"); //$NON-NLS-1$
		final StringConcatenationClient content = new StringConcatenationClient() {
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
				it.append(getCodeElementExtractor().getAbstractAppenderImpl());
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
		final CodeElementExtractor.ElementDescription parameter = getCodeElementExtractor().getFormalParameter();
		final FormalParameterDescription exparameter = new FormalParameterDescription(parameter,
				findAssignmentFromFeatureName(parameter.getGrammarComponent(),
						getCodeBuilderConfig().getParameterDefaultValueGrammarName()));
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
					it.append(getCodeElementExtractor().getFormalParameterContainerType());
					it.append(" context;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(parameter.getElementType());
					it.append(" parameter;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(getExpressionBuilderInterface());
					it.append(" defaultValue;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Inject.class);
					it.newLine();
					it.append("\t\tprivate "); //$NON-NLS-1$
					it.append(TypesFactory.class);
					it.append(" jvmTypesFactory;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					it.append("\t@"); //$NON-NLS-1$
					it.append(Inject.class);
					it.newLine();
					it.append(" private "); //$NON-NLS-1$
					it.append(IFragmentProvider.class);
					it.append(" fragmentProvider;"); //$NON-NLS-1$
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
				it.append("\t/** Initialize the formal parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context the context of the formal parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param name the name of the formal parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(getCodeElementExtractor().getFormalParameterContainerType());
				it.append(" context, String name, "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" typeContext)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(context, name, typeContext);"); //$NON-NLS-1$
					} else {
						it.append("\t\tsetTypeResolutionContext(typeContext);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tthis.context = context;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis.parameter = "); //$NON-NLS-1$
						it.append(getXFactoryFor(parameter.getElementType()));
						it.append(".eINSTANCE.create"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(parameter.getElementType().getSimpleName()));
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
				it.append(parameter.getElementType());
				it.append(" get"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(parameter.getElementType().getSimpleName()));
				it.append("()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.get"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(parameter.getElementType().getSimpleName()));
						it.append("();"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn this.parameter;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the JvmIdentifiable that corresponds to the formal parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param container the feature call that is supposed to contains the replied identifiable element."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void setReferenceInto("); //$NON-NLS-1$
				it.append(XFeatureCall.class);
				it.append(" container) "); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append("{"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					if (forAppender) {
						it.append("this.builder.setReferenceInto(container);"); //$NON-NLS-1$
					} else {
						it.append(JvmVoid.class);
						it.append(" jvmVoid = this.jvmTypesFactory.createJvmVoid();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tif (jvmVoid instanceof "); //$NON-NLS-1$
						it.append(InternalEObject.class);
						it.append(") {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tfinal "); //$NON-NLS-1$
						it.append(InternalEObject.class);
						it.append("\t\t\tjvmVoidProxy = ("); //$NON-NLS-1$
						it.append(InternalEObject.class);
						it.append(") jvmVoid;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tfinal "); //$NON-NLS-1$
						it.append(EObject.class);
						it.append(" param = getSarlFormalParameter();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tfinal "); //$NON-NLS-1$
						it.append(Resource.class);
						it.append(" resource = param.eResource();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t// Get the derived object"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tfinal "); //$NON-NLS-1$
						it.append(parameter.getElementType());
						it.append(" jvmParam = getAssociatedElement("); //$NON-NLS-1$
						it.append(parameter.getElementType());
						it.append(".class, param, resource);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\t// Set the proxy URI"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tfinal "); //$NON-NLS-1$
						it.append(URI.class);
						it.append(" uri = "); //$NON-NLS-1$
						it.append(EcoreUtil2.class);
						it.append(".getNormalizedURI(jvmParam);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tjvmVoidProxy.eSetProxyURI(uri);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tcontainer.setFeature(jvmVoid);"); //$NON-NLS-1$
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
					it.append(Strings.toFirstUpper(parameter.getElementType().getSimpleName()));
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
				if (exparameter.getDefaultValueAssignment() != null) {
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
					final String accessor = "get" //$NON-NLS-1$
							+ Strings.toFirstUpper(getCodeBuilderConfig().getParameterDefaultValueGrammarName())
							+ "()"; //$NON-NLS-1$
					it.append(" "); //$NON-NLS-1$
					it.append(accessor);
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
							it.append(Strings.toFirstUpper(parameter.getElementType().getSimpleName()));
							it.append("().set"); //$NON-NLS-1$
							it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterDefaultValueGrammarName()));
							it.append("(it);"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t\t}"); //$NON-NLS-1$
							it.newLine();
							it.append("\t\t\t\t}, getTypeResolutionContext());"); //$NON-NLS-1$
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
					if (!forAppender && !forInterface) {
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
							it.append(accessor);
							it.append(");"); //$NON-NLS-1$
						}
						it.newLine();
						it.append("\t}"); //$NON-NLS-1$
						it.newLineIfNotEmpty();
						it.newLine();
					}
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

	/** Description of a formal parameter.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class FormalParameterDescription {

		private final CodeElementExtractor.ElementDescription element;

		private final Assignment defaultValueAssignment;

		/** Constructor.
		 *
		 * @param element the description of the element.
		 * @param defaultValueAssignment the assignement for the default value.
		 */
		public FormalParameterDescription(CodeElementExtractor.ElementDescription element, Assignment defaultValueAssignment) {
			this.element = element;
			this.defaultValueAssignment = defaultValueAssignment;
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

		/** Replies the assignment for the parameter's default value.
		 *
		 * @return the default value parameter assignment.
		 */
		public Assignment getDefaultValueAssignment() {
			return this.defaultValueAssignment;
		}

	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.common.types.JvmLowerBound;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeParameterDeclarator;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.util.EmfFormatter;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

import com.google.inject.Inject;

/** Generator of the builder for type parameters.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version mwe2 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid mwe2
 */
public class TypeParameterBuilderFragment extends AbstractSubCodeBuilderFragment {

	/** Replies the implementation for the type parameter builder.
	 *
	 * @return the implementation.
	 */
	@Pure
	public TypeReference getTypeParameterBuilderImpl() {
		return getCodeElementExtractor().getElementBuilderImpl("TypeParameter"); //$NON-NLS-1$
	}

	/** Replies the custom implementation for the type parameter builder.
	 *
	 * @return the custom implementation.
	 */
	@Pure
	public TypeReference getTypeParameterBuilderImplCustom() {
		return getCodeElementExtractor().getElementBuilderImplCustom("TypeParameter"); //$NON-NLS-1$
	}

	@Override
	public void generate() {
		generateITypeParameterBuilder();
		generateTypeParameterBuilderImpl();
		if (getCodeBuilderConfig().isISourceAppendableEnable()) {
			generateTypeParameterAppender();
		}
		super.generate();
	}

	@Override
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		bindTypeReferences(factory,
				getTypeParameterBuilderInterface(),
				getTypeParameterBuilderImpl(),
				getTypeParameterBuilderImplCustom());
	}

	/** Generate the type parameter builder interface.
	 */
	protected void generateITypeParameterBuilder() {
		final var builder = getTypeParameterBuilderInterface();
		final var content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
					+ " type parameter."); //$NON-NLS-1$
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
				it.append(generateMembers(true, false));
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
		final var javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the formal parameter builder implementation.
	 */
	protected void generateTypeParameterBuilderImpl() {
		final var builderInterface = getTypeParameterBuilderInterface();
		final var builder = getTypeParameterBuilderImpl();
		final var content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Builder of a " + getLanguageName() //$NON-NLS-1$
					+ " type parameter."); //$NON-NLS-1$
				it.newLine();
				appendFileLineComment(it);
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
		final var javaFile = getFileAccessFactory().createJavaFile(builder, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the formal parameter appender.
	 */
	protected void generateTypeParameterAppender() {
		final var parameter = getCodeElementExtractor().getTypeParameter();
		final var accessor = "get" //$NON-NLS-1$
				+ Strings.toFirstUpper(parameter.elementType().getSimpleName()) + "()"; //$NON-NLS-1$
		final var builderInterface = getTypeParameterBuilderInterface();
		final var appender = getCodeElementExtractor().getElementAppenderImpl("TypeParameter"); //$NON-NLS-1$
		final var content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Appender of a " + getLanguageName() //$NON-NLS-1$
					+ " type parameter."); //$NON-NLS-1$
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
		final var javaFile = getFileAccessFactory().createJavaFile(appender, content);
		javaFile.writeTo(getSrcGen());
	}

	/** Generate the members of the builder.
	 *
	 * @param forInterface {@code true} if the code must be generated for an interface.
	 * @param forAppender {@code true} if the code must be generated for an appender.
	 * @return the code.
	 */
	protected StringConcatenationClient generateMembers(boolean forInterface, boolean forAppender) {
		final var parameter = getCodeElementExtractor().getTypeParameter();
		return new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				if (!forInterface && !forAppender) {
					appendEmptyComment(it);
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(EObject.class);
					it.append(" context;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					appendEmptyComment(it);
					it.append("\tprivate "); //$NON-NLS-1$
					it.append(parameter.elementType());
					it.append(" parameter;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					appendEmptyComment(it);
					it.append("\t@"); //$NON-NLS-1$
					it.append(Inject.class);
					it.newLine();
					it.append("\t\tprivate "); //$NON-NLS-1$
					it.append(TypesFactory.class);
					it.append(" jvmTypesFactory;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
					appendEmptyComment(it);
					it.append("\tprivate boolean hasDefaultConstraint = true;"); //$NON-NLS-1$
					it.newLineIfNotEmpty();
					it.newLine();
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
					it.append("\t/** Find the reference to the type with the given type parameters."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the type to reference"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param args the type parameters to add to the to reference to the given type"); //$NON-NLS-1$
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
					it.append("\t/** Find the reference to the type with the given type parameters."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the type to reference"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param args the type parameters to add to the to reference to the given type"); //$NON-NLS-1$
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
					it.append("\t/** Find the reference to the type with the given type parameters."); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param context the context in which the type is defined"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param type the type to reference"); //$NON-NLS-1$
					it.newLine();
					it.append("\t * @param args the type parameters to add to the to reference to the given type"); //$NON-NLS-1$
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
				it.append("\t/** Initialize the type parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * <p>Caution: This initialization function does not add the type parameter in its container."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * The container is responsible of adding the type parameter in its internal object."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * However, this function sets the declarator of the type parameter by calling"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * {@link JvmTypeParameter#setDeclarator(JvmTypeParameterDeclarator)}."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context the container of the type parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param declarator the container of the type parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param name the name of the type parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param typeContext the provider of types or null."); //$NON-NLS-1$
				it.newLine();
				appendFileLineComment(it);
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(XtendTypeDeclaration.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(JvmTypeParameterDeclarator.class);
				it.append(" declarator, String name, "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" typeContext)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(context, declarator, name, typeContext);"); //$NON-NLS-1$
					} else {
						it.append("\t\tinternalEInit(context, declarator, name, typeContext);"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Initialize the type parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * <p>Caution: This initialization function does not add the type parameter in its container."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * The container is responsible of adding the type parameter in its internal object."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * However, this function sets the declarator of the type parameter by calling"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * {@link JvmTypeParameter#setDeclarator(JvmTypeParameterDeclarator)}."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context the container of the type parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param declarator the container of the type parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param name the name of the type parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param typeContext the provider of types or null."); //$NON-NLS-1$
				it.newLine();
				appendFileLineComment(it);
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t"); //$NON-NLS-1$
				if (!forInterface) {
					it.append("public "); //$NON-NLS-1$
				}
				it.append("void eInit("); //$NON-NLS-1$
				it.append(XtendExecutable.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(JvmTypeParameterDeclarator.class);
				it.append(" declarator, String name, "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" typeContext)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\tthis.builder.eInit(context, declarator, name, typeContext);"); //$NON-NLS-1$
					} else {
						it.append("\t\tinternalEInit(context, declarator, name, typeContext);"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				if (!forInterface && !forAppender) {
					appendEmptyComment(it);
					it.append("\tprotected "); //$NON-NLS-1$
					it.append("void internalEInit("); //$NON-NLS-1$
					it.append(EObject.class);
					it.append(" context, "); //$NON-NLS-1$
					it.append(JvmTypeParameterDeclarator.class);
					it.append(" declarator, String name, "); //$NON-NLS-1$
					it.append(IJvmTypeProvider.class);
					it.append(" typeContext)"); //$NON-NLS-1$
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tsetTypeResolutionContext(typeContext);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tthis.context = context;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tthis.parameter = this.jvmTypesFactory.create"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(parameter.elementType().getSimpleName()));
					it.append("();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tthis.parameter.set"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(getCodeBuilderConfig().getParameterNameGrammarName()));
					it.append("(name);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tif (declarator != null) {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t\tthis.parameter.setDeclarator(declarator);"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t}"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tfinal "); //$NON-NLS-1$
					it.append(JvmUpperBound.class);
					it.append(" constraint = this.jvmTypesFactory.createJvmUpperBound();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tconstraint.setTypeReference(newTypeRef("); //$NON-NLS-1$
					it.append(Object.class);
					it.append(".class));"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\tgetJvmTypeParameter().getConstraints().add(constraint);"); //$NON-NLS-1$
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
				it.append("\t/** Replies the created parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the parameter."); //$NON-NLS-1$
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
				it.append(parameter.elementType());
				it.append(" get"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(parameter.elementType().getSimpleName()));
				it.append("()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.get"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(parameter.elementType().getSimpleName()));
						it.append("();"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn this.parameter;"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies a reference to the created parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the reference to the parameter."); //$NON-NLS-1$
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
				it.append(JvmTypeReference.class);
				it.append(" get"); //$NON-NLS-1$
				it.append(Strings.toFirstUpper(parameter.elementType().getSimpleName()));
				it.append("Reference()"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					if (forAppender) {
						it.append("\t\treturn this.builder.get"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(parameter.elementType().getSimpleName()));
						it.append("Reference();"); //$NON-NLS-1$
					} else {
						it.append("\t\treturn newTypeRef(this.parameter);"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the resource to which the type parameter is attached."); //$NON-NLS-1$
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
					it.append("\t\treturn get"); //$NON-NLS-1$
					it.append(Strings.toFirstUpper(parameter.elementType().getSimpleName()));
					it.append("().eResource();"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				if (!forAppender && !forInterface) {
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
						it.append(".objToStr(get"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(parameter.elementType().getSimpleName()));
						it.append("());"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
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

				it.append("\t/** Add upper type bounds."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type the type."); //$NON-NLS-1$
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
				it.append(getTypeParameterBuilderInterface());
				it.append(" addUpperConstraint(String type)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					if (forAppender) {
						it.append("this.builder.addUpperConstraint(type);"); //$NON-NLS-1$
					} else {
						it.append("addUpperConstraint(newTypeRef(this.context, type));"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t\treturn this;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Add upper type bounds."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type the type."); //$NON-NLS-1$
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
				it.append(getTypeParameterBuilderInterface());
				it.append(" addUpperConstraint("); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" type)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					if (forAppender) {
						it.append("this.builder.addUpperConstraint(type);"); //$NON-NLS-1$
					} else {
						it.append("if (this.hasDefaultConstraint) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis.hasDefaultConstraint = false;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tgetJvmTypeParameter().getConstraints().clear();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tfinal "); //$NON-NLS-1$
						it.append(JvmUpperBound.class);
						it.append(" constraint = this.jvmTypesFactory.createJvmUpperBound();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tconstraint.setTypeReference(type);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tget"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(parameter.elementType().getSimpleName()));
						it.append("().getConstraints().add(constraint);"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t\treturn this;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();

				it.append("\t/** Add lower type bounds."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type the type."); //$NON-NLS-1$
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
				it.append(getTypeParameterBuilderInterface());
				it.append(" addLowerConstraint(String type)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					if (forAppender) {
						it.append("this.builder.addLowerConstraint(type);"); //$NON-NLS-1$
					} else {
						it.append("addLowerConstraint(newTypeRef(this.context, type));"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t\treturn this;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Add lower type bounds."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type the type."); //$NON-NLS-1$
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
				it.append(getTypeParameterBuilderInterface());
				it.append(" addLowerConstraint("); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" type)"); //$NON-NLS-1$
				if (forInterface) {
					it.append(";"); //$NON-NLS-1$
				} else {
					it.append(" {"); //$NON-NLS-1$
					it.newLine();
					it.append("\t\t"); //$NON-NLS-1$
					if (forAppender) {
						it.append("this.builder.addLowerConstraint(type);"); //$NON-NLS-1$
					} else {
						it.append("if (this.hasDefaultConstraint) {"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tthis.hasDefaultConstraint = false;"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t\tgetJvmTypeParameter().getConstraints().clear();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\t}"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tfinal "); //$NON-NLS-1$
						it.append(JvmLowerBound.class);
						it.append(" constraint = this.jvmTypesFactory.createJvmLowerBound();"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tconstraint.setTypeReference(type);"); //$NON-NLS-1$
						it.newLine();
						it.append("\t\tget"); //$NON-NLS-1$
						it.append(Strings.toFirstUpper(parameter.elementType().getSimpleName()));
						it.append("().getConstraints().add(constraint);"); //$NON-NLS-1$
					}
					it.newLine();
					it.append("\t\treturn this;"); //$NON-NLS-1$
					it.newLine();
					it.append("\t}"); //$NON-NLS-1$
				}
				it.newLineIfNotEmpty();
				it.newLine();
			}
		};
	}

}

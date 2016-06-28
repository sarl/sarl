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

import javax.inject.Inject;
import javax.inject.Named;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.Primitives;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.imports.IImportsConfiguration;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReferenceFactory;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xtext.generator.model.JavaFileAccess;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/** Generator of the abstract code builder.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AbstractBuilderBuilderFragment extends AbstractSubCodeBuilderFragment {

	@Override
	public void generate() {
		generateAbstractBuilder();
	}

	/** Generate the abstract builder.
	 */
	@SuppressWarnings("checkstyle:all")
	protected void generateAbstractBuilder() {
		final TypeReference abstractBuilder = getAbstractBuilderImpl();
		StringConcatenationClient content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Abstract implementation of a builder for the " //$NON-NLS-1$
						+ getLanguageName() + " language."); //$NON-NLS-1$
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("@SuppressWarnings(\"all\")"); //$NON-NLS-1$
				it.newLine();
				it.append("public abstract class "); //$NON-NLS-1$
				it.append(abstractBuilder.getSimpleName());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(CommonTypeComputationServices.class);
				it.append(" services;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(ImportManager.class);
				it.append(" importManager;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(TypeReferences.class);
				it.append(" typeReferences;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(Primitives.class);
				it.append(" primitives;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(IImportsConfiguration.class);
				it.append(" importsConfiguration;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(IResourceFactory.class);
				it.append(" resourceFactory;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate String fileExtension;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tpublic void setFileExtensions(@"); //$NON-NLS-1$
				it.append(Named.class);
				it.append("("); //$NON-NLS-1$
				it.append(Constants.class);
				it.append(".FILE_EXTENSIONS) String fileExtensions) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.fileExtension = fileExtensions.split(\"[:;,]+\")[0];"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the script's file extension."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic String getScriptFileExtension() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.fileExtension;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the builder of type references."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the type reference builder."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(TypeReferences.class);
				it.append(" getTypeReferences() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.typeReferences;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the primitive type tools."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the primitive type tools."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(Primitives.class);
				it.append(" getPrimitiveTypes() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.primitives;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Create a reference to the given type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context - the context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param typeName - the name of the type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the type reference."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(" newTypeRef("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" context, String typeName) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(TypeReferences.class);
				it.append(" typeRefs = getTypeReferences();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn newTypeRef(context, typeName, typeRefs.getTypeForName(typeName, context));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Create a reference to the given type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context - the context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param type - the type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the type reference."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(" newTypeRef("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" context, Class<?> type) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(TypeReferences.class);
				it.append(" typeRefs = getTypeReferences();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn newTypeRef(context, type.getName(), typeRefs.getTypeForName(type, context));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(" newTypeRef("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" context, String typeName, "); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" typeReference) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!isTypeReference(typeReference) && !getPrimitiveTypes().isPrimitive(typeReference)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(TypeReferences.class);
				it.append(" typeRefs = getTypeReferences();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfor (String packageName : getImportsConfiguration().getImplicitlyImportedPackages(("); //$NON-NLS-1$
				it.append(XtextResource.class);
				it.append(") context.eResource())) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\ttypeReference = typeRefs.getTypeForName(packageName + \".\" + typeName, context);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (isTypeReference(typeReference)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tgetImportManager().addImportFor(typeReference.getType());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\treturn ("); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(") typeReference;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!isTypeReference(typeReference)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthrow new TypeNotPresentException(typeName, null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tgetImportManager().addImportFor(typeReference.getType());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn ("); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(") typeReference;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();

				it.append("\t/** Replies if the first parameter is a subtype of the second parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context - the context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param subType - the subtype to test."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param superType - the expected super type."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the type reference."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected boolean isSubTypeOf("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" subType, "); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" superType) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (isTypeReference(superType) && isTypeReference(subType)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(StandardTypeReferenceOwner.class);
				it.append(" owner = new "); //$NON-NLS-1$
				it.append(StandardTypeReferenceOwner.class);
				it.append("(services, context);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(LightweightTypeReferenceFactory.class);
				it.append(" factory = new "); //$NON-NLS-1$
				it.append(LightweightTypeReferenceFactory.class);
				it.append("(owner, false);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(LightweightTypeReference.class);
				it.append(" reference = factory.toLightweightReference(subType);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn reference.isSubtypeOf(superType.getType());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn false;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies if the given object is a valid type reference."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected boolean isTypeReference("); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" typeReference) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn (typeReference != null && !typeReference.eIsProxy()"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t&& typeReference.getType() != null && !typeReference.getType().eIsProxy());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the import's configuration."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the import's configuration."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(IImportsConfiguration.class);
				it.append(" getImportsConfiguration() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.importsConfiguration;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Compute a unused URI for a synthetic resource."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param resourceSet - the resource set in which the resource should be located."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the uri."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(URI.class);
				it.append(" computeUnusedUri("); //$NON-NLS-1$
				it.append(ResourceSet.class);
				it.append(" resourceSet) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tString name = \"__synthetic\";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfor (int i = 0; i < Integer.MAX_VALUE; ++i) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(URI.class);
				it.append(" syntheticUri = "); //$NON-NLS-1$
				it.append(URI.class);
				it.append(".createURI(name + i + \".\" + getScriptFileExtension());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (resourceSet.getResource(syntheticUri, false) == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn syntheticUri;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthrow new IllegalStateException();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the resource factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the resource factory."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(IResourceFactory.class);
				it.append(" getResourceFactory() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.resourceFactory;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies if the type could contains functions with a body."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected boolean isActionBodyAllowed("); //$NON-NLS-1$
				it.append(getLanguageTopElementType());
				it.append(" type) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn "); //$NON-NLS-1$
				if (getCodeBuilderConfig().getNoActionBodyTypes().isEmpty()) {
					it.append("true"); //$NON-NLS-1$
				} else {
					it.append("!("); //$NON-NLS-1$
					boolean first = true;
					for (String noBodyType : getCodeBuilderConfig().getNoActionBodyTypes()) {
						if (first) {
							first = false;
						} else {
							it.newLine();
							it.append("\t\t\t|| "); //$NON-NLS-1$
						}
						it.append("type instanceof "); //$NON-NLS-1$
						it.append(new TypeReference(getLanguageBasePackage() + "." + noBodyType)); //$NON-NLS-1$
					}
					it.append(")"); //$NON-NLS-1$
				}
				it.append(";"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the import manager that stores the imported types."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @return the import manager."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(ImportManager.class);
				it.append(" getImportManager() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.importManager;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
			}

		};
		JavaFileAccess javaFile = getFileAccessFactory().createJavaFile(abstractBuilder, content);
		javaFile.writeTo(getSrcGen());
	}

}

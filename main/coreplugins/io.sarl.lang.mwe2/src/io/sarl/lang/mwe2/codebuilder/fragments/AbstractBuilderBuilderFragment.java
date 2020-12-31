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
import javax.inject.Named;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.common.types.JvmAnyTypeReference;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeConstraint;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmWildcardTypeReference;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.common.types.util.Primitives;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.resource.DerivedStateAwareResource;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.imports.IImportsConfiguration;
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator;
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
		final TypeReference expressionBuilder = getExpressionBuilderImpl();
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
				it.append("\t\tprivate "); //$NON-NLS-1$
				it.append(IQualifiedNameProvider.class);
				it.append(" qualifiedNameProvider;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Inject.class);
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(JvmModelAssociator.class);
				it.append(" associations;"); //$NON-NLS-1$
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
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" typeResolutionContext;"); //$NON-NLS-1$
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
				it.append("\tprotected <T> T getAssociatedElement(Class<T> expectedType, "); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" dslObject, "); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfor (final "); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" obj : this.associations.getJvmElements(dslObject)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (expectedType.isInstance(obj)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn expectedType.cast(obj);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (resource instanceof "); //$NON-NLS-1$
				it.append(DerivedStateAwareResource.class);
				it.append(") {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t(("); //$NON-NLS-1$
				it.append(DerivedStateAwareResource.class);
				it.append(") resource).discardDerivedState();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tresource.getContents();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn getAssociatedElement(expectedType, dslObject, null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthrow new "); //$NON-NLS-1$
				it.append(IllegalStateException.class);
				it.append("(\"No JvmFormalParameter associated to \" + dslObject + \" in \" + dslObject.eContainer());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected void setTypeResolutionContext("); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" context) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.typeResolutionContext = context;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" getTypeResolutionContext() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.typeResolutionContext;"); //$NON-NLS-1$
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
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" innerFindType("); //$NON-NLS-1$
				it.append(Notifier.class);
				it.append(" context, String typeName) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(IJvmTypeProvider.class);
				it.append(" provider = getTypeResolutionContext();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(JvmType.class);
				it.append(" type = null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (provider != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\ttype = provider.findTypeByName(typeName);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(TypeReferences.class);
				it.append(" typeRefs = getTypeReferences();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (type == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\ttype = typeRefs.findDeclaredType(typeName, context);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (type == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn null;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn typeRefs.createTypeRef(type);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" findType("); //$NON-NLS-1$
				it.append(Notifier.class);
				it.append(" context, String typeName) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" type = innerFindType(context, typeName);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (!isTypeReference(type)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t"); //$NON-NLS-1$
				it.append(XtextResource.class);
				it.append(" xtextResource = toResource(context);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfor (String packageName : getImportsConfiguration().getImplicitlyImportedPackages(xtextResource)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" typeReference = innerFindType(context, packageName + \".\" + typeName);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (isTypeReference(typeReference)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\treturn typeReference;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tthrow new "); //$NON-NLS-1$
				it.append(TypeNotPresentException.class);
				it.append("(typeName, null);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn type;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprotected static "); //$NON-NLS-1$
				it.append(XtextResource.class);
				it.append(" toResource("); //$NON-NLS-1$
				it.append(Notifier.class);
				it.append(" context) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn ("); //$NON-NLS-1$
				it.append(XtextResource.class);
				it.append(") (context instanceof "); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" ? context : (("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(")context).eResource());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the type reference for the given name in the given context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(" newTypeRef(String typeName) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn newTypeRef(eResource(), typeName);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies the type reference for the given name in the given context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(" newTypeRef("); //$NON-NLS-1$
				it.append(Notifier.class);
				it.append(" context, String typeName) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" typeReference;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\ttry {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\ttypeReference = findType(context, typeName);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tgetImportManager().addImportFor(typeReference.getType());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\treturn ("); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(") typeReference;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t} catch ("); //$NON-NLS-1$
				it.append(TypeNotPresentException.class);
				it.append(" exception) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(" pref = "); //$NON-NLS-1$
				it.append(expressionBuilder);
				it.append(".parseType(context, typeName, this);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" baseType = findType(context, pref.getType().getIdentifier());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal int len = pref.getArguments().size();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append("[] args = new "); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append("[len];"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfor (int i = 0; i < len; ++i) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal "); //$NON-NLS-1$
				it.append(JvmTypeReference.class);
				it.append(" original = pref.getArguments().get(i);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (original instanceof "); //$NON-NLS-1$
				it.append(JvmAnyTypeReference.class);
				it.append(") {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\targs[i] = "); //$NON-NLS-1$
				it.append(EcoreUtil.class);
				it.append(".copy(original);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else if (original instanceof "); //$NON-NLS-1$
				it.append(JvmWildcardTypeReference.class);
				it.append(") {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tfinal "); //$NON-NLS-1$
				it.append(JvmWildcardTypeReference.class);
				it.append(" wc = EcoreUtil.copy(("); //$NON-NLS-1$
				it.append(JvmWildcardTypeReference.class);
				it.append(") original);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tfor (final "); //$NON-NLS-1$
				it.append(JvmTypeConstraint.class);
				it.append(" c : wc.getConstraints()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\tc.setTypeReference(newTypeRef(context, c.getTypeReference().getIdentifier()));"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\targs[i] = wc;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\targs[i] = newTypeRef(context, original.getIdentifier());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal "); //$NON-NLS-1$
				it.append(TypeReferences.class);
				it.append(" typeRefs = getTypeReferences();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn typeRefs.createTypeRef(baseType.getType(), args);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Replies if the first parameter is a subtype of the second parameter."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param context the context."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param subType the subtype to test."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param superType the expected super type."); //$NON-NLS-1$
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
				it.append("\t * @param resourceSet the resource set in which the resource should be located."); //$NON-NLS-1$
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
				it.append(getCodeElementExtractor().getLanguageTopElementType());
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
						it.append(new TypeReference(getCodeElementExtractor().getLanguageBasePackage() + "." + noBodyType)); //$NON-NLS-1$
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
				it.append("\tprotected String getQualifiedName("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" object) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn this.qualifiedNameProvider.getFullyQualifiedName(object).toString();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Pure.class);
				it.newLine();
				it.append("\tpublic abstract "); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" eResource();"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tpublic void dispose() {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(Resource.class);
				it.append(" resource = eResource();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(ResourceSet.class);
				it.append(" resourceSet = resource.getResourceSet();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tresourceSet.getResources().remove(resource);"); //$NON-NLS-1$
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

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.xtend.core.scoping.AbstractScope;
import org.eclipse.xtend.core.serializer.XtendSerializerScopeProvider;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeParameterDeclarator;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.impl.CompositeNodeWithSemanticElement;
import org.eclipse.xtext.resource.EObjectDescription;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.serializer.tokens.SerializerScopeProviderBinding;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociations;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;

/**
 * A {@link AbstractXtextGeneratorFragment} that enables to create a provider of {@code IScope}
 * for the seiralizer of appenders.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class SerializerScopeProviderFragment extends AbstractSubCodeBuilderFragment {

	private static final Logger LOG = Logger.getLogger(SerializerScopeProviderFragment.class);

	@Override
	public void generate() {
		super.generate();
		LOG.info("Generating the scope provider for the serializer"); //$NON-NLS-1$
		generateSerializerScopeProvider();
		generateTypeParameterScope();
	}

	@Override
	public void generateRuntimeBindings(BindingFactory factory) {
		super.generateRuntimeBindings(factory);
		final var client = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation builder) {
				builder.append("binder.bind("); //$NON-NLS-1$
				builder.append(getCodeElementExtractor().newTypeReference(IScopeProvider.class));
				builder.append(".class).annotatedWith("); //$NON-NLS-1$
				builder.append(SerializerScopeProviderBinding.class);
				builder.append(".class).to("); //$NON-NLS-1$
				builder.append(getCodeElementExtractor().getSerializerScopeProvider());
				builder.append(".class);"); //$NON-NLS-1$
			}
		};
		// The following name is defined in the super class of the module (from Xtend and Xbase).
		factory.addConfiguredBinding("SerializerIScopeProvider", client); //$NON-NLS-1$
	}

	/** Generate the provider of scope used by the serializer.
	 */
	protected void generateSerializerScopeProvider() {
		final var provider = getCodeElementExtractor().getSerializerScopeProvider();
		final var scope = getCodeElementExtractor().getSerializerTypeParameterScope();
		final var content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Provider of scope for the serializer that is used by a appender/builder of Ecore elements."); //$NON-NLS-1$
				it.newLine();
				it.append(" * "); //$NON-NLS-1$
				it.append(getFileAndLineNumber(0));
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.newLine();
				it.append("public class "); //$NON-NLS-1$
				it.append(provider.getSimpleName());
				it.append(" extends "); //$NON-NLS-1$
				it.append(XtendSerializerScopeProvider.class);
				it .append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(getInjectType());
				it.newLine();
				it.append("\tprivate "); //$NON-NLS-1$
				it.append(IJvmModelAssociations.class);
				it.append(" associations;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Override.class);
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(IScope.class);
				it.append(" getScope("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(EReference.class);
				it.append(" reference) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tvar scope = super.getScope(context, reference);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// When the "); //$NON-NLS-1$
				it.append(getLanguageName());
				it.append(" Ecore was created by the regular compiler tools, a grammar node is attached."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// When the "); //$NON-NLS-1$
				it.append(getLanguageName());
				it.append(" Ecore was builder with a coe builder, no grammar node is attached."); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif ("); //$NON-NLS-1$
				it.append(EcoreUtil2.class);
				it.append(".getExistingAdapter(context, "); //$NON-NLS-1$
				it.append(CompositeNodeWithSemanticElement.class);
				it.append(".class) == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tscope = doGetScopeIfTypeParameter(context, reference, scope);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn scope;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();				
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(IScope.class);
				it.append(" doGetScopeIfTypeParameter("); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" context, "); //$NON-NLS-1$
				it.append(EReference.class);
				it.append(" reference, "); //$NON-NLS-1$
				it.append(IScope.class);
				it.append(" baseScope) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t"); //$NON-NLS-1$
				it.append(IScope.class);
				it.append(" scope = baseScope;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (scope instanceof "); //$NON-NLS-1$
				it.append(AbstractScope.class);
				it.append(" ascope && context instanceof "); //$NON-NLS-1$
				it.append(JvmParameterizedTypeReference.class);
				it.append(" ref && ref.getType() instanceof "); //$NON-NLS-1$
				it.append(JvmTypeParameter.class);
				it.append(") {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal var jvmObject = this.associations.getPrimaryJvmElement(context);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal var typeParameters = new "); //$NON-NLS-1$
				it.append(ArrayList.class);
				it.append("<"); //$NON-NLS-1$
				it.append(List.class);
				it.append("<"); //$NON-NLS-1$
				it.append(JvmTypeParameter.class);
				it.append(">>();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (jvmObject == null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" logicalContainer = context.eContainer();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (logicalContainer != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\twhile (logicalContainer != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\tfinal var logicalContainer0 = this.associations.getPrimaryJvmElement(context);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\tif (logicalContainer0 instanceof "); //$NON-NLS-1$
				it.append(JvmTypeParameterDeclarator.class);
				it.append(" typeParamProvider) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\tif (!typeParamProvider.getTypeParameters().isEmpty()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\ttypeParameters.add(typeParamProvider.getTypeParameters());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\tfinal var eclass = logicalContainer.eClass();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\ttry {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\tfinal var sfeature = eclass.getEStructuralFeature(\""); //$NON-NLS-1$
				it.append(getCodeBuilderConfig().getTypeParameterListGrammarName());
				it.append("\");"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\tif (logicalContainer.eIsSet(sfeature)) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\t\tfinal var params = logicalContainer.eGet(sfeature);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\t\tif (params != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\t\t\tif (params instanceof "); //$NON-NLS-1$
				it.append(List.class);
				it.append("<?> list) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\t\t\t\ttypeParameters.add(("); //$NON-NLS-1$
				it.append(List.class);
				it.append("<"); //$NON-NLS-1$
				it.append(JvmTypeParameter.class);
				it.append(">) list);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\t\t\tbreak;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t} catch ("); //$NON-NLS-1$
				it.append(Throwable.class);
				it.append(" ex) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t\t// Ignore this error because it corresponding to an unknown feature for the container"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\tlogicalContainer = logicalContainer.eContainer();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t} else {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t"); //$NON-NLS-1$
				it.append(EObject.class);
				it.append(" logicalContainer = jvmObject.eContainer();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tif (logicalContainer != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\twhile (logicalContainer != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\tif (logicalContainer instanceof "); //$NON-NLS-1$
				it.append(JvmTypeParameterDeclarator.class);
				it.append(" typeParamProvider && !typeParamProvider.getTypeParameters().isEmpty()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t\ttypeParameters.add(typeParamProvider.getTypeParameters());"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t\tlogicalContainer = logicalContainer.eContainer();"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (!typeParameters.isEmpty()) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\tscope = new "); //$NON-NLS-1$
				it.append(scope);
				it.append("(typeParameters, ascope, this.associations);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn scope;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("}"); //$NON-NLS-1$
			}

		};
		final var createJavaFile = getFileAccessFactory().createJavaFile(provider, content);
		createJavaFile.writeTo(getSrcGen());
	}

	/** Generate the type-parameter scope used by the serializer.
	 */
	protected void generateTypeParameterScope() {
		final var scope = getCodeElementExtractor().getSerializerTypeParameterScope();
		final var content = new StringConcatenationClient() {
			@Override
			protected void appendTo(TargetStringConcatenation it) {
				it.append("/** Scope dedicated to type parameters for the serializer that is used by a appender/builder of Ecore elements."); //$NON-NLS-1$
				it.newLine();
				it.append(" * "); //$NON-NLS-1$
				it.append(getFileAndLineNumber(0));
				it.newLine();
				it.append(" */"); //$NON-NLS-1$
				it.append("public class "); //$NON-NLS-1$
				it.append(scope.getSimpleName());
				it.append(" extends "); //$NON-NLS-1$
				it.append(getCodeBuilderConfig().getSuperTypeParameterScopeType());
				it.append(" {"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\tprivate final "); //$NON-NLS-1$
				it.append(IJvmModelAssociations.class);
				it.append(" associations;"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t/** Constructor."); //$NON-NLS-1$
				it.newLine();
				it.append("\t *"); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param typeParameters the type parameters to include in the scope."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param parent the parent scope."); //$NON-NLS-1$
				it.newLine();
				it.append("\t * @param associations the tool for finding associated objects to an EObject."); //$NON-NLS-1$
				it.newLine();
				it.append("\t */"); //$NON-NLS-1$
				it.newLine();
				it.append("\tpublic "); //$NON-NLS-1$
				it.append(scope.getSimpleName());
				it.append("("); //$NON-NLS-1$
				it.append(List.class);
				it.append("<"); //$NON-NLS-1$
				it.append(List.class);
				it.append("<"); //$NON-NLS-1$
				it.append(JvmTypeParameter.class);
				it.append(">> typeParameters, "); //$NON-NLS-1$
				it.append(AbstractScope.class);
				it.append(" parent, "); //$NON-NLS-1$
				it.append(IJvmModelAssociations.class);
				it.append(" associations) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tsuper(typeParameters, parent);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tthis.associations = associations;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("\t@"); //$NON-NLS-1$
				it.append(Override.class);
				it.newLine();
				it.append("\tprotected "); //$NON-NLS-1$
				it.append(IEObjectDescription.class);
				it.append(" doGetSingleElement("); //$NON-NLS-1$
				it.append(QualifiedName.class);
				it.append(" name) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tfinal var candidate = super.doGetSingleElement(name);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t// This is the place of the bug fix"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\tif (candidate != null && candidate.getEObjectOrProxy() instanceof "); //$NON-NLS-1$
				it.append(JvmTypeParameter.class);
				it.append(" param) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tfinal var sourceElement = this.associations.getPrimarySourceElement(param);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\tif (sourceElement != null) {"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t\treturn "); //$NON-NLS-1$
				it.append(EObjectDescription.class);
				it.append(".create(name, sourceElement);"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\t}"); //$NON-NLS-1$
				it.newLine();
				it.append("\t\treturn candidate;"); //$NON-NLS-1$
				it.newLine();
				it.append("\t}"); //$NON-NLS-1$
				it.newLineIfNotEmpty();
				it.newLine();
				it.append("}"); //$NON-NLS-1$
			}

		};
		final var createJavaFile = getFileAccessFactory().createJavaFile(scope, content);
		createJavaFile.writeTo(getSrcGen());
	}

}

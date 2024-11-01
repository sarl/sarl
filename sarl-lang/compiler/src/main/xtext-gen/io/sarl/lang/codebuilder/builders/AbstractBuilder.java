/*
 * $Id$
 *
 * File is automatically generated by the Xtext language generator.
 * Do not change it.
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors.
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
package io.sarl.lang.codebuilder.builders;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlInterface;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
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

/** Abstract implementation of a builder for the Sarl language.
 */
@SuppressWarnings("all")
public abstract class AbstractBuilder {

	@Inject
		private IQualifiedNameProvider qualifiedNameProvider;
	@Inject
	private JvmModelAssociator associations;

	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private ImportManager importManager;

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private Primitives primitives;

	@Inject
	private IImportsConfiguration importsConfiguration;

	@Inject
	private IResourceFactory resourceFactory;

	private String fileExtension;

	private IJvmTypeProvider typeResolutionContext;

	@Inject
	public void setFileExtensions(@Named(Constants.FILE_EXTENSIONS) String fileExtensions) {
		this.fileExtension = fileExtensions.split("[:;,]+")[0];
	}

	protected <T> T getAssociatedElement(Class<T> expectedType, EObject dslObject, Resource resource) {
		for (final EObject obj : this.associations.getJvmElements(dslObject)) {
			if (expectedType.isInstance(obj)) {
				return expectedType.cast(obj);
			}
		}
		if (resource instanceof DerivedStateAwareResource $c$value) {
			$c$value.discardDerivedState();
			$c$value.getContents();
			return getAssociatedElement(expectedType, dslObject, null);
		}
		throw new IllegalStateException("No JvmFormalParameter associated to " + dslObject + " in " + dslObject.eContainer());
	}

	protected void setTypeResolutionContext(IJvmTypeProvider context) {
		this.typeResolutionContext = context;
	}

	public IJvmTypeProvider getTypeResolutionContext() {
		return this.typeResolutionContext;
	}

	/** Replies the script's file extension.
	 */
	@Pure
	public String getScriptFileExtension() {
		return this.fileExtension;
	}

	/** Replies the builder of type references.
	 *
	 * @return the type reference builder.
	 */
	@Pure
	protected TypeReferences getTypeReferences() {
		return this.typeReferences;
	}

	/** Replies the primitive type tools.
	 *
	 * @return the primitive type tools.
	 */
	@Pure
	protected Primitives getPrimitiveTypes() {
		return this.primitives;
	}

	private JvmTypeReference innerFindType(Notifier context, String typeName) {
		final IJvmTypeProvider provider = getTypeResolutionContext();
		JvmType type = null;
		if (provider != null) {
			type = provider.findTypeByName(typeName);
		}
		TypeReferences typeRefs = getTypeReferences();
		if (type == null) {
			type = typeRefs.findDeclaredType(typeName, context);
		}
		if (type == null) {
			return null;
		}
		return typeRefs.createTypeRef(type);
	}

	protected JvmTypeReference findType(Notifier context, String typeName) {
		final JvmTypeReference type = innerFindType(context, typeName);
		if (!isTypeReference(type)) {
			XtextResource xtextResource = toResource(context);
			for (String packageName : getImportsConfiguration().getImplicitlyImportedPackages(xtextResource)) {
				JvmTypeReference typeReference = innerFindType(context, packageName + "." + typeName);
				if (isTypeReference(typeReference)) {
					return typeReference;
				}
			}
			throw new TypeNotPresentException(typeName, null);
		}
		return type;
	}

	protected static XtextResource toResource(Notifier context) {
		return (XtextResource) (context instanceof Resource ? context : ((EObject) context).eResource());
	}

	/** Replies the type reference for the given name in the given context.
	 */
	public JvmParameterizedTypeReference newTypeRef(String typeName) {
		return newTypeRef(eResource(), typeName);
	}

	/** Replies the type reference for the given name in the given context.
	 */
	public JvmParameterizedTypeReference newTypeRef(Notifier context, String typeName) {
		JvmTypeReference typeReference;
		try {
			typeReference = findType(context, typeName);
			getImportManager().addImportFor(typeReference.getType());
			return (JvmParameterizedTypeReference) typeReference;
		} catch (TypeNotPresentException exception) {
		}
		final JvmParameterizedTypeReference pref = ExpressionBuilderImpl.parseType(context, typeName, this);
		final JvmTypeReference baseType = findType(context, pref.getType().getIdentifier());
		final int len = pref.getArguments().size();
		final JvmTypeReference[] args = new JvmTypeReference[len];
		for (int i = 0; i < len; ++i) {
			final JvmTypeReference original = pref.getArguments().get(i);
			if (original instanceof JvmAnyTypeReference) {
				args[i] = EcoreUtil.copy(original);
			} else if (original instanceof JvmWildcardTypeReference $c$value) {
				final JvmWildcardTypeReference wc = EcoreUtil.copy($c$value);
				for (final JvmTypeConstraint c : wc.getConstraints()) {
					c.setTypeReference(newTypeRef(context, c.getTypeReference().getIdentifier()));
				}
				args[i] = wc;
			} else {
				args[i] = newTypeRef(context, original.getIdentifier());
			}
		}
		final TypeReferences typeRefs = getTypeReferences();
		return typeRefs.createTypeRef(baseType.getType(), args);
	}

	/** Replies if the first parameter is a subtype of the second parameter.
	 *
	 * @param context the context.
	 * @param subType the subtype to test.
	 * @param superType the expected super type.
	 * @return the type reference.
	 */
	@Pure
	protected boolean isSubTypeOf(EObject context, JvmTypeReference subType, JvmTypeReference superType) {
		if (isTypeReference(superType) && isTypeReference(subType)) {
			StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(services, context);
			LightweightTypeReferenceFactory factory = new LightweightTypeReferenceFactory(owner, false);
			LightweightTypeReference reference = factory.toLightweightReference(subType);
			return reference.isSubtypeOf(superType.getType());
		}
		return false;
	}

	/** Replies if the given object is a valid type reference.
	 */
	@Pure
	protected boolean isTypeReference(JvmTypeReference typeReference) {
		return (typeReference != null && !typeReference.eIsProxy()
			&& typeReference.getType() != null && !typeReference.getType().eIsProxy());
	}

	/** Replies the import's configuration.
	 *
	 * @return the import's configuration.
	 */
	@Pure
	protected IImportsConfiguration getImportsConfiguration() {
		return this.importsConfiguration;
	}

	/** Compute a unused URI for a synthetic resource.
	 * @param resourceSet the resource set in which the resource should be located.
	 * @return the uri.
	 */
	@Pure
	protected URI computeUnusedUri(ResourceSet resourceSet) {
		String name = "__synthetic";
		for (int i = 0; i < Integer.MAX_VALUE; ++i) {
			URI syntheticUri = URI.createURI(name + i + "." + getScriptFileExtension());
			if (resourceSet.getResource(syntheticUri, false) == null) {
				return syntheticUri;
			}
		}
		throw new IllegalStateException();
	}

	/** Replies the resource factory.
	 *
	 * @return the resource factory.
	 */
	@Pure
	protected IResourceFactory getResourceFactory() {
		return this.resourceFactory;
	}

	/** Replies if the type could contains functions with a body.
	 */
	@Pure
	protected boolean isActionBodyAllowed(XtendTypeDeclaration type) {
		return !(type instanceof SarlAnnotationType
			|| type instanceof SarlCapacity
			|| type instanceof SarlEvent
			|| type instanceof SarlInterface);
	}

	/** Replies the import manager that stores the imported types.
	 *
	 * @return the import manager.
	 */
	@Pure
	protected ImportManager getImportManager() {
		return this.importManager;
	}

	protected String getQualifiedName(EObject object) {
		return this.qualifiedNameProvider.getFullyQualifiedName(object).toString();
	}

	@Pure
	public abstract Resource eResource();

	public void dispose() {
		Resource resource = eResource();
		ResourceSet resourceSet = resource.getResourceSet();
		resourceSet.getResources().remove(resource);
	}

}


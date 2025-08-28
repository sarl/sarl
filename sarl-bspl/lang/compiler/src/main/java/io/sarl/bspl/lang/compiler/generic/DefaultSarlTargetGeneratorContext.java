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

package io.sarl.bspl.lang.compiler.generic;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.generator.IGeneratorContext;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.output.FakeTreeAppendable;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReferenceFactory;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import com.google.inject.Injector;

/** Context for the generation of SARL files.
 *
 * @param <NP> the type for the name provider.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class DefaultSarlTargetGeneratorContext<NP> implements ISarlTargetGeneratorContext<NP> {

	private final Injector injector;

	private final IGeneratorContext delegate;

	private final IFileSystemAccess fileSystemAccess;

	private final NP nameProvider;

	private final String packageName;

	private final String typeName;

	private final boolean packageVisiblity;

	private final boolean isPreStage;
	
	private final ResourceSet resourceSet;

	private final EObject source;

	private final IResourceFactory resourceFactory;

	private final CommonTypeComputationServices typeServices;

	/** Constructor.
	 *
	 * @param delegate the base context to delegate to when it is not related to the SARL target generation.
	 * @param injector the injector to be used.
	 * @param fileSystemAccess the tool for accessing the file system.
	 * @param source the EObject that is the source of the compilation for this context.
	 * @param resourceSet the resource set in which the generated resources are located.
	 * @param resourceFactory the factory of resources.
	 * @param typeServices the services for managing types.
	 */
	public DefaultSarlTargetGeneratorContext(IGeneratorContext delegate, Injector injector,
			IFileSystemAccess fileSystemAccess, EObject source, ResourceSet resourceSet,
			IResourceFactory resourceFactory, CommonTypeComputationServices typeServices) {
		this(delegate, injector, false, fileSystemAccess, source, resourceSet, resourceFactory, typeServices, null, null, null, false);
	}

	/** Constructor.
	 *
	 * @param delegate the base context to delegate to when it is not related to the SARL target generation.
	 * @param injector the injector to be used.
	 * @param preStage indicates if the context is for the pre-stage of the generation.
	 * @param fileSystemAccess the tool for accessing the file system.
	 * @param source the EObject that is the source of the compilation for this context.
	 * @param resourceSet the resource set in which the generated resources are located.
	 * @param resourceFactory the factory of resources.
	 * @param typeServices the services for managing types.
	 * @param nameProvider the provider of names.
	 * @param packageName the name of the package in which all the elements will be generated.
	 * @param typeName the name of the type for which all the elements will be generated.
	 * @param packageVisibility indicates if the protocol is marked with package visibility.
	 */
	protected DefaultSarlTargetGeneratorContext(IGeneratorContext delegate,
			Injector injector, boolean preStage, IFileSystemAccess fileSystemAccess, EObject source,
			ResourceSet resourceSet, IResourceFactory resourceFactory, CommonTypeComputationServices typeServices,
			NP nameProvider, String packageName, String typeName, boolean packageVisibility) {
		this.delegate = delegate;
		this.injector = injector;
		this.isPreStage = preStage;
		this.fileSystemAccess = fileSystemAccess;
		this.source = source;
		this.resourceSet = resourceSet;
		this.resourceFactory = resourceFactory;
		this.typeServices = typeServices;
		this.nameProvider = nameProvider;
		this.packageName = packageName;
		this.typeName = typeName;
		this.packageVisiblity = packageVisibility;
	}
	
	@Override
	public Injector getInjector() {
		return this.injector;
	}

	@Override
	public boolean isPreStage() {
		return this.isPreStage;
	}
	
	@Override
	public String getPackage() {
		return this.packageName;
	}

	@Override
	public CancelIndicator getCancelIndicator() {
		return this.delegate.getCancelIndicator();
	}

	@Override
	public ISarlTargetGeneratorContext<NP> forPreStage() {
		return new DefaultSarlTargetGeneratorContext<>(this.delegate, this.injector,
				true, this.fileSystemAccess, this.source, this.resourceSet,
				this.resourceFactory, this.typeServices, this.nameProvider, this.packageName,
				this.typeName, this.packageVisiblity);
	}

	@Override
	public ISarlTargetGeneratorContext<NP> withSource(EObject source) {
		return new DefaultSarlTargetGeneratorContext<>(this.delegate, this.injector,
				this.isPreStage, this.fileSystemAccess, source, this.resourceSet,
				this.resourceFactory, this.typeServices, this.nameProvider, this.packageName,
				this.typeName, this.packageVisiblity);
	}

	@Override
	public ISarlTargetGeneratorContext<NP> withNameProvider(NP nameProvider) {
		return new DefaultSarlTargetGeneratorContext<>(this.delegate, this.injector,
				this.isPreStage, this.fileSystemAccess, this.source, this.resourceSet,
				this.resourceFactory, this.typeServices, nameProvider, this.packageName,
				this.typeName, this.packageVisiblity);
	}

	@Override
	public ISarlTargetGeneratorContext<NP> withPackage(String packageName) {
		return new DefaultSarlTargetGeneratorContext<>(this.delegate, this.injector,
				this.isPreStage, this.fileSystemAccess, this.source, this.resourceSet,
				this.resourceFactory, this.typeServices, this.nameProvider, packageName,
				this.typeName, this.packageVisiblity);
	}
	
	@Override
	public ISarlTargetGeneratorContext<NP> withTypeName(String protocolName) {
		return new DefaultSarlTargetGeneratorContext<>(this.delegate, this.injector,
				this.isPreStage, this.fileSystemAccess, this.source, this.resourceSet,
				this.resourceFactory, this.typeServices, this.nameProvider, this.packageName,
				protocolName, this.packageVisiblity);
	}

	@Override
	public ISarlTargetGeneratorContext<NP> withPackageVisibility(boolean packageVisibility) {
		return new DefaultSarlTargetGeneratorContext<>(this.delegate, this.injector,
				this.isPreStage, this.fileSystemAccess, this.source, this.resourceSet,
				this.resourceFactory, this.typeServices, this.nameProvider, this.packageName, this.typeName, packageVisibility);
	}

	@Override
	public ImportManager newImportManager(String packageName, String basename) {
		final var type = this.typeServices.getTypesFactory().createJvmGenericType();
		type.setPackageName(packageName);
		type.setSimpleName(basename);
		return new ImportManager(true, type);
	}

	@Override
	public ITreeAppendable newAppendableContent(ImportManager importManager) {
		if (importManager == null) {
			return new FakeTreeAppendable();
		}
		return new FakeTreeAppendable(importManager);
	}

	@Override
	public NP getNameProvider() {
		return this.nameProvider;
	}

	@Override
	public String getEnclosingTypeName() {
		return this.typeName;
	}

	@Override
	public boolean isPackageVisibility() {
		return this.packageVisiblity;
	}

	@Override
	public String toRelativeFilename(String packageName, String typeName, String fileExtension) {
		final var filename = new StringBuilder();
		if (!Strings.isEmpty(packageName)) {
			filename.append(packageName.replaceAll(Pattern.quote("."), Matcher.quoteReplacement("/"))).append("/"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		filename.append(typeName);
		if (!Strings.isEmpty(fileExtension)) {
			filename.append(fileExtension);
		}
		return filename.toString();
	}
	
	@Override
	public void createFile(EObject source, String packageName, String typeName, String fileExtension, String content) throws IOException {
		final var filename = toRelativeFilename(packageName, typeName, fileExtension);
		final var resourceSet = getResourceSet();
		final var resource = createResource(packageName, typeName, fileExtension, resourceSet);
		try (var is = new StringInputStream(content)) {
			resource.load(is, resourceSet.getLoadOptions());
		}
		this.fileSystemAccess.generateFile(filename, content);
	}

	@Override
	public EObject getSource() {
		return this.source;
	}

	@Override
	public ResourceSet getResourceSet() {
		return this.resourceSet;
	}

	private static URI computeUnusedUri(String packageName, String typeName, String filenameExtension, ResourceSet resourceSet) {
		final String qualifiedName;
		if (!Strings.isEmpty(packageName)) {
			qualifiedName = packageName + "." + typeName; //$NON-NLS-1$
		} else {
			qualifiedName = typeName;
		}
		
		var uri = URI.createHierarchicalURI(qualifiedName.split(Pattern.quote(".")), null, null); //$NON-NLS-1$
		if (uri != null && resourceSet.getResource(uri, false) == null) {
			return uri;
		}

		for (var i = 0; i < Integer.MAX_VALUE; ++i) {
			final var uriName = new StringBuilder();
			uriName.append(packageName.replaceAll(Pattern.quote("."), Matcher.quoteReplacement("/"))); //$NON-NLS-1$ //$NON-NLS-2$
			uriName.append(i);
			if (!filenameExtension.startsWith(".")) { //$NON-NLS-1$
				uriName.append("."); //$NON-NLS-1$
			}
			uriName.append(filenameExtension);
			
			uri = URI.createURI(uriName.toString());
			if (resourceSet.getResource(uri, false) == null) {
				return uri;
			}
		}

		throw new IllegalStateException();
	}

	@Override
	public Resource createResource(String packageName, String typeName, String filenameExtension, ResourceSet  resourceSet) {
		URI uri = computeUnusedUri(packageName, typeName, filenameExtension, resourceSet);
		Resource resource = this.resourceFactory.createResource(uri);
		resourceSet.getResources().add(resource);
		return resource;
	}

	private static LightweightTypeReference toLightweightTypeReference(
			JvmType type, EObject context, CommonTypeComputationServices services) {
		if (type == null) {
			return null;
		}
		final var owner = new StandardTypeReferenceOwner(services, context);
		final var factory = new LightweightTypeReferenceFactory(owner, false);
		final var reference = factory.toLightweightReference(type);
		return reference;
	}

	private static LightweightTypeReference toLightweightTypeReference(
			JvmTypeReference type, EObject context, CommonTypeComputationServices services) {
		if (type == null) {
			return null;
		}
		final var owner = new StandardTypeReferenceOwner(services, context);
		final var factory = new LightweightTypeReferenceFactory(owner, false);
		final var reference = factory.toLightweightReference(type);
		return reference;
	}

	@Override
	public LightweightTypeReference findType(String typeName, EObject context) {
		final var declaredType = this.typeServices.getTypeReferences().findDeclaredType(typeName, context);
		if (declaredType != null) {
			return toLightweightTypeReference(declaredType, context, this.typeServices);
		}

		final var declaredType0 = this.typeServices.getTypesFactory().createJvmGenericType();
		final var idx = typeName.lastIndexOf("."); //$NON-NLS-1$
		if (idx >= 0) {
			declaredType0.setPackageName(typeName.substring(0, idx));
			declaredType0.setSimpleName(typeName.substring(idx + 1));
		} else {
			declaredType0.setSimpleName(typeName);
		}
		return toLightweightTypeReference(declaredType0, context, this.typeServices);
	}

	@Override
	public LightweightTypeReference toTypeReference(JvmType type, EObject context) {
		return toLightweightTypeReference(type, context, this.typeServices);
	}

	@Override
	public LightweightTypeReference toTypeReference(JvmTypeReference type, EObject context) {
		return toLightweightTypeReference(type, context, this.typeServices);
	}

}
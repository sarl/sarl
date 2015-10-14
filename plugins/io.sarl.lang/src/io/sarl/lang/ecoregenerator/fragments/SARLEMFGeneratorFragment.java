/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.ecoregenerator.fragments;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.emf.codegen.ecore.genmodel.GenClass;

import org.eclipse.emf.codegen.ecore.genmodel.GenJDKLevel;
import org.eclipse.emf.codegen.ecore.genmodel.GenModel;
import org.eclipse.emf.codegen.ecore.genmodel.GenPackage;
import org.eclipse.emf.codegen.ecore.genmodel.GenRuntimeVersion;
import org.eclipse.emf.codegen.ecore.genmodel.impl.GenModelImpl;
import org.eclipse.emf.codegen.ecore.genmodel.impl.GenPackageImpl;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.common.util.WrappedException;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.resource.ContentHandler;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xpand2.XpandExecutionContext;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.generator.IGeneratorFragment;
import org.eclipse.xtext.generator.ecore.EMFGeneratorFragment;

/**
 * A {@link IGeneratorFragment} that saves the generated Ecore models and creates appropriate EMF generators with
 * a configuration suitable for SARL. Then it
 * runs the EMF generator to create the EMF classes for the generated Ecore models.
 *
 * <p>This implementation forces the EMF factory to create instances of the custom EMF elements that
 * are defined in the package <code>io.sarl.lang.sarl.impl</code>.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLEMFGeneratorFragment extends EMFGeneratorFragment {

	private GenRuntimeVersion emfRuntimeVerison;

	private final Set<String> customImplClasses = new TreeSet<>();

	private String customClassNamePattern;

	/** Set the pattern that is used to build the classname of a custom implementation.
	 *
	 * @param pattern the pattern.
	 */
	public void setCustomClassNamePattern(String pattern) {
		this.customClassNamePattern = pattern;
	}

	/** Replies the pattern that is used to build the classname of a custom implementation.
	 *
	 * @return the pattern.
	 */
	public String getCustomClassNamePattern() {
		return this.customClassNamePattern;
	}

	/** Add a qualified name in the list of classes for which the custom
	 * implementation must be used.
	 *
	 * @param qualifiedName - the qualified name to add.
	 */
	public void addCustomImplClass(String qualifiedName) {
		if (qualifiedName != null && qualifiedName.length() > 0) {
			this.customImplClasses.add(qualifiedName);
		}
	}

	/** Remove a qualified name from the list of classes for which the custom
	 * implementation must be used.
	 *
	 * @param qualifiedName - the qualified name to remove.
	 */
	public void removeCustomImplClass(String qualifiedName) {
		if (qualifiedName != null && qualifiedName.length() > 0) {
			this.customImplClasses.remove(qualifiedName);
		}
	}

	/** Replies the qualified names of the list of classes for which the custom
	 * implementation must be used.
	 *
	 * @return the qualified names.
	 */
	public Set<String> getCustomImplClasses() {
		return Collections.unmodifiableSet(this.customImplClasses);
	}

	/** FIXME: Move this function into Xtext.
	 */
	@Override
	protected GenModel getGenModel(ResourceSet rs, Grammar grammar, XpandExecutionContext ctx, List<EPackage> packs) {
		URI genModelUri = getGenModelUri(grammar, ctx);
		genModelUri = toPlatformResourceURI(genModelUri);
		Resource resource = rs.getResource(genModelUri, false);
		if (resource != null) {
			resource.unload();
			rs.getResources().remove(resource);
		}
		Resource genModelFile = rs.createResource(genModelUri, ContentHandler.UNSPECIFIED_CONTENT_TYPE);
		GenModel genModel;
		if (rs.getURIConverter().exists(genModelUri, null)) {
			try {
				genModelFile.load(null);
			} catch (IOException e) {
				throw new WrappedException(e);
			}
			if (genModelUri.hasFragment()) {
				genModel = (GenModel) genModelFile.getEObject(genModelUri.fragment());
			} else {
				genModel = (GenModel) genModelFile.getContents().get(0);
			}
		} else {
			genModel = createGenModelInstance();
			genModel.setModelDirectory(toGenModelProjectPath(getJavaModelDirectory(ctx)));
			genModel.setModelName(getModelName(grammar));
			genModel.setModelPluginID(getModelPluginID(ctx));
			genModel.setEditDirectory(toGenModelProjectPath(getEditDirectory(ctx)));
			genModel.setEditorDirectory(toGenModelProjectPath(getEditorDirectory(ctx)));
			genModel.setEditPluginID(getEditPluginID(ctx));
			genModel.setEditorPluginID(getEditorPluginID(ctx));
			genModel.setValidateModel(false);
			genModel.setForceOverwrite(true);
			genModel.setCanGenerate(true);
			genModel.setFacadeHelperClass(null);
			genModel.setBundleManifest(true);
			genModel.setUpdateClasspath(false);
			genModel.setComplianceLevel(GenJDKLevel.getByName(getJdkLevel()));
			genModel.setRuntimeVersion(this.emfRuntimeVerison);
			genModel.setRootExtendsClass("org.eclipse.emf.ecore.impl.MinimalEObjectImpl$Container"); //$NON-NLS-1$
			genModel.setLineDelimiter(getLineDelimiter());
		}
		genModelFile.getContents().add(genModel);
		return genModel;
	}

	/** Create an instance of GenModel usable by this EMF fragment.
	 *
	 * <p>FIXME: Remove when the function {@link #getGenModel(ResourceSet, Grammar, XpandExecutionContext, List)}
	 * is moved to Xtext.
	 *
	 * @return the new instance.
	 */
	protected GenModel createGenModelInstance() {
		return new GenModelImpl() {
			// The name of the additional ecore file used for load initialization is hard-coded in GenPackageImpl.
			// We want to override it to avoid validation of that file, which uses NS URIs for references.
			@Override
			public GenPackage createGenPackage() {
				return new GenPackageImpl() {
					@Override
					public String getSerializedPackageFilename() {
						return getName() + ".loadinitialization_ecore"; //$NON-NLS-1$
					}
				};
			}

			@Override
			public String getImportedName(String qualifiedName) {
				String qn = qualifiedName;
				if (getCustomImplClasses().contains(qualifiedName)) {
					String pattern = getCustomClassNamePattern();
					if (pattern != null && pattern.length() > 0) {
						qn = MessageFormat.format(pattern, qualifiedName);
					} else {
						qn = qualifiedName + "Custom"; //$NON-NLS-1$
					}
				}
				return super.getImportedName(qn);
			}
			
			@Override
			public GenClass createGenClass() {
				GenClass clazz = super.createGenClass();
				return clazz;
			}

		};
	}
	
	/**
	 * {@inheritDoc}
	 *
	 * <p>FIXME: Remove when the function {@link #getGenModel(ResourceSet, Grammar, XpandExecutionContext, List)}
	 * is moved to Xtext.
	 */
	@Override
	public void setEmfRuntimeVersion(String emfRuntimeVersion) {
		super.setEmfRuntimeVersion(emfRuntimeVersion);
		this.emfRuntimeVerison = GenRuntimeVersion.get(emfRuntimeVersion);
	}

	/**
	 * FIXME: Remove when the function {@link #getGenModel(ResourceSet, Grammar, XpandExecutionContext, List)}
	 * is moved to Xtext.
	 */
	private String getLineDelimiter() {
		return getNaming().getLineDelimiter();
	}

}


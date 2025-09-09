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

package io.sarl.lang.ui.refactoring.rename;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import com.google.common.collect.Iterables;
import com.google.common.collect.Iterators;
import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.refactoring.IChangeRedirector;
import org.eclipse.xtext.ui.refactoring.impl.AbstractProcessorBasedRenameParticipant;
import org.eclipse.xtext.ui.refactoring.impl.RefactoringResourceSetProvider;
import org.eclipse.xtext.ui.refactoring.ui.IRenameContextFactory;
import org.eclipse.xtext.ui.refactoring.ui.IRenameElementContext;

import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.sarl.SarlScript;

/** Strategy for renaming the package in a SARL script when the initial renaming is on a JDT package.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.6
 */
@SuppressWarnings("restriction")
public class SARLJdtPackageRenameParticipant extends AbstractProcessorBasedRenameParticipant {

	@Inject
	private RefactoringResourceSetProvider resourceSetProvider;

	@Inject
	private IQualifiedNameConverter nameConverter;

	@Inject
	private IRenameContextFactory renameContextFactory;

	private final String[] fileExtensions;

	/** Construct the participant.
	 *
	 * @param fileExtension the file extension.
	 */
	@Inject
	public SARLJdtPackageRenameParticipant(@Named(Constants.FILE_EXTENSIONS) String fileExtension) {
		this.fileExtensions = SarlUtils.getFileExtensions(fileExtension);
	}

	@Override
	protected List<EObject> getRenamedElementsOrProxies(EObject element) {
		// Obsolete
		return null;
	}

	private Iterator<IFile> getSarlFiles(IPackageFragment packageFragment) {
		List<Object> resourceObjects;
		try {
			resourceObjects = Arrays.asList(packageFragment.getNonJavaResources());
		} catch (JavaModelException exception) {
			resourceObjects = Collections.emptyList();
		}
		return Iterators.filter(Iterators.filter(resourceObjects.iterator(), IFile.class),
			it -> SarlUtils.hasFileExtension(it.getName(), this.fileExtensions));
	}

	@Override
	protected List<? extends IRenameElementContext> createRenameElementContexts(Object element) {
		if (getArguments().getUpdateReferences()) {
			assert element instanceof IPackageFragment;
			final var packageFragment = (IPackageFragment) element;
			final var contexts = new ArrayList<IRenameElementContext>();

			final var currentQualifiedName = this.nameConverter.toQualifiedName(packageFragment.getElementName());
			final var separator = Character.toString(IPath.SEPARATOR);
			final var newQualifiedName = this.nameConverter.toQualifiedName(getNewName()).toString(separator);

			final var sarlFiles = getSarlFiles(packageFragment);
			while (sarlFiles.hasNext()) {
				final var currentFile = sarlFiles.next();
				final var filePath = currentFile.getFullPath();
				final var resourceURI = URI.createPlatformResourceURI(filePath.toString(), true);
				final var resourceSet = this.resourceSetProvider.get(currentFile.getProject());
				final var resource = resourceSet.getResource(resourceURI, true);
				if (resource != null) {
					final var sarlScript = (SarlScript) Iterables.find(resource.getContents(), it -> it instanceof SarlScript);
					if (sarlScript != null && this.nameConverter.toQualifiedName(sarlScript.getPackage()).startsWith(currentQualifiedName)) {
						final var xtextResource = (XtextResource) resource;
						final var context = this.renameContextFactory.createRenameElementContext(
								sarlScript,
								null, null,
								xtextResource);
						final var newPath = Path.fromPortableString(filePath.toPortableString().replaceAll(
								currentQualifiedName.toString(separator), newQualifiedName));
						if (context instanceof IChangeRedirector.Aware cvalue) {
							cvalue.setChangeRedirector(source ->
								Objects.equals(source, filePath) ? newPath : source);
						}
						contexts.add(context);
					}
				}
			}
			if (!contexts.isEmpty()) {
				return contexts;
			}
		}
		return null;
	}

}

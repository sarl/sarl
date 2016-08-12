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

package io.sarl.lang.ui.refactoring.rename;

import static java.util.Collections.singletonList;
import static org.eclipse.xtext.EcoreUtil2.getPlatformResourceOrNormalizedURI;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.inject.Named;

import com.google.inject.Inject;
import org.apache.log4j.Logger;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.resource.ILocationInFileProvider;
import org.eclipse.xtext.ui.refactoring.IChangeRedirector;
import org.eclipse.xtext.ui.refactoring.IRefactoringUpdateAcceptor;
import org.eclipse.xtext.ui.refactoring.impl.AbstractProcessorBasedRenameParticipant;
import org.eclipse.xtext.ui.refactoring.impl.DefaultRenameStrategyProvider;
import org.eclipse.xtext.ui.refactoring.impl.RefactoringException;
import org.eclipse.xtext.ui.refactoring.ui.IRenameElementContext;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.util.ITextRegion;

import io.sarl.lang.sarl.SarlScript;

/** Participant to the package renaming mechanism.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLPackageRenameParticipant extends AbstractProcessorBasedRenameParticipant {

	private static final Logger LOG = Logger.getLogger(SARLPackageRenameParticipant.class);

	private static final String PACKAGE_SEPARATOR_PATTERN = "\\."; //$NON-NLS-1$

	@Inject
	private IResourceSetProvider resourceSetProvider;

	private final String fileExtension;

	/** Construct the participant.
	 *
	 * @param fileExtension the file extension.
	 */
	@Inject
	public SARLPackageRenameParticipant(@Named(Constants.FILE_EXTENSIONS) String fileExtension) {
		this.fileExtension = fileExtension;
	}

	@Override
	protected List<? extends IRenameElementContext> createRenameElementContexts(Object element) {
		assert element instanceof IPackageFragment;
		final IPackageFragment packageFragment = (IPackageFragment) element;
		final List<IRenameElementContext> contexts = new ArrayList<>();
		try {
			final ResourceSet resourceSet = this.resourceSetProvider.get(packageFragment.getJavaProject().getProject());
			final String oldPackageName = packageFragment.getElementName();
			final String[] oldPackageNameElements = oldPackageName.split(PACKAGE_SEPARATOR_PATTERN);
			final String newPackageName = getNewName();
			final String[] newPackageNameElements = newPackageName.split(PACKAGE_SEPARATOR_PATTERN);
			for (final Object resourceObject : packageFragment.getNonJavaResources()) {
				if (resourceObject instanceof IFile) {
					final IFile file = (IFile) resourceObject;
					if (file.getName().endsWith(this.fileExtension)) {
						final IPath filePath = file.getFullPath();
						final URI resourceURI = URI.createPlatformResourceURI(filePath.toString(), true);
						final Resource resource = resourceSet.getResource(resourceURI, true);
						createPackageRenameContext(oldPackageName, newPackageName, filePath,
								resource, contexts, oldPackageNameElements.length, newPackageNameElements);
					}
				}
			}
		} catch (JavaModelException exception) {
			getStatus().add(RefactoringStatus.ERROR, exception.getLocalizedMessage(), exception, LOG);
		}
		if (contexts.isEmpty()) {
			return super.createRenameElementContexts(packageFragment);
		}
		return contexts;
	}

	private static IPath buildNewPath(IPath path, int oldPackageSize, String[] newPackage) {
		final String filename = path.lastSegment();
		IPath npath = path.removeLastSegments(1 + oldPackageSize);
		for (final String element : newPackage) {
			npath = npath.append(element);
		}
		return npath.append(filename);
	}

	private static void createPackageRenameContext(String oldPackageName, String newPackageName,
			IPath oldPath,
			Resource resource, List<IRenameElementContext> contexts,
			int oldPackageSize, String[] newPackage) {
		if (!resource.getContents().isEmpty() && resource.getContents().get(0) instanceof SarlScript) {
			final SarlScript script = (SarlScript) resource.getContents().get(0);
			final String currentPackage = script.getPackage();
			if (Objects.equals(currentPackage, oldPackageName)) {
				final IPath newPath = buildNewPath(oldPath, oldPackageSize, newPackage);
				final IRenameElementContext renameElementContext =
						new Context(script, oldPackageName, newPackageName, resource);
				if (renameElementContext instanceof IChangeRedirector.Aware) {
					((IChangeRedirector.Aware) renameElementContext).setChangeRedirector(
							(source) -> Objects.equals(source, oldPath) ? newPath : source);
				}
				contexts.add(renameElementContext);
			}
		}
	}

	@Override
	protected List<EObject> getRenamedElementsOrProxies(EObject originalTarget) {
		return singletonList(originalTarget);
	}

	/** Define the context for a package renaming.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Context extends IRenameElementContext.Impl {

		private final String currentPackageName;

		private final String newPackageName;

		/** Construct the context.
		 *
		 * @param targetElement the element associated to the package to rename.
		 * @param currentPackageName the name of the package before changing.
		 * @param newPackageName the new name of the package.
		 * @param resource the resource to change.
		 */
		@Inject
		public Context(SarlScript targetElement, String currentPackageName, String newPackageName,
				Resource resource) {
			super(getPlatformResourceOrNormalizedURI(targetElement), targetElement.eClass(),
					null, null, getPlatformResourceOrNormalizedURI(resource));
			this.currentPackageName = currentPackageName;
			this.newPackageName = newPackageName;
		}

		/** Replies the current package name.
		 *
		 * @return the current name.
		 */
		public String getCurrentPackageName() {
			return this.currentPackageName;
		}

		/** Replies the new package name.
		 *
		 * @return the new name.
		 */
		public String getNewPackageName() {
			return this.newPackageName;
		}

	}

	/** Strategy for renaming the package in a SARL script.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Strategy implements DefaultRenameStrategyProvider.IInitializable {

		@Inject
		private ILocationInFileProvider locationInFileProvider;

		private Context context;

		@Override
		public boolean initialize(EObject targetEObject, IRenameElementContext renameElementContext) {
			if (renameElementContext instanceof Context
				&& targetEObject instanceof SarlScript) {
				this.context = (Context) renameElementContext;
				return true;
			}
			return false;
		}

		@Override
		public String getOriginalName() {
			return this.context.getCurrentPackageName();
		}

		@Override
		public RefactoringStatus validateNewName(String newName) {
			return new RefactoringStatus();
		}

		/** Change the package name.
		 *
		 * @param newName the new name.
		 * @param resourceSet the set of resource to use.
		 */
		protected void setPackageName(String newName, ResourceSet resourceSet) {
			final EObject object = resourceSet.getEObject(this.context.getTargetElementURI(), true);
			if (object instanceof SarlScript) {
				((SarlScript) object).setPackage(newName);
			} else {
				throw new RefactoringException("SARL script not loaded."); //$NON-NLS-1$
			}
		}

		@Override
		public void createDeclarationUpdates(String newName, ResourceSet resourceSet,
				IRefactoringUpdateAcceptor updateAcceptor) {
			updateAcceptor.accept(this.context.getTargetElementURI(),
					getDeclarationTextEdit(newName, resourceSet));
		}

		@Override
		public void applyDeclarationChange(String newName, ResourceSet resourceSet) {
			setPackageName(newName, resourceSet);
		}

		@Override
		public void revertDeclarationChange(ResourceSet resourceSet) {
			setPackageName(this.context.getCurrentPackageName(), resourceSet);
		}

		/** Replies the text update for the rename.
		 *
		 * @param newName the new package name.
		 * @param resourceSet the set of resources.
		 * @return the text update.
		 */
		protected TextEdit getDeclarationTextEdit(String newName, ResourceSet resourceSet) {
			final EObject object = resourceSet.getEObject(this.context.getTargetElementURI(), true);
			if (object instanceof SarlScript) {
				final ITextRegion region = getOriginalPackageRegion((SarlScript) object);
				if (region != null) {
					return new ReplaceEdit(region.getOffset(), region.getLength(), newName);
				}
			}
			throw new RefactoringException("SARL script not loaded."); //$NON-NLS-1$
		}

		/** Replies the text region that is corresponding to the package name.
		 *
		 * @param script the script.
		 * @return the region.
		 */
		protected ITextRegion getOriginalPackageRegion(final SarlScript script) {
			return this.locationInFileProvider.getFullTextRegion(script,
					XtendPackage.Literals.XTEND_FILE__PACKAGE, 0);
		}

	}

}

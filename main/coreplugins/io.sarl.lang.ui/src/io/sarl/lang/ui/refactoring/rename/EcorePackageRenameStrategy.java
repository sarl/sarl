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

package io.sarl.lang.ui.refactoring.rename;

import java.text.MessageFormat;
import java.util.regex.Pattern;

import com.google.inject.Inject;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.resource.ILocationInFileProvider;
import org.eclipse.xtext.ui.refactoring.IRefactoringUpdateAcceptor;
import org.eclipse.xtext.ui.refactoring.impl.DefaultRenameStrategyProvider;
import org.eclipse.xtext.ui.refactoring.impl.RefactoringException;
import org.eclipse.xtext.ui.refactoring.ui.IRenameElementContext;
import org.eclipse.xtext.util.ITextRegion;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Functions.Function1;

import io.sarl.lang.sarl.SarlScript;

/** Strategy for renaming the package in a SARL script.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class EcorePackageRenameStrategy implements DefaultRenameStrategyProvider.IInitializable {

	/** Pattern for a package name.
	 */
	static final Pattern PACKAGE_NAME_PATTERN = Pattern.compile("^\\^?[a-zA-Z$_][a-zA-Z$_0-9]*(\\.\\^?[a-zA-Z$_][a-zA-Z$_0-9]*)*$"); //$NON-NLS-1$

	@Inject
	private ILocationInFileProvider locationInFileProvider;

	private String currentPackageName;

	private Function1<? super ResourceSet, ? extends URI> uriProvider;

	/**
	 * Validate the package name.
	 *
	 * @param newName the package name to be validated.
	 * @return the status of the validation.
	 */
	public static RefactoringStatus validatePackageName(String newName) {
		if (!PACKAGE_NAME_PATTERN.matcher(newName).find()) {
			RefactoringStatus.createErrorStatus(MessageFormat.format(Messages.SARLJdtPackageRenameParticipant_0, newName));
		}
		return new RefactoringStatus();
	}

	@Override
	public boolean initialize(EObject targetEObject, IRenameElementContext renameElementContext) {
		if (targetEObject instanceof SarlScript) {
			final SarlScript script = (SarlScript) targetEObject;
			this.currentPackageName = Strings.emptyIfNull(script.getPackage());
			this.uriProvider = it -> renameElementContext.getTargetElementURI();
			return true;
		}
		return false;
	}

	@Override
	public String getOriginalName() {
		return this.currentPackageName;
	}

	@Override
	public RefactoringStatus validateNewName(String newName) {
		return validatePackageName(newName);
	}

	/** Change the package name.
	 *
	 * @param newName the new name.
	 * @param resourceSet the set of resource to use.
	 */
	protected void setPackageName(String newName, ResourceSet resourceSet) {
		final EObject object = resourceSet.getEObject(this.uriProvider.apply(resourceSet), true);
		if (object instanceof SarlScript) {
			((SarlScript) object).setPackage(newName);
		} else {
			throw new RefactoringException("SARL script not loaded."); //$NON-NLS-1$
		}
	}

	@Override
	public void createDeclarationUpdates(String newName, ResourceSet resourceSet,
			IRefactoringUpdateAcceptor updateAcceptor) {
		updateAcceptor.accept(this.uriProvider.apply(resourceSet),
				getDeclarationTextEdit(newName, resourceSet));
	}

	@Override
	public void applyDeclarationChange(String newName, ResourceSet resourceSet) {
		setPackageName(newName, resourceSet);
	}

	@Override
	public void revertDeclarationChange(ResourceSet resourceSet) {
		setPackageName(this.currentPackageName, resourceSet);
	}

	/** Replies the text update for the rename.
	 *
	 * @param newName the new package name.
	 * @param resourceSet the set of resources.
	 * @return the text update.
	 */
	protected TextEdit getDeclarationTextEdit(String newName, ResourceSet resourceSet) {
		final EObject object = resourceSet.getEObject(this.uriProvider.apply(resourceSet), true);
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

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

package io.sarl.eclipse.launching.shortcuts;

import javax.inject.Inject;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.xtext.resource.FileExtensionProvider;

import io.sarl.eclipse.SARLEclipseConfig;

/** Test UI properties for launch shortcuts.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class PropertyTester extends org.eclipse.core.expressions.PropertyTester {

	/** Name of the property that indicates an agent is detected.
	 */
	public static final String HAS_AGENT_PROPERTY = "hasAgent"; //$NON-NLS-1$

	/** Name of the property that indicates a main java function is detected.
	 */
	public static final String HAS_MAIN_PROPERTY = "hasMain"; //$NON-NLS-1$

	private FileExtensionProvider fileExtensionProvider;

	/** Change the file extension of the SARL script.
	 *
	 * @param fileExtensionProvider the file extension provider.
	 */
	@Inject
	public void setSarlFileExtension(FileExtensionProvider fileExtensionProvider) {
		this.fileExtensionProvider = fileExtensionProvider;
	}

	@Override
	public boolean test(Object receiver, String property, Object[] args, Object expectedValue) {
		if (HAS_AGENT_PROPERTY.equals(property)) {
			return testHasAgent(receiver);
		}
		if (HAS_MAIN_PROPERTY.equals(property)) {
			return testHasMain(receiver);
		}
		return false;
	}

	/** Returns if the given receiver has an agent definition.
	 *
	 * @param receiver the receiver.
	 * @return {@code true} if the receiver has an agent definition.
	 */
	protected boolean testHasAgent(Object receiver) {
		return receiver instanceof IPackageFragment
				|| receiver instanceof IPackageFragmentRoot
				|| isInSarlEditor(receiver)
				|| isSarlScript(receiver)
				|| hasSarlNatureOnProject(receiver);
	}

	/** Returns if the given receiver contains a main function.
	 *
	 * @param receiver the receiver.
	 * @return {@code true} if the receiver contains a main function.
	 */
	protected boolean testHasMain(Object receiver) {
		return isSarlScript(receiver)
				|| isInSarlEditor(receiver);
	}

	/** Returns if the given receiver is a SARL editor.
	 *
	 * @param receiver the receiver.
	 * @return {@code true} if the receiver is a SARL editor.
	 */
	protected boolean isInSarlEditor(Object receiver) {
		if (receiver instanceof IFileEditorInput) {
			final IFileEditorInput input = (IFileEditorInput) receiver;
			return this.fileExtensionProvider.isValid(input.getFile().getFileExtension());
		}
		return false;
	}

	/** Returns if the given receiver is a SARL script.
	 *
	 * @param receiver the receiver.
	 * @return {@code true} if the receiver is a SARL script.
	 */
	protected boolean isSarlScript(Object receiver) {
		if (receiver instanceof IFile) {
			final IFile file = (IFile) receiver;
			return this.fileExtensionProvider.isValid(file.getFileExtension());
		}
		return false;
	}

	/** Returns if the given receiver is a project with the SARL nature.
	 *
	 * @param receiver the receiver.
	 * @return {@code true} if the receiver has a SARL nature.
	 */
	@SuppressWarnings("static-method")
	protected boolean hasSarlNatureOnProject(Object receiver) {
		if (receiver instanceof IJavaProject) {
			final IJavaProject project = (IJavaProject) receiver;
			try {
				return project.getProject().hasNature(SARLEclipseConfig.NATURE_ID);
			} catch (CoreException exception) {
				//
			}
		} else if (receiver instanceof IProject) {
			final IProject project = (IProject) receiver;
			try {
				return project.hasNature(SARLEclipseConfig.NATURE_ID);
			} catch (CoreException exception) {
				//
			}
		}
		return false;
	}

}

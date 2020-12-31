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

package io.sarl.eclipse.wizards.elements;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.views.contentoutline.ContentOutline;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;

/** Utilities for field initialization in the wizards.
 *
 * <p>This class is copied from the Xtend library.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class FieldInitializerUtil {

	/** Replies the Java element that corresponds to the given selection.
	 *
	 * @param selection the current selection.
	 * @return the Java element.
	 */
	@SuppressWarnings("static-method")
	public IJavaElement getSelectedResource(IStructuredSelection selection) {
		IJavaElement elem = null;
		if (selection != null && !selection.isEmpty()) {
			final Object object = selection.getFirstElement();
			if (object instanceof IAdaptable) {
				final IAdaptable adaptable = (IAdaptable) object;
				elem = adaptable.getAdapter(IJavaElement.class);
				if (elem == null) {
					elem = getPackage(adaptable);
				}
			}
		}
		if (elem == null) {
			final IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			IWorkbenchPart part = activePage.getActivePart();
			if (part instanceof ContentOutline) {
				part = activePage.getActiveEditor();
			}
			if (part instanceof XtextEditor) {
				final IXtextDocument doc = ((XtextEditor) part).getDocument();
				final IFile file = doc.getAdapter(IFile.class);
				elem = getPackage(file);
			}
		}
		if (elem == null || elem.getElementType() == IJavaElement.JAVA_MODEL) {
			try {
				final IJavaProject[] projects = JavaCore.create(ResourcesPlugin.getWorkspace().getRoot()).getJavaProjects();
				if (projects.length == 1) {
					elem = projects[0];
				}
			} catch (JavaModelException e) {
				throw new RuntimeException(e.getMessage());
			}
		}
		return elem;
	}

	private static IJavaElement getPackage(IAdaptable adaptable) {
		IJavaElement elem = null;
		IResource resource = adaptable.getAdapter(IResource.class);
		if (resource != null && resource.getType() != IResource.ROOT) {
			while (elem == null && resource.getType() != IResource.PROJECT) {
				resource = resource.getParent();
				elem = resource.getAdapter(IJavaElement.class);
			}
		}
		if (elem == null) {
			elem = JavaCore.create(resource);
		}
		return elem;
	}

}

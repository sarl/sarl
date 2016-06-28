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

package io.sarl.eclipse.navigator;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.navigator.node.SARLProjectParent;

/**
 * SARL custom project navigator content provider.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ContentProvider implements ITreeContentProvider {

	private static final Object[] NO_CHILDREN = new Object[0];

	private ISARLProjectElement[] sarlProjectParents;

	@Override
	public Object[] getChildren(Object parentElement) {
		Object[] children = null;
		if (SARLProjectWorkbenchRoot.class.isInstance(parentElement)) {
			if (this.sarlProjectParents == null) {
				this.sarlProjectParents = initializeParent();
			}
			children = this.sarlProjectParents;
		} else if (ISARLProjectElement.class.isInstance(parentElement)) {
			children = ((ISARLProjectElement) parentElement).getChildren();
		} else {
			children = NO_CHILDREN;
		}

		return children;
	}

	@Override
	public Object getParent(Object element) {
		SARLEclipsePlugin.getDefault().logDebugMessage(
				"ContentProvider.getParent: " //$NON-NLS-1$
				+ element.getClass().getName());
		Object parent = null;
		if (ISARLProjectElement.class.isInstance(element)) {
			parent = ((ISARLProjectElement) element).getParent();
		}
		return parent;
	}

	@Override
	public boolean hasChildren(Object element) {
		boolean hasChildren = false;

		if (SARLProjectWorkbenchRoot.class.isInstance(element)) {
			hasChildren = this.sarlProjectParents.length > 0;
		} else if (ISARLProjectElement.class.isInstance(element)) {
			hasChildren = ((ISARLProjectElement) element).hasChildren();
		}
		// else it is not one of these so return false
		return hasChildren;
	}

	@Override
	public Object[] getElements(Object inputElement) {
		// This is the same as getChildren() so we will call that instead
		return getChildren(inputElement);
	}

	@Override
	public void dispose() {
		SARLEclipsePlugin.getDefault().logDebugMessage("ContentProvider.dispose"); //$NON-NLS-1$
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		if (oldInput != null && newInput != null) {
			SARLEclipsePlugin.getDefault().logDebugMessage(
					"ContentProvider.inputChanged: old: " //$NON-NLS-1$
					+ oldInput.getClass().getName()
					+ " new: " //$NON-NLS-1$
					+ newInput.getClass().getName());
		} else {
			SARLEclipsePlugin.getDefault().logDebugMessage(
					"ContentProvider.inputChanged"); //$NON-NLS-1$
		}
	}

	private static ISARLProjectElement[] initializeParent() {
		IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();

		List<SARLProjectParent> list = new ArrayList<>();
		for (int i = 0; i < projects.length; i++) {
			try {
				if (projects[i].getNature(SARLEclipseConfig.NATURE_ID) != null) {
					list.add(new SARLProjectParent(projects[i]));
				}
			} catch (CoreException e) {
				// Go to the next IProject
			}
		}

		SARLProjectParent[] result = new SARLProjectParent[list.size()];
		list.toArray(result);

		return result;
	}

}

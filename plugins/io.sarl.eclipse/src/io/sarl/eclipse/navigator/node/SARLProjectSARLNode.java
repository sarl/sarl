/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.navigator.node;

import io.sarl.eclipse.navigator.ISARLProjectElement;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.graphics.Image;

/**
 * The node of SARL Project called SARL just below the root node.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectSARLNode implements ISARLProjectElement {

	private static final String NAME = "SARL"; //$NON-NLS-1$

	private ISARLProjectElement parent;
	private ISARLProjectElement[] children;

	/** Construct a SARL node.
	 *
	 * @param iparent - the parent node.
	 */
	public SARLProjectSARLNode(ISARLProjectElement iparent) {
		this.parent = iparent;
	}

    @Override
	public Image getImage() {
		return this.parent.getImage();
	}

    @Override
	public Object[] getChildren() {
		if (this.children == null) {
			this.children = initializeChildren(/*getProject()*/);
		}
		// else the children are just fine

		return this.children;
	}

    @Override
	public String getText() {
		return NAME;
	}

    @Override
	public boolean hasChildren() {
		if (this.children == null) {
			this.children = initializeChildren(/*getProject()*/);
		}
		// else we have already initialized them

		return this.children.length > 0;
	}

    private static ISARLProjectElement[] initializeChildren() {
		ISARLProjectElement[] ichildren = new ISARLProjectElement[0];
		return ichildren;
    }

    @Override
	public IProject getProject() {
		return this.parent.getProject();
	}

    @Override
	public Object getParent() {
		return this.parent;
	}

}

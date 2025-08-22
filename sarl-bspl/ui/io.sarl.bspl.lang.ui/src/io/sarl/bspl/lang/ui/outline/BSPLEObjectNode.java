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

package io.sarl.bspl.lang.ui.outline;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.ui.editor.outline.IOutlineNode;
import org.eclipse.xtext.ui.editor.outline.impl.EObjectNode;

/**
 * Customize the outline page.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
class BSPLEObjectNode extends EObjectNode {

	private boolean isKey;

	/** Constructor.
	 * @param object the object represented by the node.
	 * @param parent the parent node.
	 * @param imageDescriptor the descriptor of the image related to the node..
	 * @param text the text for the node.
	 * @param isLeaf indicates if the node is a leaf.
	 */
	BSPLEObjectNode(EObject object, IOutlineNode parent, Image imageDescriptor, Object text,
			boolean isLeaf) {
		super(object, parent, imageDescriptor, text, isLeaf);
	}

	/** Change the key flag for the node.
	 *
	 * @param isKey the value of the key flag.
	 */
	public void setKey(boolean isKey) {
		this.isKey = isKey;
	}

	/** Replies the key flag for the node.
	 *
	 * @return the value of the key flag.
	 */
	public boolean isKey() {
		return this.isKey;
	}

}

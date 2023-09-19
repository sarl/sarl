/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.ui.outline;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.ui.editor.outline.IOutlineNode;
import org.eclipse.xtext.ui.editor.outline.impl.EObjectNode;

/**
 * Customize the outline page.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.13.0 20230919-093100
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 */
class SARLEObjectNode extends EObjectNode {

	private boolean isStatic;

	/** Constructor.
	 * @param object the object represented by the node.
	 * @param parent the parent node.
	 * @param imageDescriptor the descriptor of the image related to the node..
	 * @param text the text for the node.
	 * @param isLeaf indicates if the node is a leaf.
	 */
	SARLEObjectNode(EObject object, IOutlineNode parent, Image imageDescriptor, Object text,
			boolean isLeaf) {
		super(object, parent, imageDescriptor, text, isLeaf);
	}

	/** Change the static flag for the node.
	 *
	 * @param isStatic the value of the static flag.
	 */
	public void setStatic(boolean isStatic) {
		this.isStatic = isStatic;
	}

	/** Replies the static flag for the node.
	 *
	 * @return the value of the static flag.
	 */
	public boolean isStatic() {
		return this.isStatic;
	}

}

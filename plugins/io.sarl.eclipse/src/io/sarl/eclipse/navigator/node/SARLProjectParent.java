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

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.navigator.ISARLProjectElement;
import io.sarl.eclipse.navigator.SARLProjectNavigator;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.graphics.Image;

/**
 * Provides the project parent of a SARL custom project to enable a custom
 * view in {@link SARLProjectNavigator}.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectParent implements ISARLProjectElement {

	private IProject project;

	/**
	 * Image associated to the SARL Project parent folder.
	 */
	private Image image;

	private ISARLProjectElement[] children;

	/** Construct a parent element in a SARL project.
	 *
	 * @param iProject - the project.
	 */
	public SARLProjectParent(IProject iProject) {
		this.project = iProject;
	}

	@Override
	public Image getImage() {
		if (this.image == null) {
			this.image = SARLEclipsePlugin.getDefault().getImage(SARLEclipseConfig.SARL_PROJECT_IMAGE);
		}
		return this.image;
	}

	private ISARLProjectElement[] initializeChildren() {
		ISARLProjectElement[] ichildren = {
				new SARLProjectSARLNode(this),
				new SARLProjectJAVANode(this),
		};
		return ichildren;
	}

	@Override
	public ISARLProjectElement[] getChildren() {
		if (this.children == null) {
			this.children = initializeChildren(/*this.project*/);
		}
		// else we have already initialized them

		return this.children;
	}

	@Override
	public String getText() {
		return this.project.getName();
	}

	@Override
	public boolean hasChildren() {
		if (this.children == null) {
			this.children = initializeChildren(/*this.project*/);
		}
		// else we have already initialized them
		return this.children.length > 0;
	}

	@Override
	public IProject getProject() {
		return this.project;
	}

	@Override
	public Object getParent() {
		return null;
	}
}

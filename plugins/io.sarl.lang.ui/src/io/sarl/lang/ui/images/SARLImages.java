/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.ui.images;

import com.google.inject.Inject;

import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.xtext.ui.IImageHelper.IImageDescriptorHelper;
import org.eclipse.xtext.xbase.ui.labeling.XbaseImages2;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.xtext.common.types.JvmVisibility;

import com.google.inject.Singleton;

/**
 * Providers of images for the SARL IDE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("static-method")
@Singleton
public class SARLImages extends XbaseImages2 {

	@Inject
	private IImageDescriptorHelper imageHelper;

	/** Replies the image descriptor for the "packages".
	 *
	 * @return the image descriptor for the packages.
	 */
	public ImageDescriptor forPackage() {
		return JavaPluginImages.DESC_OBJS_PACKDECL;
	}

	/** Replies the image descriptor for the "agents".
	 *
	 * @return the image descriptor for the agents.
	 */
	public ImageDescriptor forAgent() {
		return this.imageHelper.getImageDescriptor("sarl-agent.png"); //$NON-NLS-1$
	}

	/** Replies the image descriptor for the "behaviors".
	 *
	 * @return the image descriptor for the behaviors.
	 */
	public ImageDescriptor forBehavior() {
		return this.imageHelper.getImageDescriptor("sarl-behavior.png"); //$NON-NLS-1$
	}

	/** Replies the image descriptor for the "capacities".
	 *
	 * @return the image descriptor for the capacities.
	 */
	public ImageDescriptor forCapacity() {
		return this.imageHelper.getImageDescriptor("sarl-capacity.png"); //$NON-NLS-1$
	}

	/** Replies the image descriptor for the "skills".
	 *
	 * @return the image descriptor for the skills.
	 */
	public ImageDescriptor forSkill() {
		return this.imageHelper.getImageDescriptor("sarl-skill.png"); //$NON-NLS-1$
	}

	/** Replies the image descriptor for the "events".
	 *
	 * @return the image descriptor for the events.
	 */
	public ImageDescriptor forEvent() {
		return this.imageHelper.getImageDescriptor("sarl-event.png"); //$NON-NLS-1$
	}

	/** Replies the image descriptor for the "behavior units".
	 *
	 * @return the image descriptor for the behavior units.
	 */
	public ImageDescriptor forBehaviorUnit() {
		return this.imageHelper.getImageDescriptor("sarl-behavior-unit.png"); //$NON-NLS-1$
	}

	/** Replies the image descriptor for the "actions".
	 *
	 * @return the image descriptor for the actions.
	 */
	public ImageDescriptor forAction() {
		return forOperation(JvmVisibility.PUBLIC, 0);
	}

	/** Replies the image descriptor for the "action signatures".
	 *
	 * @return the image descriptor for the action signatures.
	 */
	public ImageDescriptor forActionSignature() {
		return forOperation(JvmVisibility.PUBLIC, JavaElementImageDescriptor.ABSTRACT);
	}

	/** Replies the image descriptor for the "SARL script".
	 *
	 * @return the image descriptor for the SARL script.
	 */
	public ImageDescriptor forFile() {
		return this.imageHelper.getImageDescriptor("sarl-file.png"); //$NON-NLS-1$
	}

	/** Replies the image descriptor for the "attributes".
	 *
	 * @param writeable - indicates if the image is for a writeable attribute or not.
	 * @return the image descriptor for the attributes.
	 */
	public ImageDescriptor forAttribute(boolean writeable) {
		if (writeable) {
			return forField(JvmVisibility.PROTECTED, 0);
		}
		return forField(JvmVisibility.PROTECTED, JavaElementImageDescriptor.FINAL);
	}

	/** Replies the image descriptor for the "capacity uses".
	 *
	 * @return the image descriptor for the capacity uses.
	 */
	public ImageDescriptor forCapacityUses() {
		return forImportContainer();
	}

	/** Replies the image descriptor for the "capacity use".
	 *
	 * @return the image descriptor for the capacity use.
	 */
	public ImageDescriptor forCapacityUse() {
		return forImport();
	}

	/** Replies the image descriptor for the "capacity requirements".
	 *
	 * @return the image descriptor for the capacity requirements.
	 */
	public ImageDescriptor forCapacityRequirements() {
		return forImportContainer();
	}

	/** Replies the image descriptor for the "capacity requirement".
	 *
	 * @return the image descriptor for the capacity requirement.
	 */
	public ImageDescriptor forCapacityRequirement() {
		return forCapacity();
	}

}

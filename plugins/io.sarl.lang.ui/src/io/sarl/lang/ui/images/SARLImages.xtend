/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.ui.images

import com.google.inject.Inject
import org.eclipse.jdt.internal.ui.JavaPluginImages
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.xtext.ui.IImageHelper.IImageDescriptorHelper
import org.eclipse.xtext.xbase.ui.labeling.XbaseImages2
import org.eclipse.jdt.ui.JavaElementImageDescriptor
import org.eclipse.xtext.common.types.JvmVisibility
import com.google.inject.Singleton

/**
 * Providers of images for the SARL IDE.
 * 
 * @author $Author: sgalland$
 */
@Singleton
public class SARLImages extends XbaseImages2 {

	@Inject
	private IImageDescriptorHelper imageHelper;

	public def ImageDescriptor forPackage() {
		JavaPluginImages::DESC_OBJS_PACKDECL
	}

	public def ImageDescriptor forAgent() {
		imageHelper.getImageDescriptor("sarl-agent.png");
	}
	
	public def ImageDescriptor forBehavior() {
		imageHelper.getImageDescriptor("sarl-behavior.png");
	}

	public def ImageDescriptor forCapacity() {
		imageHelper.getImageDescriptor("sarl-capacity.png");
	}

	public def ImageDescriptor forSkill() {
		imageHelper.getImageDescriptor("sarl-skill.png");
	}

	public def ImageDescriptor forEvent() {
		imageHelper.getImageDescriptor("sarl-event.png");
	}
	
	public def ImageDescriptor forBehaviorUnit() {
		imageHelper.getImageDescriptor("sarl-behavior-unit.png");
	}

	public def ImageDescriptor forAction() {
		forOperation(JvmVisibility::PUBLIC, 0)
	}

	public def ImageDescriptor forActionSignature() {
		forOperation(JvmVisibility::PUBLIC, JavaElementImageDescriptor::ABSTRACT)
	}

	public def ImageDescriptor forFile() {
		imageHelper.getImageDescriptor("sarl-file.png");
	}
	
	public def ImageDescriptor forAttribute(boolean writeable) {
		if (writeable) {
			forField(JvmVisibility::PROTECTED, 0)
		}
		else {
			forField(JvmVisibility::PROTECTED, JavaElementImageDescriptor::FINAL)
		}
	}

	public def ImageDescriptor forCapacityUses() {
		forImportContainer
	}

	public def ImageDescriptor forCapacityUse() {
		forImport
	}

	public def ImageDescriptor forCapacityRequirements() {
		forImportContainer
	}

	public def ImageDescriptor forCapacityRequirement() {
		forCapacity
	}

}
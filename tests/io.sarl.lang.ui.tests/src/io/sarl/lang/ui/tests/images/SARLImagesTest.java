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
package io.sarl.lang.ui.tests.images;

import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.junit.Test;

import com.google.inject.Inject;

import io.sarl.lang.ui.images.SARLImages;
import io.sarl.tests.api.AbstractSarlUiTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLImagesTest extends AbstractSarlUiTest {

	@Inject
	private SARLImages images;

	/**
	 */
	@Test
	public void forPackage() {
		assertBundleImage("packd_obj.gif", this.images.forPackage()); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void forAgent() {
		assertBundleImage("sarl-agent.png", this.images.forAgent()); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void forBehavior() {
		assertBundleImage("sarl-behavior.png", this.images.forBehavior()); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void forCapacity() {
		assertBundleImage("sarl-capacity.png", this.images.forCapacity()); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void forSkill() {
		assertBundleImage("sarl-skill.png", this.images.forSkill()); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void forEvent() {
		assertBundleImage("sarl-event.png", this.images.forEvent()); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void forBehaviorUnit() {
		assertBundleImage("sarl-behavior-unit.png", this.images.forBehaviorUnit()); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void forAction() {
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, 0,
				this.images.forAction());
	}

	/**
	 */
	@Test
	public void forActionSignature() {
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, JavaElementImageDescriptor.ABSTRACT,
				this.images.forActionSignature());
	}

	/**
	 */
	@Test
	public void forFile() {
		assertBundleImage("sarl-file.png", this.images.forFile()); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void forAttribute_writable() {
		assertJdtImage(
				JavaPluginImages.DESC_FIELD_PROTECTED, 0,
				this.images.forAttribute(true));
	}

	/**
	 */
	@Test
	public void forAttribute_notWritable() {
		assertJdtImage(
				JavaPluginImages.DESC_FIELD_PROTECTED, JavaElementImageDescriptor.FINAL,
				this.images.forAttribute(false));
	}

	/**
	 */
	@Test
	public void forCapacityUses() {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_IMPCONT, 0,
				this.images.forCapacityUses());
	}

	/**
	 */
	@Test
	public void forCapacityUse() {
		assertPlaformImage("imp_obj.png", this.images.forCapacityUse()); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void forCapacityRequirements() {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_IMPCONT, 0,
				this.images.forCapacityRequirements());
	}

	/**
	 */
	@Test
	public void forCapacityRequirement() {
		assertBundleImage("sarl-capacity.png", this.images.forCapacityRequirement()); //$NON-NLS-1$
	}

}

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

import static org.junit.Assert.assertNotNull;
import io.sarl.lang.ui.images.SARLImages;
import io.sarl.tests.api.AbstractSarlUiTest;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.junit4.XtextRunner;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
public class SARLImagesTest extends AbstractSarlUiTest {

	@Inject
	private SARLImages images;

	private static void assertImage(ImageDescriptor desc) {
		assertNotNull(desc);
		Image img = desc.createImage();
		assertNotNull(img);
	}
	
	/**
	 */
	@Test
	public void forPackage() {
		assertImage(this.images.forPackage());
	}

	/**
	 */
	@Test
	public void forAgent() {
		assertImage(this.images.forAgent());
	}

	/**
	 */
	@Test
	public void forBehavior() {
		assertImage(this.images.forBehavior());
	}

	/**
	 */
	@Test
	public void forCapacity() {
		assertImage(this.images.forCapacity());
	}

	/**
	 */
	@Test
	public void forSkill() {
		assertImage(this.images.forSkill());
	}

	/**
	 */
	@Test
	public void forEvent() {
		assertImage(this.images.forEvent());
	}

	/**
	 */
	@Test
	public void forBehaviorUnit() {
		assertImage(this.images.forBehaviorUnit());
	}

	/**
	 */
	@Test
	public void forAction() {
		assertImage(this.images.forAction());
	}

	/**
	 */
	@Test
	public void forActionSignature() {
		assertImage(this.images.forActionSignature());
	}

	/**
	 */
	@Test
	public void forFile() {
		assertImage(this.images.forFile());
	}

	/**
	 */
	@Test
	public void forAttribute_writable() {
		assertImage(this.images.forAttribute(true));
	}

	/**
	 */
	@Test
	public void forAttribute_notWritable() {
		assertImage(this.images.forAttribute(false));
	}

	/**
	 */
	@Test
	public void forCapacityUses() {
		assertImage(this.images.forCapacityUses());
	}

	/**
	 */
	@Test
	public void forCapacityUse() {
		assertImage(this.images.forCapacityUse());
	}

	/**
	 */
	@Test
	public void forCapacityRequirements() {
		assertImage(this.images.forCapacityRequirements());
	}

	/**
	 */
	@Test
	public void forCapacityRequirement() {
		assertImage(this.images.forCapacityRequirement());
	}

}

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
package io.sarl.lang.ui.tests.labeling;

import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.CapacityUses;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.RequiredCapacity;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.ui.labeling.SARLDescriptionLabelProvider;
import io.sarl.tests.api.AbstractSarlUiTest;

import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.xtext.junit4.XtextRunner;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;

import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
public class SARLDescriptionLabelProviderTest extends AbstractSarlUiTest {

	@Inject
	private SARLDescriptionLabelProvider provider;

	/**
	 */
	@Test
	public void imageSarlScript() {
		assertBundleImage(
				"sarl-file.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(SarlScript.class)));
	}

	/**
	 */
	@Test
	public void imageAgent() {
		assertBundleImage(
				"sarl-agent.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(Agent.class)));
	}

	/**
	 */
	@Test
	public void imageEvent() {
		assertBundleImage(
				"sarl-event.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(Event.class)));
	}

	/**
	 */
	@Test
	public void imageCapacity() {
		assertBundleImage(
				"sarl-capacity.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(Capacity.class)));
	}

	/**
	 */
	@Test
	public void imageSkill() {
		assertBundleImage(
				"sarl-skill.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(Skill.class)));
	}

	/**
	 */
	@Test
	public void imageBehavior() {
		assertBundleImage(
				"sarl-behavior.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(Behavior.class)));
	}

	/**
	 */
	@Test
	public void imageAttribute() {
		assertJdtImage(
				JavaPluginImages.DESC_FIELD_PROTECTED, JavaElementImageDescriptor.FINAL,
				this.provider.image(Mockito.mock(Attribute.class)));
	}

	/**
	 */
	@Test
	public void imageConstructor() {
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, JavaElementImageDescriptor.CONSTRUCTOR,
				this.provider.image(Mockito.mock(Constructor.class)));
	}

	/**
	 */
	@Test
	public void imageAction() {
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, 0,
				this.provider.image(Mockito.mock(Action.class)));
	}

	/**
	 */
	@Test
	public void imageActionSignature() {
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, JavaElementImageDescriptor.ABSTRACT,
				this.provider.image(Mockito.mock(ActionSignature.class)));
	}

	/**
	 */
	@Test
	public void imageCapacityUses() {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_IMPCONT, 0,
				this.provider.image(Mockito.mock(CapacityUses.class)));
	}

	/**
	 */
	@Test
	public void imageRequiredCapacity() {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_IMPCONT, 0,
				this.provider.image(Mockito.mock(RequiredCapacity.class)));
	}

	/**
	 */
	@Test
	public void imageBehaviorUnit() {
		assertBundleImage(
				"sarl-behavior-unit.png",  //$NON-NLS-1$
				this.provider.image(Mockito.mock(BehaviorUnit.class)));
	}

}

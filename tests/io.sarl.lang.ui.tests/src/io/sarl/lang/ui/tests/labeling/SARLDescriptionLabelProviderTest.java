/*
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.ui.labeling.SARLDescriptionLabelProvider;
import io.sarl.tests.api.AbstractSarlUiTest;

import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtext.xbase.XExpression;
import org.junit.Test;
import org.mockito.Mockito;

import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLDescriptionLabelProviderTest extends AbstractSarlUiTest {

	@Inject
	private SARLDescriptionLabelProvider provider;

	/**
	 */
	@Test
	public void imageSarlScript() {
		assertBundleImage(
				"sarl-file.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(XtendFile.class)));
	}

	/**
	 */
	@Test
	public void imageAgent() {
		assertBundleImage(
				"sarl-agent.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(SarlAgent.class)));
	}

	/**
	 */
	@Test
	public void imageEvent() {
		assertBundleImage(
				"sarl-event.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(SarlEvent.class)));
	}

	/**
	 */
	@Test
	public void imageCapacity() {
		assertBundleImage(
				"sarl-capacity.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(SarlCapacity.class)));
	}

	/**
	 */
	@Test
	public void imageSkill() {
		assertBundleImage(
				"sarl-skill.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(SarlSkill.class)));
	}

	/**
	 */
	@Test
	public void imageBehavior() {
		assertBundleImage(
				"sarl-behavior.png", //$NON-NLS-1$
				this.provider.image(Mockito.mock(SarlBehavior.class)));
	}

	/**
	 */
	@Test
	public void imageAttribute() {
		assertJdtImage(
				JavaPluginImages.DESC_FIELD_PROTECTED, 0,
				this.provider.image(Mockito.mock(XtendField.class)));
	}

	/**
	 */
	@Test
	public void imageConstructor() {
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, JavaElementImageDescriptor.CONSTRUCTOR,
				this.provider.image(Mockito.mock(XtendConstructor.class)));
	}

	/**
	 */
	@Test
	public void imageAction() {
		SarlAction action = Mockito.mock(SarlAction.class);
		Mockito.when(action.getExpression()).thenReturn(Mockito.mock(XExpression.class));
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, 0,
				this.provider.image(action));
	}

	/**
	 */
	@Test
	public void imageActionSignature() {
		SarlAction action = Mockito.mock(SarlAction.class);
		Mockito.when(action.getExpression()).thenReturn(null);
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, JavaElementImageDescriptor.ABSTRACT,
				this.provider.image(action));
	}

	/**
	 */
	@Test
	public void imageCapacityUses() {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_IMPCONT, 0,
				this.provider.image(Mockito.mock(SarlCapacityUses.class)));
	}

	/**
	 */
	@Test
	public void imageRequiredCapacity() {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_IMPCONT, 0,
				this.provider.image(Mockito.mock(SarlRequiredCapacity.class)));
	}

	/**
	 */
	@Test
	public void imageBehaviorUnit() {
		assertBundleImage(
				"sarl-behavior-unit.png",  //$NON-NLS-1$
				this.provider.image(Mockito.mock(SarlBehaviorUnit.class)));
	}

}

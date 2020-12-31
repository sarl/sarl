/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

import static org.mockito.Mockito.when;

import com.google.inject.Inject;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociator;
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator;
import org.junit.Test;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.ui.labeling.SARLDescriptionLabelProvider;
import io.sarl.tests.api.AbstractSarlUiTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLDescriptionLabelProviderTest extends AbstractSarlUiTest {

	@Inject
	private SARLDescriptionLabelProvider provider;
	
	@Inject
	private IJvmModelAssociator jvmModelAssociator;

	/**
	 */
	@Test
	public void imageSarlScript() {
		assertBundleImage(
				"sarl-file.png", //$NON-NLS-1$
				this.provider.image(mock(SarlScript.class)));
	}
	
	private <T extends XtendMember> T mockMember(Class<T> type, JvmVisibility visibility) {
		return mockMember(type, visibility, null);
	}

	private <T extends XtendMember> T mockMember(Class<T> type, JvmVisibility visibility, JvmMember member) {
		T m = mock(type);
		when(m.getVisibility()).thenReturn(visibility);
		if (member != null) {
			EList<Adapter> adapters = new BasicEList<>();
			adapters.add(new JvmModelAssociator.Adapter());
			XtextResource r = mock(XtextResource.class);
			when(r.getLanguageName()).thenReturn("io.sarl.lang.SARL"); //$NON-NLS-1$
			when(r.eAdapters()).thenReturn(adapters);
			when(member.eResource()).thenReturn(r);
			when(m.eResource()).thenReturn(r);
			this.jvmModelAssociator.associate(m, member);
		}
		return m;
	}
	
	private static JvmOperation mockJvmOperation(String name, boolean isAbstract) {
		JvmOperation operation = mock(JvmOperation.class);
		when(operation.isAbstract()).thenReturn(isAbstract);
		when(operation.isStatic()).thenReturn(false);
		when(operation.isDefault()).thenReturn(false);
		when(operation.isDeprecated()).thenReturn(false);
		when(operation.isFinal()).thenReturn(false);
		when(operation.isSynchronized()).thenReturn(false);
		when(operation.isNative()).thenReturn(false);
		when(operation.getSimpleName()).thenReturn(name);
		when(operation.getAnnotations()).thenReturn(new BasicEList<JvmAnnotationReference>());
		return operation;
	}

	/**
	 */
	@Test
	public void imageAgent() {
		assertBundleImage(
				"sarl-agent.png", //$NON-NLS-1$
				this.provider.image(mockMember(SarlAgent.class, JvmVisibility.PUBLIC)));
	}

	/**
	 */
	@Test
	public void imageEvent() {
		assertBundleImage(
				"sarl-event.png", //$NON-NLS-1$
				this.provider.image(mockMember(SarlEvent.class, JvmVisibility.PUBLIC)));
	}

	/**
	 */
	@Test
	public void imageCapacity() {
		assertBundleImage(
				"sarl-capacity.png", //$NON-NLS-1$
				this.provider.image(mockMember(SarlCapacity.class, JvmVisibility.PUBLIC)));
	}

	/**
	 */
	@Test
	public void imageSkill() {
		assertBundleImage(
				"sarl-skill.png", //$NON-NLS-1$
				this.provider.image(mockMember(SarlSkill.class, JvmVisibility.PUBLIC)));
	}

	/**
	 */
	@Test
	public void imageBehavior() {
		assertBundleImage(
				"sarl-behavior.png", //$NON-NLS-1$
				this.provider.image(mockMember(SarlBehavior.class, JvmVisibility.PUBLIC)));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void imageAttribute() throws Exception {
		assertJdtImage(
				JavaPluginImages.DESC_FIELD_PROTECTED, 0,
				this.provider.image(mockMember(SarlField.class, JvmVisibility.PROTECTED)));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void imageConstructor() throws Exception {
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, JavaElementImageDescriptor.CONSTRUCTOR,
				this.provider.image(mockMember(SarlConstructor.class, JvmVisibility.PUBLIC)));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void imageAction() throws Exception {
		SarlAction action = mockMember(SarlAction.class, JvmVisibility.PROTECTED, mockJvmOperation("fct", false)); //$NON-NLS-1$
		when(action.getName()).thenReturn("fct"); //$NON-NLS-1$
		when(action.getExpression()).thenReturn(mock(XExpression.class));
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PROTECTED, 0,
				this.provider.image(action));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void imageActionSignature() throws Exception {
		SarlAction action = mockMember(SarlAction.class, JvmVisibility.PUBLIC, mockJvmOperation("fct", true)); //$NON-NLS-1$
		when(action.getName()).thenReturn("fct"); //$NON-NLS-1$
		when(action.getExpression()).thenReturn(null);
		//
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, JavaElementImageDescriptor.ABSTRACT,
				this.provider.image(action));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void imageCapacityUses() throws Exception {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_IMPCONT, 0,
				this.provider.image(mock(SarlCapacityUses.class)));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void imageRequiredCapacity() throws Exception {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_IMPCONT, 0,
				this.provider.image(mock(SarlRequiredCapacity.class)));
	}

	/**
	 */
	@Test
	public void imageBehaviorUnit() {
		assertBundleImage(
				"sarl-behavior-unit.png",  //$NON-NLS-1$
				this.provider.image(mock(SarlBehaviorUnit.class)));
	}

}

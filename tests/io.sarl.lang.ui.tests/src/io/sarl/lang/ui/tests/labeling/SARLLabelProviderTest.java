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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

import com.google.inject.Inject;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmTypeReference;
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
import io.sarl.lang.ui.labeling.SARLLabelProvider;
import io.sarl.tests.api.AbstractSarlUiTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLLabelProviderTest extends AbstractSarlUiTest {

	private static final String PACKAGE_STATEMENT = "package io.sarl.lang.ui.tests.labeling\n"; //$NON-NLS-1$

	@Inject
	private SARLLabelProvider provider;

	@Inject
	private IJvmModelAssociator jvmModelAssociator;

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
	 * @throws Exception
	 */
	@Test
	public void getImageDescriptorPackage() throws Exception {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_PACKDECL, 0,
				this.provider.getImageDescriptor(getClass().getPackage()));
	}
	
	/**
	 */
	@Test
	public void getImageDescriptorSarlScript() {
		assertBundleImage(
				"sarl-file.png", //$NON-NLS-1$
				this.provider.getImageDescriptor(mock(SarlScript.class)));
	}

	/**
	 */
	@Test
	public void getImageDescriptorAgent() {
		assertBundleImage(
				"sarl-agent.png", //$NON-NLS-1$
				this.provider.getImageDescriptor(mockMember(SarlAgent.class, JvmVisibility.PUBLIC)));
	}

	/**
	 */
	@Test
	public void getImageDescriptorEvent() {
		assertBundleImage(
				"sarl-event.png", //$NON-NLS-1$
				this.provider.getImageDescriptor(mockMember(SarlEvent.class, JvmVisibility.PUBLIC)));
	}

	/**
	 */
	@Test
	public void getImageDescriptorCapacity() {
		assertBundleImage(
				"sarl-capacity.png", //$NON-NLS-1$
				this.provider.getImageDescriptor(mockMember(SarlCapacity.class, JvmVisibility.PUBLIC)));
	}

	/**
	 */
	@Test
	public void getImageDescriptorSkill() {
		assertBundleImage(
				"sarl-skill.png", //$NON-NLS-1$
				this.provider.getImageDescriptor(mockMember(SarlSkill.class, JvmVisibility.PUBLIC)));
	}

	/**
	 */
	@Test
	public void getImageDescriptorBehavior() {
		assertBundleImage(
				"sarl-behavior.png", //$NON-NLS-1$
				this.provider.getImageDescriptor(mockMember(SarlBehavior.class, JvmVisibility.PUBLIC)));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getImageDescriptorAttribute() throws Exception {
		assertJdtImage(
				JavaPluginImages.DESC_FIELD_PROTECTED, 0,
				this.provider.getImageDescriptor(mockMember(SarlField.class, JvmVisibility.PROTECTED)));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getImageDescriptorConstructor() throws Exception {
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, JavaElementImageDescriptor.CONSTRUCTOR,
				this.provider.getImageDescriptor(mockMember(SarlConstructor.class, JvmVisibility.PUBLIC)));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getImageDescriptorAction() throws Exception {
		SarlAction action = mockMember(SarlAction.class, JvmVisibility.PROTECTED, mockJvmOperation("fct", false)); //$NON-NLS-1$
		when(action.getName()).thenReturn("fct"); //$NON-NLS-1$
		when(action.getExpression()).thenReturn(mock(XExpression.class));
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PROTECTED, 0,
				this.provider.getImageDescriptor(action));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getImageDescriptorActionSignature() throws Exception {
		SarlAction action = mockMember(SarlAction.class, JvmVisibility.PUBLIC, mockJvmOperation("fct", true)); //$NON-NLS-1$
		when(action.getName()).thenReturn("fct"); //$NON-NLS-1$
		when(action.getExpression()).thenReturn(null);
		//
		assertJdtImage(
				JavaPluginImages.DESC_MISC_PUBLIC, JavaElementImageDescriptor.ABSTRACT,
				this.provider.getImageDescriptor(action));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getImageDescriptorCapacityUses() throws Exception {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_IMPCONT, 0,
				this.provider.getImageDescriptor(mock(SarlCapacityUses.class)));
	}

	/**
	 * @throws Exception 
	 */
	@Test
	public void getImageDescriptorRequiredCapacity() throws Exception {
		assertJdtImage(
				JavaPluginImages.DESC_OBJS_IMPCONT, 0,
				this.provider.getImageDescriptor(mock(SarlRequiredCapacity.class)));
	}

	/**
	 */
	@Test
	public void getImageDescriptorBehaviorUnit() {
		assertBundleImage(
				"sarl-behavior-unit.png",  //$NON-NLS-1$
				this.provider.getImageDescriptor(mock(SarlBehaviorUnit.class)));
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextJvmParametizedTypeReference_0() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "agent A1 { }\n" //$NON-NLS-1$
				+ "agent A2 extends A1 { }\n"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		JvmParameterizedTypeReference superType = agent.getExtends();
		assertNotNull(superType);
		Object text = this.provider.getText(superType);
		assertNotNull(text);
		assertEquals("A1", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextJvmParametizedTypeReference_1() throws Exception {
		SarlEvent event = helper().sarlTypeDeclaration(
				SarlEvent.class,
				PACKAGE_STATEMENT
				+ "event E1 {\n" //$NON-NLS-1$
				+ "var attr : org.eclipse.xtext.xbase.lib.Pair<java.lang.Integer,java.lang.Double>\n" //$NON-NLS-1$
				+ "}"); //$NON-NLS-1$
		validate(event.eResource()).assertNoErrors();
		EList<XtendMember> features = event.getMembers();
		assertNotNull(features);
		assertEquals(1, features.size());
		EObject eObject = features.get(0);
		assertTrue(eObject instanceof SarlField);
		SarlField attr = (SarlField)eObject;
		JvmTypeReference typeReference = attr.getType();
		assertTrue(typeReference instanceof JvmParameterizedTypeReference);
		JvmParameterizedTypeReference parametizedTypeReference = (JvmParameterizedTypeReference)typeReference;
		Object text = this.provider.getText(parametizedTypeReference);
		assertNotNull(text);
		assertEquals("Pair<Integer, Double>", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextJvmTypeReference() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "agent A1 { }\n" //$NON-NLS-1$
				+ "agent A2 extends A1 { }\n"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		JvmParameterizedTypeReference superType = agent.getExtends();
		assertNotNull(superType);
		Object text = this.provider.getText(superType);
		assertNotNull(text);
		assertEquals("A1", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextSarlScript() throws Exception {
		SarlScript script = helper().sarlScript(
				helper().generateFilename(),
				PACKAGE_STATEMENT);
		validate(script.eResource()).assertNoErrors();
		Object text = this.provider.getText(script);
		assertNotNull(text);
		assertEquals("unittest", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextAgent() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "agent A1 { }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object text = this.provider.getText(agent);
		assertNotNull(text);
		assertEquals("A1", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextEvent() throws Exception {
		SarlEvent event = helper().sarlTypeDeclaration(
				SarlEvent.class,
				PACKAGE_STATEMENT
				+ "event E1 { }"); //$NON-NLS-1$
		validate(event.eResource()).assertNoErrors();
		Object text = this.provider.getText(event);
		assertNotNull(text);
		assertEquals("E1", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextCapacity() throws Exception {
		SarlCapacity capacity = helper().sarlTypeDeclaration(
				SarlCapacity.class,
				PACKAGE_STATEMENT
				+ "capacity C1 { }"); //$NON-NLS-1$
		validate(capacity.eResource()).assertNoErrors();
		Object text = this.provider.getText(capacity);
		assertNotNull(text);
		assertEquals("C1", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextSkill() throws Exception {
		SarlSkill skill = helper().sarlTypeDeclaration(
				SarlSkill.class,
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "skill S1 implements C1 { }"); //$NON-NLS-1$
		validate(skill.eResource()).assertNoErrors();
		Object text = this.provider.getText(skill);
		assertNotNull(text);
		assertEquals("S1", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextBehavior() throws Exception {
		SarlBehavior behavior = helper().sarlTypeDeclaration(
				SarlBehavior.class,
				PACKAGE_STATEMENT
				+ "behavior B1 { }"); //$NON-NLS-1$
		validate(behavior.eResource()).assertNoErrors();
		Object text = this.provider.getText(behavior);
		assertNotNull(text);
		assertEquals("B1", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextAttribute() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "agent A1 { var myAttr : boolean }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlField);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("myAttr : boolean", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextConstructor() throws Exception {
		SarlEvent event = helper().sarlTypeDeclaration(
				SarlEvent.class,
				PACKAGE_STATEMENT
				+ "event E1 { new (id:int) { } }"); //$NON-NLS-1$
		validate(event.eResource()).assertNoErrors();
		Object feature = event.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlConstructor);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("new(int)", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextAction_noParam_noReturn() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "agent A1 { def myAction { } }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlAction);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("myAction() : void", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextAction_noParam_return() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "agent A1 { def myAction : int { 0 } }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlAction);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("myAction() : int", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextAction_param_noReturn() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "agent A1 { def myAction(a:char) { } }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlAction);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("myAction(char) : void", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextAction_param_return() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "agent A1 { def myAction(a:char) : int { 0 } }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlAction);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("myAction(char) : int", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextActionSignature_noParam_noReturn() throws Exception {
		SarlCapacity capacity = helper().sarlTypeDeclaration(
				SarlCapacity.class,
				PACKAGE_STATEMENT
				+ "capacity C1 { def myAction }"); //$NON-NLS-1$
		validate(capacity.eResource()).assertNoErrors();
		Object feature = capacity.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlAction);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("myAction() : void", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextActionSignature_noParam_return() throws Exception {
		SarlCapacity capacity = helper().sarlTypeDeclaration(
				SarlCapacity.class,
				PACKAGE_STATEMENT
				+ "capacity C1 { def myAction : int }"); //$NON-NLS-1$
		validate(capacity.eResource()).assertNoErrors();
		Object feature = capacity.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlAction);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("myAction() : int", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextActionSignature_param_noReturn() throws Exception {
		SarlCapacity capacity = helper().sarlTypeDeclaration(
				SarlCapacity.class,
				PACKAGE_STATEMENT
				+ "capacity C1 { def myAction(a:char) }"); //$NON-NLS-1$
		validate(capacity.eResource()).assertNoErrors();
		Object feature = capacity.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlAction);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("myAction(char) : void", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextActionSignature_param_return() throws Exception {
		SarlCapacity capacity = helper().sarlTypeDeclaration(
				SarlCapacity.class,
				PACKAGE_STATEMENT
				+ "capacity C1 { def myAction(a:char) : int }"); //$NON-NLS-1$
		validate(capacity.eResource()).assertNoErrors();
		Object feature = capacity.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlAction);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("myAction(char) : int", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextCapacityUses() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "agent A1 { uses C1 }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlCapacityUses);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("capacity uses", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextRequiredCapacity() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "capacity C1 { }\n" //$NON-NLS-1$
				+ "agent A1 { requires C1 }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlRequiredCapacity);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("required capacities", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextBehaviorUnit_0() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "event E1 { }\n" //$NON-NLS-1$
				+ "agent A1 { on E1 { } }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlBehaviorUnit);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("on E1", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextBehaviorUnit_1() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "event E1 { }\n" //$NON-NLS-1$
				+ "agent A1 { on E1 [ true ] { } }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlBehaviorUnit);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("on E1 [true]", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextBehaviorUnit_2() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "event E1 { }\n" //$NON-NLS-1$
				+ "agent A1 { on E1 [ 3 > 5 ] { } }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlBehaviorUnit);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("on E1 [3 > 5]", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextBehaviorUnit_3() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "event E1 { }\n" //$NON-NLS-1$
				+ "agent A1 { on E1 [ 3 <=5 ] { } }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlBehaviorUnit);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("on E1 [3 <=5]", text); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void getTextBehaviorUnit_4() throws Exception {
		SarlAgent agent = helper().sarlTypeDeclaration(
				SarlAgent.class,
				PACKAGE_STATEMENT
				+ "event E1 { }\n" //$NON-NLS-1$
				+ "agent A1 { on E1 [ 1+2+3+4+5+6+7+8+9+10 < 100 ] { } }"); //$NON-NLS-1$
		validate(agent.eResource()).assertNoErrors();
		Object feature = agent.getMembers().get(0);
		assertNotNull(feature);
		assertTrue(feature instanceof SarlBehaviorUnit);
		Object text = this.provider.getText(feature);
		assertNotNull(text);
		assertEquals("on E1 [1+2+3+4...]", text); //$NON-NLS-1$
	}

}

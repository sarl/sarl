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
package io.sarl.lang.ui.tests.outline;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.ui.outline.SARLOutlineNodeComparator;
import io.sarl.tests.api.Nullable;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.ui.editor.outline.impl.EObjectNode;
import org.eclipse.xtext.ui.editor.outline.impl.EStructuralFeatureNode;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Before;
import org.junit.Test;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLOutlineNodeComparatorTest {

	@Nullable
	private SARLOutlineNodeComparator comparator;
	
	@Nullable
	private EStructuralFeatureNode sarlScript;
	@Nullable
	private EStructuralFeatureNode agentFeature1;
	@Nullable
	private EStructuralFeatureNode agentFeature2;
	@Nullable
	private EStructuralFeatureNode behaviorFeature1;
	@Nullable
	private EStructuralFeatureNode behaviorFeature2;
	@Nullable
	private EStructuralFeatureNode capacityFeature1;
	@Nullable
	private EStructuralFeatureNode capacityFeature2;
	@Nullable
	private EStructuralFeatureNode skillFeature1;
	@Nullable
	private EStructuralFeatureNode skillFeature2;
	@Nullable
	private EStructuralFeatureNode eventFeature1;
	@Nullable
	private EStructuralFeatureNode eventFeature2;
	@Nullable
	private EStructuralFeatureNode otherFeature1;
	@Nullable
	private EStructuralFeatureNode otherFeature2;

	@Nullable
	private EObjectNode importFeature1;
	@Nullable
	private EObjectNode importFeature2;
	@Nullable
	private EObjectNode capacityUseFeature1;
	@Nullable
	private EObjectNode capacityUseFeature2;
	@Nullable
	private EObjectNode capacityRequirementFeature1;
	@Nullable
	private EObjectNode capacityRequirementFeature2;
	@Nullable
	private EObjectNode attributeFeature1;
	@Nullable
	private EObjectNode attributeFeature2;
	@Nullable
	private EObjectNode constructorFeature1;
	@Nullable
	private EObjectNode constructorFeature2;
	@Nullable
	private EObjectNode actionFeature1;
	@Nullable
	private EObjectNode actionFeature2;
	@Nullable
	private EObjectNode actionSignatureFeature1;
	@Nullable
	private EObjectNode actionSignatureFeature2;
	@Nullable
	private EObjectNode behaviorUnitFeature1;
	@Nullable
	private EObjectNode behaviorUnitFeature2;
	
	/**
	 */
	@Before
	public void setUp() {
		this.sarlScript = mock(EStructuralFeatureNode.class);
		when(this.sarlScript.getEStructuralFeature()).thenReturn(XtendPackage.Literals.XTEND_FILE__PACKAGE);
		
		this.agentFeature1 = mock(EStructuralFeatureNode.class);
		this.agentFeature2 = mock(EStructuralFeatureNode.class);
		
		this.behaviorFeature1 = mock(EStructuralFeatureNode.class);
		this.behaviorFeature2 = mock(EStructuralFeatureNode.class);
		
		this.capacityFeature1 = mock(EStructuralFeatureNode.class);
		this.capacityFeature2 = mock(EStructuralFeatureNode.class);
		
		this.skillFeature1 = mock(EStructuralFeatureNode.class);
		this.skillFeature2 = mock(EStructuralFeatureNode.class);
		
		this.eventFeature1 = mock(EStructuralFeatureNode.class);
		this.eventFeature2 = mock(EStructuralFeatureNode.class);
		
		this.otherFeature1 = mock(EStructuralFeatureNode.class);
		when(this.otherFeature1.getEStructuralFeature()).thenReturn(mock(EStructuralFeature.class));
		this.otherFeature2 = mock(EStructuralFeatureNode.class);
		when(this.otherFeature2.getEStructuralFeature()).thenReturn(mock(EStructuralFeature.class));
		
		this.importFeature1 = mock(EObjectNode.class);
		when(this.importFeature1.getEClass()).thenReturn(XtypePackage.Literals.XIMPORT_SECTION);
		this.importFeature2 = mock(EObjectNode.class);
		when(this.importFeature2.getEClass()).thenReturn(XtypePackage.Literals.XIMPORT_SECTION);
		
		this.capacityUseFeature1 = mock(EObjectNode.class);
		when(this.capacityUseFeature1.getEClass()).thenReturn(SarlPackage.Literals.SARL_CAPACITY_USES);
		this.capacityUseFeature2 = mock(EObjectNode.class);
		when(this.capacityUseFeature2.getEClass()).thenReturn(SarlPackage.Literals.SARL_CAPACITY_USES);
		
		this.capacityRequirementFeature1 = mock(EObjectNode.class);
		when(this.capacityRequirementFeature1.getEClass()).thenReturn(SarlPackage.Literals.SARL_REQUIRED_CAPACITY);
		this.capacityRequirementFeature2 = mock(EObjectNode.class);
		when(this.capacityRequirementFeature2.getEClass()).thenReturn(SarlPackage.Literals.SARL_REQUIRED_CAPACITY);
		
		this.attributeFeature1 = mock(EObjectNode.class);
		when(this.attributeFeature1.getEClass()).thenReturn(XtendPackage.Literals.XTEND_FIELD);
		this.attributeFeature2 = mock(EObjectNode.class);
		when(this.attributeFeature2.getEClass()).thenReturn(XtendPackage.Literals.XTEND_FIELD);
		
		this.constructorFeature1 = mock(EObjectNode.class);
		when(this.constructorFeature1.getEClass()).thenReturn(XtendPackage.Literals.XTEND_CONSTRUCTOR);
		this.constructorFeature2 = mock(EObjectNode.class);
		when(this.constructorFeature2.getEClass()).thenReturn(XtendPackage.Literals.XTEND_CONSTRUCTOR);
		
		this.actionFeature1 = mock(EObjectNode.class);
		when(this.actionFeature1.getEClass()).thenReturn(SarlPackage.Literals.SARL_ACTION);
		this.actionFeature2 = mock(EObjectNode.class);
		when(this.actionFeature2.getEClass()).thenReturn(SarlPackage.Literals.SARL_ACTION);
		
		this.behaviorUnitFeature1 = mock(EObjectNode.class);
		when(this.behaviorUnitFeature1.getEClass()).thenReturn(SarlPackage.Literals.SARL_BEHAVIOR_UNIT);
		this.behaviorUnitFeature2 = mock(EObjectNode.class);
		when(this.behaviorUnitFeature2.getEClass()).thenReturn(SarlPackage.Literals.SARL_BEHAVIOR_UNIT);
		
		this.comparator = new SARLOutlineNodeComparator();
	}
	
	/**
	 */
	@Test
	public void getCategory_order_sarlScript() {
		int index = this.comparator.getCategory(this.sarlScript);
		
		assertTrue(index < this.comparator.getCategory(this.agentFeature1));
		assertTrue(index < this.comparator.getCategory(this.agentFeature2));
		assertTrue(index < this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index < this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index < this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index < this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index < this.comparator.getCategory(this.skillFeature1));
		assertTrue(index < this.comparator.getCategory(this.skillFeature2));
		assertTrue(index < this.comparator.getCategory(this.eventFeature1));
		assertTrue(index < this.comparator.getCategory(this.eventFeature2));
		assertTrue(index < this.comparator.getCategory(this.otherFeature1));
		assertTrue(index < this.comparator.getCategory(this.otherFeature2));
	}
	
	/**
	 */
	@Test
	public void getCategory_order_agentFeature1() {
		int index = this.comparator.getCategory(this.agentFeature1);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_agentFeature2() {
		int index = this.comparator.getCategory(this.agentFeature2);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_behaviorFeature1() {
		int index = this.comparator.getCategory(this.behaviorFeature1);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_behaviorFeature2() {
		int index = this.comparator.getCategory(this.behaviorFeature2);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_capacityFeature1() {
		int index = this.comparator.getCategory(this.capacityFeature1);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_capacityFeature2() {
		int index = this.comparator.getCategory(this.capacityFeature2);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_skillFeature1() {
		int index = this.comparator.getCategory(this.skillFeature1);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_skillFeature2() {
		int index = this.comparator.getCategory(this.skillFeature2);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_eventFeature1() {
		int index = this.comparator.getCategory(this.eventFeature1);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_eventFeature2() {
		int index = this.comparator.getCategory(this.eventFeature2);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_otherFeature1() {
		int index = this.comparator.getCategory(this.otherFeature1);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature2));
	}

	/**
	 */
	@Test
	public void getCategory_order_otherFeature2() {
		int index = this.comparator.getCategory(this.otherFeature2);
		
		assertTrue(index > this.comparator.getCategory(this.sarlScript));
		assertTrue(index == this.comparator.getCategory(this.agentFeature1));
		assertTrue(index == this.comparator.getCategory(this.agentFeature2));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature1));
		assertTrue(index == this.comparator.getCategory(this.behaviorFeature2));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature1));
		assertTrue(index == this.comparator.getCategory(this.capacityFeature2));
		assertTrue(index == this.comparator.getCategory(this.skillFeature1));
		assertTrue(index == this.comparator.getCategory(this.skillFeature2));
		assertTrue(index == this.comparator.getCategory(this.eventFeature1));
		assertTrue(index == this.comparator.getCategory(this.eventFeature2));
		assertTrue(index == this.comparator.getCategory(this.otherFeature1));
	}

}

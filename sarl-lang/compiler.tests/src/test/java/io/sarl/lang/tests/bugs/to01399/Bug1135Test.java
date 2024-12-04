/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.tests.bugs.to01399;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.google.inject.Inject;

import foo.ecore.SubEvent;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.builders.ISarlBehaviorUnitBuilder;
import io.sarl.lang.codebuilder.builders.ISarlCapacityBuilder;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.tests.api.AbstractSarlTest;

/** Testing class for issue: Cannot create Skill implementing a capacity with a generic type.
 *
 * <p>https://github.com/sarl/sarl/issues/1135
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1135"
 */
@DisplayName("Bug #1135")
@SuppressWarnings("all")
@Tag("core")
@Tag("unit")
public class Bug1135Test extends AbstractSarlTest {

	@Inject
	private CodeBuilderFactory factory;

	private ResourceSet resource;

	private ISarlCapacityBuilder builder;

	@BeforeEach
	public void setUp() {
		this.resource = new ResourceSetImpl();
		this.builder = this.factory.createSarlCapacity("C", this.resource);
	}

	@Test
	@DisplayName("getData")
	public void getData() {
		var actionBuilder = this.builder.addDefSarlAction("getData");
		var typeParameterBuilder = actionBuilder.addTypeParameter("T");
		var typeParameter = typeParameterBuilder.getJvmTypeParameter();
		actionBuilder.addParameter("name").setParameterType("String");
		var typeParameterRef0 = typeParameterBuilder.newTypeRef(typeParameter);
		actionBuilder.addParameter("type").setParameterType(actionBuilder.newTypeRef(Class.class, typeParameterRef0));
		var typeParameterRef1 = typeParameterBuilder.newTypeRef(typeParameter);
		actionBuilder.setReturnType(typeParameterRef1);
		//
		var capacity = this.builder.getSarlCapacity();
		assertEquals(1, capacity.getMembers().size());
		var mbr = capacity.getMembers().get(0);
		var act = assertInstanceOf(SarlAction.class, mbr);

		assertEquals("getData", act.getName());

		assertEquals(1, act.getTypeParameters().size());
		var tp = assertInstanceOf(JvmTypeParameter.class, act.getTypeParameters().get(0));
		assertEquals("T", tp.getName());
		assertEquals(0, tp.getConstraints().size());
		
		assertEquals(2, act.getParameters().size());
		var param0 = assertInstanceOf(SarlFormalParameter.class, act.getParameters().get(0));
		assertEquals("name", param0.getName());
		assertEquals("java.lang.String", param0.getParameterType().getType().getIdentifier());
		var param1 = assertInstanceOf(SarlFormalParameter.class, act.getParameters().get(1));
		assertEquals("type", param1.getName());
		assertEquals("java.lang.Class", param1.getParameterType().getType().getIdentifier());

		var rt = act.getReturnType();
		assertNotNull(rt);
		assertSame(tp, rt.getType());
	}

	@Test
	@DisplayName("setData")
	public void setData() {
		var actionBuilder = this.builder.addDefSarlAction("setData");
		var typeParameterBuilder = actionBuilder.addTypeParameter("T");
		var typeParameter = typeParameterBuilder.getJvmTypeParameter();
		actionBuilder.addParameter("name").setParameterType("String");
		var typeParameterRef0 = typeParameterBuilder.newTypeRef(typeParameter);
		actionBuilder.addParameter("type").setParameterType(actionBuilder.newTypeRef(Class.class, typeParameterRef0));
		var typeParameterRef1 = typeParameterBuilder.newTypeRef(typeParameter);
		actionBuilder.addParameter("value").setParameterType(typeParameterRef1);
		var typeParameterRef2 = typeParameterBuilder.newTypeRef(typeParameter);
		actionBuilder.setReturnType(typeParameterRef2);
		//
		var capacity = this.builder.getSarlCapacity();
		assertEquals(1, capacity.getMembers().size());
		var mbr = capacity.getMembers().get(0);
		var act = assertInstanceOf(SarlAction.class, mbr);

		assertEquals("setData", act.getName());

		assertEquals(1, act.getTypeParameters().size());
		var tp = assertInstanceOf(JvmTypeParameter.class, act.getTypeParameters().get(0));
		assertEquals("T", tp.getName());
		assertEquals(0, tp.getConstraints().size());
		
		assertEquals(3, act.getParameters().size());
		var param0 = assertInstanceOf(SarlFormalParameter.class, act.getParameters().get(0));
		assertEquals("name", param0.getName());
		assertEquals("java.lang.String", param0.getParameterType().getType().getIdentifier());
		var param1 = assertInstanceOf(SarlFormalParameter.class, act.getParameters().get(1));
		assertEquals("type", param1.getName());
		assertEquals("java.lang.Class", param1.getParameterType().getType().getIdentifier());
		var param2 = assertInstanceOf(SarlFormalParameter.class, act.getParameters().get(2));
		assertEquals("value", param2.getName());
		assertEquals("T", param2.getParameterType().getType().getIdentifier());

		var rt = act.getReturnType();
		assertNotNull(rt);
		assertSame(tp, rt.getType());
	}

}

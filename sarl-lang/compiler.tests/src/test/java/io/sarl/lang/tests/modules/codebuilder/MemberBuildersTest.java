/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.tests.modules.codebuilder;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.when;
import static io.sarl.tests.api.tools.TestAssertions.*;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Generated;

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.xtext.common.types.JvmLowerBound;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.google.inject.Inject;

import foo.ecore.SubAgent;
import foo.ecore.SubEvent;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.builders.ISarlActionBuilder;
import io.sarl.lang.codebuilder.builders.ISarlAgentBuilder;
import io.sarl.lang.codebuilder.builders.ISarlBehaviorUnitBuilder;
import io.sarl.lang.codebuilder.builders.ISarlConstructorBuilder;
import io.sarl.lang.codebuilder.builders.ISarlFieldBuilder;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Event;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.tests.api.AbstractSarlTest;

/** Test the builders of {@code CodeBuilderFactory}.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@DisplayName("Member builders of CodeBuilderFactory")
@SuppressWarnings("all")
@Tag("core")
@Tag("unit")
public class MemberBuildersTest {

	@DisplayName("on")
	@Nested
	public class SarlBehaviorUnitTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlBehaviorUnitBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlBehaviorUnit(SubEvent.class.getName(), this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("getExpression")
		public void getExpression() {
			final var expression = this.builder.getExpression();
			assertNotNull(expression);
		}

		@Test
		@DisplayName("getGuard")
		public void getGuard() {
			final var guard = this.builder.getGuard();
			assertNotNull(guard);
		}

	}

	@DisplayName("def")
	@Nested
	public class DefSarlActionTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlActionBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createDefSarlAction("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addAnnotation")
		public void addAnnotation() {
			this.builder.addAnnotation(Generated.class.getName());
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var annotations = action.getAnnotations();
			assertEquals(1, annotations.size());
			assertEquals(Generated.class.getName(), annotations.get(0).getAnnotationType().getQualifiedName());
		}

		@Test
		@DisplayName("addException")
		public void addException() {
			this.builder.addException(NullPointerException.class.getName());
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var exceptions = action.getExceptions();
			assertEquals(1, exceptions.size());
			assertEquals(NullPointerException.class.getName(), exceptions.get(0).getQualifiedName());
		}

		@Test
		@DisplayName("addFiredEvent")
		public void addFiredEvent() {
			this.builder.addFiredEvent(Event.class.getName());
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var events = action.getFiredEvents();
			assertEquals(1, events.size());
			assertEquals(Event.class.getName(), events.get(0).getQualifiedName());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var modifiers = action.getModifiers();
			assertContains(modifiers, "package", "def");
		}

		@Test
		@DisplayName("addParameter(boolean)")
		public void addParameter_boolean() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean");
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean", param.getParameterType().getQualifiedName());
		}

		@Test
		@DisplayName("addParameter(boolean) with type reference")
		public void addParameter_boolean_typeref() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			var ref = paramBuilder.newTypeRef("boolean");
			paramBuilder.setParameterType(ref);
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean", param.getParameterType().getQualifiedName());
		}

		@Test
		@DisplayName("addParameter(boolean*)")
		public void addParameter_boolean_variadic() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean[]");
			paramBuilder.setVarArg(true);
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean[]", param.getParameterType().getQualifiedName());
			assertTrue(param.isVarArg());
		}

		@Test
		@DisplayName("addParameter(boolean=true)")
		public void addParameter_boolean_defaultValue() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean");
			paramBuilder.getDefaultValue().setExpression("true");
			
			var action = this.builder.getSarlAction();
			assertNotNull(action);
			var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			var param = (SarlFormalParameter) parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean", param.getParameterType().getQualifiedName());
			var defaultValue = param.getDefaultValue();
			var literal = Assertions.assertInstanceOf(XBooleanLiteral.class, defaultValue);
			assertTrue(literal.isIsTrue());
		}

		@Test
		@DisplayName("addParameter(extension boolean)")
		public void addParameter_boolean_extension() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean");
			paramBuilder.setExtension(true);
			
			var action = this.builder.getSarlAction();
			assertNotNull(action);
			var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			var param = (SarlFormalParameter) parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean", param.getParameterType().getQualifiedName());
			assertTrue(param.isExtension());
		}

		@Test
		@DisplayName("addTypeParameter(P)")
		public void addTypeParameter_p() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var cst = assertInstanceOf(JvmUpperBound.class, param.getConstraints().get(0));
			assertEquals(Object.class.getName(), cst.getTypeReference().getIdentifier());
		}

		@Test
		@DisplayName("addTypeParameter(P extends Integer)")
		public void addTypeParameter_p_extends_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addUpperConstraint(Integer.class.getName());
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmUpperBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}

		@Test
		@DisplayName("addTypeParameter(P super Integer)")
		public void addTypeParameter_p_super_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addLowerConstraint(Integer.class.getName());
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmLowerBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}

		@Test
		@DisplayName("getExpression")
		public void getExpression() {
			final var expression = this.builder.getExpression();
			assertNotNull(expression);
		}


		@Test
		@DisplayName("setReturnType(boolean)")
		public void setReturnType() {
			this.builder.setReturnType("boolean");
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var type = action.getReturnType();
			assertNotNull(type);
			assertEquals("boolean", type.getQualifiedName());
		}

	}

	@DisplayName("override")
	@Nested
	public class OverrideSarlActionTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlActionBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createOverrideSarlAction("xyz", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addAnnotation")
		public void addAnnotation() {
			this.builder.addAnnotation(Generated.class.getName());
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var annotations = action.getAnnotations();
			assertEquals(1, annotations.size());
			assertEquals(Generated.class.getName(), annotations.get(0).getAnnotationType().getQualifiedName());
		}

		@Test
		@DisplayName("addException")
		public void addException() {
			this.builder.addException(NullPointerException.class.getName());
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var exceptions = action.getExceptions();
			assertEquals(1, exceptions.size());
			assertEquals(NullPointerException.class.getName(), exceptions.get(0).getQualifiedName());
		}

		@Test
		@DisplayName("addFiredEvent")
		public void addFiredEvent() {
			this.builder.addFiredEvent(Event.class.getName());
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var events = action.getFiredEvents();
			assertEquals(1, events.size());
			assertEquals(Event.class.getName(), events.get(0).getQualifiedName());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var modifiers = action.getModifiers();
			assertContains(modifiers, "package", "override");
		}

		@Test
		@DisplayName("addParameter(boolean)")
		public void addParameter_boolean() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean");
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean", param.getParameterType().getQualifiedName());
		}

		@Test
		@DisplayName("addParameter(boolean*)")
		public void addParameter_boolean_variadic() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean[]");
			paramBuilder.setVarArg(true);
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean[]", param.getParameterType().getQualifiedName());
			assertTrue(param.isVarArg());
		}

		@Test
		@DisplayName("addParameter(boolean=true)")
		public void addParameter_boolean_defaultValue() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean");
			paramBuilder.getDefaultValue().setExpression("true");
			
			var action = this.builder.getSarlAction();
			assertNotNull(action);
			var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			var param = (SarlFormalParameter) parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean", param.getParameterType().getQualifiedName());
			var defaultValue = param.getDefaultValue();
			var literal = Assertions.assertInstanceOf(XBooleanLiteral.class, defaultValue);
			assertTrue(literal.isIsTrue());
		}

		@Test
		@DisplayName("addParameter(extension boolean)")
		public void addParameter_boolean_extension() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean");
			paramBuilder.setExtension(true);
			
			var action = this.builder.getSarlAction();
			assertNotNull(action);
			var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			var param = (SarlFormalParameter) parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean", param.getParameterType().getQualifiedName());
			assertTrue(param.isExtension());
		}

		@Test
		@DisplayName("addTypeParameter(P)")
		public void addTypeParameter_p() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var cst = assertInstanceOf(JvmUpperBound.class, param.getConstraints().get(0));
			assertEquals(Object.class.getName(), cst.getTypeReference().getIdentifier());
		}

		@Test
		@DisplayName("addTypeParameter(P extends Integer)")
		public void addTypeParameter_p_extends_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addUpperConstraint(Integer.class.getName());
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmUpperBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}

		@Test
		@DisplayName("addTypeParameter(P super Integer)")
		public void addTypeParameter_p_super_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addLowerConstraint(Integer.class.getName());
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var parameters = action.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmLowerBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}

		@Test
		@DisplayName("getExpression")
		public void getExpression() {
			final var expression = this.builder.getExpression();
			assertNotNull(expression);
		}


		@Test
		@DisplayName("setReturnType(boolean)")
		public void setReturnType() {
			this.builder.setReturnType("boolean");
			
			final var action = this.builder.getSarlAction();
			assertNotNull(action);
			final var type = action.getReturnType();
			assertNotNull(type);
			assertEquals("boolean", type.getQualifiedName());
		}

	}

	@DisplayName("new")
	@Nested
	public class SarlConstructorTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlConstructorBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlConstructor(this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addException")
		public void addException() {
			this.builder.addException(NullPointerException.class.getName());
			final var action = this.builder.getSarlConstructor();
			assertNotNull(action);
			final var exceptions = action.getExceptions();
			assertEquals(1, exceptions.size());
			assertEquals(NullPointerException.class.getName(), exceptions.get(0).getQualifiedName());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");
			final var action = this.builder.getSarlConstructor();
			assertNotNull(action);
			final var modifiers = action.getModifiers();
			assertContains(modifiers, "package");
		}

		@Test
		@DisplayName("addParameter(boolean)")
		public void addParameter_boolean() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean");
			
			final var action = this.builder.getSarlConstructor();
			assertNotNull(action);
			final var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean", param.getParameterType().getQualifiedName());
		}

		@Test
		@DisplayName("addParameter(boolean*)")
		public void addParameter_boolean_variadic() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean[]");
			paramBuilder.setVarArg(true);
			
			final var action = this.builder.getSarlConstructor();
			assertNotNull(action);
			final var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean[]", param.getParameterType().getQualifiedName());
			assertTrue(param.isVarArg());
		}

		@Test
		@DisplayName("addParameter(boolean=true)")
		public void addParameter_boolean_defaultValue() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean");
			paramBuilder.getDefaultValue().setExpression("true");
			
			var action = this.builder.getSarlConstructor();
			assertNotNull(action);
			var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			var param = (SarlFormalParameter) parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean", param.getParameterType().getQualifiedName());
			var defaultValue = param.getDefaultValue();
			var literal = Assertions.assertInstanceOf(XBooleanLiteral.class, defaultValue);
			assertTrue(literal.isIsTrue());
		}

		@Test
		@DisplayName("addParameter(extension boolean)")
		public void addParameter_boolean_extension() {
			final var paramBuilder = this.builder.addParameter("param");
			assertNotNull(paramBuilder);

			paramBuilder.setParameterType("boolean");
			paramBuilder.setExtension(true);
			
			var action = this.builder.getSarlConstructor();
			assertNotNull(action);
			var parameters = action.getParameters();
			assertEquals(1, parameters.size());
			var param = (SarlFormalParameter) parameters.get(0);
			assertEquals("param", param.getName());
			assertEquals("boolean", param.getParameterType().getQualifiedName());
			assertTrue(param.isExtension());
		}

		@Test
		@DisplayName("addTypeParameter(P)")
		public void addTypeParameter_p() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);
			
			final var action = this.builder.getSarlConstructor();
			assertNotNull(action);
			final var parameters = action.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var cst = assertInstanceOf(JvmUpperBound.class, param.getConstraints().get(0));
			assertEquals(Object.class.getName(), cst.getTypeReference().getIdentifier());
		}

		@Test
		@DisplayName("addTypeParameter(P extends Integer)")
		public void addTypeParameter_p_extends_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addUpperConstraint(Integer.class.getName());
			
			final var action = this.builder.getSarlConstructor();
			assertNotNull(action);
			final var parameters = action.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmUpperBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}

		@Test
		@DisplayName("addTypeParameter(P super Integer)")
		public void addTypeParameter_p_super_integer() {
			final var paramBuilder = this.builder.addTypeParameter("P");
			assertNotNull(paramBuilder);

			paramBuilder.addLowerConstraint(Integer.class.getName());
			
			final var action = this.builder.getSarlConstructor();
			assertNotNull(action);
			final var parameters = action.getTypeParameters();
			assertEquals(1, parameters.size());
			final var param = parameters.get(0);
			assertEquals("P", param.getName());
			assertEquals(1, param.getConstraints().size());
			var constraint = param.getConstraints().get(0);
			var upper = Assertions.assertInstanceOf(JvmLowerBound.class, constraint);
			assertEquals(Integer.class.getName(), upper.getTypeReference().getQualifiedName());
		}

		@Test
		@DisplayName("getExpression")
		public void getExpression() {
			final var expression = this.builder.getExpression();
			assertNotNull(expression);
		}

	}

	@DisplayName("var")
	@Nested
	public class VarSarlFieldTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlFieldBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createVarSarlField("abc", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");
			final var action = this.builder.getSarlField();
			assertNotNull(action);
			final var modifiers = action.getModifiers();
			assertContains(modifiers, "package", "var");
		}

		@Test
		@DisplayName("getInitialValue")
		public void getInitialValue() {
			final var expression = this.builder.getInitialValue();
			assertNotNull(expression);
		}

		@Test
		@DisplayName("setType")
		public void setType() {
			this.builder.setType(Number.class.getName());
			
			final var field = this.builder.getSarlField();
			assertNotNull(field);
			final var type = field.getType();
			assertNotNull(type);
			assertEquals(Number.class.getName(), type.getQualifiedName());
		}

	}

	@DisplayName("val")
	@Nested
	public class ValSarlFieldTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlFieldBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createValSarlField("abc", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addModifier")
		public void addModifier() {
			this.builder.addModifier("package");
			final var action = this.builder.getSarlField();
			assertNotNull(action);
			final var modifiers = action.getModifiers();
			assertContains(modifiers, "package", "val");
		}

		@Test
		@DisplayName("getInitialValue")
		public void getInitialValue() {
			final var expression = this.builder.getInitialValue();
			assertNotNull(expression);
		}

		@Test
		@DisplayName("setType")
		public void setType() {
			this.builder.setType(Number.class.getName());
			
			final var field = this.builder.getSarlField();
			assertNotNull(field);
			final var type = field.getType();
			assertNotNull(type);
			assertEquals(Number.class.getName(), type.getQualifiedName());
		}

	}

}

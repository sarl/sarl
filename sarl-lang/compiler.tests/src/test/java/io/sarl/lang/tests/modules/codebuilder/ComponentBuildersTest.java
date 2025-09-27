/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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
import org.eclipse.xtext.common.types.JvmVoid;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.access.impl.ITypeFactory;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.google.inject.Inject;

import foo.ecore.SubAgent;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.builders.IBlockExpressionBuilder;
import io.sarl.lang.codebuilder.builders.IExpressionBuilder;
import io.sarl.lang.codebuilder.builders.ISarlActionBuilder;
import io.sarl.lang.codebuilder.builders.ISarlAgentBuilder;
import io.sarl.lang.codebuilder.builders.ISarlEnumLiteralBuilder;
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
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@DisplayName("Component builders of CodeBuilderFactory")
@SuppressWarnings("all")
@Tag("core")
@Tag("unit")
public class ComponentBuildersTest {

	@DisplayName("Enum literal")
	@Nested
	public class EnumLiteralTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private ISarlEnumLiteralBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createSarlEnumLiteral("abc", this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

	}

	@DisplayName("XBlockExpression")
	@Nested
	public class XBlockExpressionTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		private ResourceSet resource;

		private IBlockExpressionBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createXBlockExpression(this.resource);
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}

		@Test
		@DisplayName("addExpression 1")
		public void addExpression_1() {
			var expr = this.builder.addExpression();
			assertNotNull(expr);
			expr.setExpression("a = 1");

			var block = this.builder.getXBlockExpression();
			assertNotNull(block);
			assertEquals(1, block.getExpressions().size());
			assertSame(expr.getXExpression(), block.getExpressions().get(0));
		}

		@Test
		@DisplayName("addExpression 2")
		public void addExpression_2() {
			var expr1 = this.builder.addExpression();
			assertNotNull(expr1);
			expr1.setExpression("a = 1");
			var expr2 = this.builder.addExpression();
			assertNotNull(expr2);
			expr2.setExpression("a = 1");

			assertNotSame(expr1, expr2);

			var block = this.builder.getXBlockExpression();
			assertNotNull(block);
			assertEquals(2, block.getExpressions().size());
			assertSame(expr1.getXExpression(), block.getExpressions().get(0));
			assertSame(expr2.getXExpression(), block.getExpressions().get(1));
		}

		@Test
		@DisplayName("getAutoGeneratedActionString")
		public void getAutoGeneratedActionString() {
			assertEqualsExceptNewLines("TODO Auto-generated code.", this.builder.getAutoGeneratedActionString());
		}

		@Test
		@DisplayName("setDefaultAutoGeneratedContent(boolean)")
		public void setDefaultAutoGeneratedContent() {
			this.builder.setDefaultAutoGeneratedContent(boolean.class.getName());

			var block = this.builder.getXBlockExpression();
			assertNotNull(block);
			assertEquals(1, block.getExpressions().size());
			var value = Assertions.assertInstanceOf(XBooleanLiteral.class, block.getExpressions().get(0));
			assertFalse(value.isIsTrue());
		}
		
	}

	@DisplayName("XExpression")
	@Nested
	public class XExpressionTest extends AbstractSarlTest {

		@Inject
		private CodeBuilderFactory factory;

		@Inject
		private XbaseFactory typesFactory;

		private ResourceSet resource;

		private IExpressionBuilder builder;

		@BeforeEach
		public void setUp() {
			this.resource = new ResourceSetImpl();
			this.builder = this.factory.createXExpression(this.resource);
			this.builder.setExpression("null");
		}

		@Test
		@DisplayName("eResource")
		public void eResource() {
			assertSame(this.resource, this.builder.eResource().getResourceSet());
		}
		
		@Test
		@DisplayName("createReferenceToSuper")
		public void createReferenceToSuper() {
			var call = this.builder.createReferenceToSuper();
			assertNotNull(call);
			// Event is the super type that is selected from the SARL grammar and that enables
			// the definition of an initial value expression for fields.
			assertEquals(Event.class.getName(), call.getFeature().getIdentifier());
		}
		
		@Test
		@DisplayName("createReferenceToThis")
		public void createReferenceToThis() {
			var call = this.builder.createReferenceToThis();
			assertNotNull(call);
			assertEquals("io.sarl.lang.foo.FooType", call.getFeature().getIdentifier());
		}

		@Test
		@DisplayName("getDefaultValueForType(boolean)")
		public void getDefaultValueForType_boolean() {
			assertEquals("false", this.builder.getDefaultValueForType("boolean"));
		}

		@Test
		@DisplayName("getDefaultValueForType(Boolean)")
		public void getDefaultValueForType_Boolean() {
			assertEquals("false", this.builder.getDefaultValueForType(Boolean.class.getName()));
		}

		@Test
		@DisplayName("getDefaultValueForType(byte)")
		public void getDefaultValueForType_byte() {
			assertEquals("(0 as byte)", this.builder.getDefaultValueForType("byte"));
		}

		@Test
		@DisplayName("getDefaultValueForType(Byte)")
		public void getDefaultValueForType_Byte() {
			assertEquals("(0 as byte)", this.builder.getDefaultValueForType(Byte.class.getName()));
		}

		@Test
		@DisplayName("getDefaultValueForType(short)")
		public void getDefaultValueForType_short() {
			assertEquals("(0 as short)", this.builder.getDefaultValueForType("short"));
		}

		@Test
		@DisplayName("getDefaultValueForType(Short)")
		public void getDefaultValueForType_Short() {
			assertEquals("(0 as short)", this.builder.getDefaultValueForType(Short.class.getName()));
		}

		@Test
		@DisplayName("getDefaultValueForType(int)")
		public void getDefaultValueForType_int() {
			assertEquals("0", this.builder.getDefaultValueForType("int"));
		}

		@Test
		@DisplayName("getDefaultValueForType(Integer)")
		public void getDefaultValueForType_Integer() {
			assertEquals("0", this.builder.getDefaultValueForType(Integer.class.getName()));
		}

		@Test
		@DisplayName("getDefaultValueForType(long)")
		public void getDefaultValueForType_long() {
			assertEquals("0", this.builder.getDefaultValueForType("long"));
		}

		@Test
		@DisplayName("getDefaultValueForType(Long)")
		public void getDefaultValueForType_Long() {
			assertEquals("0", this.builder.getDefaultValueForType(Long.class.getName()));
		}

		@Test
		@DisplayName("getDefaultValueForType(float)")
		public void getDefaultValueForType_float() {
			assertEquals("0.0f", this.builder.getDefaultValueForType("float"));
		}

		@Test
		@DisplayName("getDefaultValueForType(Float)")
		public void getDefaultValueForType_Float() {
			assertEquals("0.0f", this.builder.getDefaultValueForType(Float.class.getName()));
		}

		@Test
		@DisplayName("getDefaultValueForType(double)")
		public void getDefaultValueForType_double() {
			assertEquals("0.0", this.builder.getDefaultValueForType("double"));
		}

		@Test
		@DisplayName("getDefaultValueForType(Double)")
		public void getDefaultValueForType_Double() {
			assertEquals("0.0", this.builder.getDefaultValueForType(Double.class.getName()));
		}

		@Test
		@DisplayName("getDefaultValueForType(char)")
		public void getDefaultValueForType_char() {
			assertEquals("(0 as char)", this.builder.getDefaultValueForType("char"));
		}

		@Test
		@DisplayName("getDefaultValueForType(Character)")
		public void getDefaultValueForType_Character() {
			assertEquals("(0 as char)", this.builder.getDefaultValueForType(Character.class.getName()));
		}

		@Test
		@DisplayName("getDefaultValueForType(String)")
		public void getDefaultValueForType_String() {
			assertEquals("null", this.builder.getDefaultValueForType(String.class.getName()));
		}

		@Test
		@DisplayName("getDefaultValueForType(Object)")
		public void getDefaultValueForType_Object() {
			assertEquals("null", this.builder.getDefaultValueForType(Object.class.getName()));
		}

		@Test
		@DisplayName("getDefaultValueForType(void)")
		public void getDefaultValueForType_void() {
			assertEquals("", this.builder.getDefaultValueForType("void"));
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(boolean)")
		public void getDefaultXExpressionForType_boolean() {
			var expr = this.builder.getDefaultXExpressionForType("boolean");
			assertNotNull(expr);
			var value = Assertions.assertInstanceOf(XBooleanLiteral.class, expr);
			assertFalse(value.isIsTrue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(Boolean)")
		public void getDefaultXExpressionForType_Boolean() {
			var expr = this.builder.getDefaultXExpressionForType(Boolean.class.getName());
			assertNotNull(expr);
			var value = Assertions.assertInstanceOf(XBooleanLiteral.class, expr);
			assertFalse(value.isIsTrue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(byte)")
		public void getDefaultXExpressionForType_byte() {
			var expr = this.builder.getDefaultXExpressionForType("byte");
			assertNotNull(expr);
			var cst = Assertions.assertInstanceOf(XCastedExpression.class, expr);
			assertEquals(byte.class.getName(), cst.getType().getIdentifier());
			var cexpr = Assertions.assertInstanceOf(XNumberLiteral.class, cst.getTarget());
			assertEquals("0", cexpr.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(Byte)")
		public void getDefaultXExpressionForType_Byte() {
			var expr = this.builder.getDefaultXExpressionForType(Byte.class.getName());
			assertNotNull(expr);
			var cst = Assertions.assertInstanceOf(XCastedExpression.class, expr);
			assertEquals(byte.class.getName(), cst.getType().getIdentifier());
			var cexpr = Assertions.assertInstanceOf(XNumberLiteral.class, cst.getTarget());
			assertEquals("0", cexpr.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(short)")
		public void getDefaultXExpressionForType_short() {
			var expr = this.builder.getDefaultXExpressionForType("short");
			assertNotNull(expr);
			var cst = Assertions.assertInstanceOf(XCastedExpression.class, expr);
			assertEquals(short.class.getName(), cst.getType().getIdentifier());
			var cexpr = Assertions.assertInstanceOf(XNumberLiteral.class, cst.getTarget());
			assertEquals("0", cexpr.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(Short)")
		public void getDefaultXExpressionForType_Short() {
			var expr = this.builder.getDefaultXExpressionForType(Short.class.getName());
			assertNotNull(expr);
			var cst = Assertions.assertInstanceOf(XCastedExpression.class, expr);
			assertEquals(short.class.getName(), cst.getType().getIdentifier());
			var cexpr = Assertions.assertInstanceOf(XNumberLiteral.class, cst.getTarget());
			assertEquals("0", cexpr.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(int)")
		public void getDefaultXExpressionForType_int() {
			var expr = this.builder.getDefaultXExpressionForType("int");
			assertNotNull(expr);
			var value = Assertions.assertInstanceOf(XNumberLiteral.class, expr);
			assertEquals("0", value.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(Integer)")
		public void getDefaultXExpressionForType_Integer() {
			var expr = this.builder.getDefaultXExpressionForType(Integer.class.getName());
			assertNotNull(expr);
			var value = Assertions.assertInstanceOf(XNumberLiteral.class, expr);
			assertEquals("0", value.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(long)")
		public void getDefaultXExpressionForType_long() {
			var expr = this.builder.getDefaultXExpressionForType("long");
			assertNotNull(expr);
			var value = Assertions.assertInstanceOf(XNumberLiteral.class, expr);
			assertEquals("0", value.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(Long)")
		public void getDefaultXExpressionForType_Long() {
			var expr = this.builder.getDefaultXExpressionForType(Long.class.getName());
			assertNotNull(expr);
			var value = Assertions.assertInstanceOf(XNumberLiteral.class, expr);
			assertEquals("0", value.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(Float)")
		public void getDefaultXExpressionForType_float() {
			var expr = this.builder.getDefaultXExpressionForType("float");
			assertNotNull(expr);
			var value = Assertions.assertInstanceOf(XNumberLiteral.class, expr);
			assertEquals("0.0f", value.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(Float)")
		public void getDefaultXExpressionForType_Float() {
			var expr = this.builder.getDefaultXExpressionForType(Float.class.getName());
			assertNotNull(expr);
			var value = Assertions.assertInstanceOf(XNumberLiteral.class, expr);
			assertEquals("0.0f", value.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(double)")
		public void getDefaultXExpressionForType_double() {
			var expr = this.builder.getDefaultXExpressionForType("double");
			assertNotNull(expr);
			var value = Assertions.assertInstanceOf(XNumberLiteral.class, expr);
			assertEquals("0.0", value.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(Double)")
		public void getDefaultXExpressionForType_Double() {
			var expr = this.builder.getDefaultXExpressionForType(Double.class.getName());
			assertNotNull(expr);
			var value = Assertions.assertInstanceOf(XNumberLiteral.class, expr);
			assertEquals("0.0", value.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(char)")
		public void getDefaultXExpressionForType_char() {
			var expr = this.builder.getDefaultXExpressionForType("char");
			assertNotNull(expr);
			var cst = Assertions.assertInstanceOf(XCastedExpression.class, expr);
			assertEquals(char.class.getName(), cst.getType().getIdentifier());
			var cexpr = Assertions.assertInstanceOf(XNumberLiteral.class, cst.getTarget());
			assertEquals("0", cexpr.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(Character)")
		public void getDefaultXExpressionForType_Character() {
			var expr = this.builder.getDefaultXExpressionForType(Character.class.getName());
			assertNotNull(expr);
			var cst = Assertions.assertInstanceOf(XCastedExpression.class, expr);
			assertEquals(char.class.getName(), cst.getType().getIdentifier());
			var cexpr = Assertions.assertInstanceOf(XNumberLiteral.class, cst.getTarget());
			assertEquals("0", cexpr.getValue());
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(String)")
		public void getDefaultXExpressionForType_String() {
			var expr = this.builder.getDefaultXExpressionForType(String.class.getName());
			assertNotNull(expr);
			Assertions.assertInstanceOf(XNullLiteral.class, expr);
		}

		@Test
		@DisplayName("getDefaultXExpressionForType(Object)")
		public void getDefaultXExpressionForType_Object() {
			var expr = this.builder.getDefaultXExpressionForType(Object.class.getName());
			assertNotNull(expr);
			Assertions.assertInstanceOf(XNullLiteral.class, expr);
		}

		@Test
		@DisplayName("setExpression(\"2\")")
		public void setExpression_1() {
			this.builder.setExpression("2");
			
			var expr = this.builder.getXExpression();
			assertNotNull(expr);
			var literal = Assertions.assertInstanceOf(XNumberLiteral.class, expr);
			assertEquals("2", literal.getValue());
		}

		@Test
		@DisplayName("setExpression(\"b = null\")")
		public void setExpression_2() {
			this.builder.setExpression("b = null");
			
			var expr = this.builder.getXExpression();
			assertNotNull(expr);
			var assign = Assertions.assertInstanceOf(XAssignment.class, expr);

			Assertions.assertInstanceOf(XNullLiteral.class, assign.getValue());
		}
		
		@Test
		@DisplayName("setXExpression(null)")
		public void setXExpression_null() {
			var literal0 = this.typesFactory.createXNullLiteral();
			this.builder.setXExpression(literal0);
			
			var expr = this.builder.getXExpression();
			assertNotNull(expr);
			var literal1 = Assertions.assertInstanceOf(XNullLiteral.class, expr);
			assertSame(literal0, literal1);
		}
		
	}

}

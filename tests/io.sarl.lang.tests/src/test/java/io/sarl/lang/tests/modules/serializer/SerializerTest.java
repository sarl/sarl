/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
package io.sarl.lang.tests.modules.serializer;

import static org.junit.Assert.assertEquals;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.resource.SaveOptions;
import org.eclipse.xtext.serializer.ISerializer;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.tests.api.AbstractSarlTest;

/** This class tests the {@link ISerializer} for SARL.
 * The serializer is creating a string from SARL Ecore elements.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	SerializerTest.Events.class,
	SerializerTest.Agents.class,
	SerializerTest.Behaviors.class,
	SerializerTest.Capacities.class,
	SerializerTest.Skills.class,
	SerializerTest.Values.class,
	SerializerTest.Variables.class,
	SerializerTest.Constructors.class,
	SerializerTest.Actions.class,
	SerializerTest.ExceptionActions.class,
	SerializerTest.EventActions.class,
	SerializerTest.ExceptionEventActions.class,
	SerializerTest.EventExceptionActions.class,
	SerializerTest.BehaviorUnits.class,
	SerializerTest.CapacityUses.class,
	SerializerTest.RequireCapacities.class,
})
@SuppressWarnings("all")
public class SerializerTest {
	
	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static abstract class AbstractSerializerTest extends AbstractSarlTest {
		
		@Inject
		protected ISerializer serializer;

		protected EObject object;
		
		protected void assertSerialize(String expected) {
			SaveOptions.Builder builder = SaveOptions.newBuilder();
			// No formatting
			//builder.format();
			String text = serializer.serialize(object, builder.getOptions());
			assertEquals(expected, text);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Events extends AbstractSerializerTest {

		@Test
		public void empty_noBlock_noSuper() throws Exception {
			String s = "event Foo";
			this.object = event(s, true);
			assertSerialize(s);
		}

		@Test
		public void empty_block_noSuper() throws Exception {
			String s = "event Foo { }";
			this.object = event(s, true);
			assertSerialize(s);
		}

		@Test
		public void empty_noBlock_super() throws Exception {
			String s = "event Foo extends foo.ecore.SubEvent";
			this.object = event(s, true);
			assertSerialize(s);
		}

		@Test
		public void empty_block_super() throws Exception {
			String s = "event Foo extends foo.ecore.SubEvent { }";
			this.object = event(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Agents extends AbstractSerializerTest {

		@Test
		public void empty_noSuper() throws Exception {
			String s = "agent Foo { }";
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void empty_super() throws Exception {
			String s = "agent Foo extends foo.ecore.SubAgent { }";
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Behaviors extends AbstractSerializerTest {

		@Test
		public void empty_noSuper() throws Exception {
			String s = "behavior Foo { }";
			this.object = behavior(s, true);
			assertSerialize(s);
		}

		@Test
		public void empty_super() throws Exception {
			String s = "behavior Foo extends foo.ecore.SubBehavior { }";
			this.object = behavior(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Capacities extends AbstractSerializerTest {

		@Test
		public void empty_noSuper() throws Exception {
			String s = "capacity Foo { }";
			this.object = capacity(s, true);
			assertSerialize(s);
		}

		@Test
		public void empty_super() throws Exception {
			String s = "capacity Foo extends foo.ecore.SubCapacity { }";
			this.object = capacity(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Skills extends AbstractSerializerTest {

		@Test
		public void empty_noSuper() throws Exception {
			String s = "skill Foo implements foo.ecore.SubCapacity { }";
			this.object = skill(s, true);
			assertSerialize(s);
		}

		@Test
		public void empty_super() throws Exception {
			String s = "skill Foo extends foo.ecore.SubSkill implements foo.ecore.SubCapacity { }";
			this.object = skill(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Values extends AbstractSerializerTest {

		@Test
		public void value_expr_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"val foo = 6.0f",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void value_typeExpr_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"val foo : float = 6.0f",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void value_expr_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"val foo = 6.0f",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void value_typeExpr_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"val foo : float = 6.0f",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Variables extends AbstractSerializerTest {

		@Test
		public void variable_expr_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"var foo = 6.0f",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void variable_type_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"var foo : float",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void variable_typeExpr_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"var foo : float = 6.0f",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void variable_expr_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"var foo = 6.0f",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void variable_type_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"var foo : float",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void variable_typeExpr_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"var foo : float = 6.0f",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Constructors extends AbstractSerializerTest {

		@Test
		public void noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"new() { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParDelim_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"new { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"new(b : int) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void params_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"new(b : int, c : float) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"new(b : int = 5) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noSuper_0() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"new(b : int = 6, c : float) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noSuper_1() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"new(b : int, c : float = 5.9f) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noSuper_2() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"new(b : int = 8, c : float = 5.9f) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"new(b : int, c : float*) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDefVar_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"new(b : int = 6, c : float*) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParDelim_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"new { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"new(b : int) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void params_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"new(b : int, c : float) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"new(b : int = 5) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_super_0() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"new(b : int = 6, c : float) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_super_1() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"new(b : int, c : float = 5.9f) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_super_2() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"new(b : int = 8, c : float = 5.9f) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"new(b : int, c : float*) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDefVar_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"new(b : int = 6, c : float*) { super(null, null) }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Actions extends AbstractSerializerTest {

		@Test
		public void noPar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct() { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int) { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int*) { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noPar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct() : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int) : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int*) : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}
		
		@Test
		public void noPar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noPar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) : float { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ExceptionActions extends AbstractSerializerTest {

		@Test
		public void noPar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct() throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int) throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int*) throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noPar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct() : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int) : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int*) : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}
		
		@Test
		public void noPar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noPar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) : float throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}
	
	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class EventActions extends AbstractSerializerTest {

		@Test
		public void noPar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct() fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int) fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int*) fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noPar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct() : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int) : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int*) : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}
		
		@Test
		public void noPar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noPar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) : float fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ExceptionEventActions extends AbstractSerializerTest {

		@Test
		public void noPar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct() throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int) throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int*) throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noPar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct() : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int) : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int*) : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}
		
		@Test
		public void noPar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noPar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void noParam_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void param_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramDef_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramVar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsDef_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void paramsVar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) : float throws Exception fires foo.Event1, foo.Event2 { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class EventExceptionActions extends AbstractSerializerTest {

		@Test
		public void noPar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void noParam_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct() fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct() fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void param_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct(a : int) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramDef_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramVar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int*) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct(a : int*) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramsDef_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramsVar_noReturn_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void noPar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void noParam_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct() : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct() : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void param_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct(a : int) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramDef_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct(a : int = 5) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramVar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int*) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct(a : int*) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramsDef_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramsVar_return_noSuper() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo {",
					"def fct(a : int = 6, b : float*) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}
		
		@Test
		public void noPar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void noParam_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void param_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramDef_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramVar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramsDef_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramsVar_noReturn_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void noPar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void noParam_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct() : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void param_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramDef_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 5) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramVar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int*) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramsDef_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

		@Test
		public void paramsVar_return_super() throws Exception {
			String s = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			String e = multilineString(
					"agent Foo extends foo.ecore.SubAgent {",
					"def fct(a : int = 6, b : float*) : float fires foo.Event1, foo.Event2 throws Exception { 1 }",
					"}");
			this.object = agent(s, true);
			assertSerialize(e);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class BehaviorUnits extends AbstractSerializerTest {

		@Test
		public void noGuard() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"on foo.ecore.SubEvent { println(\"hello world\") }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void trueGuard() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"on foo.ecore.SubEvent [ true ] { println(\"hello world\") }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void falseGuard() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"on foo.ecore.SubEvent [ false ] { println(\"hello world\") }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void generalGuard() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"on foo.ecore.SubEvent [ occurrence.isFromMe ] { println(\"hello world\") }",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class CapacityUses extends AbstractSerializerTest {

		@Test
		public void one_one() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"uses foo.ecore.SubCapacity",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void one_two() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"uses foo.ecore.SubCapacity, foo.ecore.SubCapacity2",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void two_one() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"uses foo.ecore.SubCapacity",
					"uses foo.ecore.SubCapacity2",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void two_two() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"uses foo.ecore.SubCapacity, foo.ecore.SubCapacity2",
					"uses foo.ecore.SubCapacity3",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class RequireCapacities extends AbstractSerializerTest {

		@Test
		public void one_one() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"requires foo.ecore.SubCapacity",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void one_two() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"requires foo.ecore.SubCapacity, foo.ecore.SubCapacity2",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void two_one() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"requires foo.ecore.SubCapacity",
					"requires foo.ecore.SubCapacity2",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

		@Test
		public void two_two() throws Exception {
			String s = multilineString(
					"agent Foo {",
					"requires foo.ecore.SubCapacity, foo.ecore.SubCapacity2",
					"requires foo.ecore.SubCapacity3",
					"}");
			this.object = agent(s, true);
			assertSerialize(s);
		}

	}

}

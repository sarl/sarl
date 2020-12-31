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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.modules.actionprototype;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("ActionPrototype")
@Tag("core")
@Tag("unit")
public class ActionPrototypeTest {

	private ActionParameterTypes parameters;

	private ActionPrototype prototype;

	@BeforeEach
	public void setUp() {
		this.parameters = new ActionParameterTypes("int,float,java.lang.String*");
		this.prototype = new ActionPrototype("myfct", this.parameters, false);
	}

	@Test
	public void getActionName() {
		assertEquals("myfct", this.prototype.getActionName());
	}

	@Test
	public void getParametersTypes() {
		assertSame(this.parameters, this.prototype.getParametersTypes());
	}

	@Test
	public void testClone() {
		ActionPrototype c = this.prototype.clone();
		assertNotSame(this.prototype, c);
		assertEquals("myfct", c.getActionName());
		assertNotSame(this.parameters, c.getParametersTypes());
		assertEquals(this.parameters, c.getParametersTypes());
	}

	@Test
	public void testEquals_0() {
		assertTrue(this.prototype.equals(this.prototype));
	}

	@Test
	public void testEquals_1() {
		assertFalse(this.prototype.equals(null));
	}

	@Test
	public void testEquals_2() {
		ActionPrototype c = this.prototype.clone();
		assertTrue(this.prototype.equals(c));
		assertTrue(c.equals(this.prototype));
	}

	@Test
	public void testEquals_3() {
		ActionParameterTypes params = new ActionParameterTypes("int,float,java.lang.String*");
		ActionPrototype proto = new ActionPrototype("myfct", params, false);
		assertTrue(this.prototype.equals(proto));
		assertTrue(proto.equals(this.prototype));
	}

	@Test
	public void testEquals_4() {
		ActionParameterTypes params = new ActionParameterTypes("int,float");
		ActionPrototype proto = new ActionPrototype("myfct", params, false);
		assertFalse(this.prototype.equals(proto));
		assertFalse(proto.equals(this.prototype));
	}

	@Test
	public void testEquals_5() {
		ActionParameterTypes params = new ActionParameterTypes("int,float,java.lang.String*");
		ActionPrototype proto = new ActionPrototype("myfct2", params, false);
		assertFalse(this.prototype.equals(proto));
		assertFalse(proto.equals(this.prototype));
	}

	@Test
	public void testHashCode_0() {
		assertEquals(this.prototype.hashCode(), this.prototype.hashCode());
	}

	@Test
	public void testHashCode_1() {
		ActionPrototype c = this.prototype.clone();
		assertEquals(this.prototype.hashCode(), c.hashCode());
	}

	@Test
	public void testHashCode_2() {
		ActionParameterTypes params = new ActionParameterTypes("int,float,java.lang.String*");
		ActionPrototype proto = new ActionPrototype("myfct", params, false);
		assertEquals(this.prototype.hashCode(), proto.hashCode());
	}

	@Test
	public void testHashCode_3() {
		ActionParameterTypes params = new ActionParameterTypes("int,float");
		ActionPrototype proto = new ActionPrototype("myfct", params, false);
		assertNotEquals(this.prototype.hashCode(), proto.hashCode());
	}

	@Test
	public void testHashCode_5() {
		ActionParameterTypes params = new ActionParameterTypes("int,float,java.lang.String*");
		ActionPrototype proto = new ActionPrototype("myfct2", params, false);
		assertNotEquals(this.prototype.hashCode(), proto.hashCode());
	}

	@Test
	public void testToString() {
		assertEquals("myfct(int,float,java.lang.String*)", this.prototype.toString());
	}

	@Test
	public void compareTo_0() {
		assertEquals(0, this.prototype.compareTo(this.prototype));
	}

	@Test
	public void compareTo_1() {
		assertEquals(Integer.MAX_VALUE, this.prototype.compareTo(null));
	}

	@Test
	public void compareTo_2() {
		ActionPrototype c = this.prototype.clone();
		assertEquals(0, this.prototype.compareTo(c));
		assertEquals(0, c.compareTo(this.prototype));
	}

	@Test
	public void compareTo_3() {
		ActionParameterTypes params = new ActionParameterTypes("int,float,java.lang.String*");
		ActionPrototype proto = new ActionPrototype("myfct", params, false);
		assertEquals(0, this.prototype.compareTo(proto));
		assertEquals(0, proto.compareTo(this.prototype));
	}

	@Test
	public void compareTo_4() {
		ActionParameterTypes params = new ActionParameterTypes("int,float");
		ActionPrototype proto = new ActionPrototype("myfct", params, false);
		assertEquals(1, this.prototype.compareTo(proto));
		assertEquals(-1, proto.compareTo(this.prototype));
	}

	@Test
	public void compareTo_5() {
		ActionParameterTypes params = new ActionParameterTypes("int,float,java.lang.String*");
		ActionPrototype proto = new ActionPrototype("myfct2", params, false);
		assertEquals(-1, this.prototype.compareTo(proto));
		assertEquals(1, proto.compareTo(this.prototype));
	}

	@Test
	public void toActionID() {
		assertEquals(
				"myfct_int_float_javalangStringArray",
				this.prototype.toActionId());
	}

}

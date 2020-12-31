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
import static org.junit.jupiter.api.Assertions.assertTrue;

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
@DisplayName("ActionParameterTypes")
@Tag("core")
@Tag("unit")
public class ActionParameterTypesTest {

	@Test
	public void ActionParameterTypesBooleanInt_true() {
		ActionParameterTypes types = new ActionParameterTypes(true, 5);
		assertTrue(types.isVarArg());
		assertTrue(types.isVoid());
		assertEquals(0, types.size());
	}

	@Test
	public void ActionParameterTypesBooleanInt_false() {
		ActionParameterTypes types = new ActionParameterTypes(false, 5);
		assertFalse(types.isVarArg());
		assertTrue(types.isVoid());
		assertEquals(0, types.size());
	}

	@Test
	public void ActionParameterTypesTestString_empty() {
		ActionParameterTypes types = new ActionParameterTypes("");
		assertFalse(types.isVarArg());
		assertTrue(types.isVoid());
		assertEquals(0, types.size());
	}

	@Test
	public void ActionParameterTypesTestString_int() {
		ActionParameterTypes types = new ActionParameterTypes("int");
		assertFalse(types.isVarArg());
		assertFalse(types.isVoid());
		assertEquals(1, types.size());
		assertEquals("int", types.get(0));
	}

	@Test
	public void ActionParameterTypesTestString_intString() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String");
		assertFalse(types.isVarArg());
		assertFalse(types.isVoid());
		assertEquals(2, types.size());
		assertEquals("int", types.get(0));
		assertEquals("java.lang.String", types.get(1));
	}

	@Test
	public void ActionParameterTypesTestString_intStringVarArg() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String*");
		assertTrue(types.isVarArg());
		assertFalse(types.isVoid());
		assertEquals(2, types.size());
		assertEquals("int", types.get(0));
		assertEquals("java.lang.String[]", types.get(1));
	}

	@Test
	public void testEquals_0() {
		ActionParameterTypes types0 = new ActionParameterTypes("int,java.lang.String*");
		ActionParameterTypes types1 = new ActionParameterTypes("int,java.lang.String*");
		assertTrue(types0.equals(types1));
		assertTrue(types1.equals(types0));
	}

	@Test
	public void testEquals_1() {
		ActionParameterTypes types0 = new ActionParameterTypes("int,java.lang.String*");
		ActionParameterTypes types1 = new ActionParameterTypes("int,java.lang.String[]");
		assertFalse(types0.equals(types1));
		assertFalse(types1.equals(types0));
	}

	@Test
	public void testEquals_2() {
		ActionParameterTypes types0 = new ActionParameterTypes("int,java.lang.String*");
		ActionParameterTypes types1 = new ActionParameterTypes("int");
		assertFalse(types0.equals(types1));
		assertFalse(types1.equals(types0));
	}

	@Test
	public void testEquals_3() {
		ActionParameterTypes types0 = new ActionParameterTypes("int,java.lang.String*");
		assertFalse(types0.equals(null));
	}

	@Test
	public void testHashCode_0() {
		ActionParameterTypes types0 = new ActionParameterTypes("int,java.lang.String*");
		ActionParameterTypes types1 = new ActionParameterTypes("int,java.lang.String*");
		assertEquals(types0.hashCode(), types1.hashCode());
	}

	@Test
	public void testHashCode_1() {
		ActionParameterTypes types0 = new ActionParameterTypes("int,java.lang.String*");
		ActionParameterTypes types1 = new ActionParameterTypes("int,java.lang.String[]");
		assertNotEquals(types0.hashCode(), types1.hashCode());
	}

	@Test
	public void isVarArg_0() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String*");
		assertTrue(types.isVarArg());
	}

	@Test
	public void isVarArg_1() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String[]");
		assertFalse(types.isVarArg());
	}

	@Test
	public void isVarArg_2() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String");
		assertFalse(types.isVarArg());
	}

	@Test
	public void isVarArg_3() {
		ActionParameterTypes types = new ActionParameterTypes("");
		assertFalse(types.isVarArg());
	}

	@Test
	public void isVoid_0() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String*");
		assertFalse(types.isVoid());
	}

	@Test
	public void isVoid_1() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String[]");
		assertFalse(types.isVoid());
	}

	@Test
	public void isVoid_2() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String");
		assertFalse(types.isVoid());
	}

	@Test
	public void isVoid_3() {
		ActionParameterTypes types = new ActionParameterTypes("");
		assertTrue(types.isVoid());
	}

	@Test
	public void testClone_0() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String*");
		ActionParameterTypes c = types.clone();
		assertNotSame(types, c);
		assertEquals(types, c);
	}

	@Test
	public void testClone_1() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String[]");
		ActionParameterTypes c = types.clone();
		assertNotSame(types, c);
		assertEquals(types, c);
	}

	@Test
	public void testClone_2() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String");
		ActionParameterTypes c = types.clone();
		assertNotSame(types, c);
		assertEquals(types, c);
	}

	@Test
	public void testClone_3() {
		ActionParameterTypes types = new ActionParameterTypes("");
		ActionParameterTypes c = types.clone();
		assertNotSame(types, c);
		assertEquals(types, c);
	}

	@Test
	public void compareTo_0() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String*");
		assertEquals(0, types.compareTo(types));
	}

	@Test
	public void compareTo_1() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String*");
		assertEquals(Integer.MAX_VALUE, types.compareTo(null));
	}

	@Test
	public void compareTo_2() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String*");
		ActionParameterTypes c = types.clone();
		assertEquals(0, types.compareTo(c));
		assertEquals(0, c.compareTo(types));
	}

	@Test
	public void compareTo_3() {
		ActionParameterTypes types0 = new ActionParameterTypes("int,java.lang.String*");
		ActionParameterTypes types1 = new ActionParameterTypes("int,java.lang.String*");
		assertEquals(0, types0.compareTo(types1));
		assertEquals(0, types1.compareTo(types0));
	}

	@Test
	public void compareTo_4() {
		ActionParameterTypes types0 = new ActionParameterTypes("int,java.lang.String*");
		ActionParameterTypes types1 = new ActionParameterTypes("int*");
		assertEquals(1, types0.compareTo(types1));
		assertEquals(-1, types1.compareTo(types0));
	}

	@Test
	public void compareTo_5() {
		ActionParameterTypes types0 = new ActionParameterTypes("int,java.lang.String*");
		ActionParameterTypes types1 = new ActionParameterTypes("int, int*");
		assertEquals(1, types0.compareTo(types1));
		assertEquals(-1, types1.compareTo(types0));
	}

	@Test
	public void toActionPrototype_0() {
		ActionParameterTypes types = new ActionParameterTypes("int,java.lang.String*");
		ActionPrototype prototype = types.toActionPrototype("myfct");
		assertEquals("myfct(int,java.lang.String*)", prototype.toString());
	}

	@Test
	public void toActionPrototype_1() {
		ActionParameterTypes types = new ActionParameterTypes("");
		ActionPrototype prototype = types.toActionPrototype("myfct");
		assertEquals("myfct()", prototype.toString());
	}

}

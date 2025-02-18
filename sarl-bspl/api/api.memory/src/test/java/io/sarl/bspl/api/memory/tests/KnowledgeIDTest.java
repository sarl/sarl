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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.bspl.api.memory.tests;

import static io.sarl.tests.api.tools.TestAssertions.assertContains;
import static io.sarl.tests.api.tools.TestAssertions.*;
import static io.sarl.tests.api.tools.TestAssertions.assertStrictlyPositive;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.bspl.api.memory.KnowledgeID;

/**
 * Test of {@code KnowledgeID}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@DisplayName("KnowledgeID")
@Tag("unit")
@Tag("api")
@SuppressWarnings("all")
public class KnowledgeIDTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	@DisplayName("Getters")
	@Nested
	public class GettersTest {

		private KnowledgeID test;

		@BeforeEach
		public void setUp() {
			this.test = new KnowledgeID("abc", "keyA", "keyB", "keyC");
		}
	
		@Test
		@DisplayName("getName")
		public void getName() {
			assertEquals("abc", this.test.getName());
		}
	
		@Test
		@DisplayName("getKeys")
		public void getKeys() {
			var k = this.test.getKeys();
			assertNotNull(k);
			assertEquals(3, k.length);
			assertContains(Arrays.asList(k), "keyA", "keyB", "keyC");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	@DisplayName("compareTo")
	@Nested
	public class CompareToTest {

		private KnowledgeID test;

		@BeforeEach
		public void setUp() {
			this.test = new KnowledgeID("abc", "keyA", "keyB", "keyC");
		}

		@Test
		@DisplayName("this")
		public void compareTo_this() {
			assertEquals(0, this.test.compareTo(this.test));
		}
	
		@Test
		@DisplayName("null")
		public void compareTo_null() {
			assertStrictlyPositive(this.test.compareTo(null));
		}
	
		@Test
		@DisplayName("this.name < n0")
		public void compareTo_nameLower() {
			var id0 = new KnowledgeID("acc", "keyA", "keyB", "keyC");
			assertStrictlyNegative(this.test.compareTo(id0));
		}
	
		@Test
		@DisplayName("this.name > n0")
		public void compareTo_nameUpper() {
			var id0 = new KnowledgeID("aac", "keyA", "keyB", "keyC");
			assertStrictlyPositive(this.test.compareTo(id0));
		}
	
		@Test
		@DisplayName("this.key0 < k0")
		public void compareTo_key0Lower() {
			var id0 = new KnowledgeID("abc", "keyB", "keyB", "keyC");
			assertStrictlyNegative(this.test.compareTo(id0));
		}
	
		@Test
		@DisplayName("this.key0 > k0")
		public void compareTo_key0Upper() {
			var id0 = new KnowledgeID("abc", "key", "keyB", "keyC");
			assertStrictlyPositive(this.test.compareTo(id0));
		}
	
		@Test
		@DisplayName("this.key1 < k1")
		public void compareTo_key1Lower() {
			var id0 = new KnowledgeID("abc", "keyA", "keyC", "keyC");
			assertStrictlyNegative(this.test.compareTo(id0));
		}
	
		@Test
		@DisplayName("this.key1 > k1")
		public void compareTo_key1Upper() {
			var id0 = new KnowledgeID("abc", "keyA", "keyA", "keyC");
			assertStrictlyPositive(this.test.compareTo(id0));
		}
	
		@Test
		@DisplayName("this.key2 < k2")
		public void compareTo_key2Lower() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyD");
			assertStrictlyNegative(this.test.compareTo(id0));
		}
	
		@Test
		@DisplayName("this.key2 > k2")
		public void compareTo_key2Upper() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyB");
			assertStrictlyPositive(this.test.compareTo(id0));
		}
	
		@Test
		@DisplayName("equals")
		public void compareTo_equals() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyC");
			assertEquals(0, this.test.compareTo(id0));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	@DisplayName("this <=> ?")
	@Nested
	public class SpaceshipTest {

		private KnowledgeID test;

		@BeforeEach
		public void setUp() {
			this.test = new KnowledgeID("abc", "keyA", "keyB", "keyC");
		}

		@Test
		@DisplayName("this <=> this")
		public void operatorSpaceship_this() {
			assertEquals(0, this.test.operator_spaceship(this.test));
		}
	
		@Test
		@DisplayName("this <=> null")
		public void operatorSpaceship_null() {
			assertStrictlyPositive(this.test.operator_spaceship(null));
		}
	
		@Test
		@DisplayName("this <=> id - this.name < n0")
		public void operatorSpaceship_nameLower() {
			var id0 = new KnowledgeID("acc", "keyA", "keyB", "keyC");
			assertStrictlyNegative(this.test.operator_spaceship(id0));
		}
	
		@Test
		@DisplayName("this <=> id - this.name > n0")
		public void operatorSpaceship_nameUpper() {
			var id0 = new KnowledgeID("aac", "keyA", "keyB", "keyC");
			assertStrictlyPositive(this.test.operator_spaceship(id0));
		}
	
		@Test
		@DisplayName("this <=> id - this.key0 < k0")
		public void operatorSpaceship_key0Lower() {
			var id0 = new KnowledgeID("abc", "keyB", "keyB", "keyC");
			assertStrictlyNegative(this.test.operator_spaceship(id0));
		}
	
		@Test
		@DisplayName("this <=> id - this.key0 > k0")
		public void operatorSpaceship_key0Upper() {
			var id0 = new KnowledgeID("abc", "key", "keyB", "keyC");
			assertStrictlyPositive(this.test.operator_spaceship(id0));
		}
	
		@Test
		@DisplayName("this <=> id - this.key1 < k1")
		public void operatorSpaceship_key1Lower() {
			var id0 = new KnowledgeID("abc", "keyA", "keyC", "keyC");
			assertStrictlyNegative(this.test.operator_spaceship(id0));
		}
	
		@Test
		@DisplayName("this <=> id - this.key1 > k1")
		public void operatorSpaceship_key1Upper() {
			var id0 = new KnowledgeID("abc", "keyA", "keyA", "keyC");
			assertStrictlyPositive(this.test.operator_spaceship(id0));
		}
	
		@Test
		@DisplayName("this <=> id - this.key2 < k2")
		public void operatorSpaceship_key2Lower() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyD");
			assertStrictlyNegative(this.test.operator_spaceship(id0));
		}
	
		@Test
		@DisplayName("this <=> id - this.key2 > k2")
		public void operatorSpaceship_key2Upper() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyB");
			assertStrictlyPositive(this.test.operator_spaceship(id0));
		}
	
		@Test
		@DisplayName("this <=> id === 0")
		public void operatorSpaceship_equals() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyC");
			assertEquals(0, this.test.operator_spaceship(id0));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	@DisplayName("equals")
	@Nested
	public class EqualsTest {

		private KnowledgeID test;

		@BeforeEach
		public void setUp() {
			this.test = new KnowledgeID("abc", "keyA", "keyB", "keyC");
		}

		@Test
		@DisplayName("this")
		public void equals_this() {
			assertTrue(this.test.equals(this.test));
		}
	
		@Test
		@DisplayName("null")
		public void equals_null() {
			assertFalse(this.test.equals(null));
		}
	
		@Test
		@DisplayName("this.name < n0")
		public void equals_nameLower() {
			var id0 = new KnowledgeID("acc", "keyA", "keyB", "keyC");
			assertFalse(this.test.equals(id0));
		}
	
		@Test
		@DisplayName("this.name > n0")
		public void equals_nameUpper() {
			var id0 = new KnowledgeID("aac", "keyA", "keyB", "keyC");
			assertFalse(this.test.equals(id0));
		}
	
		@Test
		@DisplayName("this.key0 < k0")
		public void equals_key0Lower() {
			var id0 = new KnowledgeID("abc", "keyB", "keyB", "keyC");
			assertFalse(this.test.equals(id0));
		}
	
		@Test
		@DisplayName("this.key0 > k0")
		public void equals_key0Upper() {
			var id0 = new KnowledgeID("abc", "key", "keyB", "keyC");
			assertFalse(this.test.equals(id0));
		}
	
		@Test
		@DisplayName("this.key1 < k1")
		public void equals_key1Lower() {
			var id0 = new KnowledgeID("abc", "keyA", "keyC", "keyC");
			assertFalse(this.test.equals(id0));
		}
	
		@Test
		@DisplayName("this.key1 > k1")
		public void equals_key1Upper() {
			var id0 = new KnowledgeID("abc", "keyA", "keyA", "keyC");
			assertFalse(this.test.equals(id0));
		}
	
		@Test
		@DisplayName("this.key2 < k2")
		public void equals_key2Lower() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyD");
			assertFalse(this.test.equals(id0));
		}
	
		@Test
		@DisplayName("this.key2 > k2")
		public void equals_key2Upper() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyB");
			assertFalse(this.test.equals(id0));
		}
	
		@Test
		@DisplayName("equals")
		public void equals_equals() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyC");
			assertTrue(this.test.equals(id0));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	@DisplayName("hashCode")
	@Nested
	public class HashCodeTest {

		private KnowledgeID test;

		@BeforeEach
		public void setUp() {
			this.test = new KnowledgeID("abc", "keyA", "keyB", "keyC");
		}

		@Test
		@DisplayName("this")
		public void hashCode_this() {
			assertNotZero(this.test.hashCode());
		}
	
		@Test
		@DisplayName("this.name < n0")
		public void equals_nameLower() {
			var id0 = new KnowledgeID("acc", "keyA", "keyB", "keyC");
			assertNotEquals(this.test.hashCode(), id0.hashCode());
		}
	
		@Test
		@DisplayName("this.name > n0")
		public void equals_nameUpper() {
			var id0 = new KnowledgeID("aac", "keyA", "keyB", "keyC");
			assertNotEquals(this.test.hashCode(), id0.hashCode());
		}
	
		@Test
		@DisplayName("this.key0 < k0")
		public void equals_key0Lower() {
			var id0 = new KnowledgeID("abc", "keyB", "keyB", "keyC");
			assertNotEquals(this.test.hashCode(), id0.hashCode());
		}
	
		@Test
		@DisplayName("this.key0 > k0")
		public void equals_key0Upper() {
			var id0 = new KnowledgeID("abc", "key", "keyB", "keyC");
			assertNotEquals(this.test.hashCode(), id0.hashCode());
		}
	
		@Test
		@DisplayName("this.key1 < k1")
		public void equals_key1Lower() {
			var id0 = new KnowledgeID("abc", "keyA", "keyC", "keyC");
			assertNotEquals(this.test.hashCode(), id0.hashCode());
		}
	
		@Test
		@DisplayName("this.key1 > k1")
		public void equals_key1Upper() {
			var id0 = new KnowledgeID("abc", "keyA", "keyA", "keyC");
			assertNotEquals(this.test.hashCode(), id0.hashCode());
		}
	
		@Test
		@DisplayName("this.key2 < k2")
		public void equals_key2Lower() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyD");
			assertNotEquals(this.test.hashCode(), id0.hashCode());
		}
	
		@Test
		@DisplayName("this.key2 > k2")
		public void equals_key2Upper() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyB");
			assertNotEquals(this.test.hashCode(), id0.hashCode());
		}
	
		@Test
		@DisplayName("equals")
		public void equals_equals() {
			var id0 = new KnowledgeID("abc", "keyA", "keyB", "keyC");
			assertEquals(this.test.hashCode(), id0.hashCode());
		}

	}

}

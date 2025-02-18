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

import static io.sarl.tests.api.tools.TestAssertions.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.TreeMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.bspl.api.memory.KnowledgeID;
import io.sarl.bspl.api.memory.KnowledgeMissingException;
import io.sarl.bspl.api.memory.TreeMapStateManager;

/**
 * Test of {@code TreeMapStateManager}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@DisplayName("TreeMapStateManager")
@Tag("unit")
@Tag("api")
@SuppressWarnings("all")
public class TreeMapStateManagerTest {

	@Nested
	@DisplayName("Empty knowledge base")
	public class EmptyKnowledgeBaseTest {

		@Nested
		@DisplayName("getKnowledge")
		public class GetKnowledgeTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				this.test = new TreeMapStateManager();
			}
	
			@Test
			@DisplayName("a/b/id0, null")
			public void getKnowledge_id0_null() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00, null);
				});
			}
		
			@Test
			@DisplayName("a/b/id0, String")
			public void getKnowledge_id0_string() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00, String.class);
				});
			}
			
			@Test
			@DisplayName("a/b/id0, Double")
			public void getKnowledge_id0_double() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00, Double.class);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void getKnowledge_id1_null() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10, null);
				});
			}
		
			@Test
			@DisplayName("a/b/id1, String")
			public void getKnowledge_id1_string() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10, String.class);
				});
			}
			
			@Test
			@DisplayName("a/b/id1, Double")
			public void getKnowledge_id1_double() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10, Double.class);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void getKnowledge_id1c_null() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11, null);
				});
			}
		
			@Test
			@DisplayName("a/c/id1, String")
			public void getKnowledge_id1c_string() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11, String.class);
				});
			}
			
			@Test
			@DisplayName("a/c/id1, Double")
			public void getKnowledge_id1c_double() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11, Double.class);
				});
			}

		}

		@Nested
		@DisplayName("setKnowledge")
		public class SetKnowledgeTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				this.test = new TreeMapStateManager();
			}

			@Test
			@DisplayName("a/b/id0, null")
			public void setKnowledge_id0_null() {
				this.test.setKnowledge(this.id00, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, \"ab\"")
			public void setKnowledge_id0_string() {
				this.test.setKnowledge(this.id00, "ab");
				assertEquals("ab", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, 9.876")
			public void setKnowledge_id0_double() {
				this.test.setKnowledge(this.id00, 9.876);
				assertEquals(9.876, this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void setKnowledge_id1_null() {
				this.test.setKnowledge(this.id10, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, \"ab\"")
			public void setKnowledge_id1_string() {
				this.test.setKnowledge(this.id10, "ab");
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertEquals("ab", this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, 9.876")
			public void setKnowledge_id1_double() {
				this.test.setKnowledge(this.id10, 9.876);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertEquals(9.876, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void setKnowledge_id1c_null() {
				this.test.setKnowledge(this.id11, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, \"ab\"")
			public void setKnowledge_id1c_string() {
				this.test.setKnowledge(this.id11, "ab");
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertEquals("ab", this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/c/id1, 9.876")
			public void setKnowledge_id1c_double() {
				this.test.setKnowledge(this.id11, 9.876);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertEquals(9.876, this.test.getKnowledge(this.id11));
			}

		}

		@Nested
		@DisplayName("setKnowledgeIfAbsent")
		public class SetKnowledgeIfAbsentTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				this.test = new TreeMapStateManager();
			}

			@Test
			@DisplayName("a/b/id0, null")
			public void setKnowledgeIfAbsent_id0_null() {
				this.test.setKnowledgeIfAbsent(this.id00, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, \"ab\"")
			public void setKnowledgeIfAbsent_id0_string() {
				this.test.setKnowledgeIfAbsent(this.id00, "ab");
				assertEquals("ab", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, 9.876")
			public void setKnowledgeIfAbsent_id0_double() {
				this.test.setKnowledgeIfAbsent(this.id00, 9.876);
				assertEquals(9.876, this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void setKnowledgeIfAbsent_id1_null() {
				this.test.setKnowledgeIfAbsent(this.id10, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, \"ab\"")
			public void setKnowledgeIfAbsent_id1_string() {
				this.test.setKnowledgeIfAbsent(this.id10, "ab");
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertEquals("ab", this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, 9.876")
			public void setKnowledgeIfAbsent_id1_double() {
				this.test.setKnowledgeIfAbsent(this.id10, 9.876);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertEquals(9.876, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void setKnowledgeIfAbsent_id1c_null() {
				this.test.setKnowledgeIfAbsent(this.id11, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, \"ab\"")
			public void setKnowledgeIfAbsent_id1c_string() {
				this.test.setKnowledgeIfAbsent(this.id11, "ab");
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertEquals("ab", this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/c/id1, 9.876")
			public void setKnowledgeIfAbsent_id1c_double() {
				this.test.setKnowledgeIfAbsent(this.id11, 9.876);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertEquals(9.876, this.test.getKnowledge(this.id11));
			}

		}

		@Nested
		@DisplayName("getIDsWithName")
		public class GetIDsWithNameTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				this.test = new TreeMapStateManager();
			}
	
			@Test
			@DisplayName("id0")
			public void id0() {
				assertContains(this.test.getIDsWithName("id0"));
			}

			@Test
			@DisplayName("id1")
			public void id1() {
				assertContains(this.test.getIDsWithName("id1"));
			}

			@Test
			@DisplayName("id2")
			public void id2() {
				assertContains(this.test.getIDsWithName("id2"));
			}

		}

		@Nested
		@DisplayName("isDefined")
		public class IsDefinedTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				this.test = new TreeMapStateManager();
			}
	
			@Test
			@DisplayName("a/b/id0")
			public void id00() {
				var id = new KnowledgeID("id0", "a", "b");
				assertFalse(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/b/id1")
			public void id10() {
				var id = new KnowledgeID("id1", "a", "b");
				assertFalse(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/c/id1")
			public void id11() {
				var id = new KnowledgeID("id1", "a", "c");
				assertFalse(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/b/id2")
			public void id20() {
				var id = new KnowledgeID("id2", "a", "b");
				assertFalse(this.test.isDefined(id));
			}

		}

		@Nested
		@DisplayName("removeKnowledge")
		public class RemoveKnowledgeTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				this.test = new TreeMapStateManager();
			}
	
			@Test
			@DisplayName("a/b/id0")
			public void id00() {
				var id = new KnowledgeID("id0", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs());
			}

			@Test
			@DisplayName("a/b/id1")
			public void id10() {
				var id = new KnowledgeID("id1", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs());
			}

			@Test
			@DisplayName("a/c/id1")
			public void id11() {
				var id = new KnowledgeID("id1", "a", "c");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs());
			}

			@Test
			@DisplayName("a/b/id2")
			public void id20() {
				var id = new KnowledgeID("id2", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs());
			}

		}

		@Nested
		@DisplayName("getLocalStateManagerLock")
		public class GetLocalStateManagerLockTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				this.test = new TreeMapStateManager();
			}
	
			@Test
			@DisplayName("get")
			public void get() {
				assertSame(this.test, this.test.getLocalStateManagerLock());
			}

		}

	}

	@Nested
	@DisplayName("Knowledge base with a/b/id0")
	public class Id00BaseTest {

		@Nested
		@DisplayName("getKnowledge")
		public class GetKnowledgeTest {

			private TreeMapStateManager test;
	
			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				this.test = new TreeMapStateManager(map);
			}
		
			@Test
			@DisplayName("a/b/id0, null")
			public void getKnowledge_id0_null() {
				assertEquals("abcdef", this.test.getKnowledge(this.id00, null));
			}
		
			@Test
			@DisplayName("a/b/id0, String")
			public void getKnowledge_id0_string() {
				assertEquals("abcdef", this.test.getKnowledge(this.id00, String.class));
			}
			
			@Test
			@DisplayName("a/b/id0, Double")
			public void getKnowledge_id0_double() {
				assertThrows(ClassCastException.class, () -> {
					this.test.getKnowledge(this.id00, Double.class);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void getKnowledge_id1_null() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10, null);
				});
			}
		
			@Test
			@DisplayName("a/b/id1, String")
			public void getKnowledge_id1_string() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10, String.class);
				});
			}
			
			@Test
			@DisplayName("a/b/id1, Double")
			public void getKnowledge_id1_double() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10, Double.class);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void getKnowledge_id1c_null() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11, null);
				});
			}
		
			@Test
			@DisplayName("a/c/id1, String")
			public void getKnowledge_id1c_string() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11, String.class);
				});
			}
			
			@Test
			@DisplayName("a/c/id1, Double")
			public void getKnowledge_id1c_double() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11, Double.class);
				});
			}

		}

		@Nested
		@DisplayName("setKnowledge")
		public class SetKnowledgeTest {

			private TreeMapStateManager test;
	
			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				this.test = new TreeMapStateManager(map);
			}

			@Test
			@DisplayName("a/b/id0, null")
			public void setKnowledge_id0_null() {
				this.test.setKnowledge(this.id00, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, \"ab\"")
			public void setKnowledge_id0_string() {
				this.test.setKnowledge(this.id00, "ab");
				assertEquals("ab", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, 9.876")
			public void setKnowledge_id0_double() {
				this.test.setKnowledge(this.id00, 9.876);
				assertEquals(9.876, this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void setKnowledge_id1_null() {
				this.test.setKnowledge(this.id10, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, \"ab\"")
			public void setKnowledge_id1_string() {
				this.test.setKnowledge(this.id10, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals("ab", this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, 9.876")
			public void setKnowledge_id1_double() {
				this.test.setKnowledge(this.id10, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(9.876, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void setKnowledge_id1c_null() {
				this.test.setKnowledge(this.id11, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, \"ab\"")
			public void setKnowledge_id1c_string() {
				this.test.setKnowledge(this.id11, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertEquals("ab", this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/c/id1, 9.876")
			public void setKnowledge_id1c_double() {
				this.test.setKnowledge(this.id11, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertEquals(9.876, this.test.getKnowledge(this.id11));
			}

		}

		@Nested
		@DisplayName("setKnowledgeIfAbsent")
		public class SetKnowledgeIfAbsentTest {

			private TreeMapStateManager test;
	
			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				this.test = new TreeMapStateManager(map);
			}

			@Test
			@DisplayName("a/b/id0, null")
			public void setKnowledgeIfAbsent_id0_null() {
				this.test.setKnowledgeIfAbsent(this.id00, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, \"ab\"")
			public void setKnowledgeIfAbsent_id0_string() {
				this.test.setKnowledgeIfAbsent(this.id00, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, 9.876")
			public void setKnowledgeIfAbsent_id0_double() {
				this.test.setKnowledgeIfAbsent(this.id00, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void setKnowledgeIfAbsent_id1_null() {
				this.test.setKnowledgeIfAbsent(this.id10, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, \"ab\"")
			public void setKnowledgeIfAbsent_id1_string() {
				this.test.setKnowledgeIfAbsent(this.id10, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals("ab", this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, 9.876")
			public void setKnowledgeIfAbsent_id1_double() {
				this.test.setKnowledgeIfAbsent(this.id10, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(9.876, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void setKnowledgeIfAbsent_id1c_null() {
				this.test.setKnowledgeIfAbsent(this.id11, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, \"ab\"")
			public void setKnowledgeIfAbsent_id1c_string() {
				this.test.setKnowledgeIfAbsent(this.id11, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertEquals("ab", this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/c/id1, 9.876")
			public void setKnowledgeIfAbsent_id1c_double() {
				this.test.setKnowledgeIfAbsent(this.id11, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertEquals(9.876, this.test.getKnowledge(this.id11));
			}

		}

		@Nested
		@DisplayName("getIDsWithName")
		public class GetIDsWithNameTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("id0")
			public void id0() {
				assertContains(this.test.getIDsWithName("id0"), this.id00);
			}

			@Test
			@DisplayName("id1")
			public void id1() {
				assertContains(this.test.getIDsWithName("id1"));
			}

			@Test
			@DisplayName("id2")
			public void id2() {
				assertContains(this.test.getIDsWithName("id2"));
			}

		}

		@Nested
		@DisplayName("isDefined")
		public class IsDefinedTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("a/b/id0")
			public void id00() {
				var id = new KnowledgeID("id0", "a", "b");
				assertTrue(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/b/id1")
			public void id10() {
				var id = new KnowledgeID("id1", "a", "b");
				assertFalse(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/c/id1")
			public void id11() {
				var id = new KnowledgeID("id1", "a", "c");
				assertFalse(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/b/id2")
			public void id20() {
				var id = new KnowledgeID("id2", "a", "b");
				assertFalse(this.test.isDefined(id));
			}

		}

		@Nested
		@DisplayName("removeKnowledge")
		public class RemoveKnowledgeTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("a/b/id0")
			public void id00() {
				var id = new KnowledgeID("id0", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs());
			}

			@Test
			@DisplayName("a/b/id1")
			public void id10() {
				var id = new KnowledgeID("id1", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id00);
			}

			@Test
			@DisplayName("a/c/id1")
			public void id11() {
				var id = new KnowledgeID("id1", "a", "c");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id00);
			}

			@Test
			@DisplayName("a/b/id2")
			public void id20() {
				var id = new KnowledgeID("id2", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id00);
			}

		}

		@Nested
		@DisplayName("getLocalStateManagerLock")
		public class GetLocalStateManagerLockTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("get")
			public void get() {
				assertSame(this.test, this.test.getLocalStateManagerLock());
			}

		}

	}

	@Nested
	@DisplayName("Knowledge base with a/b/{id0,id1}")
	public class Id00Id10BaseTest {

		@Nested
		@DisplayName("getKnowledge")
		public class GetKnowledgeTest {

			private TreeMapStateManager test;
	
			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				this.test = new TreeMapStateManager(map);
			}
		
			@Test
			@DisplayName("a/b/id0, null")
			public void getKnowledge_id0_null() {
				assertEquals("abcdef", this.test.getKnowledge(this.id00, null));
			}
		
			@Test
			@DisplayName("a/b/id0, String")
			public void getKnowledge_id0_string() {
				assertEquals("abcdef", this.test.getKnowledge(this.id00, String.class));
			}
			
			@Test
			@DisplayName("a/b/id0, Double")
			public void getKnowledge_id0_double() {
				assertThrows(ClassCastException.class, () -> {
					this.test.getKnowledge(this.id00, Double.class);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void getKnowledge_id1_null() {
				assertEquals(1.23456, this.test.getKnowledge(this.id10, null));
			}
		
			@Test
			@DisplayName("a/b/id1, String")
			public void getKnowledge_id1_string() {
				assertThrows(ClassCastException.class, () -> {
					this.test.getKnowledge(this.id10, String.class);
				});
			}
			
			@Test
			@DisplayName("a/b/id1, Double")
			public void getKnowledge_id1_double() {
				assertEquals(1.23456, this.test.getKnowledge(this.id10, null));
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void getKnowledge_id1c_null() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11, null);
				});
			}
		
			@Test
			@DisplayName("a/c/id1, String")
			public void getKnowledge_id1c_string() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11, String.class);
				});
			}
			
			@Test
			@DisplayName("a/c/id1, Double")
			public void getKnowledge_id1c_double() {
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11, Double.class);
				});
			}

		}

		@Nested
		@DisplayName("setKnowledge")
		public class SetKnowledgeTest {

			private TreeMapStateManager test;
	
			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				this.test = new TreeMapStateManager(map);
			}

			@Test
			@DisplayName("a/b/id0, null")
			public void setKnowledge_id0_null() {
				this.test.setKnowledge(this.id00, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, \"ab\"")
			public void setKnowledge_id0_string() {
				this.test.setKnowledge(this.id00, "ab");
				assertEquals("ab", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, 9.876")
			public void setKnowledge_id0_double() {
				this.test.setKnowledge(this.id00, 9.876);
				assertEquals(9.876, this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void setKnowledge_id1_null() {
				this.test.setKnowledge(this.id10, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, \"ab\"")
			public void setKnowledge_id1_string() {
				this.test.setKnowledge(this.id10, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals("ab", this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, 9.876")
			public void setKnowledge_id1_double() {
				this.test.setKnowledge(this.id10, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(9.876, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void setKnowledge_id1c_null() {
				this.test.setKnowledge(this.id11, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, \"ab\"")
			public void setKnowledge_id1c_string() {
				this.test.setKnowledge(this.id11, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals("ab", this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/c/id1, 9.876")
			public void setKnowledge_id1c_double() {
				this.test.setKnowledge(this.id11, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.876, this.test.getKnowledge(this.id11));
			}

		}

		@Nested
		@DisplayName("setKnowledgeIfAbsent")
		public class SetKnowledgeIfAbsentTest {

			private TreeMapStateManager test;
	
			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				this.test = new TreeMapStateManager(map);
			}

			@Test
			@DisplayName("a/b/id0, null")
			public void setKnowledgeIfAbsent_id0_null() {
				this.test.setKnowledgeIfAbsent(this.id00, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, \"ab\"")
			public void setKnowledgeIfAbsent_id0_string() {
				this.test.setKnowledgeIfAbsent(this.id00, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id0, 9.876")
			public void setKnowledgeIfAbsent_id0_double() {
				this.test.setKnowledgeIfAbsent(this.id00, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void setKnowledgeIfAbsent_id1_null() {
				this.test.setKnowledgeIfAbsent(this.id10, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, \"ab\"")
			public void setKnowledgeIfAbsent_id1_string() {
				this.test.setKnowledgeIfAbsent(this.id10, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, 9.876")
			public void setKnowledgeIfAbsent_id1_double() {
				this.test.setKnowledgeIfAbsent(this.id10, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void setKnowledgeIfAbsent_id1c_null() {
				this.test.setKnowledgeIfAbsent(this.id11, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, \"ab\"")
			public void setKnowledgeIfAbsent_id1c_string() {
				this.test.setKnowledgeIfAbsent(this.id11, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals("ab", this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/c/id1, 9.876")
			public void setKnowledgeIfAbsent_id1c_double() {
				this.test.setKnowledgeIfAbsent(this.id11, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.876, this.test.getKnowledge(this.id11));
			}

		}

		@Nested
		@DisplayName("getIDsWithName")
		public class GetIDsWithNameTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("id0")
			public void id0() {
				assertContains(this.test.getIDsWithName("id0"), this.id00);
			}

			@Test
			@DisplayName("id1")
			public void id1() {
				assertContains(this.test.getIDsWithName("id1"), this.id10);
			}

			@Test
			@DisplayName("id2")
			public void id2() {
				assertContains(this.test.getIDsWithName("id2"));
			}

		}

		@Nested
		@DisplayName("isDefined")
		public class IsDefinedTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("a/b/id0")
			public void id00() {
				var id = new KnowledgeID("id0", "a", "b");
				assertTrue(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/b/id1")
			public void id10() {
				var id = new KnowledgeID("id1", "a", "b");
				assertTrue(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/c/id1")
			public void id11() {
				var id = new KnowledgeID("id1", "a", "c");
				assertFalse(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/b/id2")
			public void id20() {
				var id = new KnowledgeID("id2", "a", "b");
				assertFalse(this.test.isDefined(id));
			}

		}

		@Nested
		@DisplayName("removeKnowledge")
		public class RemoveKnowledgeTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("a/b/id0")
			public void id00() {
				var id = new KnowledgeID("id0", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id10);
			}

			@Test
			@DisplayName("a/b/id1")
			public void id10() {
				var id = new KnowledgeID("id1", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id00);
			}

			@Test
			@DisplayName("a/c/id1")
			public void id11() {
				var id = new KnowledgeID("id1", "a", "c");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id00, this.id10);
			}

			@Test
			@DisplayName("a/b/id2")
			public void id20() {
				var id = new KnowledgeID("id2", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id00, this.id10);
			}

		}

		@Nested
		@DisplayName("getLocalStateManagerLock")
		public class GetLocalStateManagerLockTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("get")
			public void get() {
				assertSame(this.test, this.test.getLocalStateManagerLock());
			}

		}

	}

	@Nested
	@DisplayName("Knowledge base with a/b/{id0,id1,id1c}")
	public class Id00Id10Id11BaseTest {

		@Nested
		@DisplayName("getKnowledge")
		public class GetKnowledgeTest {

			private TreeMapStateManager test;
	
			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				map.put(this.id11.clone(), Double.valueOf(9.5658));
				this.test = new TreeMapStateManager(map);
			}
		
			@Test
			@DisplayName("a/b/id0, null")
			public void getKnowledge_id0_null() {
				assertEquals("abcdef", this.test.getKnowledge(this.id00, null));
			}
		
			@Test
			@DisplayName("a/b/id0, String")
			public void getKnowledge_id0_string() {
				assertEquals("abcdef", this.test.getKnowledge(this.id00, String.class));
			}
			
			@Test
			@DisplayName("a/b/id0, Double")
			public void getKnowledge_id0_double() {
				assertThrows(ClassCastException.class, () -> {
					this.test.getKnowledge(this.id00, Double.class);
				});
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void getKnowledge_id1_null() {
				assertEquals(1.23456, this.test.getKnowledge(this.id10, null));
			}
		
			@Test
			@DisplayName("a/b/id1, String")
			public void getKnowledge_id1_string() {
				assertThrows(ClassCastException.class, () -> {
					this.test.getKnowledge(this.id10, String.class);
				});
			}
			
			@Test
			@DisplayName("a/b/id1, Double")
			public void getKnowledge_id1_double() {
				assertEquals(1.23456, this.test.getKnowledge(this.id10, null));
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void getKnowledge_id1c_null() {
				assertEquals(9.5658, this.test.getKnowledge(this.id11, null));
			}
		
			@Test
			@DisplayName("a/c/id1, String")
			public void getKnowledge_id1c_string() {
				assertThrows(ClassCastException.class, () -> {
					this.test.getKnowledge(this.id11, String.class);
				});
			}
			
			@Test
			@DisplayName("a/c/id1, Double")
			public void getKnowledge_id1c_double() {
				assertEquals(9.5658, this.test.getKnowledge(this.id11, Double.class));
			}

		}

		@Nested
		@DisplayName("setKnowledge")
		public class SetKnowledgeTest {

			private TreeMapStateManager test;
	
			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				map.put(this.id11.clone(), Double.valueOf(9.5658));
				this.test = new TreeMapStateManager(map);
			}

			@Test
			@DisplayName("a/b/id0, null")
			public void setKnowledge_id0_null() {
				this.test.setKnowledge(this.id00, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/b/id0, \"ab\"")
			public void setKnowledge_id0_string() {
				this.test.setKnowledge(this.id00, "ab");
				assertEquals("ab", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/b/id0, 9.876")
			public void setKnowledge_id0_double() {
				this.test.setKnowledge(this.id00, 9.876);
				assertEquals(9.876, this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void setKnowledge_id1_null() {
				this.test.setKnowledge(this.id10, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/b/id1, \"ab\"")
			public void setKnowledge_id1_string() {
				this.test.setKnowledge(this.id10, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals("ab", this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/b/id1, 9.876")
			public void setKnowledge_id1_double() {
				this.test.setKnowledge(this.id10, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(9.876, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void setKnowledge_id1c_null() {
				this.test.setKnowledge(this.id11, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, \"ab\"")
			public void setKnowledge_id1c_string() {
				this.test.setKnowledge(this.id11, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals("ab", this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/c/id1, 9.876")
			public void setKnowledge_id1c_double() {
				this.test.setKnowledge(this.id11, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.876, this.test.getKnowledge(this.id11));
			}

		}

		@Nested
		@DisplayName("setKnowledgeIfAbsent")
		public class SetKnowledgeIfAbsentTest {

			private TreeMapStateManager test;
	
			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				map.put(this.id11.clone(), Double.valueOf(9.5658));
				this.test = new TreeMapStateManager(map);
			}

			@Test
			@DisplayName("a/b/id0, null")
			public void setKnowledgeIfAbsent_id0_null() {
				this.test.setKnowledgeIfAbsent(this.id00, null);
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id00);
				});
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/b/id0, \"ab\"")
			public void setKnowledgeIfAbsent_id0_string() {
				this.test.setKnowledgeIfAbsent(this.id00, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/b/id0, 9.876")
			public void setKnowledgeIfAbsent_id0_double() {
				this.test.setKnowledgeIfAbsent(this.id00, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/b/id1, null")
			public void setKnowledgeIfAbsent_id1_null() {
				this.test.setKnowledgeIfAbsent(this.id10, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id10);
				});
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/b/id1, \"ab\"")
			public void setKnowledgeIfAbsent_id1_string() {
				this.test.setKnowledgeIfAbsent(this.id10, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/b/id1, 9.876")
			public void setKnowledgeIfAbsent_id1_double() {
				this.test.setKnowledgeIfAbsent(this.id10, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/c/id1, null")
			public void setKnowledgeIfAbsent_id1c_null() {
				this.test.setKnowledgeIfAbsent(this.id11, null);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertThrows(KnowledgeMissingException.class, () -> {
					this.test.getKnowledge(this.id11);
				});
			}
	
			@Test
			@DisplayName("a/c/id1, \"ab\"")
			public void setKnowledgeIfAbsent_id1c_string() {
				this.test.setKnowledgeIfAbsent(this.id11, "ab");
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}
	
			@Test
			@DisplayName("a/c/id1, 9.876")
			public void setKnowledgeIfAbsent_id1c_double() {
				this.test.setKnowledgeIfAbsent(this.id11, 9.876);
				assertEquals("abcdef", this.test.getKnowledge(this.id00));
				assertEquals(1.23456, this.test.getKnowledge(this.id10));
				assertEquals(9.5658, this.test.getKnowledge(this.id11));
			}

		}

		@Nested
		@DisplayName("getIDsWithName")
		public class GetIDsWithNameTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				map.put(this.id11.clone(), Double.valueOf(9.5658));
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("id0")
			public void id0() {
				assertContains(this.test.getIDsWithName("id0"), this.id00);
			}

			@Test
			@DisplayName("id1")
			public void id1() {
				assertContains(this.test.getIDsWithName("id1"), this.id10, this.id11);
			}

			@Test
			@DisplayName("id2")
			public void id2() {
				assertContains(this.test.getIDsWithName("id2"));
			}

		}

		@Nested
		@DisplayName("isDefined")
		public class IsDefinedTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				map.put(this.id11.clone(), Double.valueOf(9.5658));
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("a/b/id0")
			public void id00() {
				var id = new KnowledgeID("id0", "a", "b");
				assertTrue(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/b/id1")
			public void id10() {
				var id = new KnowledgeID("id1", "a", "b");
				assertTrue(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/c/id1")
			public void id11() {
				var id = new KnowledgeID("id1", "a", "c");
				assertTrue(this.test.isDefined(id));
			}

			@Test
			@DisplayName("a/b/id2")
			public void id20() {
				var id = new KnowledgeID("id2", "a", "b");
				assertFalse(this.test.isDefined(id));
			}

		}

		@Nested
		@DisplayName("removeKnowledge")
		public class RemoveKnowledgeTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				map.put(this.id11.clone(), Double.valueOf(9.5658));
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("a/b/id0")
			public void id00() {
				var id = new KnowledgeID("id0", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id10, this.id11);
			}

			@Test
			@DisplayName("a/b/id1")
			public void id10() {
				var id = new KnowledgeID("id1", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id00, this.id11);
			}

			@Test
			@DisplayName("a/c/id1")
			public void id11() {
				var id = new KnowledgeID("id1", "a", "c");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id00, this.id10);
			}

			@Test
			@DisplayName("a/b/id2")
			public void id20() {
				var id = new KnowledgeID("id2", "a", "b");
				this.test.removeKnowledge(id);
				assertContains(this.test.getIDs(), this.id00, this.id10, this.id11);
			}

		}

		@Nested
		@DisplayName("getLocalStateManagerLock")
		public class GetLocalStateManagerLockTest {

			private TreeMapStateManager test;

			private KnowledgeID id00;
		
			private KnowledgeID id10;
		
			private KnowledgeID id11;
		
			@BeforeEach
			public void setUp() {
				this.id00 = new KnowledgeID("id0", "a", "b");
				this.id10 = new KnowledgeID("id1", "a", "b");
				this.id11 = new KnowledgeID("id1", "a", "c");
				var map = new TreeMap<KnowledgeID, Object>();
				map.put(this.id00.clone(), "abcdef");
				map.put(this.id10.clone(), Double.valueOf(1.23456));
				map.put(this.id11.clone(), Double.valueOf(9.5658));
				this.test = new TreeMapStateManager(map);
			}
	
			@Test
			@DisplayName("get")
			public void get() {
				assertSame(this.test, this.test.getLocalStateManagerLock());
			}

		}

	}

}

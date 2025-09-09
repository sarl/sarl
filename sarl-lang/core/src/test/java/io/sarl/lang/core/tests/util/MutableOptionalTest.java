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
package io.sarl.lang.core.tests.util;

import static org.junit.jupiter.api.Assertions.*;
import static io.sarl.tests.api.tools.TestAssertions.*;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.util.MutableOptional;
import io.sarl.lang.core.util.SarlUtils;

/** This class tests the {@link MutableOptional} for SARL.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.15
 */
@SuppressWarnings("all")
@DisplayName("MutableOptional")
@Tag("core")
@Tag("unit")
public class MutableOptionalTest {

	private MutableOptional<Integer> test;

	@BeforeEach
	public void setUp() {
		this.test = MutableOptional.of(Integer.valueOf(758));
	}
	
	@Test
	@DisplayName("static empty")
	public void empty() throws Exception {
		var test = MutableOptional.empty();
		assertTrue(test.isEmpty());
		assertFalse(test.isPresent());
		assertException(NoSuchElementException.class, () -> {
			test.get();
		});
	}

	@Test
	@DisplayName("static of(T)")
	public void of() {
		var test = MutableOptional.of(35);
		assertFalse(test.isEmpty());
		assertTrue(test.isPresent());
		assertEquals(35, test.get());
	}

	@Test
	@DisplayName("get")
	public void get() throws Exception {
		assertEquals(758, this.test.get());

		this.test.set(null);
		assertException(NoSuchElementException.class, () -> {
			this.test.get();
		});

		this.test.set(654);
		assertEquals(654, this.test.get());		
	}

	@Test
	@DisplayName("isPresent")
	public void isPresent() {
		assertTrue(this.test.isPresent());
		this.test.set(null);
		assertFalse(this.test.isPresent());		
		this.test.set(654);
		assertTrue(this.test.isPresent());		
	}

	@Test
	@DisplayName("isEmpty")
	public void isEmpty() {
		assertFalse(this.test.isEmpty());
		this.test.set(null);
		assertTrue(this.test.isEmpty());		
		this.test.set(654);
		assertFalse(this.test.isEmpty());		
	}

	@Test
	@DisplayName("ifPresent")
	public void ifPresent() {
		Integer[] val0 = { null };
		this.test.ifPresent(it -> val0[0] = it);
		assertEquals(758, val0[0]);

		Integer[] val1 = { 321 };
		this.test.set(null);
		this.test.ifPresent(it -> val1[0] = it);
		assertEquals(321, val1[0]);

		Integer[] val2 = { null };
		this.test.set(654);
		this.test.ifPresent(it -> val2[0] = it);
		assertEquals(654, val2[0]);
	}

	@Test
	@DisplayName("ifPresentOrElse")
	public void ifPresentOrElse() {
		Integer[] val0a = { null };
		Integer[] val0b = { null };
		this.test.ifPresentOrElse(it -> val0a[0] = it, () -> val0b[0] = Integer.valueOf(1));
		assertEquals(758, val0a[0]);
		assertNull(val0b[0]);

		Integer[] val1a = { 321 };
		Integer[] val1b = { 123 };
		this.test.set(null);
		this.test.ifPresentOrElse(it -> val1a[0] = it, () -> val1b[0] = Integer.valueOf(1));
		assertEquals(321, val1a[0]);
		assertEquals(1, val1b[0]);

		Integer[] val2a = { null };
		Integer[] val2b = { null };
		this.test.set(495);
		this.test.ifPresentOrElse(it -> val2a[0] = it, () -> val2b[0] = Integer.valueOf(1));
		assertEquals(495, val2a[0]);
		assertNull(val2b[0]);
	}

	@Test
	@DisplayName("filter")
	public void filter() {
		assertSame(this.test, this.test.filter(it -> it.intValue() > 400));

		this.test.set(325);
		var obj0 = this.test.filter(it -> it.intValue() > 400);
		assertNotSame(this.test, obj0);
		assertTrue(obj0.isEmpty());

		this.test.set(null);
		var obj1 = this.test.filter(it -> it.intValue() > 400);
		assertSame(this.test, obj1);
	}

	@Test
	@DisplayName("map")
	public void map() {
		var obj0 = this.test.map(it -> it.intValue() > 400);
		assertTrue(obj0.get());

		this.test.set(325);
		var obj1 = this.test.map(it -> it.intValue() > 400);
		assertFalse(obj1.get());

		this.test.set(null);
		var obj2 = this.test.map(it -> it.intValue() > 400);
		assertTrue(obj2.isEmpty());
	}

	@Test
	@DisplayName("flatMap")
	public void flatMap() {
		var obj0 = this.test.flatMap(it -> MutableOptional.of(it.intValue() > 400));
		assertTrue(obj0.get());

		this.test.set(325);
		var obj1 = this.test.flatMap(it -> MutableOptional.of(it.intValue() > 400));
		assertFalse(obj1.get());

		this.test.set(null);
		var obj2 = this.test.flatMap(it -> MutableOptional.of(it.intValue() > 400));
		assertTrue(obj2.isEmpty());
	}

	@Test
	@DisplayName("or")
	public void or() {
		var obj0 = this.test.or(() -> MutableOptional.of(357));
		assertEquals(758, obj0.get());

		this.test.set(325);
		var obj1 = this.test.or(() -> MutableOptional.of(357));
		assertEquals(325, obj1.get());

		this.test.set(null);
		var obj2 = this.test.or(() -> MutableOptional.of(357));
		assertEquals(357, obj2.get());
	}

	@Test
	@DisplayName("orElse")
	public void orElse() {
		var obj0 = this.test.orElse(357);
		assertEquals(758, obj0);

		this.test.set(325);
		var obj1 = this.test.orElse(357);
		assertEquals(325, obj1);

		this.test.set(null);
		var obj2 = this.test.orElse(357);
		assertEquals(357, obj2);
	}

	@Test
	@DisplayName("orElseGet")
	public void orElseGet() {
		var obj0 = this.test.orElseGet(() -> 357);
		assertEquals(758, obj0);

		this.test.set(325);
		var obj1 = this.test.orElseGet(() -> 357);
		assertEquals(325, obj1);

		this.test.set(null);
		var obj2 = this.test.orElseGet(() -> 357);
		assertEquals(357, obj2);
	}

	@Test
	@DisplayName("orElseThrow()")
	public void orElseThrow() throws Exception {
		var obj0 = this.test.orElseThrow();
		assertEquals(758, obj0);

		this.test.set(325);
		var obj1 = this.test.orElseThrow();
		assertEquals(325, obj1);

		this.test.set(null);
		assertException(NoSuchElementException.class, () -> {
			this.test.orElseThrow();
		});
	}

	@Test
	@DisplayName("orElseThrow(E)")
	public void orElseThrowE() throws Exception {
		var obj0 = this.test.orElseThrow(() -> new IllegalArgumentException());
		assertEquals(758, obj0);

		this.test.set(325);
		var obj1 = this.test.orElseThrow(() -> new IllegalArgumentException());
		assertEquals(325, obj1);

		this.test.set(null);
		assertException(IllegalArgumentException.class, () -> {
			this.test.orElseThrow(() -> new IllegalArgumentException());
		});
	}

	@Test
	@DisplayName("set")
	public void set() throws Exception {
		this.test.set(null);
		assertException(NoSuchElementException.class, () -> {
			this.test.get();
		});

		this.test.set(654);
		assertEquals(654, this.test.get());		
	}

}

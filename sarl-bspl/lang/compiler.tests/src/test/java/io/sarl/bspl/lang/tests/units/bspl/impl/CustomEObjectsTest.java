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
package io.sarl.bspl.lang.tests.units.bspl.impl;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.Collection;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import io.sarl.bspl.lang.bspl.BsplPackage;
import io.sarl.bspl.lang.bspl.impl.CustomEObjects;
import io.sarl.bspl.lang.tests.AbstractBsplTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("CustomEObjects")
public class CustomEObjectsTest {

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("isInputModifier")
	public class InputTest extends AbstractBsplTest {

		@Test
		@DisplayName("{}")
		public void empty() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList()));
		}

		@Test
		@DisplayName("{in}")
		public void in() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in")));
		}

		@Test
		@DisplayName("{in, out}")
		public void in_out() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "out")));
		}

		@Test
		@DisplayName("{in, out, any}")
		public void in_out_any() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "out", "any")));
		}

		@Test
		@DisplayName("{in, out, any, nil}")
		public void in_out_any_nil() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "out", "any", "nil")));
		}

		@Test
		@DisplayName("{in, out, any, nil, opt}")
		public void in_out_any_nil_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, any, opt}")
		public void in_out_any_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "out", "any", "opt")));
		}

		@Test
		@DisplayName("{in, out, nil}")
		public void in_out_nil() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "out", "nil")));
		}

		@Test
		@DisplayName("{in, out, nil, opt}")
		public void in_out_nil_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "out", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, opt}")
		public void in_out_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "out", "opt")));
		}

		@Test
		@DisplayName("{in, any}")
		public void in_any() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "any")));
		}

		@Test
		@DisplayName("{in, any, nil}")
		public void in_any_nil() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "any", "nil")));
		}

		@Test
		@DisplayName("{in, any, nil, opt}")
		public void in_any_nil_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, any, opt}")
		public void in_any_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "any", "opt")));
		}

		@Test
		@DisplayName("{in, nil}")
		public void in_nil() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "nil")));
		}

		@Test
		@DisplayName("{in, nil, opt}")
		public void in_nil_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, opt}")
		public void in_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("in", "opt")));
		}

		@Test
		@DisplayName("{out}")
		public void out() {
			assertFalse(CustomEObjects.isInputModifier(Arrays.asList("out")));
		}

		@Test
		@DisplayName("{out, any}")
		public void out_any() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("out", "any")));
		}

		@Test
		@DisplayName("{out, any, nil}")
		public void out_any_nil() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("out", "any", "nil")));
		}

		@Test
		@DisplayName("{out, any, nil, opt}")
		public void out_any_nil_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, any, opt}")
		public void out_any_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("out", "any", "opt")));
		}

		@Test
		@DisplayName("{out, nil}")
		public void out_nil() {
			assertFalse(CustomEObjects.isInputModifier(Arrays.asList("out", "nil")));
		}

		@Test
		@DisplayName("{out, nil, opt}")
		public void out_nil_opt() {
			assertFalse(CustomEObjects.isInputModifier(Arrays.asList("out", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, opt}")
		public void out_opt() {
			assertFalse(CustomEObjects.isInputModifier(Arrays.asList("out", "opt")));
		}

		@Test
		@DisplayName("{any}")
		public void any() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("any")));
		}

		@Test
		@DisplayName("{any, nil}")
		public void any_nil() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("any", "nil")));
		}

		@Test
		@DisplayName("{any, nil, opt}")
		public void any_nil_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("any", "nil", "opt")));
		}

		@Test
		@DisplayName("{any, opt}")
		public void any_opt() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("any", "opt")));
		}

		@Test
		@DisplayName("{nil}")
		public void nil() {
			assertFalse(CustomEObjects.isInputModifier(Arrays.asList("nil")));
		}

		@Test
		@DisplayName("{nil, opt}")
		public void nil_opt() {
			assertFalse(CustomEObjects.isInputModifier(Arrays.asList("nil", "opt")));
		}

		@Test
		@DisplayName("{opt}")
		public void opt() {
			assertFalse(CustomEObjects.isInputModifier(Arrays.asList("opt")));
		}

		@Test
		@DisplayName("{key}")
		public void key() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("key")));
		}

		@Test
		@DisplayName("{key, in}")
		public void key_in() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("key", "in")));
		}

		@Test
		@DisplayName("{key, out}")
		public void key_out() {
			assertFalse(CustomEObjects.isInputModifier(Arrays.asList("key", "out")));
		}

		@Test
		@DisplayName("{key, any}")
		public void key_any() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("key", "any")));
		}

		@Test
		@DisplayName("{key, nil}")
		public void key_nil() {
			assertFalse(CustomEObjects.isInputModifier(Arrays.asList("key", "nil")));
		}

		@Test
		@DisplayName("{key, opt}")
		public void key_opt() {
			assertFalse(CustomEObjects.isInputModifier(Arrays.asList("key", "opt")));
		}

		@Test
		@DisplayName("{key, private}")
		public void key_private() {
			assertTrue(CustomEObjects.isInputModifier(Arrays.asList("key", "private")));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("isOutputModifier")
	public class OutputTest extends AbstractBsplTest {

		@Test
		@DisplayName("{}")
		public void empty() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList()));
		}

		@Test
		@DisplayName("{in}")
		public void in() {
			assertFalse(CustomEObjects.isOutputModifier(Arrays.asList("in")));
		}

		@Test
		@DisplayName("{in, out}")
		public void in_out() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "out")));
		}

		@Test
		@DisplayName("{in, out, any}")
		public void in_out_any() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "out", "any")));
		}

		@Test
		@DisplayName("{in, out, any, nil}")
		public void in_out_any_nil() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "out", "any", "nil")));
		}

		@Test
		@DisplayName("{in, out, any, nil, opt}")
		public void in_out_any_nil_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, any, opt}")
		public void in_out_any_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "out", "any", "opt")));
		}

		@Test
		@DisplayName("{in, out, nil}")
		public void in_out_nil() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "out", "nil")));
		}

		@Test
		@DisplayName("{in, out, nil, opt}")
		public void in_out_nil_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "out", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, opt}")
		public void in_out_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "out", "opt")));
		}

		@Test
		@DisplayName("{in, any}")
		public void in_any() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "any")));
		}

		@Test
		@DisplayName("{in, any, nil}")
		public void in_any_nil() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "any", "nil")));
		}

		@Test
		@DisplayName("{in, any, nil, opt}")
		public void in_any_nil_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, any, opt}")
		public void in_any_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("in", "any", "opt")));
		}

		@Test
		@DisplayName("{in, nil}")
		public void in_nil() {
			assertFalse(CustomEObjects.isOutputModifier(Arrays.asList("in", "nil")));
		}

		@Test
		@DisplayName("{in, nil, opt}")
		public void in_nil_opt() {
			assertFalse(CustomEObjects.isOutputModifier(Arrays.asList("in", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, opt}")
		public void in_opt() {
			assertFalse(CustomEObjects.isOutputModifier(Arrays.asList("in", "opt")));
		}

		@Test
		@DisplayName("{out}")
		public void out() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("out")));
		}

		@Test
		@DisplayName("{out, any}")
		public void out_any() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("out", "any")));
		}

		@Test
		@DisplayName("{out, any, nil}")
		public void out_any_nil() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("out", "any", "nil")));
		}

		@Test
		@DisplayName("{out, any, nil, opt}")
		public void out_any_nil_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, any, opt}")
		public void out_any_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("out", "any", "opt")));
		}

		@Test
		@DisplayName("{out, nil}")
		public void out_nil() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("out", "nil")));
		}

		@Test
		@DisplayName("{out, nil, opt}")
		public void out_nil_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("out", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, opt}")
		public void out_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("out", "opt")));
		}

		@Test
		@DisplayName("{any}")
		public void any() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("any")));
		}

		@Test
		@DisplayName("{any, nil}")
		public void any_nil() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("any", "nil")));
		}

		@Test
		@DisplayName("{any, nil, opt}")
		public void any_nil_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("any", "nil", "opt")));
		}

		@Test
		@DisplayName("{any, opt}")
		public void any_opt() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("any", "opt")));
		}

		@Test
		@DisplayName("{nil}")
		public void nil() {
			assertFalse(CustomEObjects.isOutputModifier(Arrays.asList("nil")));
		}

		@Test
		@DisplayName("{nil, opt}")
		public void nil_opt() {
			assertFalse(CustomEObjects.isOutputModifier(Arrays.asList("nil", "opt")));
		}

		@Test
		@DisplayName("{opt}")
		public void opt() {
			assertFalse(CustomEObjects.isOutputModifier(Arrays.asList("opt")));
		}

		@Test
		@DisplayName("{key}")
		public void key() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("key")));
		}

		@Test
		@DisplayName("{key, in}")
		public void key_in() {
			assertFalse(CustomEObjects.isOutputModifier(Arrays.asList("key", "in")));
		}

		@Test
		@DisplayName("{key, out}")
		public void key_out() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("key", "out")));
		}

		@Test
		@DisplayName("{key, any}")
		public void key_any() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("key", "any")));
		}

		@Test
		@DisplayName("{key, nil}")
		public void key_nil() {
			assertFalse(CustomEObjects.isOutputModifier(Arrays.asList("key", "nil")));
		}

		@Test
		@DisplayName("{key, opt}")
		public void key_opt() {
			assertFalse(CustomEObjects.isOutputModifier(Arrays.asList("key", "opt")));
		}

		@Test
		@DisplayName("{key, private}")
		public void key_private() {
			assertTrue(CustomEObjects.isOutputModifier(Arrays.asList("key", "private")));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("isAnyModifier")
	public class AnyTest extends AbstractBsplTest {

		@Test
		@DisplayName("{}")
		public void isAnyModifier() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList()));
		}

		@Test
		@DisplayName("{in}")
		public void in() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("in")));
		}

		@Test
		@DisplayName("{in, out}")
		public void in_out() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("in", "out")));
		}

		@Test
		@DisplayName("{in, out, any}")
		public void in_out_any() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("in", "out", "any")));
		}

		@Test
		@DisplayName("{in, out, any, nil}")
		public void in_out_any_nil() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("in", "out", "any", "nil")));
		}

		@Test
		@DisplayName("{in, out, any, nil, opt}")
		public void in_out_any_nil_opt() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("in", "out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, any, opt}")
		public void in_out_any_opt() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("in", "out", "any", "opt")));
		}

		@Test
		@DisplayName("{in, out, nil}")
		public void in_out_nil() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("in", "out", "nil")));
		}

		@Test
		@DisplayName("{in, out, nil, opt}")
		public void in_out_nil_opt() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("in", "out", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, opt}")
		public void in_out_opt() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("in", "out", "opt")));
		}

		@Test
		@DisplayName("{in, any}")
		public void in_any() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("in", "any")));
		}

		@Test
		@DisplayName("{in, any, nil}")
		public void in_any_nil() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("in", "any", "nil")));
		}

		@Test
		@DisplayName("{in, any, nil, opt}")
		public void in_any_nil_opt() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("in", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, any, opt}")
		public void in_any_opt() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("in", "any", "opt")));
		}

		@Test
		@DisplayName("{in, nil}")
		public void in_nil() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("in", "nil")));
		}

		@Test
		@DisplayName("{in, nil, opt}")
		public void in_nil_opt() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("in", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, opt}")
		public void in_opt() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("in", "opt")));
		}

		@Test
		@DisplayName("{out}")
		public void out() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("out")));
		}

		@Test
		@DisplayName("{out, any}")
		public void out_any() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("out", "any")));
		}

		@Test
		@DisplayName("{out, any, nil}")
		public void out_any_nil() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("out", "any", "nil")));
		}

		@Test
		@DisplayName("{out, any, nil, opt}")
		public void out_any_nil_opt() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, any, opt}")
		public void out_any_opt() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("out", "any", "opt")));
		}

		@Test
		@DisplayName("{out, nil}")
		public void out_nil() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("out", "nil")));
		}

		@Test
		@DisplayName("{out, nil, opt}")
		public void out_nil_opt() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("out", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, opt}")
		public void out_opt() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("out", "opt")));
		}

		@Test
		@DisplayName("{any}")
		public void any() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("any")));
		}

		@Test
		@DisplayName("{any, nil}")
		public void any_nil() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("any", "nil")));
		}

		@Test
		@DisplayName("{any, nil, opt}")
		public void any_nil_opt() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("any", "nil", "opt")));
		}

		@Test
		@DisplayName("{any, opt}")
		public void any_opt() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("any", "opt")));
		}

		@Test
		@DisplayName("{nil}")
		public void nil() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("nil")));
		}

		@Test
		@DisplayName("{nil, opt}")
		public void nil_opt() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("nil", "opt")));
		}

		@Test
		@DisplayName("{opt}")
		public void opt() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("opt")));
		}

		@Test
		@DisplayName("{key}")
		public void key() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("key")));
		}

		@Test
		@DisplayName("{key, in}")
		public void key_in() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("key", "in")));
		}

		@Test
		@DisplayName("{key, out}")
		public void key_out() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("key", "out")));
		}

		@Test
		@DisplayName("{key, any}")
		public void key_any() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("key", "any")));
		}

		@Test
		@DisplayName("{key, nil}")
		public void key_nil() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("key", "nil")));
		}

		@Test
		@DisplayName("{key, opt}")
		public void key_opt() {
			assertFalse(CustomEObjects.isAnyModifier(Arrays.asList("key", "opt")));
		}

		@Test
		@DisplayName("{key, private}")
		public void key_private() {
			assertTrue(CustomEObjects.isAnyModifier(Arrays.asList("key", "private")));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("isKeyModifier")
	public class KeyTest extends AbstractBsplTest {

		@Test
		@DisplayName("{}")
		public void isKeyModifier() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList()));
		}

		@Test
		@DisplayName("{in}")
		public void in() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in")));
		}

		@Test
		@DisplayName("{in, out}")
		public void in_out() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "out")));
		}

		@Test
		@DisplayName("{in, out, any}")
		public void in_out_any() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "out", "any")));
		}

		@Test
		@DisplayName("{in, out, any, nil}")
		public void in_out_any_nil() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "out", "any", "nil")));
		}

		@Test
		@DisplayName("{in, out, any, nil, opt}")
		public void in_out_any_nil_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, any, opt}")
		public void in_out_any_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "out", "any", "opt")));
		}

		@Test
		@DisplayName("{in, out, nil}")
		public void in_out_nil() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "out", "nil")));
		}

		@Test
		@DisplayName("{in, out, nil, opt}")
		public void in_out_nil_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "out", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, opt}")
		public void in_out_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "out", "opt")));
		}

		@Test
		@DisplayName("{in, any}")
		public void in_any() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "any")));
		}

		@Test
		@DisplayName("{in, any, nil}")
		public void in_any_nil() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "any", "nil")));
		}

		@Test
		@DisplayName("{in, any, nil, opt}")
		public void in_any_nil_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, any, opt}")
		public void in_any_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "any", "opt")));
		}

		@Test
		@DisplayName("{in, nil}")
		public void in_nil() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "nil")));
		}

		@Test
		@DisplayName("{in, nil, opt}")
		public void in_nil_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, opt}")
		public void in_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("in", "opt")));
		}

		@Test
		@DisplayName("{out}")
		public void out() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("out")));
		}

		@Test
		@DisplayName("{out, any}")
		public void out_any() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("out", "any")));
		}

		@Test
		@DisplayName("{out, any, nil}")
		public void out_any_nil() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("out", "any", "nil")));
		}

		@Test
		@DisplayName("{out, any, nil, opt}")
		public void out_any_nil_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, any, opt}")
		public void out_any_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("out", "any", "opt")));
		}

		@Test
		@DisplayName("{out, nil}")
		public void out_nil() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("out", "nil")));
		}

		@Test
		@DisplayName("{out, nil, opt}")
		public void out_nil_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("out", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, opt}")
		public void out_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("out", "opt")));
		}

		@Test
		@DisplayName("{any}")
		public void any() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("any")));
		}

		@Test
		@DisplayName("{any, nil}")
		public void any_nil() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("any", "nil")));
		}

		@Test
		@DisplayName("{any, nil, opt}")
		public void any_nil_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("any", "nil", "opt")));
		}

		@Test
		@DisplayName("{any, opt}")
		public void any_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("any", "opt")));
		}

		@Test
		@DisplayName("{nil}")
		public void nil() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("nil")));
		}

		@Test
		@DisplayName("{nil, opt}")
		public void nil_opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("nil", "opt")));
		}

		@Test
		@DisplayName("{opt}")
		public void opt() {
			assertFalse(CustomEObjects.isKeyModifier(Arrays.asList("opt")));
		}

		@Test
		@DisplayName("{key}")
		public void key() {
			assertTrue(CustomEObjects.isKeyModifier(Arrays.asList("key")));
		}

		@Test
		@DisplayName("{key, in}")
		public void key_in() {
			assertTrue(CustomEObjects.isKeyModifier(Arrays.asList("key", "in")));
		}

		@Test
		@DisplayName("{key, out}")
		public void key_out() {
			assertTrue(CustomEObjects.isKeyModifier(Arrays.asList("key", "out")));
		}

		@Test
		@DisplayName("{key, any}")
		public void key_any() {
			assertTrue(CustomEObjects.isKeyModifier(Arrays.asList("key", "any")));
		}

		@Test
		@DisplayName("{key, nil}")
		public void key_nil() {
			assertTrue(CustomEObjects.isKeyModifier(Arrays.asList("key", "nil")));
		}

		@Test
		@DisplayName("{key, opt}")
		public void key_opt() {
			assertTrue(CustomEObjects.isKeyModifier(Arrays.asList("key", "opt")));
		}

		@Test
		@DisplayName("{key, private}")
		public void key_private() {
			assertTrue(CustomEObjects.isKeyModifier(Arrays.asList("key", "private")));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("isNilModifier")
	public class NilTest extends AbstractBsplTest {

		@Test
		@DisplayName("{}")
		public void isNilModifier() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList()));
		}

		@Test
		@DisplayName("{in}")
		public void in() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("in")));
		}

		@Test
		@DisplayName("{in, out}")
		public void in_out() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("in", "out")));
		}

		@Test
		@DisplayName("{in, out, any}")
		public void in_out_any() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("in", "out", "any")));
		}

		@Test
		@DisplayName("{in, out, any, nil}")
		public void in_out_any_nil() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("in", "out", "any", "nil")));
		}

		@Test
		@DisplayName("{in, out, any, nil, opt}")
		public void in_out_any_nil_opt() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("in", "out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, any, opt}")
		public void in_out_any_opt() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("in", "out", "any", "opt")));
		}

		@Test
		@DisplayName("{in, out, nil}")
		public void in_out_nil() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("in", "out", "nil")));
		}

		@Test
		@DisplayName("{in, out, nil, opt}")
		public void in_out_nil_opt() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("in", "out", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, opt}")
		public void in_out_opt() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("in", "out", "opt")));
		}

		@Test
		@DisplayName("{in, any}")
		public void in_any() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("in", "any")));
		}

		@Test
		@DisplayName("{in, any, nil}")
		public void in_any_nil() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("in", "any", "nil")));
		}

		@Test
		@DisplayName("{in, any, nil, opt}")
		public void in_any_nil_opt() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("in", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, any, opt}")
		public void in_any_opt() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("in", "any", "opt")));
		}

		@Test
		@DisplayName("{in, nil}")
		public void in_nil() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("in", "nil")));
		}

		@Test
		@DisplayName("{in, nil, opt}")
		public void in_nil_opt() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("in", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, opt}")
		public void in_opt() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("in", "opt")));
		}

		@Test
		@DisplayName("{out}")
		public void out() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("out")));
		}

		@Test
		@DisplayName("{out, any}")
		public void out_any() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("out", "any")));
		}

		@Test
		@DisplayName("{out, any, nil}")
		public void out_any_nil() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("out", "any", "nil")));
		}

		@Test
		@DisplayName("{out, any, nil, opt}")
		public void out_any_nil_opt() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, any, opt}")
		public void out_any_opt() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("out", "any", "opt")));
		}

		@Test
		@DisplayName("{out, nil}")
		public void out_nil() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("out", "nil")));
		}

		@Test
		@DisplayName("{out, nil, opt}")
		public void out_nil_opt() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("out", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, opt}")
		public void out_opt() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("out", "opt")));
		}

		@Test
		@DisplayName("{any}")
		public void any() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("any")));
		}

		@Test
		@DisplayName("{any, nil}")
		public void any_nil() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("any", "nil")));
		}

		@Test
		@DisplayName("{any, nil, opt}")
		public void any_nil_opt() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("any", "nil", "opt")));
		}

		@Test
		@DisplayName("{any, opt}")
		public void any_opt() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("any", "opt")));
		}

		@Test
		@DisplayName("{nil}")
		public void nil() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("nil")));
		}

		@Test
		@DisplayName("{nil, opt}")
		public void nil_opt() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("nil", "opt")));
		}

		@Test
		@DisplayName("{opt}")
		public void opt() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("opt")));
		}

		@Test
		@DisplayName("{key}")
		public void key() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("key")));
		}

		@Test
		@DisplayName("{key, in}")
		public void key_in() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("key", "in")));
		}

		@Test
		@DisplayName("{key, out}")
		public void key_out() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("key", "out")));
		}

		@Test
		@DisplayName("{key, any}")
		public void key_any() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("key", "any")));
		}

		@Test
		@DisplayName("{key, nil}")
		public void key_nil() {
			assertTrue(CustomEObjects.isNilModifier(Arrays.asList("key", "nil")));
		}

		@Test
		@DisplayName("{key, opt}")
		public void key_opt() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("key", "opt")));
		}

		@Test
		@DisplayName("{key, private}")
		public void key_private() {
			assertFalse(CustomEObjects.isNilModifier(Arrays.asList("key", "private")));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("isOptionalModifier")
	public class OptionalTest extends AbstractBsplTest {

		@Test
		@DisplayName("{}")
		public void isOptionalModifier() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList()));
		}

		@Test
		@DisplayName("{in}")
		public void in() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("in")));
		}

		@Test
		@DisplayName("{in, out}")
		public void in_out() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("in", "out")));
		}

		@Test
		@DisplayName("{in, out, any}")
		public void in_out_any() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("in", "out", "any")));
		}

		@Test
		@DisplayName("{in, out, any, nil}")
		public void in_out_any_nil() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("in", "out", "any", "nil")));
		}

		@Test
		@DisplayName("{in, out, any, nil, opt}")
		public void in_out_any_nil_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("in", "out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, any, opt}")
		public void in_out_any_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("in", "out", "any", "opt")));
		}

		@Test
		@DisplayName("{in, out, nil}")
		public void in_out_nil() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("in", "out", "nil")));
		}

		@Test
		@DisplayName("{in, out, nil, opt}")
		public void in_out_nil_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("in", "out", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, out, opt}")
		public void in_out_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("in", "out", "opt")));
		}

		@Test
		@DisplayName("{in, any}")
		public void in_any() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("in", "any")));
		}

		@Test
		@DisplayName("{in, any, nil}")
		public void in_any_nil() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("in", "any", "nil")));
		}

		@Test
		@DisplayName("{in, any, nil, opt}")
		public void in_any_nil_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("in", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, any, opt}")
		public void in_any_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("in", "any", "opt")));
		}

		@Test
		@DisplayName("{in, nil}")
		public void in_nil() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("in", "nil")));
		}

		@Test
		@DisplayName("{in, nil, opt}")
		public void in_nil_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("in", "nil", "opt")));
		}

		@Test
		@DisplayName("{in, opt}")
		public void in_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("in", "opt")));
		}

		@Test
		@DisplayName("{out}")
		public void out() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("out")));
		}

		@Test
		@DisplayName("{out, any}")
		public void out_any() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("out", "any")));
		}

		@Test
		@DisplayName("{out, any, nil}")
		public void out_any_nil() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("out", "any", "nil")));
		}

		@Test
		@DisplayName("{out, any, nil, opt}")
		public void out_any_nil_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("out", "any", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, any, opt}")
		public void out_any_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("out", "any", "opt")));
		}

		@Test
		@DisplayName("{out, nil}")
		public void out_nil() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("out", "nil")));
		}

		@Test
		@DisplayName("{out, nil, opt}")
		public void out_nil_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("out", "nil", "opt")));
		}

		@Test
		@DisplayName("{out, opt}")
		public void out_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("out", "opt")));
		}

		@Test
		@DisplayName("{any}")
		public void any() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("any")));
		}

		@Test
		@DisplayName("{any, nil}")
		public void any_nil() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("any", "nil")));
		}

		@Test
		@DisplayName("{any, nil, opt}")
		public void any_nil_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("any", "nil", "opt")));
		}

		@Test
		@DisplayName("{any, opt}")
		public void any_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("any", "opt")));
		}

		@Test
		@DisplayName("{nil}")
		public void nil() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("nil")));
		}

		@Test
		@DisplayName("{nil, opt}")
		public void nil_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("nil", "opt")));
		}

		@Test
		@DisplayName("{opt}")
		public void opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("opt")));
		}

		@Test
		@DisplayName("{key}")
		public void key() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("key")));
		}

		@Test
		@DisplayName("{key, in}")
		public void key_in() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("key", "in")));
		}

		@Test
		@DisplayName("{key, out}")
		public void key_out() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("key", "out")));
		}

		@Test
		@DisplayName("{key, any}")
		public void key_any() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("key", "any")));
		}

		@Test
		@DisplayName("{key, nil}")
		public void key_nil() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("key", "nil")));
		}

		@Test
		@DisplayName("{key, opt}")
		public void key_opt() {
			assertTrue(CustomEObjects.isOptionalModifier(Arrays.asList("key", "opt")));
		}

		@Test
		@DisplayName("{key, private}")
		public void key_private() {
			assertFalse(CustomEObjects.isOptionalModifier(Arrays.asList("key", "private")));
		}

	}

}

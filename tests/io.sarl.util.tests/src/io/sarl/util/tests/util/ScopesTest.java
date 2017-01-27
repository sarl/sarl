/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.util.tests.util;

import static org.junit.Assert.*;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.mockito.Mock;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Scope;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.util.AddressScope;
import io.sarl.util.Scopes;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public final class ScopesTest extends AbstractSarlTest {

	@Mock
	private Address base1;

	@Mock
	private Address base2;

	@Mock
	private Address base3;

	@Mock
	private Address base4;

	@Test
	public void allParticipants() {
		Scope<Address> scope = Scopes.allParticipants();
		assertTrue(scope.matches(this.base1));
		assertTrue(scope.matches(this.base2));
		assertTrue(scope.matches(this.base3));
		assertTrue(scope.matches(this.base4));
	}

	@Test
	public void addresses() {
		Scope<Address> scope = Scopes.addresses(this.base1, this.base2);
		assertTrue(scope.matches(this.base1));
		assertTrue(scope.matches(this.base2));
		assertFalse(scope.matches(this.base3));
		assertFalse(scope.matches(this.base4));
	}

	@Test
	public void notAddresses() {
		Scope<Address> scope = Scopes.notAddresses(this.base1, this.base2);
		assertFalse(scope.matches(this.base1));
		assertFalse(scope.matches(this.base2));
		assertTrue(scope.matches(this.base3));
		assertTrue(scope.matches(this.base4));
	}

	@Test
	public void not() {
		Scope<Address> s1 = Scopes.addresses(this.base1, this.base2);
		Scope<Address> scope = Scopes.not(s1);
		assertFalse(scope.matches(this.base1));
		assertFalse(scope.matches(this.base2));
		assertTrue(scope.matches(this.base3));
		assertTrue(scope.matches(this.base4));
	}

	@Test
	public void or() {
		Scope<Address> s1 = Scopes.addresses(this.base1, this.base2);
		Scope<Address> s2 = Scopes.addresses(this.base3, this.base2);
		Scope<Address> scope = Scopes.or(s1, s2);
		assertTrue(scope.matches(this.base1));
		assertTrue(scope.matches(this.base2));
		assertTrue(scope.matches(this.base3));
		assertFalse(scope.matches(this.base4));
	}

	@Test
	public void and() {
		Scope<Address> s1 = Scopes.addresses(this.base1, this.base2);
		Scope<Address> s2 = Scopes.addresses(this.base3, this.base2);
		Scope<Address> scope = Scopes.and(s1, s2);
		assertFalse(scope.matches(this.base1));
		assertTrue(scope.matches(this.base2));
		assertFalse(scope.matches(this.base3));
		assertFalse(scope.matches(this.base4));
	}

	@Test
	public void xor() {
		Scope<Address> s1 = Scopes.addresses(this.base1, this.base2);
		Scope<Address> s2 = Scopes.addresses(this.base3, this.base2);
		Scope<Address> scope = Scopes.xor(s1, s2);
		assertTrue(scope.matches(this.base1));
		assertFalse(scope.matches(this.base2));
		assertTrue(scope.matches(this.base3));
		assertFalse(scope.matches(this.base4));
	}

}

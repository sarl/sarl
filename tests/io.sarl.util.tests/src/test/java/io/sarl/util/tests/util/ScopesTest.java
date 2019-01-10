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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.util.tests.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import static org.mockito.Mockito.*;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Scope;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.ManualMocking;
import io.sarl.tests.api.Nullable;
import io.sarl.util.Scopes;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@ManualMocking
public final class ScopesTest extends AbstractSarlTest {

	@Nullable
	private UUID id1;

	@Nullable
	private UUID id2;

	@Mock
	private Address base1;

	@Mock
	private Address base2;

	@Mock
	private Address base3;

	@Mock
	private Address base4;

	@Before
	public void setUp() {
		this.id1 = UUID.randomUUID();
		this.id2 = UUID.randomUUID();
		MockitoAnnotations.initMocks(this);
		when(this.base1.getUUID()).thenReturn(this.id1);
		when(this.base2.getUUID()).thenReturn(this.id2);
		when(this.base3.getUUID()).thenReturn(UUID.randomUUID());
		when(this.base4.getUUID()).thenReturn(UUID.randomUUID());
	}
	
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
	public void identifiers() {
		Scope<Address> scope = Scopes.identifiers(this.id1, this.id2);
		assertTrue(scope.matches(this.base1));
		assertTrue(scope.matches(this.base2));
		assertFalse(scope.matches(this.base3));
		assertFalse(scope.matches(this.base4));
	}

	@Test
	public void notIdentifiers() {
		Scope<Address> scope = Scopes.notIdentifiers(this.id1, this.id2);
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

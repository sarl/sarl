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
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import io.sarl.lang.core.Address;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.ManualMocking;
import io.sarl.tests.api.Nullable;
import io.sarl.util.IdentifierScope;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@ManualMocking
public class IdentifierScopeTest extends AbstractSarlTest {

	@Nullable
	private UUID base1;

	@Nullable
	private UUID base2;

	@Mock
	private Address adr1;

	@Mock
	private Address adr2;

	@Mock
	private Address adr3;

	private IdentifierScope scope;

	@Before
	public void setUp() {
		this.base1 = UUID.randomUUID();
		this.base2 = UUID.randomUUID();
		MockitoAnnotations.initMocks(this);
		when(this.adr1.getUUID()).thenReturn(this.base1);
		when(this.adr2.getUUID()).thenReturn(this.base2);
		when(this.adr3.getUUID()).thenReturn(UUID.randomUUID());
		this.scope = new IdentifierScope(this.base1, this.base2);
	}

	@Test
	public void matches() {
		assertTrue(this.scope.matches(this.adr1));
		assertTrue(this.scope.matches(this.adr2));
		assertFalse(this.scope.matches(this.adr3));
	}

}


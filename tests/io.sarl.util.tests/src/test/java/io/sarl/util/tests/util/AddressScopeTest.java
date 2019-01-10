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

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;

import io.sarl.lang.core.Address;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.util.AddressScope;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class AddressScopeTest extends AbstractSarlTest {

	@Mock
	private Address base1;

	@Mock
	private Address base2;

	@Mock
	private Address base3;

	private AddressScope scope;

	@Before
	public void setUp() {
		this.scope = new AddressScope(this.base1, this.base2);
	}

	@Test
	public void matches() {
		assertTrue(this.scope.matches(this.base1));
		assertTrue(this.scope.matches(this.base2));
		assertFalse(this.scope.matches(this.base3));
	}

}


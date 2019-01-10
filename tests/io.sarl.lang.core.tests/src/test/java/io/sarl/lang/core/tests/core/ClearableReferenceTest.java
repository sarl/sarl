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
package io.sarl.lang.core.tests.core;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;

import io.sarl.lang.util.ClearableReference;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class ClearableReferenceTest extends AbstractSarlTest {

	@Mock
	private Object referencedObject;

	private ClearableReference<Object> reference;

	@Before
	public void setUp() {
		this.reference = mockReference(this.referencedObject);
	}

	private static ClearableReference<Object> mockReference(Object object) {
		ClearableReference<Object> reference = new ClearableReference<>(object);
		return spy(reference);
	}

	@Test
	public void get() {
		assertSame(this.referencedObject, this.reference.get());
	}

	@Test
	public void clear() {
		assertSame(this.referencedObject, this.reference.get());
		Object r = this.reference.clear();
		assertNull(this.reference.get());
		assertSame(this.referencedObject, r);
	}

}

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
package io.sarl.lang.core.tests.core;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.spy;
import static io.sarl.tests.api.tools.TestMockito.mock;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import io.sarl.lang.core.AtomicSkillReference;
import io.sarl.lang.core.Skill;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("AtomicSkillReference")
@Tag("unit")
@Tag("core")
public class AtomicSkillReferenceTest extends AbstractSarlTest {

	@Nullable
	private Skill referencedObject;

	@Nullable
	private AtomicSkillReference reference;

	@BeforeEach
	public void setUp() {
		this.referencedObject = new Skill() {};
		this.reference = new AtomicSkillReference(this.referencedObject);
	}

	@Test
	public void get() {
		assertEquals(1, this.referencedObject.getReferenceCount());
		assertSame(this.referencedObject, this.reference.get());
		assertEquals(1, this.referencedObject.getReferenceCount());
	}

	@Test
	public void clear() {
		assertEquals(1, this.referencedObject.getReferenceCount());
		assertSame(this.referencedObject, this.reference.get());
		Object r = this.reference.clear();
		assertEquals(0, this.referencedObject.getReferenceCount());
		assertNull(this.reference.get());
		assertSame(this.referencedObject, r);
		assertEquals(0, this.referencedObject.getReferenceCount());
	}

}
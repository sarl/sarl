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

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.UnimplementedCapacityException;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class UnimplementedCapacityExceptionTest extends AbstractSarlTest {

	private UUID agentID;

	private Class<? extends Capacity> capacity;

	private UnimplementedCapacityException exception;

	/**
	 */
	@Before
	public void setUp() {
		this.agentID = UUID.randomUUID();
		this.capacity = Capacity.class;
		this.exception = new UnimplementedCapacityException(
				this.capacity,
				this.agentID);
	}

	/**
	 */
	@Test
	public void getCallingAgent() {
		assertSame(this.agentID, this.exception.getCallingAgent());
	}

	/**
	 */
	@Test
	public void getUnimplementedCapacity() {
		assertSame(this.capacity, this.exception.getUnimplementedCapacity());
	}

}

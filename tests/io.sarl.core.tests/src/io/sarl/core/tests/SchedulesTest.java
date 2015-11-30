/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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
package io.sarl.core.tests;

import static org.junit.Assert.assertEquals;
import io.sarl.core.AgentTask;
import io.sarl.lang.core.Capacity;

import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.Before;
import org.junit.Test;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SchedulesTest extends AbstractSarlCoreTest<Capacity> {

	/**
	 */
	@Before
	public void setUp() {
		loadSARL("io.sarl.core.Schedules", Capacity.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void memberCount() {
		assertEquals(7, this.type.getDeclaredMethods().length);
	}

	/**
	 */
	@Test
	public void inLongProcedure1() {
		assertMethod("in", AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void inAgentTaskLongProcedure1() {
		assertMethod("in", AgentTask.class, AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void task() {
		assertMethod("task", AgentTask.class, String.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void cancelAgentTask() {
		assertMethod("cancel", boolean.class, AgentTask.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void cancelAgentTaskBoolean() {
		assertMethod("cancel", boolean.class, AgentTask.class, boolean.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void everyLongProcedure1() {
		assertMethod("every", AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void everyAgentTaskLongProcedure1() {
		assertMethod("every", AgentTask.class, AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

}

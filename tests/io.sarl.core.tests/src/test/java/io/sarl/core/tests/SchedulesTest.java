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
package io.sarl.core.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.core.AgentTask;
import io.sarl.lang.core.Capacity;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Schedules")
@Tag("unit")
@Tag("api")
public class SchedulesTest extends AbstractSarlCoreTest<Capacity> {

	@BeforeEach
	public void setUp() {
		loadSARL("io.sarl.core.Schedules", Capacity.class); //$NON-NLS-1$
	}

	@Test
	public void memberCount() {
		assertEquals(0, this.type.getDeclaredFields().length, "number of fields");
		assertEquals(0, this.type.getDeclaredConstructors().length, "number of constructors");
		assertEquals(10+6+6, this.type.getDeclaredMethods().length, "number of methods");
	}

	@Test
	public void inLongProcedure1() {
		assertMethod("in", AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	@Test
	public void inAgentTaskLongProcedure1() {
		assertMethod("in", AgentTask.class, AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	@Test
	public void atLongProcedure1() {
		assertMethod("at", AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	@Test
	public void atAgentTaskLongProcedure1() {
		assertMethod("at", AgentTask.class, AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	@Test
	public void task() {
		assertMethod("task", AgentTask.class, String.class); //$NON-NLS-1$
	}

	@Test
	public void setName() {
		assertMethod("setName", void.class, AgentTask.class, String.class); //$NON-NLS-1$
	}

	@Test
	public void cancelAgentTask() {
		assertMethod("cancel", boolean.class, AgentTask.class); //$NON-NLS-1$
	}

	@Test
	public void cancelAgentTaskBoolean() {
		assertMethod("cancel", boolean.class, AgentTask.class, boolean.class); //$NON-NLS-1$
	}

	@Test
	public void isCanceledAgentTask() {
		assertMethod("isCanceled", boolean.class, AgentTask.class); //$NON-NLS-1$
	}

	@Test
	public void everyLongProcedure1() {
		assertMethod("every", AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	@Test
	public void everyAgentTaskLongProcedure1() {
		assertMethod("every", AgentTask.class, AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	@Test
	public void atFixedDelayLongProcedure1() {
		assertMethod("atFixedDelay", AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	@Test
	public void atFixedDelayAgentTaskLongProcedure1() {
		assertMethod("atFixedDelay", AgentTask.class, AgentTask.class, long.class, Procedure1.class); //$NON-NLS-1$
	}

	@Test
	public void executeProcedure1() {
		assertMethod("execute", AgentTask.class, Procedure1.class); //$NON-NLS-1$
	}

	@Test
	public void executeAgentTaskProcedure1() {
		assertMethod("execute", AgentTask.class, AgentTask.class, Procedure1.class); //$NON-NLS-1$
	}

}

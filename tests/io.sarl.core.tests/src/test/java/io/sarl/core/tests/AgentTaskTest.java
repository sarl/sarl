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
package io.sarl.core.tests;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doReturn;

import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentMatchers;

import io.sarl.core.AgentTask;
import io.sarl.lang.core.Agent;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class AgentTaskTest extends AbstractSarlTest {

	private AgentTask task;

	@Before
	public void setUp() {
		this.task = new AgentTask();
	}

	@Test
	public void getProcedure() {
		assertNull(this.task.getProcedure());
		Procedure1 proc = mock(Procedure1.class);
		this.task.setProcedure(proc);
		assertSame(proc, this.task.getProcedure());
	}

	@Test
	public void setProcedure() {
		Procedure1 proc = mock(Procedure1.class);
		this.task.setProcedure(proc);
		assertSame(proc, this.task.getProcedure());
		this.task.setProcedure(null);
		assertNull(this.task.getProcedure());
	}

	@Test
	public void getGuard() {
		assertNull(this.task.getGuard());
		Function1 func = mock(Function1.class);
		this.task.setGuard(func);
		assertSame(func, this.task.getGuard());
	}

	@Test
	public void setGuard() {
		Function1 func = mock(Function1.class);
		this.task.setGuard(func);
		assertSame(func, this.task.getGuard());
		this.task.setGuard(null);
		assertNull(this.task.getGuard());
	}

	@Test
	public void getName() {
		assertNull(this.task.getName());
		String name = UUID.randomUUID().toString();
		this.task.setTaskName(name);
		assertSame(name, this.task.getName());
	}

	@Test
	public void setTaskName() {
		String name = UUID.randomUUID().toString();
		this.task.setTaskName(name);
		assertSame(name, this.task.getName());
		this.task.setTaskName(null);
		assertNull(this.task.getName());
	}

	@Test
	public void unless_positive() {
		Function1<? super Agent, ? extends Boolean> guard = (agent) -> Boolean.TRUE;

		assertSame(this.task, this.task.unless(guard));

		Function1<? super Agent, ? extends Boolean> negativeGuard = this.task.getGuard();
		assertNotSame(guard, negativeGuard);
		assertFalse(negativeGuard.apply(null));
	}

	@Test
	public void unless_negative() {
		Function1<? super Agent, ? extends Boolean> guard = (agent) -> Boolean.FALSE;

		assertSame(this.task, this.task.unless(guard));

		Function1<? super Agent, ? extends Boolean> negativeGuard = this.task.getGuard();
		assertNotSame(guard, negativeGuard);
		assertTrue(negativeGuard.apply(null));
	}

	@Test
	public void ifTrue() {
		Function1<? super Agent, ? extends Boolean> guard = mock(Function1.class);
		doReturn(Boolean.TRUE).when(guard).apply(ArgumentMatchers.any(Agent.class));

		assertSame(this.task, this.task.ifTrue(guard));

		Function1<? super Agent, ? extends Boolean> newGuard = this.task.getGuard();
		assertSame(guard, newGuard);
	}

}

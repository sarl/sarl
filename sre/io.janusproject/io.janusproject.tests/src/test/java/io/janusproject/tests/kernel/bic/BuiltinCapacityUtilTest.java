/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.janusproject.tests.kernel.bic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import io.janusproject.kernel.bic.BuiltinCapacityUtil;
import io.janusproject.kernel.bic.InnerContextSkill;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.ExternalContextAccess;
import io.sarl.core.InnerContextAccess;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Skill;
import io.sarl.lang.util.ClearableReference;
import io.sarl.lang.util.SynchronizedIterable;
import io.sarl.tests.api.Nullable;
import io.sarl.util.concurrent.Collections3;
import io.sarl.util.concurrent.NoReadWriteLock;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class BuiltinCapacityUtilTest extends AbstractJanusTest {

	@Nullable
	private InnerContextSkill innerSkill;

	@Nullable
	private ExternalContextAccess contextSkill;

	@Nullable
	private Agent agent;

	@Before
	public void setUp() {
		this.innerSkill = mock(InnerContextSkill.class);
		this.contextSkill = mock(ExternalContextAccess.class);
		this.agent = new TestAgent(this.innerSkill, this.contextSkill);
		this.agent = Mockito.spy(this.agent);
	}

	@Test
	public void getContextsOf_emptycontextlist() throws Exception {
		Mockito.when(this.contextSkill.getAllContexts())
				.thenReturn(Collections3.synchronizedCollection(Collections.<AgentContext> emptyList(), NoReadWriteLock.SINGLETON));
		SynchronizedIterable<AgentContext> c = BuiltinCapacityUtil.getContextsOf(this.agent);
		assertNotNull(c);
		assertFalse(c.iterator().hasNext());
	}

	@Test
	public void getContextsOf_onecontext() throws Exception {
		AgentContext context = mock(AgentContext.class);
		Mockito.when(this.contextSkill.getAllContexts())
				.thenReturn(Collections3.synchronizedCollection(Collections.singletonList(context), NoReadWriteLock.SINGLETON));
		SynchronizedIterable<AgentContext> c = BuiltinCapacityUtil.getContextsOf(this.agent);
		assertNotNull(c);
		List<AgentContext> list = new ArrayList<>();
		Iterator<AgentContext> iterator = c.iterator();
		while (iterator.hasNext()) {
			list.add(iterator.next());
		}
		assertFalse(list.isEmpty());
		assertEquals(1, list.size());
		assertTrue(list.contains(context));
	}

	@Test
	public void getContextsOf_twocontexts() throws Exception {
		AgentContext context1 = mock(AgentContext.class);
		AgentContext context2 = mock(AgentContext.class);
		Mockito.when(this.contextSkill.getAllContexts())
				.thenReturn(Collections3.synchronizedCollection(Arrays.asList(context1, context2), NoReadWriteLock.SINGLETON));
		SynchronizedIterable<AgentContext> c = BuiltinCapacityUtil.getContextsOf(this.agent);
		assertNotNull(c);
		List<AgentContext> list = new ArrayList<>();
		Iterator<AgentContext> iterator = c.iterator();
		while (iterator.hasNext()) {
			list.add(iterator.next());
		}
		assertFalse(list.isEmpty());
		assertEquals(2, list.size());
		assertTrue(list.contains(context1));
		assertTrue(list.contains(context2));
	}

	@Test
	public void getContextIn_genericbic_noinnercontext() throws Exception {
		Mockito.when(this.innerSkill.getInnerContext()).thenReturn(null);
		Mockito.doReturn(false).when(this.innerSkill).hasInnerContext();
		AgentContext context = BuiltinCapacityUtil.getContextIn(this.agent);
		assertNull(context);
	}

	@Test
	public void getContextIn_genericbic_innercontext() throws Exception {
		AgentContext innerContext = mock(AgentContext.class);
		Mockito.when(this.innerSkill.getInnerContext()).thenReturn(innerContext);
		Mockito.doReturn(true).when(this.innerSkill).hasInnerContext();
		AgentContext context = BuiltinCapacityUtil.getContextIn(this.agent);
		assertSame(innerContext, context);
	}

	@Test
	public void getContextIn_janusbic_noinnercontext() throws Exception {
		this.innerSkill = mock(InnerContextSkill.class);
		Mockito.when(this.innerSkill.getInnerContext()).thenReturn(null);
		Mockito.doReturn(false).when(this.innerSkill).hasInnerContext();
		AgentContext context = BuiltinCapacityUtil.getContextIn(this.agent);
		assertNull(context);
	}

	public static class TestAgent extends Agent {
		
		@Nullable
		private final InnerContextAccess innerSkill;

		@Nullable
		private final ExternalContextAccess contextSkill;
		
		public TestAgent(InnerContextAccess innerSkill, ExternalContextAccess contextSkill) {
			super(mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null);
			this.innerSkill = innerSkill;
			this.contextSkill = contextSkill;
		}
		
		@Override
		protected ClearableReference<Skill> $getSkill(Class<? extends Capacity> capacity) {
			if (capacity.equals(InnerContextAccess.class))
				return new ClearableReference(this.innerSkill);
			return new ClearableReference(this.contextSkill);
		}
	}

}

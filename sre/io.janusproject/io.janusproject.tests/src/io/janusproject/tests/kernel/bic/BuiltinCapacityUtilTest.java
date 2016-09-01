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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.UUID;

import io.janusproject.kernel.bic.BuiltinCapacityUtil;
import io.janusproject.kernel.bic.InnerContextSkill;
import io.janusproject.tests.testutils.AbstractJanusTest;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import io.sarl.core.ExternalContextAccess;
import io.sarl.core.InnerContextAccess;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.util.Collections3;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class BuiltinCapacityUtilTest extends AbstractJanusTest {

	@Nullable
	private InnerContextAccess innerSkill;

	@Nullable
	private ExternalContextAccess contextSkill;

	@Nullable
	private Agent agent;

	@Before
	public void setUp() {
		this.innerSkill = Mockito.mock(InnerContextAccess.class);
		this.contextSkill = Mockito.mock(ExternalContextAccess.class);
		this.agent = new Agent(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null) {
			/**
			 * {@inheritDoc}
			 */
			@Override
			protected <S extends Capacity> S getSkill(Class<S> capacity) {
				if (capacity.equals(InnerContextAccess.class))
					return capacity.cast(BuiltinCapacityUtilTest.this.innerSkill);
				return capacity.cast(BuiltinCapacityUtilTest.this.contextSkill);
			}
		};
		this.agent = Mockito.spy(this.agent);
	}

	@Test
	public void getContextsOf_emptycontextlist() throws Exception {
		Mockito.when(this.contextSkill.getAllContexts())
				.thenReturn(Collections3.synchronizedCollection(Collections.<AgentContext> emptyList(), this));
		SynchronizedCollection<AgentContext> c = BuiltinCapacityUtil.getContextsOf(this.agent);
		assertNotNull(c);
		assertTrue(c.isEmpty());
	}

	@Test
	public void getContextsOf_onecontext() throws Exception {
		AgentContext context = Mockito.mock(AgentContext.class);
		Mockito.when(this.contextSkill.getAllContexts())
				.thenReturn(Collections3.synchronizedCollection(Collections.singletonList(context), this));
		SynchronizedCollection<AgentContext> c = BuiltinCapacityUtil.getContextsOf(this.agent);
		assertNotNull(c);
		assertFalse(c.isEmpty());
		assertEquals(1, c.size());
		assertTrue(c.contains(context));
	}

	@Test
	public void getContextsOf_twocontexts() throws Exception {
		AgentContext context1 = Mockito.mock(AgentContext.class);
		AgentContext context2 = Mockito.mock(AgentContext.class);
		Mockito.when(this.contextSkill.getAllContexts())
				.thenReturn(Collections3.synchronizedCollection(Arrays.asList(context1, context2), this));
		SynchronizedCollection<AgentContext> c = BuiltinCapacityUtil.getContextsOf(this.agent);
		assertNotNull(c);
		assertFalse(c.isEmpty());
		assertEquals(2, c.size());
		assertTrue(c.contains(context1));
		assertTrue(c.contains(context2));
	}

	@Test
	public void getContextIn_genericbic_noinnercontext() throws Exception {
		Mockito.when(this.innerSkill.getInnerContext()).thenReturn(null);
		AgentContext context = BuiltinCapacityUtil.getContextIn(this.agent);
		assertNull(context);
	}

	@Test
	public void getContextIn_genericbic_innercontext() throws Exception {
		AgentContext innerContext = Mockito.mock(AgentContext.class);
		Mockito.when(this.innerSkill.getInnerContext()).thenReturn(innerContext);
		AgentContext context = BuiltinCapacityUtil.getContextIn(this.agent);
		assertSame(innerContext, context);
	}

	@Test
	public void getContextIn_janusbic_noinnercontext() throws Exception {
		this.innerSkill = Mockito.mock(InnerContextSkill.class);
		Mockito.when(this.innerSkill.getInnerContext()).thenReturn(null);
		Mockito.when(this.reflect.invoke(
				((InnerContextSkill) this.innerSkill), "hasInnerContext")).thenReturn(false);
		AgentContext context = BuiltinCapacityUtil.getContextIn(this.agent);
		assertNull(context);
	}

	@Test
	public void getContextIn_janusgeneric_innercontext() throws Exception {
		this.innerSkill = Mockito.mock(InnerContextSkill.class);
		AgentContext innerContext = Mockito.mock(AgentContext.class);
		Mockito.when(this.innerSkill.getInnerContext()).thenReturn(innerContext);
		Mockito.when(this.reflect.invoke(
				((InnerContextSkill) this.innerSkill), "hasInnerContext")).thenReturn(true);
		AgentContext context = BuiltinCapacityUtil.getContextIn(this.agent);
		assertSame(innerContext, context);
	}

}

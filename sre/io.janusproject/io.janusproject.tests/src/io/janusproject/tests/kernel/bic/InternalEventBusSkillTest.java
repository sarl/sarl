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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;

import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.internal.verification.Times;

import io.janusproject.kernel.bic.InternalEventBusSkill;
import io.janusproject.kernel.bic.internaleventdispatching.AgentInternalEventsDispatcher;
import io.janusproject.services.logging.LogService;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.Destroy;
import io.sarl.core.Initialize;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class InternalEventBusSkillTest extends AbstractJanusTest {

	@Mock
	private AgentInternalEventsDispatcher eventBus;

	@Mock
	private LogService logger;

	@Mock
	private Agent agent;

	@Mock
	private Address innerAddress;

	@InjectMocks
	private InternalEventBusSkill skill;

	@Test
	public void asEventListener() {
		assertNotNull(this.skill.asEventListener());
	}

	@Test
	public void getInnerDefaultSpaceAddress() {
		assertSame(this.innerAddress, this.skill.getInnerDefaultSpaceAddress());
	}

	@Test
	public void registerEventListener() {
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.skill.registerEventListener(eventListener);
		ArgumentCaptor<Object> argument = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.eventBus, new Times(1)).register(argument.capture());
		assertSame(eventListener, argument.getValue());
	}

	@Test
	public void unregisterEventListener() {
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.skill.registerEventListener(eventListener);
		//
		this.skill.unregisterEventListener(eventListener);
		ArgumentCaptor<Object> argument = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.eventBus, new Times(1)).unregister(argument.capture());
		assertSame(eventListener, argument.getValue());
	}

	@Test
	public void hasRegisteredEventListener() {
		assertFalse(this.skill.hasRegisteredEventListener(Object.class));
		//
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.skill.registerEventListener(eventListener);
		assertTrue(this.skill.hasRegisteredEventListener(Object.class));
		assertTrue(this.skill.hasRegisteredEventListener(EventListener.class));
		assertFalse(this.skill.hasRegisteredEventListener(Behavior.class));
	}

	@Test
	public void getRegisteredEventListeners() {
		Collection<Object> collection;
		//
		collection = new ArrayList<>();
		assertEquals(0, this.skill.getRegisteredEventListeners(Object.class, collection));
		assertContains(collection);
		//
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.skill.registerEventListener(eventListener);
		//
		collection = new ArrayList<>();
		assertEquals(1, this.skill.getRegisteredEventListeners(Object.class, collection));
		assertContains(collection, eventListener);
		//
		collection = new ArrayList<>();
		assertEquals(1, this.skill.getRegisteredEventListeners(EventListener.class, collection));
		assertContains(collection, eventListener);
		//
		collection = new ArrayList<>();
		assertEquals(0, this.skill.getRegisteredEventListeners(Behavior.class, collection));
		assertContains(collection);
	}

	@Test
	public void install() throws Exception {
		this.reflect.invoke(this.skill, "install");
		ArgumentCaptor<Object> argument = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.eventBus, new Times(1)).register(argument.capture());
		assertSame(this.agent, argument.getValue());
	}

	@Test
	public void uninstall() throws Exception {
		this.reflect.invoke(this.skill, "install");
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.skill.registerEventListener(eventListener);
		//
		this.reflect.invoke(this.skill, "uninstall");
		ArgumentCaptor<Object> argument = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.eventBus, new Times(2)).unregister(argument.capture());
		assertSame(this.agent, argument.getAllValues().get(0));
		assertSame(eventListener, argument.getAllValues().get(1));
	}

	@Test
	public void selfEvent_other_notinitialized() {
		Event event = Mockito.mock(Event.class);
		this.skill.selfEvent(event);
		Mockito.verifyZeroInteractions(this.eventBus);
	}

	@Test
	public void selfEvent_other_initialized() {
		Initialize initEvent = Mockito.mock(Initialize.class);
		this.skill.selfEvent(initEvent);
		//
		Event event = Mockito.mock(Event.class);
		this.skill.selfEvent(event);
		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, new Times(1)).asyncDispatch(argument.capture());
		assertSame(event, argument.getValue());
	}

	@Test
	public void selfEvent_initialize() {
		Initialize event = Mockito.mock(Initialize.class);
		this.skill.selfEvent(event);
		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, new Times(1)).immediateDispatch(argument.capture());
		assertSame(event, argument.getValue());
	}

	@Test
	public void selfEvent_destroy() {
		Destroy event = Mockito.mock(Destroy.class);
		this.skill.selfEvent(event);
		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, new Times(1)).immediateDispatch(argument.capture());
		assertSame(event, argument.getValue());
	}

}

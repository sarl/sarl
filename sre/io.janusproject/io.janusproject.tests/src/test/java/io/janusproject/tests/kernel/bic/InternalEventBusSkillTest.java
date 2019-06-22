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

import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;

import javax.inject.Inject;

import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import io.janusproject.kernel.bic.InternalEventBusCapacity;
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
import io.sarl.lang.core.Skill.UninstallationStage;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class InternalEventBusSkillTest extends AbstractJanusTest {

	@Inject
	private ReflectExtensions reflect;

	@Mock(name="eventDispatcher")
	private AgentInternalEventsDispatcher eventBus;

	@Mock(name="logger")
	private LogService logger;

	@Mock
	private Agent agent;

	@Mock
	private Address innerAddress;

	private InternalEventBusSkill skill;

	@Before
	public void setUp() throws Exception {
		this.skill = this.reflect.newInstance(InternalEventBusSkill.class, this.agent, this.innerAddress);
		this.reflect.set(this.skill, "eventDispatcher", this.eventBus);
		this.reflect.set(this.skill, "logger", this.logger);
	}
	
	@Test
	public void asEventListener() {
		assertNotNull(this.skill.asEventListener());
	}

	@Test
	public void getInnerDefaultSpaceAddress() {
		assertSame(this.innerAddress, this.skill.getInnerDefaultSpaceAddress());
	}

	@Test
	public void registerEventListener_null_disableInitializeFiring() {
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.skill.registerEventListener(eventListener, false, null);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument2 = ArgumentCaptor.forClass(Function1.class);
		ArgumentCaptor<Procedure1<Object>> argument3 = ArgumentCaptor.forClass(Procedure1.class);
		ArgumentCaptor<Object> argument4 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Event> argument5 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).register(argument1.capture(), argument2.capture(), argument3.capture());
		Mockito.verify(this.eventBus, Mockito.never()).immediateDispatchTo(argument4.capture(), argument5.capture());
		assertSame(eventListener, argument1.getValue());
		assertNull(argument2.getValue());
	}

	@Test
	public void registerEventListener_validFilter_disableInitializeFiring() {
		EventListener eventListener = Mockito.mock(EventListener.class);
		Function1<? super Event, ? extends Boolean> filter = (event) -> true;
		this.skill.registerEventListener(eventListener, false, filter);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument2 = ArgumentCaptor.forClass(Function1.class);
		ArgumentCaptor<Procedure1<Object>> argument3 = ArgumentCaptor.forClass(Procedure1.class);
		ArgumentCaptor<Object> argument4 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Event> argument5 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).register(argument1.capture(), argument2.capture(), argument3.capture());
		Mockito.verify(this.eventBus, Mockito.never()).immediateDispatchTo(argument4.capture(), argument5.capture());
		assertSame(eventListener, argument1.getValue());
		assertSame(filter, argument2.getValue());
	}

	@Test
	public void registerEventListener_invalidFilter_disableInitializeFiring() {
		EventListener eventListener = Mockito.mock(EventListener.class);
		Function1<? super Event, ? extends Boolean> filter = (event) -> false;
		this.skill.registerEventListener(eventListener, false, filter);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument2 = ArgumentCaptor.forClass(Function1.class);
		ArgumentCaptor<Procedure1<Object>> argument3 = ArgumentCaptor.forClass(Procedure1.class);
		ArgumentCaptor<Object> argument4 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Event> argument5 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).register(argument1.capture(), argument2.capture(), argument3.capture());
		Mockito.verify(this.eventBus, Mockito.never()).immediateDispatchTo(argument4.capture(), argument5.capture());
		assertSame(eventListener, argument1.getValue());
		assertSame(filter, argument2.getValue());
	}

	@Test
	public void registerEventListener_null_enableInitializeFiring_running() throws Exception {
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.reflect.invoke(this.skill, "setOwnerState", InternalEventBusCapacity.OwnerState.ALIVE);
		this.skill.registerEventListener(eventListener, true, null);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument2 = ArgumentCaptor.forClass(Function1.class);
		ArgumentCaptor<Procedure1<Object>> argument3 = ArgumentCaptor.forClass(Procedure1.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).register(argument1.capture(), argument2.capture(), argument3.capture());
		assertSame(eventListener, argument1.getValue());
		assertNull(argument2.getValue());
		assertNotNull(argument3.getValue());
	}

	@Test
	public void registerEventListener_validFilter_enableInitializeFiring_running() throws Exception {
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.reflect.invoke(this.skill, "setOwnerState", InternalEventBusCapacity.OwnerState.ALIVE);
		Function1<? super Event, ? extends Boolean> filter = (event) -> true;
		this.skill.registerEventListener(eventListener, true, filter);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument2 = ArgumentCaptor.forClass(Function1.class);
		ArgumentCaptor<Procedure1<Object>> argument3 = ArgumentCaptor.forClass(Procedure1.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).register(argument1.capture(), argument2.capture(), argument3.capture());
		assertSame(eventListener, argument1.getValue());
		assertSame(filter, argument2.getValue());
		assertNotNull(argument3.getValue());
	}

	@Test
	public void registerEventListener_invalidFilter_enableInitializeFiring_running() throws Exception {
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.reflect.invoke(this.skill, "setOwnerState", InternalEventBusCapacity.OwnerState.ALIVE);
		Function1<? super Event, ? extends Boolean> filter = (event) -> false;
		this.skill.registerEventListener(eventListener, true, filter);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument2 = ArgumentCaptor.forClass(Function1.class);
		ArgumentCaptor<Procedure1<Object>> argument3 = ArgumentCaptor.forClass(Procedure1.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).register(argument1.capture(), argument2.capture(), argument3.capture());
		assertSame(eventListener, argument1.getValue());
		assertSame(filter, argument2.getValue());
		assertNotNull(argument3.getValue());
	}

	@Test
	public void registerEventListener_null_enableInitializeFiring_notRunning() {
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.skill.registerEventListener(eventListener, true, null);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument2 = ArgumentCaptor.forClass(Function1.class);
		ArgumentCaptor<Procedure1<Object>> argument3 = ArgumentCaptor.forClass(Procedure1.class);
		ArgumentCaptor<Object> argument4 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Event> argument5 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).register(argument1.capture(), argument2.capture(), argument3.capture());
		Mockito.verify(this.eventBus, Mockito.never()).immediateDispatchTo(argument4.capture(), argument5.capture());
		assertSame(eventListener, argument1.getValue());
		assertNull(argument2.getValue());
	}

	@Test
	public void registerEventListener_validFilter_enableInitializeFiring_notRunning() {
		EventListener eventListener = Mockito.mock(EventListener.class);
		Function1<? super Event, ? extends Boolean> filter = (event) -> true;
		this.skill.registerEventListener(eventListener, true, filter);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument2 = ArgumentCaptor.forClass(Function1.class);
		ArgumentCaptor<Procedure1<Object>> argument3 = ArgumentCaptor.forClass(Procedure1.class);
		ArgumentCaptor<Object> argument4 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Event> argument5 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).register(argument1.capture(), argument2.capture(), argument3.capture());
		Mockito.verify(this.eventBus, Mockito.never()).immediateDispatchTo(argument4.capture(), argument5.capture());
		assertSame(eventListener, argument1.getValue());
		assertSame(filter, argument2.getValue());
	}

	@Test
	public void registerEventListener_invalidFilter_enableInitializeFiring_notRunning() {
		EventListener eventListener = Mockito.mock(EventListener.class);
		Function1<? super Event, ? extends Boolean> filter = (event) -> false;
		this.skill.registerEventListener(eventListener, true, filter);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument2 = ArgumentCaptor.forClass(Function1.class);
		ArgumentCaptor<Procedure1<Object>> argument3 = ArgumentCaptor.forClass(Procedure1.class);
		ArgumentCaptor<Object> argument4 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Event> argument5 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).register(argument1.capture(), argument2.capture(), argument3.capture());
		Mockito.verify(this.eventBus, Mockito.never()).immediateDispatchTo(argument4.capture(), argument5.capture());
		assertSame(eventListener, argument1.getValue());
		assertSame(filter, argument2.getValue());
	}

	@Test
	public void unregisterEventListener_enableDestroyFiring_running() throws Exception {
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.reflect.invoke(this.skill, "setOwnerState", InternalEventBusCapacity.OwnerState.ALIVE);
		this.reflect.invoke(this.skill, "setOwnerState", InternalEventBusCapacity.OwnerState.ALIVE);
		this.skill.registerEventListener(eventListener, false, null);
		//
		this.skill.unregisterEventListener(eventListener, true);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Procedure1<Object>> argument2 = ArgumentCaptor.forClass(Procedure1.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).unregister(argument1.capture(), argument2.capture());
		assertSame(eventListener, argument1.getValue());
		assertNotNull(argument2.getValue());
	}

	@Test
	public void unregisterEventListener_enableDestroyFiring_notRunning() {
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.skill.registerEventListener(eventListener, false, null);
		//
		this.skill.unregisterEventListener(eventListener, true);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Procedure1<Object>> argument2 = ArgumentCaptor.forClass(Procedure1.class);
		ArgumentCaptor<Object> argument3 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Event> argument4 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).unregister(argument1.capture(), argument2.capture());
		Mockito.verify(this.eventBus, Mockito.never()).immediateDispatchTo(argument3.capture(), argument4.capture());
		assertSame(eventListener, argument1.getValue());
	}

	@Test
	public void unregisterEventListener_disableDestroyFiring() {
		EventListener eventListener = Mockito.mock(EventListener.class);
		this.skill.registerEventListener(eventListener, false, null);
		//
		this.skill.unregisterEventListener(eventListener, false);
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Procedure1<Object>> argument2 = ArgumentCaptor.forClass(Procedure1.class);
		ArgumentCaptor<Object> argument3 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Event> argument4 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).unregister(argument1.capture(), argument2.capture());
		Mockito.verify(this.eventBus, Mockito.never()).immediateDispatchTo(argument3.capture(), argument4.capture());
		assertSame(eventListener, argument1.getValue());
	}

	@Test
	public void install() throws Exception {
		this.reflect.invoke(this.skill, "install");
		ArgumentCaptor<Object> argument1 = ArgumentCaptor.forClass(Object.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument2 = ArgumentCaptor.forClass(Function1.class);
		ArgumentCaptor<Procedure1<Object>> argument3 = ArgumentCaptor.forClass(Procedure1.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).register(argument1.capture(), argument2.capture(), argument3.capture());
		assertSame(this.agent, argument1.getValue());
		assertNull(argument2.getValue());
		assertNull(argument3.getValue());
	}

	@Test
	public void uninstall_Pre() throws Exception {
		this.reflect.invoke(this.skill, "install");
		//
		this.reflect.invoke(this.skill, "uninstall", UninstallationStage.PRE_DESTROY_EVENT);
		ArgumentCaptor<Procedure1<Object>> argument = ArgumentCaptor.forClass(Procedure1.class);
		Mockito.verify(this.eventBus, Mockito.never()).unregisterAll(argument.capture());
	}

	@Test
	public void uninstall_Post() throws Exception {
		this.reflect.invoke(this.skill, "install");
		//
		this.reflect.invoke(this.skill, "uninstall", UninstallationStage.POST_DESTROY_EVENT);
		ArgumentCaptor<Procedure1<Object>> argument = ArgumentCaptor.forClass(Procedure1.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).unregisterAll(argument.capture());
	}

	@Test
	public void selfEvent_other_notinitialized() {
		Event event = Mockito.mock(Event.class);
		this.skill.selfEvent(event);
		Mockito.verifyZeroInteractions(this.eventBus);
	}

	@Test
	public void selfEvent_other_initialized() {
		Initialize initEvent = new Initialize(UUID.randomUUID());
		this.skill.selfEvent(initEvent);
		//
		Event event = Mockito.mock(Event.class);
		this.skill.selfEvent(event);
		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).asyncDispatch(argument.capture());
		assertSame(event, argument.getValue());
	}

	@Test
	public void selfEvent_initialize() {
		Initialize event = new Initialize(UUID.randomUUID());
		this.skill.selfEvent(event);
		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).immediateDispatch(argument.capture());
		assertSame(event, argument.getValue());
	}

	@Test
	public void selfEvent_destroy() {
		Destroy event = new Destroy();
		this.skill.selfEvent(event);
		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.eventBus, Mockito.times(1)).immediateDispatch(argument.capture());
		assertSame(event, argument.getValue());
	}

}

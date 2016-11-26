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
package io.janusproject.tests.kernel.bic.internaleventdispatching;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;

import io.janusproject.kernel.bic.internaleventdispatching.AgentInternalEventsDispatcher;
import io.janusproject.services.executor.ExecutorService;

import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Event;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;
import io.sarl.util.Collections3;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class AgentInternalEventsDispatcherTest extends AbstractSarlTest {

	@Nullable
	private AgentInternalEventsDispatcher dispatcher;

	@Mock
	private ExecutorService executorService;

	@Before
	public void setUp() {
		Mockito.doAnswer((it) -> {
			((Runnable) it.getArgument(0)).run();
			return null;
		}).when(this.executorService).execute(Mockito.any(Runnable.class));
		this.dispatcher = new AgentInternalEventsDispatcher(this.executorService);
	}

	@Test
	public void immediateDispatch_noRegister_event() {
		this.dispatcher.immediateDispatch(new Event() { });
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.never()).execute(argument.capture());
	}

	@Test
	public void immediateDispatch_noRegister_myEvent() {
		this.dispatcher.immediateDispatch(new MyEvent(0));
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.never()).execute(argument.capture());
	}

	@Test
	public void immediateDispatch_register_event() {
		this.dispatcher.register(new MyAgent());
		this.dispatcher.immediateDispatch(new Event() { });
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.only()).execute(argument.capture());
	}

	@Test
	public void immediateDispatch_register_myEvent_n0() {
		this.dispatcher.register(new MyAgent());
		this.dispatcher.immediateDispatch(new MyEvent(0));
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.only()).execute(argument.capture());
	}

	@Test
	public void immediateDispatch_register_myEvent_n1() {
		this.dispatcher.register(new MyAgent());
		this.dispatcher.immediateDispatch(new MyEvent(1));
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.times(2)).execute(argument.capture());
	}

	@Test
	public void asyncDispatch_noRegister_event() {
		this.dispatcher.asyncDispatch(new Event() { });
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.only()).execute(argument.capture());
	}

	@Test
	public void asyncDispatch_noRegister_myEvent() {
		this.dispatcher.asyncDispatch(new MyEvent(0));
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.only()).execute(argument.capture());
	}

	@Test
	public void asyncDispatch_register_event() {
		this.dispatcher.register(new MyAgent());
		this.dispatcher.asyncDispatch(new Event() { });
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.times(2)).execute(argument.capture());
	}

	@Test
	public void asyncDispatch_register_myEvent_n0() {
		this.dispatcher.register(new MyAgent());
		this.dispatcher.asyncDispatch(new MyEvent(0));
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.times(2)).execute(argument.capture());
	}

	@Test
	public void asyncDispatch_register_myEvent_n1() {
		this.dispatcher.register(new MyAgent());
		this.dispatcher.asyncDispatch(new MyEvent(1));
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.times(3)).execute(argument.capture());
	}

	@Test
	public void unregister() {
		Object ag = new MyAgent();
		this.dispatcher.register(ag);
		this.dispatcher.unregister(ag);
		this.dispatcher.immediateDispatch(new Event() { });
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService, Mockito.never()).execute(argument.capture());
	}

	public static class MyEvent extends Event {
		public final int n;
		public MyEvent(int n) {
			this.n = n;
		}
	}

	public static class MyAgent extends Agent {

		public MyAgent() {
			super(null, UUID.randomUUID(), UUID.randomUUID());
		}

		@PerceptGuardEvaluator
		private void $perception$guard$evaluator1(Event event, Collection<Runnable> runners) {
			runners.add(() -> $perception$guard$callback1(event, event));
		}

		private void $perception$guard$callback1(Event occurrence, Event it) {
		}

		@PerceptGuardEvaluator
		private void $perception$guard$evaluator2(MyEvent event, Collection<Runnable> runners) {
			if (event.n > 0) {
				runners.add(() -> $perception$guard$callback2(event, event));
			}
		}

		private void $perception$guard$callback2(MyEvent occurrence, MyEvent it) {
		}

	}

}

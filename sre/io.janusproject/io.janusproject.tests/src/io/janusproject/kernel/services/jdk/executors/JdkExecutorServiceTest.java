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
package io.janusproject.kernel.services.jdk.executors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.internal.verification.Times;

import io.janusproject.testutils.AbstractDependentServiceTest;
import io.janusproject.testutils.AvoidServiceStartForTest;
import io.janusproject.testutils.StartServiceForTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@StartServiceForTest
public final class JdkExecutorServiceTest extends AbstractDependentServiceTest<JdkExecutorService> {

	@Mock
	private Runnable runnable;

	@Mock
	private Callable<?> callable;

	@Mock
	private ScheduledExecutorService scheduledExecutorService;

	@Mock
	private ExecutorService executorService;

	/**
	 * 
	 */
	public JdkExecutorServiceTest() {
		super(io.janusproject.services.executor.ExecutorService.class);
	}

	@Override
	public JdkExecutorService newService() {
		return new JdkExecutorService();
	}

	@Override
	public void getServiceDependencies() {
		assertContains(this.service.getServiceDependencies());
	}

	@Override
	public void getServiceWeakDependencies() {
		assertContains(this.service.getServiceWeakDependencies());
	}

	@Test
	public void submitRunnable() {
		this.service.submit(this.runnable);
		ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
		Mockito.verify(this.executorService).submit(argument.capture());
		assertSame(this.runnable, argument.getValue());
	}

	@Test
	public void submitRunnableObject() {
		Object result = Mockito.mock(Object.class);
		this.service.submit(this.runnable, result);
		ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
		ArgumentCaptor<Object> argument2 = ArgumentCaptor.forClass(Object.class);
		Mockito.verify(this.executorService).submit(argument1.capture(), argument2.capture());
		assertSame(this.runnable, argument1.getValue());
		assertSame(result, argument2.getValue());
	}

	@Test
	public void submitCallable() {
		this.service.submit(this.callable);
		ArgumentCaptor<Callable> argument = ArgumentCaptor.forClass(Callable.class);
		Mockito.verify(this.executorService).submit(argument.capture());
		assertSame(this.callable, argument.getValue());
	}

	@Test
	public void scheduleRunnable() {
		this.service.schedule(this.runnable, 5, TimeUnit.MILLISECONDS);
		ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
		ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<TimeUnit> argument3 = ArgumentCaptor.forClass(TimeUnit.class);
		Mockito.verify(this.scheduledExecutorService).schedule(argument1.capture(), argument2.capture(), argument3.capture());
		assertSame(this.runnable, argument1.getValue());
		assertEquals(new Long(5), argument2.getValue());
		assertSame(TimeUnit.MILLISECONDS, argument3.getValue());
	}

	@Test
	public void scheduleCallable() {
		this.service.schedule(this.callable, 5, TimeUnit.MILLISECONDS);
		ArgumentCaptor<Callable> argument1 = ArgumentCaptor.forClass(Callable.class);
		ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<TimeUnit> argument3 = ArgumentCaptor.forClass(TimeUnit.class);
		Mockito.verify(this.scheduledExecutorService).schedule(argument1.capture(), argument2.capture(), argument3.capture());
		assertSame(this.callable, argument1.getValue());
		assertEquals(new Long(5), argument2.getValue());
		assertSame(TimeUnit.MILLISECONDS, argument3.getValue());
	}

	@Test
	public void scheduleAtFixedRate() {
		this.service.scheduleAtFixedRate(this.runnable, 10, 5, TimeUnit.MILLISECONDS);
		ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
		ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<Long> argument3 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<TimeUnit> argument4 = ArgumentCaptor.forClass(TimeUnit.class);
		Mockito.verify(this.scheduledExecutorService).scheduleAtFixedRate(argument1.capture(), argument2.capture(),
				argument3.capture(), argument4.capture());
		assertSame(this.runnable, argument1.getValue());
		assertEquals(new Long(10), argument2.getValue());
		assertEquals(new Long(5), argument3.getValue());
		assertSame(TimeUnit.MILLISECONDS, argument4.getValue());
	}

	@Test
	public void scheduleWithFixedDelay() {
		this.service.scheduleWithFixedDelay(this.runnable, 10, 5, TimeUnit.MILLISECONDS);
		ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
		ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<Long> argument3 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<TimeUnit> argument4 = ArgumentCaptor.forClass(TimeUnit.class);
		Mockito.verify(this.scheduledExecutorService).scheduleWithFixedDelay(argument1.capture(), argument2.capture(),
				argument3.capture(), argument4.capture());
		assertSame(this.runnable, argument1.getValue());
		assertEquals(new Long(10), argument2.getValue());
		assertEquals(new Long(5), argument3.getValue());
		assertSame(TimeUnit.MILLISECONDS, argument4.getValue());
	}

	@AvoidServiceStartForTest
	@Test
	public void getExecutorService() {
		assertSame(this.executorService, this.service.getExecutorService());
	}

	@AvoidServiceStartForTest
	@Test
	public void doStop_noinit() {
		try {
			this.service.doStop();
			fail("IllegalStateException is expected"); //$NON-NLS-1$
		} catch (IllegalStateException exception) {
			// This exception is fired by notifyStopped()
		}
		Mockito.verify(this.scheduledExecutorService, new Times(1)).shutdown();
		Mockito.verify(this.executorService, new Times(1)).shutdown();
		Mockito.verify(this.scheduledExecutorService, new Times(1)).shutdownNow();
		Mockito.verify(this.executorService, new Times(1)).shutdownNow();
	}

	@Test
	public void doStop_init() {
		this.service.doStop();
		Mockito.verify(this.scheduledExecutorService, new Times(1)).shutdown();
		Mockito.verify(this.executorService, new Times(1)).shutdown();
		Mockito.verify(this.scheduledExecutorService, new Times(1)).shutdownNow();
		Mockito.verify(this.executorService, new Times(1)).shutdownNow();
	}

}

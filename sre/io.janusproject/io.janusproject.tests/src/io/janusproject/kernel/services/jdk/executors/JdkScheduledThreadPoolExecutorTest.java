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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.lang.Thread.UncaughtExceptionHandler;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import io.janusproject.services.executor.ChuckNorrisException;
import io.janusproject.testutils.AbstractJanusTest;
import io.janusproject.testutils.FutureExceptionMatcher;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.mockito.exceptions.base.MockitoException;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({ "javadoc" })
public class JdkScheduledThreadPoolExecutorTest extends AbstractJanusTest {

	static final Object VALUE = new Object();

	@Nullable
	private TerminationListener termListener;

	@Nullable
	private JdkScheduledThreadPoolExecutor executor;

	@Nullable
	private UncaughtExceptionHandler handler;

	@Before
	public void setUp() {
		this.termListener = new TerminationListener();
		this.handler = Mockito.mock(UncaughtExceptionHandler.class);
		this.executor = new JdkScheduledThreadPoolExecutor(new JdkThreadFactory(this.handler));
		this.executor.addTaskListener(this.termListener);
	}

	@After
	public void tearDown() {
		this.executor.shutdownNow();
		this.executor.removeTaskListener(this.termListener);
	}

	@Test
	public void submitRunnable_succeed() throws Exception {
		Runnable mock = new RunnableMock(0);
		Future<?> f = this.executor.submit(mock);
		waitTaskTermination();
		assertNotNull(f);
		assertNull(f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void submitRunnableObject_succeed() throws Exception {
		Runnable mock = new RunnableMock(0);
		Future<?> f = this.executor.submit(mock, VALUE);
		waitTaskTermination();
		assertNotNull(f);
		assertSame(VALUE, f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void submitCallable_succeed() throws Exception {
		Callable<?> mock = new RunnableMock(0);
		Future<?> f = this.executor.submit(mock);
		waitTaskTermination();
		assertNotNull(f);
		assertSame(VALUE, f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void executeRunnable_succeed() throws Exception {
		Runnable mock = new RunnableMock(0);
		this.executor.execute(mock);
		waitTaskTermination();
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void scheduleRunnable_succeed() throws Exception {
		Runnable mock = new RunnableMock(0);
		Future<?> f = this.executor.schedule(mock, 1, TimeUnit.SECONDS);
		waitTaskTermination();
		assertNotNull(f);
		assertNull(f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void scheduleCallable_succeed() throws Exception {
		Callable<?> mock = new RunnableMock(0);
		Future<?> f = this.executor.schedule(mock, 1, TimeUnit.SECONDS);
		waitTaskTermination();
		assertNotNull(f);
		assertSame(VALUE, f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void scheduleAtFixedRate_succeed() throws Exception {
		CountDownRunnableMock mock = new CountDownRunnableMock(0);
		Future<?> f = this.executor.scheduleAtFixedRate(mock, 1, 1, TimeUnit.SECONDS);
		mock.setFuture(f);
		waitTaskTermination();

		assertNotNull(f);
		assertThat(f, new FutureExceptionMatcher(CancellationException.class));

		ArgumentCaptor<Thread> argument1 = ArgumentCaptor.forClass(Thread.class);
		ArgumentCaptor<Throwable> argument2 = ArgumentCaptor.forClass(Throwable.class);
		Mockito.verify(this.handler).uncaughtException(argument1.capture(), argument2.capture());
		assertThrowables(InterruptedException.class, CancellationException.class, argument2.getValue());
	}

	@Test
	public void scheduleWithFixedDelay_succeed() throws Exception {
		CountDownRunnableMock mock = new CountDownRunnableMock(0);
		Future<?> f = this.executor.scheduleWithFixedDelay(mock, 1, 1, TimeUnit.SECONDS);
		mock.setFuture(f);
		waitTaskTermination();

		assertNotNull(f);
		assertThat(f, new FutureExceptionMatcher(CancellationException.class));

		ArgumentCaptor<Thread> argument1 = ArgumentCaptor.forClass(Thread.class);
		ArgumentCaptor<Throwable> argument2 = ArgumentCaptor.forClass(Throwable.class);
		Mockito.verify(this.handler).uncaughtException(argument1.capture(), argument2.capture());
		assertThrowables(InterruptedException.class, CancellationException.class, argument2.getValue());
	}

	@Test
	public void submitRunnable_chucknorris() throws Exception {
		Runnable mock = new RunnableMock(2);
		Future<?> f = this.executor.submit(mock);
		waitTaskTermination();
		assertNotNull(f);
		assertNull(f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void submitRunnableObject_chucknorris() throws Exception {
		Runnable mock = new RunnableMock(2);
		Future<?> f = this.executor.submit(mock, VALUE);
		waitTaskTermination();
		assertNotNull(f);
		assertSame(VALUE, f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void submitCallable_chucknorris() throws Exception {
		Callable<?> mock = new RunnableMock(2);
		Future<?> f = this.executor.submit(mock);
		waitTaskTermination();
		assertNotNull(f);
		assertNull(f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void executeCallable_chucknorris() throws Exception {
		Runnable mock = new RunnableMock(2);
		this.executor.execute(mock);
		waitTaskTermination();
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void scheduleRunnable_chucknorris() throws Exception {
		Runnable mock = new RunnableMock(2);
		Future<?> f = this.executor.schedule(mock, 1, TimeUnit.SECONDS);
		waitTaskTermination();
		assertNotNull(f);
		assertNull(f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void scheduleCallable_chucknorris() throws Exception {
		Callable<?> mock = new RunnableMock(2);
		Future<?> f = this.executor.schedule(mock, 1, TimeUnit.SECONDS);
		waitTaskTermination();
		assertNotNull(f);
		assertNull(f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void scheduleAtFixedRate_chucknorris() throws Exception {
		CountDownRunnableMock mock = new CountDownRunnableMock(2);
		Future<?> f = this.executor.scheduleAtFixedRate(mock, 1, 1, TimeUnit.SECONDS);
		mock.setFuture(f);
		waitTaskTermination();

		assertNotNull(f);
		assertNull(f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void scheduleWithFixedDelay_chucknorris() throws Exception {
		CountDownRunnableMock mock = new CountDownRunnableMock(2);
		Future<?> f = this.executor.scheduleWithFixedDelay(mock, 1, 1, TimeUnit.SECONDS);
		mock.setFuture(f);
		waitTaskTermination();

		assertNotNull(f);
		assertNull(f.get());
		Mockito.verifyZeroInteractions(this.handler);
	}

	@Test
	public void submitRunnable_exception() throws Exception {
		Runnable mock = new RunnableMock(1);
		Future<?> f = this.executor.submit(mock);
		waitTaskTermination();

		assertNotNull(f);
		assertThat(f, new FutureExceptionMatcher(MockitoException.class));

		ArgumentCaptor<Thread> argument1 = ArgumentCaptor.forClass(Thread.class);
		ArgumentCaptor<Throwable> argument2 = ArgumentCaptor.forClass(Throwable.class);
		Mockito.verify(this.handler).uncaughtException(argument1.capture(), argument2.capture());
		assertThrowable(MockitoException.class, argument2.getValue());
	}

	@Test
	public void submitRunnableObject_exception() throws Exception {
		Runnable mock = new RunnableMock(1);
		Future<?> f = this.executor.submit(mock, VALUE);
		waitTaskTermination();

		assertNotNull(f);
		assertThat(f, new FutureExceptionMatcher(MockitoException.class));

		ArgumentCaptor<Thread> argument1 = ArgumentCaptor.forClass(Thread.class);
		ArgumentCaptor<Throwable> argument2 = ArgumentCaptor.forClass(Throwable.class);
		Mockito.verify(this.handler).uncaughtException(argument1.capture(), argument2.capture());
		assertThrowable(MockitoException.class, argument2.getValue());
	}

	@Test
	public void submitCallable_exception() throws Exception {
		Callable<?> mock = new RunnableMock(1);
		Future<?> f = this.executor.submit(mock);
		waitTaskTermination();

		assertNotNull(f);
		assertThat(f, new FutureExceptionMatcher(MockitoException.class));

		ArgumentCaptor<Thread> argument1 = ArgumentCaptor.forClass(Thread.class);
		ArgumentCaptor<Throwable> argument2 = ArgumentCaptor.forClass(Throwable.class);
		Mockito.verify(this.handler).uncaughtException(argument1.capture(), argument2.capture());
		assertThrowable(MockitoException.class, argument2.getValue());
	}

	@Test
	public void executeRunnable_exception() throws Exception {
		Runnable mock = new RunnableMock(1);
		this.executor.execute(mock);
		waitTaskTermination();

		ArgumentCaptor<Thread> argument1 = ArgumentCaptor.forClass(Thread.class);
		ArgumentCaptor<Throwable> argument2 = ArgumentCaptor.forClass(Throwable.class);
		Mockito.verify(this.handler).uncaughtException(argument1.capture(), argument2.capture());
		assertThrowable(MockitoException.class, argument2.getValue());
	}

	@Test
	public void scheduleRunnable_exception() throws Exception {
		Runnable mock = new RunnableMock(1);
		Future<?> f = this.executor.schedule(mock, 1, TimeUnit.SECONDS);
		waitTaskTermination();

		assertNotNull(f);
		assertThat(f, new FutureExceptionMatcher(MockitoException.class));

		ArgumentCaptor<Thread> argument1 = ArgumentCaptor.forClass(Thread.class);
		ArgumentCaptor<Throwable> argument2 = ArgumentCaptor.forClass(Throwable.class);
		Mockito.verify(this.handler).uncaughtException(argument1.capture(), argument2.capture());
		assertThrowable(MockitoException.class, argument2.getValue());
	}

	@Test
	public void scheduleCallable_exception() throws Exception {
		Callable<?> mock = new RunnableMock(1);
		Future<?> f = this.executor.schedule(mock, 1, TimeUnit.SECONDS);
		waitTaskTermination();

		assertNotNull(f);
		assertThat(f, new FutureExceptionMatcher(MockitoException.class));

		ArgumentCaptor<Thread> argument1 = ArgumentCaptor.forClass(Thread.class);
		ArgumentCaptor<Throwable> argument2 = ArgumentCaptor.forClass(Throwable.class);
		Mockito.verify(this.handler).uncaughtException(argument1.capture(), argument2.capture());
		assertThrowable(MockitoException.class, argument2.getValue());
	}

	@Test
	public void scheduleAtFixedRate_exception() throws Exception {
		CountDownRunnableMock mock = new CountDownRunnableMock(1);
		Future<?> f = this.executor.scheduleAtFixedRate(mock, 1, 1, TimeUnit.SECONDS);
		mock.setFuture(f);
		waitTaskTermination();

		assertNotNull(f);
		assertThat(f, new FutureExceptionMatcher(MockitoException.class));

		ArgumentCaptor<Thread> argument1 = ArgumentCaptor.forClass(Thread.class);
		ArgumentCaptor<Throwable> argument2 = ArgumentCaptor.forClass(Throwable.class);
		Mockito.verify(this.handler).uncaughtException(argument1.capture(), argument2.capture());
		assertThrowable(MockitoException.class, argument2.getValue());
	}

	@Test
	public void scheduleWithFixedDelay_exception() throws Exception {
		CountDownRunnableMock mock = new CountDownRunnableMock(1);
		Future<?> f = this.executor.scheduleWithFixedDelay(mock, 1, 1, TimeUnit.SECONDS);
		mock.setFuture(f);
		waitTaskTermination();

		assertNotNull(f);
		assertThat(f, new FutureExceptionMatcher(MockitoException.class));

		ArgumentCaptor<Thread> argument1 = ArgumentCaptor.forClass(Thread.class);
		ArgumentCaptor<Throwable> argument2 = ArgumentCaptor.forClass(Throwable.class);
		Mockito.verify(this.handler).uncaughtException(argument1.capture(), argument2.capture());
		assertThrowable(MockitoException.class, argument2.getValue());
	}

	private static void assertThrowable(Class<? extends Throwable> expectedType, Object actual) {
		if (!expectedType.isInstance(actual)) {
			fail("An exception of type '" + expectedType //$NON-NLS-1$
					+ "' is expected; but the actual value is of type: " + actual); //$NON-NLS-1$
		}
	}

	private static void assertThrowables(Class<? extends Throwable> expectedType1, Class<? extends Throwable> expectedType2,
			Object actual) {
		if (!expectedType1.isInstance(actual) && !expectedType2.isInstance(actual)) {
			fail("An exception of type '" + expectedType1 //$NON-NLS-1$
					+ "' or of type '" + expectedType2 //$NON-NLS-1$
					+ "' is expected; but the actual value is of type: " + actual); //$NON-NLS-1$
		}
	}

	private void waitTaskTermination() throws Exception {
		this.termListener.waitForTermination();
		this.executor.shutdown();
		this.executor.awaitTermination(30, TimeUnit.SECONDS);
		assertEquals(0, this.executor.getActiveCount());
		Thread.sleep(250);
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class RunnableMock implements Runnable, Callable<Object> {

		private final int state;

		/**
		 * @param state
		 */
		RunnableMock(int state) {
			this.state = state;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void run() {
			switch (this.state) {
			case 1:
				throw new MockitoException(""); //$NON-NLS-1$
			case 2:
				throw new ChuckNorrisException();
			default:
			}
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Object call() {
			switch (this.state) {
			case 1:
				throw new MockitoException(""); //$NON-NLS-1$
			case 2:
				throw new ChuckNorrisException();
			default:
			}
			return VALUE;
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class CountDownRunnableMock implements Runnable {

		private final int state;
		private final AtomicInteger count = new AtomicInteger(2);
		private Future<?> future = null;

		/**
		 * @param state
		 */
		CountDownRunnableMock(int state) {
			this.state = state;
		}

		/**
		 * The task knows its future.
		 * 
		 * @param future
		 */
		public void setFuture(Future<?> future) {
			this.future = future;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void run() {
			int c = this.count.decrementAndGet();
			if (c <= 0) {
				switch (this.state) {
				case 1:
					throw new MockitoException(""); //$NON-NLS-1$
				case 2:
					throw new ChuckNorrisException();
				default:
					if (this.future != null) {
						this.future.cancel(true);
					}
				}
			}
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class TerminationListener implements JdkTaskListener {

		private final AtomicBoolean finished = new AtomicBoolean(false);

		/**
		 */
		TerminationListener() {
			//
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public void taskFinished(Thread thread, Runnable task) {
			this.finished.set(true);
		}

		public void waitForTermination() {
			while (!this.finished.get()) {
				Thread.yield();
			}
		}

	}

}

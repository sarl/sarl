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
package io.sarl.tests.api.tools;

import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

/** Utilities for executing a timeout.
 *
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class TestTimeout {

	private static final int TIME_OUT = 5000;
	
	private TestTimeout() {
		//
	}

	/** Start a time out on the operation.
	 *
	 * @param enable programmatic flag for enabling the time out.
	 * @return the time out manager.
	 * @since 0.10
	 */
	public static TimeOutHandler startTimeOut(boolean enable) {
		return startTimeOut(TIME_OUT, enable);
	}

	/** Start a time out on the operation.
	 *
	 * @param timeout the duration before time out.
	 * @param enable programmatic flag for enabling the time out.
	 * @return the time out manager.
	 * @since 0.11
	 */
	public static TimeOutHandler startTimeOut(int timeout, boolean enable) {
		return startTimeOut(timeout, enable, null);
	}

	/** Start a time out on the operation.
	 *
	 * @return the time out manager.
	 * @since 0.9
	 */
	public static TimeOutHandler startTimeOut() {
		return startTimeOut(TIME_OUT);
	}

	/** Start a time out on the operation.
	 *
	 * @param timeout the duration before time out.
	 * @return the time out manager.
	 * @since 0.11
	 */
	public static TimeOutHandler startTimeOut(int timeout) {
		return startTimeOut(timeout, true, null);
	}

	/** Start a time out on the operation.
	 *
	 * @param enable programmatic flag for enabling the time out.
	 * @param predicate the condition for stopping the timeout loop.
	 * @return the time out manager.
	 * @since 0.11
	 */
	public static TimeOutHandler startTimeOut(boolean enable, Predicate predicate) {
		return startTimeOut(TIME_OUT, enable, predicate);
	}

	/** Start a time out on the operation.
	 *
	 * @param timeout the duration before time out.
	 * @param enable programmatic flag for enabling the time out.
	 * @param predicate the condition for stopping the timeout loop.
	 * @return the time out manager.
	 * @since 0.11
	 */
	public static TimeOutHandler startTimeOut(int timeout, boolean enable, Predicate predicate) {
		final TimeOutHandler handler = newTimeOut(timeout, predicate);
		if (enable) {
			handler.startAsync();
		}
		return handler;
	}

	/** Start a time out on the operation.
	 *
	 * @return the time out manager.
	 * @param predicate the condition for stopping the timeout loop.
	 * @since 0.11
	 */
	public static TimeOutHandler startTimeOut(Predicate predicate) {
		return startTimeOut(TIME_OUT, predicate);
	}

	/** Start a time out on the operation.
	 *
	 * @param timeout the duration before time out.
	 * @param predicate the condition for stopping the timeout loop.
	 * @return the time out manager.
	 * @since 0.11
	 */
	public static TimeOutHandler startTimeOut(int timeout, Predicate predicate) {
		return startTimeOut(timeout, true, predicate);
	}

	/** Create a timeout object.
	 *
	 * @param predicate the condition for stopping the timeout loop.
	 * @return the time out object.
	 * @since 0.11
	 */
	public static TimeOutHandler newTimeOut(Predicate predicate) {
		return newTimeOut(TIME_OUT, predicate);
	}

	/** Create a timeout object.
	 *
	 * @param timeout the duration before time out.
	 * @param predicate the condition for stopping the timeout loop.
	 * @return the time out object.
	 * @since 0.11
	 */
	public static TimeOutHandler newTimeOut(int timeout, Predicate predicate) {
		return new TimeOutHandler(timeout, predicate);
	}

	/** An object for managing the time out of operations.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.11
	 */
	@FunctionalInterface
	public interface Predicate {
		
		/** Test something in order to stop the timeout loop.
		 *
		 * @return {@code true} to stop the timeout loop.
		 */
		boolean test();
	}

	/** An object for managing the time out of operations.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	public static class TimeOutHandler implements Runnable {

		private Thread thread;

		private Thread threadToBreak;

		private final AtomicBoolean continueLoop = new AtomicBoolean(true);
		
		private final AtomicBoolean timeout = new AtomicBoolean();

		private final Predicate predicate;

		private final int timeoutDuration;
	
		/** Constructor.
		 *
		 * @param predicate the condition for stopping the timeout loop.
		 * @since 0.11
		 */
		TimeOutHandler(int duration, Predicate predicate) {
			this.timeoutDuration = duration;
			this.predicate = predicate;
		}

		/** Start the time out process.
		 */
		public void startAsync() {
			this.threadToBreak = Thread.currentThread();
			this.thread = new Thread() {
				@Override
				public void run() {
					TimeOutHandler.this.run();
				}
			};
			this.thread.setDaemon(true);
			this.thread.setName("Test TimeOut Manager");
			this.thread.start();
		}

		@Override
		public void run() {
			final long endTime = System.currentTimeMillis() + this.timeoutDuration;
			while (TimeOutHandler.this.continueLoop.get()
					&& System.currentTimeMillis() <= endTime) {
				Thread.yield();
				if (TimeOutHandler.this.predicate != null) {
					TimeOutHandler.this.continueLoop.set(!TimeOutHandler.this.predicate.test());
				}
			}
			if (TimeOutHandler.this.continueLoop.get()) {
				TimeOutHandler.this.timeout.set(true);
				TimeOutHandler.this.stop();
			}
		}

		/** Replies if the process has stopped on a time out.
		 *
		 * @return {@code true} if time out.
		 * @since 0.11
		 */
		public boolean isTimeout() {
			return this.timeout.get();
		}

		/** Stop the time out process.
		 */
		@SuppressWarnings("deprecation")
		public synchronized void stop() {
			this.continueLoop.set(false);
			if (this.thread != null) {
				this.thread = null;
				if (this.threadToBreak != null) {
					try {
						this.threadToBreak.stop();
					} catch (ThreadDeath exception) {
						if (this.timeout.get()) {
							throw new RuntimeException(new TimeoutException());
						}
					} finally {
						this.threadToBreak = null;
					}
				}
			}
		}

	}

}

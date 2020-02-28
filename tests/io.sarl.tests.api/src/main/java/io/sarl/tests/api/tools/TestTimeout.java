/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
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
		final TimeOutHandler handler = new TimeOutHandler();
		if (enable) {
			handler.start();
		}
		return handler;
	}

	/** Start a time out on the operation.
	 *
	 * @return the time out manager.
	 * @since 0.9
	 */
	public static TimeOutHandler startTimeOut() {
		return startTimeOut(true);
	}

	/** An object for managing the time out of operations.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	public static class TimeOutHandler {

		private static final int TIME_OUT = 10000;
		
		private Thread thread;

		private Thread threadToBreak;

		private final AtomicBoolean continueLoop = new AtomicBoolean(true);
		
		private final AtomicBoolean timeout = new AtomicBoolean();

		/** Constructor.
		 */
		TimeOutHandler() {
			//
		}

		/** Start the time out process.
		 */
		void start() {
			this.threadToBreak = Thread.currentThread();
			this.thread = new Thread() {
				@Override
				public void run() {
					final long endTime = System.currentTimeMillis() + TIME_OUT;
					while (TimeOutHandler.this.continueLoop.get()
							&& System.currentTimeMillis() <= endTime) {
						Thread.yield();
					}
					if (TimeOutHandler.this.continueLoop.get()) {
						TimeOutHandler.this.timeout.set(true);
						TimeOutHandler.this.stop();
					}
				}
			};
			this.thread.setDaemon(true);
			this.thread.setName("Test TimeOut Manager");
			this.thread.start();
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

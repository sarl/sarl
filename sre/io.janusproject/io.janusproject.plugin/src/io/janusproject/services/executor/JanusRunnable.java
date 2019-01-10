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

package io.janusproject.services.executor;

/**
 * A specific Janus runnable that is catching the {@link EarlyExitException}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class JanusRunnable implements Runnable {

	private final Runnable runnable;

	/** Constructor.
	 * @param runnable the wrapped task.
	 */
	public JanusRunnable(Runnable runnable) {
		assert runnable != null;
		this.runnable = runnable;
	}

	/** Constructor without wrapped task.
	 */
	protected JanusRunnable() {
		this.runnable = null;
	}

	/** Replies the wrapped task.
	 *
	 * @return the runnable.
	 */
	public Runnable getWrappedRunnable() {
		return this.runnable;
	}

	@Override
	public void run() {
		runWithEarlyExitSupport();
	}

	/** Run the wrapped task with the early exist support.
	 * The {@link EarlyExitException} is silently catched.
	 */
	protected final void runWithEarlyExitSupport() {
		try {
			this.runnable.run();
		} catch (EarlyExitException ex) {
			//
		}
	}

	@Override
	public String toString() {
		return this.runnable.toString();
	}

	@Override
	public boolean equals(Object obj) {
		return this.runnable.equals(obj);
	}

	@Override
	public int hashCode() {
		return this.runnable.hashCode();
	}

}

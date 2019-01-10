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

import java.util.concurrent.Callable;

/**
 * A specific Janus callable that is catching the {@link EarlyExitException}.
 *
 * @param <T> the type of the result.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class JanusCallable<T> implements Callable<T> {

	private final Callable<T> callable;

	/** Constructor.
	 * @param callable the wrapped task.
	 */
	public JanusCallable(Callable<T> callable) {
		assert callable != null;
		this.callable = callable;
	}

	/** Replies the wrapped task.
	 *
	 * @return the callable.
	 */
	public Callable<T> getWrappedCallable() {
		return this.callable;
	}

	@Override
	public T call() throws Exception {
		return callWithEarlyExitSupport();
	}

	/** Run the wrapped task with the early exist support.
	 * The {@link EarlyExitException} is silently catched.
	 *
	 * @return the computed value.
	 * @throws Exception the error in the wrapped task, exit {@link EarlyExitException}
	 */
	protected final T callWithEarlyExitSupport() throws Exception {
		try {
			return this.callable.call();
		} catch (EarlyExitException e) {
			return null;
		}
	}

	@Override
	public String toString() {
		return this.callable.toString();
	}

	@Override
	public boolean equals(Object obj) {
		return this.callable.equals(obj);
	}

	@Override
	public int hashCode() {
		return this.callable.hashCode();
	}

}

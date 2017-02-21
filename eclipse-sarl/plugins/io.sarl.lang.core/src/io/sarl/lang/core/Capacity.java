/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.core;

/** Root type for all the capacities in the SARL language.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface Capacity {

	/** Wrapper to a capacity that enable to manage and provide the caller to the capacity function.
	 *
	 * @param <C> the type of the wrapper capacity.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.5
	 */
	abstract class ContextAwareCapacityWrapper<C extends Capacity> implements Capacity {

		private static final ThreadLocal<Object> CALLER = new ThreadLocal<>();

		/** The wrapped capacity.
		 */
		protected final C capacity;

		private final Object caller;

		/** Constructor.
		 *
		 * @param capacity the wrapped capacity.
		 * @param caller the owner of the wrapper, that should be the caller of the capacity's function.
		 */
		public ContextAwareCapacityWrapper(C capacity, Object caller) {
			assert capacity != null;
			this.capacity = capacity;
			this.caller = caller;
		}

		/** Ensure that the local-thread variable stores the caller.
		 */
		protected final void ensureCallerInLocalThread() {
			CALLER.set(this.caller);
		}

		/** Reset the local-thread variable storing the caller.
		 */
		@SuppressWarnings("static-method")
		protected final void resetCallerInLocalThread() {
			CALLER.remove();
		}

		/** Replies the caller of the capacity functions.
		 *
		 * @return the caller, or {@code null} if no caller is registered.
		 */
		public static final Object getCaller() {
			return CALLER.get();
		}

	}

}

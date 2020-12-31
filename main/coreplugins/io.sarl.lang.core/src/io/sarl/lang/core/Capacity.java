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
	 * @ExcludeFromApidoc
	 */
	abstract class ContextAwareCapacityWrapper<C extends Capacity> implements Capacity {

		/** The wrapped capacity.
		 */
		protected final C capacity;

		private final AgentTrait caller;

		/** Constructor.
		 *
		 * @param capacity the wrapped capacity.
		 * @param caller the owner of the wrapper, that should be the caller of the capacity's function.
		 */
		public ContextAwareCapacityWrapper(C capacity, AgentTrait caller) {
			assert capacity != null;
			this.capacity = capacity;
			this.caller = caller;
		}

		/** Ensure that the local-thread variable stores the caller.
		 */
		protected final void ensureCallerInLocalThread() {
			Capacities.CALLER.set(this.caller);
		}

		/** Reset the local-thread variable storing the caller.
		 */
		@SuppressWarnings("static-method")
		protected final void resetCallerInLocalThread() {
			Capacities.CALLER.remove();
		}

		/** Replies the capacity to delegate to..
		 *
		 * @return the capacity.
		 */
		public C getDelegate() {
			return this.capacity;
		}

	}

}

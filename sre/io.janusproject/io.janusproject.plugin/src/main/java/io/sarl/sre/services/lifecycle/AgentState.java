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

package io.sarl.sre.services.lifecycle;

/**
 * Describe the states of an agent.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public enum AgentState {

	/**
	 * The agent is unstarted: before initialization process.
	 */
	UNSTARTED {
		@Override
		public boolean isAsynchronousEventHandling() {
			return false;
		}

		@Override
		public boolean isBlockingEventHandling() {
			return false;
		}

		@Override
		public boolean isPreAlive() {
			return true;
		}

		@Override
		public boolean isAlive() {
			return false;
		}

		@Override
		public boolean isPostAlive() {
			return false;
		}
	},

	/**
	 * The agent is under creation.
	 */
	INITIALIZING {
		@Override
		public boolean isAsynchronousEventHandling() {
			return true;
		}

		@Override
		public boolean isBlockingEventHandling() {
			return true;
		}

		@Override
		public boolean isPreAlive() {
			return false;
		}

		@Override
		public boolean isAlive() {
			return true;
		}

		@Override
		public boolean isPostAlive() {
			return false;
		}
	},

	/**
	 * The owner of the event bus is running.
	 */
	ALIVE {
		@Override
		public boolean isAsynchronousEventHandling() {
			return true;
		}

		@Override
		public boolean isBlockingEventHandling() {
			return true;
		}

		@Override
		public boolean isPreAlive() {
			return false;
		}

		@Override
		public boolean isAlive() {
			return true;
		}

		@Override
		public boolean isPostAlive() {
			return false;
		}
	},

	/**
	 * The owner of the event bus is under destruction.
	 */
	DYING {
		@Override
		public boolean isAsynchronousEventHandling() {
			return false;
		}

		@Override
		public boolean isBlockingEventHandling() {
			return true;
		}

		@Override
		public boolean isPreAlive() {
			return false;
		}

		@Override
		public boolean isAlive() {
			return false;
		}

		@Override
		public boolean isPostAlive() {
			return true;
		}
	},

	/**
	 * The owner of the event bus was destroyed.
	 */
	DEAD {
		@Override
		public boolean isAsynchronousEventHandling() {
			return false;
		}

		@Override
		public boolean isBlockingEventHandling() {
			return false;
		}

		@Override
		public boolean isPreAlive() {
			return false;
		}

		@Override
		public boolean isAlive() {
			return false;
		}

		@Override
		public boolean isPostAlive() {
			return true;
		}
	};

	/** Replies if the state accepts asynchronous event handling.
	 *
	 * @return {@code true} if the state accept event handling.
	 * @since 0.5
	 */
	public abstract boolean isAsynchronousEventHandling();

	/** Replies if the state accepts blocking event handling.
	 *
	 * @return {@code true} if the state accept block event handling.
	 * @since 0.5
	 */
	public abstract boolean isBlockingEventHandling();

	/** Replies if the state is one of the living states (initializing and alive).
	 *
	 * @return {@code true} if the state is an agent life state.
	 * @since 0.7
	 */
	public abstract boolean isAlive();

	/** Replies if the state is before the agent life.
	 *
	 * @return {@code true} if the state is before the agent life.
	 * @since 0.7
	 */
	public abstract boolean isPreAlive();

	/** Replies if the state is after the agent life.
	 *
	 * @return {@code true} if the state is after the agent life.
	 * @since 0.5
	 */
	public abstract boolean isPostAlive();

}

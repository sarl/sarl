/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.util;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Scope;

/**
 * This class consists exclusively of static methods that operate on or return scopes.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class Scopes {

	private Scopes() {
		//
	}

	/**
	 * Scope matching all agents in a Space.
	 *
	 * @param <T> - type of the elements in the scope.
	 * @return the scope that corresponds to all.
	 */
	public static <T> Scope<T> allParticipants() {
		return new AlwaysTrueScope<>();
	}

	/** Create an scope restricted to the given addresses.
	 *
	 * @param addresses - addresses to put in the scope.
	 * @return the scope restricted to the given addresses.
	 */
	public static Scope<Address> addresses(Address... addresses) {
		return new AddressScope(addresses);
	}

	/** A scope that is matching all the elements.
	 *
	 * @param <T> - the type of the elements to match to.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class AlwaysTrueScope<T> implements Scope<T> {

		private static final long serialVersionUID = -3193147362292037L;

		AlwaysTrueScope() {
			//
		}

		@Override
		public String toString() {
			return "AlwaysTRUE"; //$NON-NLS-1$
		}

		@Override
		public boolean matches(T element) {
			return true;
		}

	}

}

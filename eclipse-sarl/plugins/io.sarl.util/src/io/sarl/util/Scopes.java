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

	/** Create an scope that is fordidding the given addresses.
	 *
	 * @param addresses - addresses to exclude from the scope.
	 * @return the scope forbidding the given addresses.
	 * @since 0.5
	 */
	public static Scope<Address> notAddresses(Address... addresses) {
		return not(addresses(addresses));
	}

	/** Create an scope that is the negation of the given scope.
	 *
	 * @param scope the scope to negate.
	 * @return the negative scope.
	 * @since 0.5
	 */
	public static Scope<Address> not(Scope<Address> scope) {
		return new NotScope<>(scope);
	}

	/** Create an scope that is the boolean-or of the two given scopes.
	 *
	 * <p>This operation is a short-circuit operation.
	 *
	 * @param scope1 the first scope.
	 * @param scope2 the second scope.
	 * @return the or scope.
	 * @since 0.5
	 */
	public static Scope<Address> or(Scope<Address> scope1, Scope<Address> scope2) {
		return new OrScope<>(scope1, scope2);
	}

	/** Create an scope that is the boolean-and of the two given scopes.
	 *
	 * <p>This operation is a short-circuit operation.
	 *
	 * @param scope1 the first scope.
	 * @param scope2 the second scope.
	 * @return the and scope.
	 * @since 0.5
	 */
	public static Scope<Address> and(Scope<Address> scope1, Scope<Address> scope2) {
		return new AndScope<>(scope1, scope2);
	}

	/** Create an scope that is the boolean-xor of the two given scopes.
	 *
	 * <p>This operation is a short-circuit operation.
	 *
	 * @param scope1 the first scope.
	 * @param scope2 the second scope.
	 * @return the xor scope.
	 * @since 0.5
	 */
	public static Scope<Address> xor(Scope<Address> scope1, Scope<Address> scope2) {
		return new XorScope<>(scope1, scope2);
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

	/** A scope that is matching the negation of the given scope.
	 *
	 * @param <T> - the type of the elements to match to.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class NotScope<T> implements Scope<T> {

		private static final long serialVersionUID = 6711860434407397637L;

		private final Scope<T> scope;

		NotScope(Scope<T> scope) {
			this.scope = scope;
		}

		@Override
		public String toString() {
			return "! " + this.scope.toString(); //$NON-NLS-1$
		}

		@Override
		public boolean matches(T element) {
			return !this.scope.matches(element);
		}

	}

	/** A scope that is matching the given scopes with a boolean-and operator.
	 *
	 * @param <T> - the type of the elements to match to.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class AndScope<T> implements Scope<T> {

		private static final long serialVersionUID = 6718760434407395637L;

		private final Scope<T>[] scopes;

		@SafeVarargs
		AndScope(Scope<T>... scopes) {
			this.scopes = scopes;
		}

		@Override
		public String toString() {
			final StringBuilder buffer = new StringBuilder();
			if (this.scopes.length > 0) {
				buffer.append(this.scopes[0]);
				for (int i = 1; i < this.scopes.length; ++i) {
					buffer.append(" && "); //$NON-NLS-1$
					buffer.append(this.scopes[i]);
				}
			}
			return buffer.toString();
		}

		@Override
		public boolean matches(T element) {
			for (final Scope<T> scope : this.scopes) {
				if (!scope.matches(element)) {
					return false;
				}
			}
			return true;
		}

	}

	/** A scope that is matching the given scopes with a boolean-or operator.
	 *
	 * @param <T> - the type of the elements to match to.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class OrScope<T> implements Scope<T> {

		private static final long serialVersionUID = 6718860434407395637L;

		private final Scope<T>[] scopes;

		@SafeVarargs
		OrScope(Scope<T>... scopes) {
			this.scopes = scopes;
		}

		@Override
		public String toString() {
			final StringBuilder buffer = new StringBuilder();
			if (this.scopes.length > 0) {
				buffer.append(this.scopes[0]);
				for (int i = 1; i < this.scopes.length; ++i) {
					buffer.append(" || "); //$NON-NLS-1$
					buffer.append(this.scopes[i]);
				}
			}
			return buffer.toString();
		}

		@Override
		public boolean matches(T element) {
			for (final Scope<T> scope : this.scopes) {
				if (scope.matches(element)) {
					return true;
				}
			}
			return false;
		}

	}

	/** A scope that is matching the given scopes with a boolean-xor operator.
	 *
	 * @param <T> - the type of the elements to match to.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class XorScope<T> implements Scope<T> {

		private static final long serialVersionUID = 6718860434407397637L;

		private final Scope<T>[] scopes;

		@SafeVarargs
		XorScope(Scope<T>... scopes) {
			this.scopes = scopes;
		}

		@Override
		public String toString() {
			final StringBuilder buffer = new StringBuilder();
			if (this.scopes.length > 0) {
				buffer.append(this.scopes[0]);
				for (int i = 1; i < this.scopes.length; ++i) {
					buffer.append(" ^ "); //$NON-NLS-1$
					buffer.append(this.scopes[i]);
				}
			}
			return buffer.toString();
		}

		@Override
		public boolean matches(T element) {
			boolean active = false;
			for (final Scope<T> scope : this.scopes) {
				if (scope.matches(element)) {
					if (active) {
						return false;
					}
					active = true;
				}
			}
			return active;
		}

	}

}

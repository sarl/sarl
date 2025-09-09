/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.mwe2.inject;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.Singleton;
import com.google.inject.name.Named;

/**
 * Type of injection mechanism.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version mwe2 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid mwe2
 * @since 0.15
 */
public enum InjectionAPI {

	/** Jakarta API.
	 */
	JAKARTA {
		@Override
		public Class<?> getInjectType() {
			return jakarta.inject.Inject.class;
		}

		@Override
		public Class<?> getNamedType() {
			return jakarta.inject.Named.class;
		}

		@Override
		public Class<?> getProviderType() {
			return jakarta.inject.Provider.class;
		}

		@Override
		public Class<?> getSingletonType() {
			return jakarta.inject.Singleton.class;
		}
	},

	/** Javax API.
	 *
	 * @Deprecated This API should not be used any more.
	 */
	@Deprecated
	JAVAX {
		@Override
		public Class<?> getInjectType() {
			return javax.inject.Inject.class;
		}

		@Override
		public Class<?> getNamedType() {
			return javax.inject.Named.class;
		}

		@Override
		public Class<?> getProviderType() {
			return javax.inject.Provider.class;
		}

		@Override
		public Class<?> getSingletonType() {
			return javax.inject.Singleton.class;
		}
	},

	/** Google Guice.
	 */
	GOOGLE_GUICE {
		@Override
		public Class<?> getInjectType() {
			return Inject.class;
		}

		@Override
		public Class<?> getNamedType() {
			return Named.class;
		}

		@Override
		public Class<?> getProviderType() {
			return Provider.class;
		}

		@Override
		public Class<?> getSingletonType() {
			return Singleton.class;
		}
	};

	/** Replies the default injection API.
	 *
	 * @return the default API, never {@code null}.
	 */
	public static InjectionAPI getDefault() {
		return JAKARTA;
	}

	/** Replies the type for {@code @Inject}.
	 *
	 * @return the inject annotation type.
	 * @since 0.14
	 */
	public abstract Class<?> getInjectType();

	/** Replies the type for {@code @Named}.
	 *
	 * @return the named annotation type.
	 * @since 0.14
	 */
	public abstract Class<?> getNamedType();

	/** Replies the type for {@code @Provider}.
	 *
	 * @return the provider annotation type.
	 * @since 0.14
	 */
	public abstract Class<?> getProviderType();

	/** Replies the type for {@code @Singleton}.
	 *
	 * @return the singleton annotation type.
	 * @since 0.14
	 */
	public abstract Class<?> getSingletonType();

}


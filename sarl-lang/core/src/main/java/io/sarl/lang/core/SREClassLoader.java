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

package io.sarl.lang.core;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Utility functions for helping to obtain a class loader that could be used by the
 * SRE.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class SREClassLoader {

	private SREClassLoader() {
		//
	}

	/** Load a class with the preferred class loaders.
	 *
	 * @param classname the name of the class to load.
	 * @param context the class loader to use first. If could be {@code null}. In this case
	 *     the system class loaders are used.
	 * @return the class.
	 * @throws ClassNotFoundException if the class cannot be found.
	 */
	@Pure
	@Inline(value = "$3.loadClass($1, true, $2)", imported = SREClassLoader.class)
	public static Class<?> loadClass(String classname, ClassLoader context) throws ClassNotFoundException {
		return loadClass(classname, true, context);
	}

	private static Class<?> loadClass(String classname) {
		try {
			return Class.forName(classname, true,
					ClassLoader.getSystemClassLoader());
		} catch (Exception exception) {
			// Ignore this exception
		}
		try {
			return Class.forName(classname, true, getPreferredSREClassloader());
		} catch (Exception exception) {
			// Ignore this exception
		}
		return null;
	}

	/** Load a class with the preferred class loaders.
	 *
	 * @param classname the name of the class to load.
	 * @param initialize indicates if the class must be initialize.
	 * @param context the class loader to use first. If could be {@code null}. In this case
	 *     the system class loaders are used.
	 * @return the class.
	 * @throws ClassNotFoundException if the class cannot be found.
	 */
	@Pure
	public static Class<?> loadClass(String classname, boolean initialize, ClassLoader context) throws ClassNotFoundException {
		ClassNotFoundException ex = null;
		try {
			if (context != null) {
				return context.loadClass(classname);
			}
		} catch (ClassNotFoundException exception) {
			ex = exception;
		}
		final var type = loadClass(classname);
		if (type == null) {
			if (ex != null) {
				throw ex;
			}
			throw new ClassNotFoundException(classname);
		}
		return type;
	}

	/** Replies the class loader that should be preferred for loading classes in the SRE.
	 *
	 * @return the preferred class loader; never {@code null}.
	 * @since 0.13
	 */
	public static ClassLoader getPreferredSREClassloader() {
		return Thread.currentThread().getContextClassLoader();
	}

	/** Replies the class loader that should be preferred for loading classes in the SRE.
	 *
	 * @param <T> the expected type of the class loader.
	 * @param type the expected type of the class loader.
	 * @return the preferred class loader; or {@code null} if the class loader is not of the specified type.
	 * @since 0.13
	 */
	public static <T extends ClassLoader> T getPreferredSREClassloader(Class<T> type) {
		final var cl = getPreferredSREClassloader();
		if (type.isInstance(cl)) {
			return type.cast(cl);
		}
		return null;
	}

	/** Change the class loader that should be preferred for loading classes in the SRE.
	 *
	 * @param classLoader the preferred class loader; never {@code null}.
	 * @since 0.13
	 */
	public static void setPreferredSREClassloader(ClassLoader classLoader) {
		assert classLoader != null;
		Thread.currentThread().setContextClassLoader(classLoader);
	}

}

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

package io.sarl.bootstrap;

import java.security.AccessController;
import java.security.PrivilegedAction;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Utility functions for helping to obtain a class loader that could be used by the
 * SRE.
 *
 * @author $Author: sgalland$
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
		final Class<?> type = AccessController.doPrivileged((PrivilegedAction<Class<?>>) () -> {
			try {
				return Class.forName(classname, true,
						ClassLoader.getSystemClassLoader());
			} catch (Exception exception) {
				// Ignore this exception
			}
			try {
				return Class.forName(classname, true,
						Thread.currentThread().getContextClassLoader());
			} catch (Exception exception) {
				// Ignore this exception
			}
			return null;
		});
		if (type == null) {
			if (ex != null) {
				throw ex;
			}
			throw new ClassNotFoundException(classname);
		}
		return type;
	}

}

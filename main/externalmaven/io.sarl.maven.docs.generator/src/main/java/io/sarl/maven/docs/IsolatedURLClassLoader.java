/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
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

package io.sarl.maven.docs;

import java.net.URL;
import java.net.URLClassLoader;

/** An isolated class loader.
 * This class loader does nto invokes its parent before its how behavior.
 * This is a difference with the standard class loading contract.
 * Additionally, the parent class loader is the system class loader.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
final class IsolatedURLClassLoader extends URLClassLoader {

	private final ClassLoader parentClassLoader = ClassLoader.getSystemClassLoader();

	/** Constructor.
	 *
	 * @param urls the class path for this loader.
	 */
	IsolatedURLClassLoader(URL[] urls) {
		super(urls, null);
	}

	@Override
	public synchronized Class<?> loadClass(String className) throws ClassNotFoundException {
		// Find any already loaded class
		Class<?> c = findLoadedClass(className);

		ClassNotFoundException ex = null;
		if (c == null) {
			try {
				c = findClass(className);
			} catch (ClassNotFoundException e) {
				ex = e;
				if (this.parentClassLoader != null) {
					c = this.parentClassLoader.loadClass(className);
				}
			}
		}
		if (c == null) {
			throw ex;
		}
		return c;
	}

}

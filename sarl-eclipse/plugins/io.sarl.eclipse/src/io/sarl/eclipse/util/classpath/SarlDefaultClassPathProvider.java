/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.util.classpath;

import java.util.Collection;

import org.eclipse.jdt.core.IClasspathEntry;

/** Provide the default class path libraries for a SARL project.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse 0.13.0 20230919-093100
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.10
 */
@FunctionalInterface
public interface SarlDefaultClassPathProvider {

	/**
	 * Returns the default class path entries to be added on new projects.
	 * By default this is the JRE container as selected by the user.
	 *
	 * @param classpathEntries the collection in which the classpath entries will be added.
	 */
	void putDefaultClasspathEntriesIn(Collection<IClasspathEntry> classpathEntries);

}

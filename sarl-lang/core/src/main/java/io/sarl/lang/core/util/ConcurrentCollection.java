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

package io.sarl.lang.core.util;

import java.util.Collection;

import org.eclipse.xtext.xbase.lib.Pure;

/** Represent a collection of objects with is thread-safe.
 *
 * @param <T> the type of the objects in the collection.
 * @author $Author: sgalland$
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.12
 */
public interface ConcurrentCollection<T> extends Collection<T> {

	/** Replies the first element in the collection.
	 * If there is no element, an exception is thrown.
	 *
	 * @return the first element.
	 */
	@Pure
	T getFirst();

}

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

import java.io.Serializable;

import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Defines the scope of an Event. Scopes are specialized based on the addressing
 * mechanism internally used by a {@link Space}.
 *
 * <p>A scope is a predicate used to filter the potentially called listeners for a given event.
 * The most basic Scope is represented by a collection of Addresses (Agent, Role, etc.
 *
 * @param <T> is the type of objects that should be matched by this scope.
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@FunctionalInterface
public interface Scope<T> extends Serializable {

	/**
	 * Checks whether the element is included in this scope.
	 *
	 * @param element the element to test. <strong>It must NOT BE NULL</strong>.
	 * @return true if the element is inside the scope, false otherwise
	 */
	@Pure
	boolean matches(T element);

}

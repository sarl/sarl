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

package io.sarl.lang.jvmmodel;

import java.util.Objects;

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.lib.Inline;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlEvent;

/** Provide the default visibility modifier for elements.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 */
@ImplementedBy(DefaultVisibilityProvider.class)
public interface IDefaultVisibilityProvider {

	/** Replies the default visibility modifier of the given element.
	 *
	 * @param element the element.
	 * @return the default visibility.
	 */
	JvmVisibility getDefaultJvmVisibility(EObject element);

	/** Replies the default visibility modifier for the given element when it is inside the given container.
	 *
	 * @param container the container.
	 * @param element the element type.
	 * @return the default visibility.
	 * @since 0.6
	 */
	JvmVisibility getDefaultJvmVisibility(EObject container, EClass element);

	/** Replies if the given visiblity is the default visibility for the given element.
	 *
	 * @param element the element
	 * @param visibility the visibility to test.
	 * @return {@code true} if the given visibility is the default one.
	 */
	default boolean isDefaultVisibility(EObject element, JvmVisibility visibility) {
		return Objects.equals(getDefaultJvmVisibility(element), visibility);
	}
	
	/** Replies the default visibility of a field when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.6
	 */
	@Inline("getFieldDefaultVisibilityIn(($1).getClass())")
	static JvmVisibility getFieldDefaultVisibilityIn(EObject container) {
		final Class<?> type = container == null ? null : container.getClass();
		return getFieldDefaultVisibilityIn(type);
	}

	/** Replies the default visibility of a field when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getFieldDefaultVisibilityIn(Class<?> container) {
		if (container != null) {
			if (SarlEvent.class.isAssignableFrom(container)
					|| XtendInterface.class.isAssignableFrom(container)
					|| XtendAnnotationType.class.isAssignableFrom(container)) {
				return JvmVisibility.PUBLIC;
			}
		}
		return JvmVisibility.PRIVATE;
	}

	/** Replies the default visibility of a field when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getFieldDefaultVisibilityIn(Tester container) {
		if (container != null) {
			if (container.isEvent()
					|| container.isInterface()
					|| container.isAnnotationType()) {
				return JvmVisibility.PUBLIC;
			}
		}
		return JvmVisibility.PRIVATE;
	}

	/** Replies the default visibility of an action when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.6
	 */
	@Inline("getActionDefaultVisibilityIn(($1).getClass())")
	static JvmVisibility getActionDefaultVisibilityIn(EObject container) {
		final Class<?> type = container == null ? null : container.getClass();
		return getActionDefaultVisibilityIn(type);
	}

	/** Replies the default visibility of an action when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getActionDefaultVisibilityIn(Class<?> container) {
		if (container != null && SarlAgent.class.isAssignableFrom(container)) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of an action when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getActionDefaultVisibilityIn(Tester container) {
		if (container != null && container.isAgent()) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of an annotation type when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.6
	 */
	@Inline("getAnnotationTypeDefaultVisibilityIn(($1).getClass())")
	static JvmVisibility getAnnotationTypeDefaultVisibilityIn(EObject container) {
		final Class<?> type = container == null ? null : container.getClass();
		return getAnnotationTypeDefaultVisibilityIn(type);
	}

	/** Replies the default visibility of an annotation type when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getAnnotationTypeDefaultVisibilityIn(Class<?> container) {
		if (container != null && SarlAgent.class.isAssignableFrom(container)) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of an annotation type when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getAnnotationTypeDefaultVisibilityIn(Tester container) {
		if (container != null && container.isAgent()) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of a class when inside the given container.
	 *
	 * @param container the container. If it is {@code null}, the root type is assumed.
	 * @return the default visibility.
	 * @since 0.6
	 */
	@Inline("getClassDefaultVisibilityIn(($1).getClass())")
	static JvmVisibility getClassDefaultVisibilityIn(EObject container) {
		final Class<?> type = container == null ? null : container.getClass();
		return getClassDefaultVisibilityIn(type);
	}

	/** Replies the default visibility of a class when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getClassDefaultVisibilityIn(Class<?> container) {
		if (container != null && SarlAgent.class.isAssignableFrom(container)) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of a class when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getClassDefaultVisibilityIn(Tester container) {
		if (container != null && container.isAgent()) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of an enumeration when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.6
	 */
	@Inline("getEnumerationDefaultVisibilityIn(($1).getClass())")
	static JvmVisibility getEnumerationDefaultVisibilityIn(EObject container) {
		final Class<?> type = container == null ? null : container.getClass();
		return getEnumerationDefaultVisibilityIn(type);
	}

	/** Replies the default visibility of an enumeration when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getEnumerationDefaultVisibilityIn(Class<?> container) {
		if (container != null && SarlAgent.class.isAssignableFrom(container)) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of an enumeration when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getEnumerationDefaultVisibilityIn(Tester container) {
		if (container != null && container.isAgent()) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of an interface when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.6
	 */
	@Inline("getInterfaceDefaultVisibilityIn(($1).getClass())")
	static JvmVisibility getInterfaceDefaultVisibilityIn(EObject container) {
		final Class<?> type = container == null ? null : container.getClass();
		return getInterfaceDefaultVisibilityIn(type);
	}

	/** Replies the default visibility of an interface when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getInterfaceDefaultVisibilityIn(Class<?> container) {
		if (container != null && SarlAgent.class.isAssignableFrom(container)) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of an interface when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.13
	 */
	static JvmVisibility getInterfaceDefaultVisibilityIn(Tester container) {
		if (container != null && container.isAgent()) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Test the types of the elements.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version compiler 0.13.0 20230919-093056
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 * @since 0.13
	 */
	interface Tester {

		/** Replies if the container is an agent.
		 *
		 * @return {@code true} if the container is an agent.
		 */
		boolean isAgent();

		/** Replies if the container is an event.
		 *
		 * @return {@code true} if the container is an event.
		 */
		boolean isEvent();

		/** Replies if the container is an interface.
		 *
		 * @return {@code true} if the container is an interface.
		 */
		boolean isInterface();

		/** Replies if the container is an annotation type.
		 *
		 * @return {@code true} if the container is an annotation type.
		 */
		boolean isAnnotationType();

	}

}

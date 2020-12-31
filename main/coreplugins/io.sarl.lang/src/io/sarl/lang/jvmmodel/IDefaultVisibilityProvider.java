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

package io.sarl.lang.jvmmodel;

import java.util.Objects;

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtext.common.types.JvmVisibility;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlEvent;

/** Provide the default visibility modifier for elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
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
	 * @return <code>true</code> if the given visibility is the default one.
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
	static JvmVisibility getFieldDefaultVisibilityIn(EObject container) {
		if (container instanceof SarlEvent
				|| container instanceof XtendInterface
				|| container instanceof XtendAnnotationType) {
			return JvmVisibility.PUBLIC;
		}
		return JvmVisibility.PRIVATE;
	}

	/** Replies the default visibility of an action when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.6
	 */
	static JvmVisibility getActionDefaultVisibilityIn(EObject container) {
		if (container instanceof SarlAgent) {
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
	static JvmVisibility getAnnotationTypeDefaultVisibilityIn(EObject container) {
		if (container instanceof SarlAgent) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of a class when inside the given container.
	 *
	 * @param container the container.
	 * @return the default visibility.
	 * @since 0.6
	 */
	static JvmVisibility getClassDefaultVisibilityIn(EObject container) {
		if (container instanceof SarlAgent) {
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
	static JvmVisibility getEnumerationDefaultVisibilityIn(EObject container) {
		if (container instanceof SarlAgent) {
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
	static JvmVisibility getInterfaceDefaultVisibilityIn(EObject container) {
		if (container instanceof SarlAgent) {
			return JvmVisibility.PROTECTED;
		}
		return JvmVisibility.PUBLIC;
	}

}

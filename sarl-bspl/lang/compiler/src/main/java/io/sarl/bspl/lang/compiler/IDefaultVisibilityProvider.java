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

package io.sarl.bspl.lang.compiler;

import java.util.Objects;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmVisibility;

import com.google.inject.ImplementedBy;

/** Provide the default visibility modifier for elements.
 *
 * <p>Copied from the SARL compiler code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@ImplementedBy(DefaultVisibilityProvider.class)
public interface IDefaultVisibilityProvider {

	/** Replies the default visibility modifier of the given element.
	 *
	 * @param element the element.
	 * @return the default visibility.
	 */
	JvmVisibility getDefaultJvmVisibility(EObject element);

	/** Replies if the given visiblity is the default visibility for the given element.
	 *
	 * @param element the element
	 * @param visibility the visibility to test.
	 * @return {@code true} if the given visibility is the default one.
	 */
	default boolean isDefaultVisibility(EObject element, JvmVisibility visibility) {
		return Objects.equals(getDefaultJvmVisibility(element), visibility);
	}

	/** Replies the default visibility of a protocol.
	 *
	 * @return the default visibility.
	 */
	static JvmVisibility getProtocolDefaultVisibility() {
		return JvmVisibility.PUBLIC;
	}

	/** Replies the default visibility of a protocol member.
	 *
	 * @return the default visibility.
	 */
	static JvmVisibility getProtocolMemberDefaultVisibility() {
		return JvmVisibility.PUBLIC;
	}

}

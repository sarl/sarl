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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociations;

import com.google.inject.Inject;
import com.google.inject.Singleton;

import io.sarl.bspl.lang.bspl.BsplProtocol;
import io.sarl.bspl.lang.bspl.BsplProtocolMember;

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
@Singleton
public class DefaultVisibilityProvider implements IDefaultVisibilityProvider {

	@Inject
	private IJvmModelAssociations associations;

	/** Constructor.
	 */
	public DefaultVisibilityProvider() {
		//
	}

	@Override
	public JvmVisibility getDefaultJvmVisibility(EObject element) {
		var realObject = element;
		if (realObject instanceof JvmIdentifiableElement) {
			final var obj = this.associations.getPrimarySourceElement(realObject);
			if (obj != null) {
				realObject = obj;
			}
		}
		if (realObject instanceof BsplProtocol protocol) {
			return protocol.getDefaultVisibility();
		} else 	if (realObject instanceof BsplProtocolMember member) {
			return member.getDefaultVisibility();
		}
		return JvmVisibility.PUBLIC;
	}

}

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

package io.sarl.bspl.lang.bspl.impl;

import org.eclipse.xtext.common.types.JvmVisibility;

import io.sarl.bspl.lang.compiler.IDefaultVisibilityProvider;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the BSPL protocol member.
 * <!-- end-user-doc -->
 *
 * <p>The following features are implemented:
 * <ul>
 *   <li>{@link #getDefaultVisibility()} <em>Default visibility of fields in SARL elements</em>}</li>
 *   <li>{@link #isPublicVisibility()} <em>Indicates if the member has a public visibility</em>}</li>
 *   <li>{@link #isProtectedVisibility()} <em>Indicates if the member has a visibility limited to the package and sub protocols</em>}</li>
 *   <li>{@link #isPackageVisibility()} <em>Indicates if the member has a visibility limited to the package</em>}</li>
 *   <li>{@link #isPrivateVisibility()} <em>Indicates if the member has a visibility limited to the protocol only</em>}</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class BsplProtocolMemberImplCustom extends BsplProtocolMemberImpl {

	/**
	 * <!-- begin-user-doc -->
	 * Constructor.
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected BsplProtocolMemberImplCustom() {
		super();
	}

	@Override
	public JvmVisibility getDefaultVisibility() {
		return IDefaultVisibilityProvider.getProtocolMemberDefaultVisibility();
	}

	@Override
	public JvmVisibility getSpecifiedVisibility() {
		for (final var modifier : getModifiers()) {
			if ("public".equals(modifier)) { //$NON-NLS-1$
				return JvmVisibility.PUBLIC;
			}
			if ("protected".equals(modifier)) { //$NON-NLS-1$
				return JvmVisibility.PROTECTED;
			}
			if ("package".equals(modifier)) { //$NON-NLS-1$
				return JvmVisibility.DEFAULT;
			}
			if ("private".equals(modifier)) { //$NON-NLS-1$
				return JvmVisibility.PRIVATE;
			}
		}
		return null;
	}
	
	@Override
	public JvmVisibility getVisibility() {
		final var visibility = getSpecifiedVisibility();
		return visibility != null ? visibility : getDefaultVisibility();
	}

	@Override
	public boolean isPublicVisibility() {
		return getVisibility() == JvmVisibility.PUBLIC;
	}

	@Override
	public boolean isPackageVisibility() {
		return getVisibility() == JvmVisibility.DEFAULT;
	}

	@Override
	public boolean isProtectedVisibility() {
		return getVisibility() == JvmVisibility.PROTECTED;
	}

	@Override
	public boolean isPrivateVisibility() {
		return getVisibility() == JvmVisibility.PRIVATE;
	}

}

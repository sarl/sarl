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

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the BSPL parameter declaration.
 * <!-- end-user-doc -->
 *
 * <p>The following features are implemented:
 * <ul>
 *   <li>{@link #isInput()} <em>Indicates if the {@code in} modifier is attached to this argument</em>}</li>
 *   <li>{@link #isKey()} <em>Indicates if the {@code key} modifier is attached to this argument</em>}</li>
 *   <li>{@link #isAny()} <em>Indicates if the {@code any} modifier is attached to this argument</em>}</li>
 *   <li>{@link #isNil()} <em>Indicates if the {@code nil} modifier is attached to this argument</em>}</li>
 *   <li>{@link #isOptional()} <em>Indicates if the {@code opt} modifier is attached to this argument</em>}</li>
 *   <li>{@link #isOutput()} <em>Indicates if the {@code out} modifier is attached to this argument</em>}</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class BsplProtocolParameterImplCustom extends BsplProtocolParameterImpl {

	/**
	 * <!-- begin-user-doc -->
	 * Constructor.
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected BsplProtocolParameterImplCustom() {
		super();
	}

	@Override
	public boolean isInput() {
		return getModifiers().contains("in"); //$NON-NLS-1$
	}

	@Override
	public boolean isKey() {
		return getModifiers().contains("key"); //$NON-NLS-1$
	}

	@Override
	public boolean isAny() {
		return getModifiers().contains("any"); //$NON-NLS-1$
	}

	@Override
	public boolean isNil() {
		return getModifiers().contains("nil"); //$NON-NLS-1$
	}

	@Override
	public boolean isOptional() {
		return getModifiers().contains("opt"); //$NON-NLS-1$
	}

	@Override
	public boolean isOutput() {
		return getModifiers().contains("out"); //$NON-NLS-1$
	}

}

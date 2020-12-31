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

package io.sarl.lang.sarl.impl;

import org.eclipse.xtext.common.types.JvmVisibility;

import io.sarl.lang.jvmmodel.IDefaultVisibilityProvider;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the XtendInterface grammar element for SARL.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link #getDefaultVisibility() <em>Default visibility of fields in SARL elements</em>}</li>
 * </ul>
 * </p>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlInterfaceImplCustom extends SarlInterfaceImpl {

	/**
	 * <!-- begin-user-doc -->
	 * Constructor.
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlInterfaceImplCustom() {
		super();
	}

	@Override
	protected JvmVisibility getDefaultVisibility() {
		return IDefaultVisibilityProvider.getInterfaceDefaultVisibilityIn(getDeclaringType());
	}

}

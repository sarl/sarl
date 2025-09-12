/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

import org.eclipse.xtext.util.Strings;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the SarlEvent grammar element for SARL.
 * <!-- end-user-doc -->
 *
 * <p>The following features are implemented:
 * <ul>
 *   <li>{@link #isAbstract() <em>Replies if the "abstract" modifier is associated to the element</em>}</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlEventImplCustom extends SarlEventImpl {

	/**
	 * <!-- begin-user-doc -->
	 * Constructor.
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlEventImplCustom() {
		super();
	}

	@Override
	public boolean isAbstract() {
		for (final var modifier: getModifiers()) {
			if (Strings.equal(modifier, "abstract")) {  //$NON-NLS-1$
				return true;
			}
			if (Strings.equal(modifier, "final")) {  //$NON-NLS-1$
				return false;
			}
		}
		return false;
	}

}

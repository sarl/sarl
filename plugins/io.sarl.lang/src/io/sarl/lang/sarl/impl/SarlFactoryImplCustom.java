/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlInterface;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the SarlFactoryImpl.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link #createSarlAction() <em>Custom SARL action</em>}</li>
 * </ul>
 * </p>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlFactoryImplCustom extends SarlFactoryImpl {

	/**
	 * <!-- begin-user-doc -->
	 * Constructor.
	 * <!-- end-user-doc -->
	 */
	public SarlFactoryImplCustom() {
		super();
	}

	@Override
	public SarlAction createSarlAction() {
		return new SarlActionImplCustom();
	}

	@Override
	public SarlAnnotationType createSarlAnnotationType() {
		return new SarlAnnotationTypeImplCustom();
	}

	@Override
	public SarlClass createSarlClass() {
		return new SarlClassImplCustom();
	}

	@Override
	public SarlEnumeration createSarlEnumeration() {
		return new SarlEnumerationImplCustom();
	}

	@Override
	public SarlField createSarlField() {
		return new SarlFieldImplCustom();
	}

	@Override
	public SarlInterface createSarlInterface() {
		return new SarlInterfaceImplCustom();
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlSkill;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the SarlFactoryImpl.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link #createSarlAgent() <em>Custom SARL agent</em>}</li>
 *   <li>{@link #createSarlAnnotationType() <em>Custom SARL annotation type</em>}</li>
 *   <li>{@link #createSarlClass() <em>Custom SARL class</em>}</li>
 *   <li>{@link #createSarlInterface() <em>Custom SARL interface</em>}</li>
 *   <li>{@link #createSarlEnumeration() <em>Custom SARL enumeration</em>}</li>
 *   <li>{@link #createSarlField() <em>Custom SARL field</em>}</li>
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
	public SarlAgent createSarlAgent() {
		return new SarlAgentImplCustom();
	}

	@Override
	public SarlBehavior createSarlBehavior() {
		return new SarlBehaviorImplCustom();
	}

	@Override
	public SarlEvent createSarlEvent() {
		return new SarlEventImplCustom();
	}

	@Override
	public SarlSkill createSarlSkill() {
		return new SarlSkillImplCustom();
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
	public SarlInterface createSarlInterface() {
		return new SarlInterfaceImplCustom();
	}

	@Override
	public SarlAction createSarlAction() {
		return new SarlActionImplCustom();
	}

	@Override
	public SarlField createSarlField() {
		return new SarlFieldImplCustom();
	}

}

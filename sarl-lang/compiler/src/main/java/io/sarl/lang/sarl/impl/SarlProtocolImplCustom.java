/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.sarl.SarlProtocolMessage;
import io.sarl.lang.sarl.SarlProtocolParameter;
import io.sarl.lang.sarl.SarlProtocolRole;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the SarlProtocol grammar element for SARL.
 * <!-- end-user-doc -->
 *
 * <p>The following features are implemented:
 * <ul>
 *   <li>{@link #isAbstract() <em>Replies if the "abstract" modifier is associated to the element</em>}</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @author $Author: mbaldoni$
 * @author $Author: cbaroglio$
 * @author $Author: rmicalizio$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class SarlProtocolImplCustom extends SarlProtocolImpl {

	/**
	 * <!-- begin-user-doc -->
	 * Constructor.
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlProtocolImplCustom() {
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

	@Override
	public EList<String> getRoleNames() {
		final var list = new BasicEList<String>();
		for (final var member : getMembers()) {
			if (member instanceof SarlProtocolRole role) {
				list.addAll(role.getNames());
			}
		}
		return list;
	}

	@Override
	public EList<SarlProtocolRole> getRoles() {
		final var list = new BasicEList<SarlProtocolRole>();
		for (final var member : getMembers()) {
			if (member instanceof SarlProtocolRole role) {
				list.add(role);
			}
		}
		return list;
	}

	@Override
	public EList<String> getParameterNames() {
		final var list = new BasicEList<String>();
		for (final var member : getMembers()) {
			if (member instanceof SarlProtocolParameter parameter) {
				list.add(parameter.getName());
			}
		}
		return list;
	}

	@Override
	public EList<SarlProtocolParameter> getParameters() {
		final var list = new BasicEList<SarlProtocolParameter>();
		for (final var member : getMembers()) {
			if (member instanceof SarlProtocolParameter parameter) {
				list.add(parameter);
			}
		}
		return list;
	}

	@Override
	public EList<SarlProtocolMessage> getMessages() {
		final var list = new BasicEList<SarlProtocolMessage>();
		for (final var member : getMembers()) {
			if (member instanceof SarlProtocolMessage message) {
				list.add(message);
			}
		}
		return list;
	}

}

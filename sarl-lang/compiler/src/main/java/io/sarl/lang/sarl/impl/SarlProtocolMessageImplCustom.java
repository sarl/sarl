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

import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.emf.common.util.EList;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmVisibility;

import com.google.common.base.Strings;

import io.sarl.lang.jvmmodel.IDefaultVisibilityProvider;
import io.sarl.lang.sarl.SarlProtocol;
import io.sarl.lang.util.BSPLConstants;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the protocol message grammar element for SARL.
 * <!-- end-user-doc -->
 *
 * <p>The following features are implemented:
 * <ul>
 *   <li>{@link #getDefaultVisibility() <em>Default visibility of fields in SARL elements</em>}</li>
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
public class SarlProtocolMessageImplCustom extends SarlProtocolMessageImpl {

	private final AtomicBoolean dispatched = new AtomicBoolean();

	/**
	 * <!-- begin-user-doc -->
	 * Constructor.
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlProtocolMessageImplCustom() {
		super();
	}

	@Override
	protected JvmVisibility getDefaultVisibility() {
		return IDefaultVisibilityProvider.getActionDefaultVisibilityIn(getDeclaringType());
	}

	@Override
	public EList<String> getModifiers() {
		var mods = super.getModifiers();
		if (!this.dispatched.getAndSet(true)) {
			updateToModifiers();
			mods = super.getModifiers();
		}
		return mods;
	}


	@Override
	public String getTo() {
		var to = super.getTo();
		if (!this.dispatched.getAndSet(true)) {
			updateToModifiers();
			to = super.getTo();
		}
		return to;
	}
	
	private void updateToModifiers() {
		getModifiers().clear();
		String to = null;
		for (final var rp : getRawTarget()) {
			if (BSPLConstants.MODIFIERS.contains(rp)) {
				getModifiers().add(rp);
			} else if (Strings.isNullOrEmpty(to)) {
				to = rp;
			} else {
				getModifiers().add(rp);
			}
		}
		if (!Strings.isNullOrEmpty(to)) {
			setTo(to);
		}
	}

	@Override
	public boolean isInTargetRole() {
		return getModifiers().contains(BSPLConstants.IN);
	}

	@Override
	public boolean isOutTargetRole() {
		return getModifiers().contains(BSPLConstants.OUT);
	}

	@Override
	public String getIdentifier() {
		final var buffer = new StringBuilder();
		buffer.append(getName()).append(":"); //$NON-NLS-1$
		buffer.append(getFrom()).append(":"); //$NON-NLS-1$
		buffer.append(getTo());
		return buffer.toString();
	}

	@Override
	public SarlProtocol getProtocol() {
		return EcoreUtil2.getContainerOfType(eContainer(), SarlProtocol.class);
	}

}

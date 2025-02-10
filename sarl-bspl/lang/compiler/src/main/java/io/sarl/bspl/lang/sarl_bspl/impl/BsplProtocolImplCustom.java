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

package io.sarl.bspl.lang.sarl_bspl.impl;

import java.util.stream.Collectors;

import org.eclipse.emf.common.util.DelegatingEList.UnmodifiableEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.xtext.common.types.JvmVisibility;

import io.sarl.bspl.lang.compiler.IDefaultVisibilityProvider;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolMessage;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolParameter;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolProtocolCall;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolRole;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the BSPL protocol.
 * <!-- end-user-doc -->
 *
 * <p>The following features are implemented:
 * <ul>
 *   <li>{@link #getDefaultVisibility()} <em>Default visibility of fields in SARL elements</em>}</li>
 *   <li>{@link #getRoles()} <em>The members of the protocols that are roles</em>}</li>
 *   <li>{@link #getParameters()} <em>The members of the protocols that are parameters</em>}</li>
 *   <li>{@link #getMessages()} <em>The members of the protocols that are messages</em>}</li>
 *   <li>{@link #getProtocolCalls()} <em>The members of the protocols that are protocol calls</em>}</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BsplProtocolImplCustom extends BsplProtocolImpl {

	/**
	 * <!-- begin-user-doc -->
	 * Constructor.
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected BsplProtocolImplCustom() {
		super();
	}

	@Override
	public JvmVisibility getDefaultVisibility() {
		return IDefaultVisibilityProvider.getProtocolDefaultVisibility();
	}
	
	@Override
	public EList<BsplProtocolRole> getRoles() {
		final var list = getMembers().stream().filter(it -> it instanceof BsplProtocolRole).map(it -> (BsplProtocolRole) it).collect(Collectors.toList());
		return new UnmodifiableEList<>(list);
	}

	@Override
	public EList<BsplProtocolParameter> getParameters() {
		final var list = getMembers().stream().filter(it -> it instanceof BsplProtocolParameter).map(it -> (BsplProtocolParameter) it).collect(Collectors.toList());
		return new UnmodifiableEList<>(list);
	}

	@Override
	public EList<BsplProtocolMessage> getMessages() {
		final var list = getMembers().stream().filter(it -> it instanceof BsplProtocolMessage).map(it -> (BsplProtocolMessage) it).collect(Collectors.toList());
		return new UnmodifiableEList<>(list);
	}

	@Override
	public EList<BsplProtocolProtocolCall> getProtocolCalls() {
		final var list = getMembers().stream().filter(it -> it instanceof BsplProtocolProtocolCall).map(it -> (BsplProtocolProtocolCall) it).collect(Collectors.toList());
		return new UnmodifiableEList<>(list);
	}

}

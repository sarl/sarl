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
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReferenceFactory;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import com.google.inject.Inject;
import com.google.inject.Singleton;

import io.sarl.bspl.api.protocol.impl.ProtocolRole;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolRole;

/** Provider of names for protocols.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class DefaultProtocolNames implements IProtocolNames {

	@Inject
	private CommonTypeComputationServices typeServices;
	
	@Override
	public Class<?> getProtocoRoleGenericInterface() {
		return ProtocolRole.class;
	}

	@Override
	public String getProtocolEnumerationName(String protocolName) {
		return protocolName + "Role"; //$NON-NLS-1$
	}
	
	private static LightweightTypeReference toLightweightTypeReference(
			JvmType type, EObject context, CommonTypeComputationServices services) {
		if (type == null) {
			return null;
		}
		final var owner = new StandardTypeReferenceOwner(services, context);
		final var factory = new LightweightTypeReferenceFactory(owner, false);
		final var reference = factory.toLightweightReference(type);
		return reference;
	}

	/*private static LightweightTypeReference toLightweightTypeReference(
			JvmTypeReference type, EObject context, CommonTypeComputationServices services) {
		if (type == null) {
			return null;
		}
		final var owner = new StandardTypeReferenceOwner(services, context);
		final var factory = new LightweightTypeReferenceFactory(owner, false);
		final var reference = factory.toLightweightReference(type);
		return reference;
	}*/

	private LightweightTypeReference ensureType(String typeName, EObject context) {
		final var declaredType = this.typeServices.getTypeReferences().findDeclaredType(typeName, context);
		if (declaredType != null) {
			return toLightweightTypeReference(declaredType, context, this.typeServices);
		}

		final var declaredType0 = this.typeServices.getTypesFactory().createJvmGenericType();
		final var idx = typeName.lastIndexOf("."); //$NON-NLS-1$
		if (idx >= 0) {
			declaredType0.setPackageName(typeName.substring(0, idx));
			declaredType0.setSimpleName(typeName.substring(idx + 1));
		} else {
			declaredType0.setSimpleName(typeName);
		}
		return toLightweightTypeReference(declaredType0, context, this.typeServices);

		/*final var typeReference = this.typeServices.getTypeReferences().getTypeForName(typeName, context);
		if (typeReference != null) {
			return toLightweightTypeReference(typeReference, context, this.typeServices);
		}
		throw new IllegalArgumentException();*/
	}

	@Override
	public String getProtocolCapacityName(String packageName, String protocolName, BsplProtocolRole role) {
		final var basename = new StringBuilder();
		basename.append(role.getName()).append("ProtocolCapacity"); //$NON-NLS-1$
		return basename.toString();
	}

	@Override
	public LightweightTypeReference getProtocolCapacity(String packageName, String protocolName, BsplProtocolRole role) {
		final var fullName = new StringBuilder();
		appendAdapterPackageName(packageName, protocolName, fullName);
		fullName.append(".").append(getProtocolCapacityName(packageName, protocolName, role)); //$NON-NLS-1$
		return ensureType(fullName.toString(), role);
	}

	@Override
	public String getProtocolSkillName(String packageName, String protocolName, BsplProtocolRole role) {
		final var basename = new StringBuilder();
		basename.append(role.getName()).append("ProtocolSkill"); //$NON-NLS-1$
		return basename.toString();
	}

	@Override
	public LightweightTypeReference getProtocolSkill(String packageName, String protocolName, BsplProtocolRole role) {
		final var fullName = new StringBuilder();
		appendAdapterPackageName(packageName, protocolName, fullName);
		fullName.append(".").append(getProtocolSkillName(packageName, protocolName, role)); //$NON-NLS-1$
		return ensureType(fullName.toString(), role);
	}

	@Override
	public String getProtocolBehaviorName(String packageName, String protocolName, BsplProtocolRole role) {
		final var basename = new StringBuilder();
		basename.append(role.getName()).append("ProtocolReactiveBehavior"); //$NON-NLS-1$
		return basename.toString();
	}

	@Override
	public LightweightTypeReference getProtocolBehavior(String packageName, String protocolName, BsplProtocolRole role) {
		final var fullName = new StringBuilder();
		appendAdapterPackageName(packageName, protocolName, fullName);
		fullName.append(".").append(getProtocolBehaviorName(packageName, protocolName, role)); //$NON-NLS-1$
		return ensureType(fullName.toString(), role);
	}

	@Override
	public String getEnabledMessageListFunctionName(String messageName) {
		final var fullName = new StringBuilder();
		fullName.append("getEnabled").append(messageName).append("Messages"); //$NON-NLS-1$ //$NON-NLS-2$
		return fullName.toString();
	}
	
	@Override
	public String getProtocolMessageQualifiedName(String packageName, String protocolName, String messageName) {
		final var fullName = new StringBuilder();
		appendAdapterPackageName(packageName, protocolName, fullName);
		fullName.append(".messages.").append(messageName); //$NON-NLS-1$
		return fullName.toString();
	}

	@Override
	public String getSendMessageFunctionName(String messageName) {
		final var fullName = new StringBuilder();
		fullName.append("send").append(messageName).append("Message"); //$NON-NLS-1$ //$NON-NLS-2$
		return fullName.toString();
	}

	private static void appendAdapterPackageName(String packageName, String protocolName, StringBuilder receiver) {
		if (!Strings.isEmpty(packageName)) {
			receiver.append(packageName).append("."); //$NON-NLS-1$
		}
		receiver.append(protocolName.toLowerCase()).append("_adapters"); //$NON-NLS-1$
	}

	@Override
	public String getProtocolAdapterPackageName(String packageName, String protocolName) {
		final var fn = new StringBuilder();
		appendAdapterPackageName(packageName, protocolName, fn);
		return fn.toString();
	}
	
}

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

import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import com.google.inject.ImplementedBy;

import io.sarl.bspl.lang.bspl.BsplProtocolRole;

/** Provider of names for protocols.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@ImplementedBy(DefaultProtocolNames.class)
public interface IProtocolNames {

	/** Replies the generic interface for all protocol roles.
	 *
	 * @return the type.
	 */
	Class<?> getProtocoRoleGenericInterface();

	/** Replies the basename of the protocol enumeration.
	 *
	 * @param protocolName the name of the protocol.
	 * @return the name.
	 */
	String getProtocolEnumerationName(String protocolName);

	/** Replies the basename of the protocol capacity for the given role in the given protocol.
	 *
	 * @param packageName the name of of the package in which the protocol is defined.
	 * @param protocolName the name of the protocol.
	 * @param role the role.
	 * @return the capacity basename.
	 */
	String getProtocolCapacityName(String packageName, String protocolName, BsplProtocolRole role);

	/** Replies the type of the protocol capacity for the given role in the given protocol.
	 *
	 * @param packageName the name of of the package in which the protocol is defined.
	 * @param protocolName the name of the protocol.
	 * @param role the role.
	 * @return the capacity type.
	 */
	LightweightTypeReference getProtocolCapacity(String packageName, String protocolName, BsplProtocolRole role);

	/** Replies the basename of the protocol skill for the given role in the given protocol.
	 *
	 * @param packageName the name of of the package in which the protocol is defined.
	 * @param protocolName the name of the protocol.
	 * @param role the role.
	 * @return the skill basename.
	 */
	String getProtocolSkillName(String packageName, String protocolName, BsplProtocolRole role);

	/** Replies the type of the protocol skill for the given role in the given protocol.
	 *
	 * @param packageName the name of of the package in which the protocol is defined.
	 * @param protocolName the name of the protocol.
	 * @param role the role.
	 * @return the skill type.
	 */
	LightweightTypeReference getProtocolSkill(String packageName, String protocolName, BsplProtocolRole role);

	/** Replies the basename of the protocol reactive behavior for the given role in the given protocol.
	 *
	 * @param packageName the name of of the package in which the protocol is defined.
	 * @param protocolName the name of the protocol.
	 * @param role the role.
	 * @return the reactive behavior basename.
	 */
	String getProtocolBehaviorName(String packageName, String protocolName, BsplProtocolRole role);

	/** Replies the type of the protocol reactive behavior for the given role in the given protocol.
	 *
	 * @param packageName the name of of the package in which the protocol is defined.
	 * @param protocolName the name of the protocol.
	 * @param role the role.
	 * @return the reactive behavior type.
	 */
	LightweightTypeReference getProtocolBehavior(String packageName, String protocolName, BsplProtocolRole role);

	/** Replies the name of the function that permits to get the list of enabled messages with the given name.
	 *
	 * @param messageName the name of the message.
	 * @return the function name.
	 */
	String getEnabledMessageListFunctionName(String messageName);

	/** Replies the name of the message.
	 *
	 * @param packageName the name of of the package in which the protocol is defined.
	 * @param protocolName the name of the protocol.
	 * @param messageName the basename of the message.
	 * @return the fully qualified name of the message.
	 */
	String getProtocolMessageQualifiedName(String packageName, String protocolName, String messageName);

	/** Replies the name of the function that permits to send a message.
	 *
	 * @param messageName the name of the message.
	 * @return the function name.
	 */
	String getSendMessageFunctionName(String messageName);

	/** Replies the name of the package that contains the protocol adapters.
	 *
	 * @param packageName the original package name.
	 * @param protocolName the name of the protocol.
	 * @return the adapter package name.
	 */
	String getProtocolAdapterPackageName(String packageName, String protocolName);

}

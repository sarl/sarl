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

package io.sarl.bspl.lang.compiler.fragments;

import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;

import com.google.inject.Singleton;

import io.sarl.bspl.api.protocol.impl.ProtocolMessage;
import io.sarl.bspl.lang.bspl.BsplProtocolMessage;
import io.sarl.bspl.lang.bspl.BsplProtocolRole;
import io.sarl.bspl.lang.compiler.IProtocolNames;
import io.sarl.bspl.lang.compiler.generic.ISarlTargetGeneratorContext;
import io.sarl.lang.core.annotation.SarlAsynchronousExecution;

/** The generator of the BSPL role's capacity.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class BsplProtocolCapacityGeneratorFragment {

	/** Run the pre-stage actions for the BSPL role's capacity.
	 *
	 * @param messages the messages in the BSPL protocol.
	 * @param roles the roles in the BSPL protocol.
	 * @param context the generation context.
	 * @throws IOException when the file cannot be generated on the device.
	 */
	public void preGenerate(List<BsplProtocolMessage> messages, List<BsplProtocolRole> roles, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		//
	}

	/** Generate the SARL code that is specific to BSPL role's capacity.
	 *
	 * @param messages the messages in the BSPL protocol.
	 * @param roles the roles in the BSPL protocol.
	 * @param context the generation context.
	 * @throws IOException when the file cannot be generated on the device.
	 */
	@SuppressWarnings("static-method")
	public void generate(List<BsplProtocolMessage> messages, List<BsplProtocolRole> roles, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		final var names =  context.getNameProvider();
		for (final var role : roles) {
			generateProtocolCapacity(messages, role, context, (messageNames, receiver) -> {
				var first = true;
				for (final var message : messageNames) {
					if (first) {
						first = false;
					} else {
						receiver.newLine();
					}

					final var messageQualifiedName = names.getProtocolMessageQualifiedName(context.getPackage(), context.getEnclosingTypeName(), message);
					
					final var messageType0 = context.findType(messageQualifiedName, role);
					receiver
						.newLine().append("@").append(SarlAsynchronousExecution.class).newLine() //$NON-NLS-1$
						.append("def ").append(names.getGetEnabledMessagesFunctionName(message)).append(" : ") //$NON-NLS-1$ //$NON-NLS-2$
						.append(List.class).append("<").append(ProtocolMessage.class).append("<") //$NON-NLS-1$ //$NON-NLS-2$
						.append(messageType0).append(">>"); //$NON-NLS-1$

					final var messageType1 = context.findType(messageQualifiedName, role);
					receiver
						.newLine().append("@").append(SarlAsynchronousExecution.class).newLine() //$NON-NLS-1$
						.append("def ").append(names.getSendMessageFunctionName(message)).append("(m : ") //$NON-NLS-1$ //$NON-NLS-2$
						.append(ProtocolMessage.class).append("<") //$NON-NLS-1$
						.append(messageType1).append(">)"); //$NON-NLS-1$
				}
			});
		}
	}

	private static void generateProtocolCapacity(List<BsplProtocolMessage> messages, BsplProtocolRole role, ISarlTargetGeneratorContext<IProtocolNames> context, Procedure2<Set<String>, ITreeAppendable> generator) throws IOException {
		final var sentMessages = new TreeSet<String>();
		final var roleName = role.getName();
		for (final var message : messages) {
			if (roleName.equals(message.getFrom())) {
				sentMessages.add(message.getMessage());
			}
		}

		if (!sentMessages.isEmpty()) {
			final var names = context.getNameProvider();
			final var capacityName = names.getProtocolCapacityName(context.getPackage(), context.getEnclosingTypeName(), role);
			final var capacityPackageName = names.getProtocolAdapterPackageName(context.getPackage(), context.getEnclosingTypeName());
	
			final var importManager = context.newImportManager(capacityPackageName, capacityName);
			final var content = context.newAppendableContent(importManager);
	
			content.append("capacity ").append(capacityName) //$NON-NLS-1$
				.append(" {").increaseIndentation(); //$NON-NLS-1$
	
			if (generator != null) {
				generator.apply(sentMessages, content);
			}
	
			content.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
	
			context.createSarlFile(capacityPackageName, capacityName, importManager, content);
		}
	}

}

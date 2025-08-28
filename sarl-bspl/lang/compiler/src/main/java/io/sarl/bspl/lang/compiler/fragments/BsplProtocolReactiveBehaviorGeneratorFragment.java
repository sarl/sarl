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
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure0;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure5;

import com.google.inject.Singleton;

import io.sarl.bspl.lang.bspl.BsplProtocolArgument;
import io.sarl.bspl.lang.bspl.BsplProtocolMessage;
import io.sarl.bspl.lang.bspl.BsplProtocolParameter;
import io.sarl.bspl.lang.bspl.BsplProtocolRole;
import io.sarl.bspl.lang.compiler.IProtocolNames;
import io.sarl.bspl.lang.compiler.generic.ISarlTargetGeneratorContext;
import io.sarl.lang.core.util.OutParameter;

/** The generator of the BSPL reactive behavior for a BSPL role.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class BsplProtocolReactiveBehaviorGeneratorFragment {

	/** Run the pre-stage actions for the BSPL role's reactive behavior.
	 *
	 * @param messages the messages in the BSPL protocol.
	 * @param roles the roles in the BSPL protocol.
	 * @param parameters the parameters in the BSPL protocol.
	 * @param context the generation context.
	 * @throws IOException when the file cannot be generated on the device.
	 */
	public void preGenerate(List<BsplProtocolMessage> messages, List<BsplProtocolRole> roles, List<BsplProtocolParameter> parameters, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		//
	}

	/** Generate the SARL code that is specific to BSPL reactive behavior.
	 *
	 * @param messages the messages in the BSPL protocol.
	 * @param roles the roles in the BSPL protocol.
	 * @param parameters the parameters in the BSPL protocol.
	 * @param context the generation context.
	 * @throws IOException when the file cannot be generated on the device.
	 */
	@SuppressWarnings("static-method")
	public void generate(List<BsplProtocolMessage> messages, List<BsplProtocolRole> roles, List<BsplProtocolParameter> parameters, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		for (final var role : roles) {
			generateReactiveBehavior(messages, role, parameters, context, (messageName, receivableMessages, parametersMapping, initializer, receiver) -> {
				final var names =  context.getNameProvider();
				final var messageQualifiedName = names.getProtocolMessageQualifiedName(context.getPackage(), context.getEnclosingTypeName(), messageName);

				final Comparator<BsplProtocolArgument> comparator = (a, b) -> a.getName().compareTo(b.getName());
				final var inKeys = new TreeSet<>(comparator);
				final var outParams = new TreeSet<>(comparator);
				receivableMessages.stream().map(it -> it.getArguments()).flatMap(List::stream).forEach(it -> {
					if (it.isKey() && it.isInput()) {
						inKeys.add(it);
					} else if (it.isOutput()) {
						outParams.add(it);
					}
				});
				
				if (!outParams.isEmpty()) {
					if (initializer!= null) {
						initializer.apply();
					}
					final var messageType0 = context.findType(messageQualifiedName, role);
					receiver.newLine().newLine().append("on ").append(messageType0).append(" {").increaseIndentation(); //$NON-NLS-1$ //$NON-NLS-2$
					
					if (!outParams.isEmpty()) {
						final var params = parameters.stream().collect(Collectors.toMap(it -> it.getName(), it -> it));
						for (final var outParam : outParams) {
							final var param = params.get(outParam.getName());
							if (param == null || !param.isPrivateVisibility()) {
								receiver.newLine().append("new ").append(names.getKnowledgeIdGenericInterface()).append("(\"") //$NON-NLS-1$ //$NON-NLS-2$
									.append(Strings.convertToJavaString(outParam.getName())).append("\""); //$NON-NLS-1$
								for (final var key : inKeys) {
									receiver.append(", occurrence.").append(key.getName()).append(" as "); //$NON-NLS-1$ //$NON-NLS-2$
									context.appendTypeReferenceOrObject(receiver, role, () -> parametersMapping.get(key.getName()).getType());
								}
								receiver.append(").setKnowledge(typeof("); //$NON-NLS-1$
								context.appendTypeReferenceOrObject(receiver, role, () -> parametersMapping.get(outParam.getName()).getType());
								receiver.append("), occurrence.").append(outParam.getName()).append(")"); //$NON-NLS-1$ //$NON-NLS-2$
							}
						}
					}

					receiver.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
				}
			});
		}
	}

	private static void generateReactiveBehavior(List<BsplProtocolMessage> messages, BsplProtocolRole role, List<BsplProtocolParameter> parameters,
			ISarlTargetGeneratorContext<IProtocolNames> context, Procedure5<String, List<BsplProtocolMessage>, Map<String, BsplProtocolParameter>, Procedure0, ITreeAppendable> generator) throws IOException {
		final var receivedMessages = new TreeMap<String, List<BsplProtocolMessage>>();

		final var roleName = role.getName();
		Map<String, BsplProtocolParameter> parametersMapping = null;
		for (final var message : messages) {
			if (roleName.equals(message.getTo())) {
				final var msgList = receivedMessages.computeIfAbsent(message.getMessage(), it -> new ArrayList<>());
				msgList.add(message);
				if (parametersMapping == null) {
					parametersMapping = parameters.stream().collect(Collectors.toMap(it -> it.getName(), it -> it));
				}
			}
		}
		
		if (!receivedMessages.isEmpty()) {
			final var names = context.getNameProvider();
			final var behaviorName = names.getProtocolBehaviorName(context.getPackage(), context.getEnclosingTypeName(), role);
			final var behaviorPackageName = names.getProtocolAdapterPackageName(context.getPackage(), context.getEnclosingTypeName());

			final var importManager = context.newImportManager(behaviorPackageName, behaviorName);
			final var content = context.newAppendableContent(importManager);

			if (context.isPackageVisibility()) {
				content.append("package "); //$NON-NLS-1$
			} else {
				content.append("public "); //$NON-NLS-1$
			}

			content.append("behavior ").append(behaviorName) //$NON-NLS-1$
			.append(" extends ").append(names.getProtocolBehaviorGenericInterface()) //$NON-NLS-1$
			.append(" {").increaseIndentation(); //$NON-NLS-1$
			
			if (generator != null) {
				var first = true;
				final OutParameter<Procedure0> initializer = new OutParameter<>();
				initializer.set(() -> {
					content.newLine().append("uses ").append(names.getLocalStageManagerGenericInterface()); //$NON-NLS-1$
					initializer.clear();
				});
				for (final var message : receivedMessages.entrySet()) {
					if (first) {
						first = false;
					} else {
						content.newLine();
					}
					generator.apply(message.getKey(), message.getValue(), parametersMapping, initializer.get(), content);
				}
			}

			content.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$

			context.createSarlFile(context.getSource(), behaviorPackageName, behaviorName, importManager, content);
		}
	}

}

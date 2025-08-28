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
import java.util.UUID;
import java.util.stream.Collectors;

import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure4;

import com.google.inject.Singleton;

import io.sarl.api.core.ExternalContextAccess;
import io.sarl.bspl.lang.bspl.BsplProtocolArgument;
import io.sarl.bspl.lang.bspl.BsplProtocolMessage;
import io.sarl.bspl.lang.bspl.BsplProtocolParameter;
import io.sarl.bspl.lang.bspl.BsplProtocolRole;
import io.sarl.bspl.lang.compiler.IProtocolNames;
import io.sarl.bspl.lang.compiler.generic.ISarlTargetGeneratorContext;
import io.sarl.lang.core.annotation.SarlAsynchronousExecution;

/** The generator of the BSPL role's skill.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class BsplProtocolSkillGeneratorFragment {

	/** Run the pre-stage actions for the BSPL role's skill.
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

	/** Generate the SARL code that is specific to BSPL role's skill.
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
			generateProtocolSkill(messages, role, parameters, context, (messageName, sendableMessages, parametersMapping, receiver) -> {
				generateGetEnabledFunction(receiver, context, role, messageName, sendableMessages, parametersMapping);
				generateSendFunction(receiver, context, role, messageName, sendableMessages, parametersMapping);
			});
		}
	}
	
	private static void generateGetEnabledFunction(ITreeAppendable receiver, ISarlTargetGeneratorContext<IProtocolNames> context, BsplProtocolRole role, String messageName, Map<String, List<BsplProtocolMessage>> sendableMessages,
			Map<String, BsplProtocolParameter> parameters) {
		final var names =  context.getNameProvider();
		final var messageQualifiedName = names.getProtocolMessageQualifiedName(context.getPackage(), context.getEnclosingTypeName(), messageName);

		final var messageType0 = context.findType(messageQualifiedName, role);
		final var messageType1 = context.findType(messageQualifiedName, role);
		receiver
		.newLine().append("@").append(SarlAsynchronousExecution.class).newLine() //$NON-NLS-1$
		.append("override ").append(names.getGetEnabledMessagesFunctionName(messageName)).append(" : ") //$NON-NLS-1$ //$NON-NLS-2$
		.append(List.class).append("<").append(names.getProtocolMessageGenericInterface()).append("<") //$NON-NLS-1$ //$NON-NLS-2$
		.append(messageType0).append(">> {").increaseIndentation(); //$NON-NLS-1$
		final var varEnabledMessages = receiver.declareSyntheticVariable(role, "enabledMessages"); //$NON-NLS-1$
		receiver.newLine().append("val ").append(varEnabledMessages).append(" = <").append(names.getProtocolMessageGenericInterface()).append("<").append(messageType1).append(">>newArrayList"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

		for (final var messageMapping : sendableMessages.entrySet()) {
			final var receiverName = Strings.convertToJavaString(messageMapping.getKey());
			final var messageType2 = context.findType(messageQualifiedName, role);
			final var messageType3 = context.findType(messageQualifiedName, role);
			final var varScope = receiver.declareSyntheticVariable(role, "scope"); //$NON-NLS-1$
			final var varSpace = receiver.declareSyntheticVariable(role, "spaceInstance"); //$NON-NLS-1$
			receiver
			.newLine().append("for (").append(varScope).append(" : getScopesFromName(\"").append(receiverName).append("\")) {").increaseIndentation() //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.newLine().append("val ").append(varSpace).append(" = ").append(varScope).append(".findSpaceMachtingScope") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			.newLine().append("if (").append(varSpace).append(" !== null) {").increaseIndentation() //$NON-NLS-1$ //$NON-NLS-2$
			.newLine().append("synchronized (localStateManagerLock) {").increaseIndentation(); //$NON-NLS-1$

			for (final var messageInstance : messageMapping.getValue()) {
				final var outParams = messageInstance.getArguments().stream().filter(it -> it.isOutput()).collect(Collectors.toList());
				if (!outParams.isEmpty()) {
					receiver.newLine().append("if ("); //$NON-NLS-1$

					var first = true;
					for (final var outParam : outParams) {
						if (first) {
							first = false;
						} else {
							receiver.append(" && "); //$NON-NLS-1$
						}
						receiver.append("!new ").append(names.getKnowledgeIdGenericInterface()).append("(\"") //$NON-NLS-1$ //$NON-NLS-2$
						.append(Strings.convertToJavaString(outParam.getName()))
						.append("\", ").append(varScope).append(".keys).isBound"); //$NON-NLS-1$ //$NON-NLS-2$
					}

					receiver.append(") {").increaseIndentation(); //$NON-NLS-1$
				}

				final var varMessageInstance = receiver.declareSyntheticVariable(role, "messageInstance"); //$NON-NLS-1$
				receiver.newLine().append("val ").append(varMessageInstance).append(" = new ").append(messageType2); //$NON-NLS-1$ //$NON-NLS-2$

				final var inParams = messageInstance.getArguments().stream().filter(it -> it.isInput()).collect(Collectors.toList());
				if (!inParams.isEmpty()) {
					var keyIndex = 0;
					for (final var inParam : inParams) {
						final var inParamName = Strings.convertToJavaString(inParam.getName());
						final var parameter = parameters.get(inParamName);
						if (parameter == null || !parameter.isPrivateVisibility()) {
							receiver.newLine().append(varMessageInstance).append(".").append(inParamName).append(" = "); //$NON-NLS-1$ //$NON-NLS-2$
							if (inParam.isKey()) {
								receiver.append(varScope).append(".keys.get(").append(Integer.toString(keyIndex)).append(") as "); //$NON-NLS-1$ //$NON-NLS-2$
								context.appendTypeReferenceOrObject(receiver, role, () -> parameters.get(inParamName).getType());
								receiver.append(" // ").append(inParamName); //$NON-NLS-1$
								++keyIndex;
							} else {
								receiver.append("new ").append(names.getKnowledgeIdGenericInterface()).append("(\"").append(inParamName) //$NON-NLS-1$ //$NON-NLS-2$
								.append("\", ").append(varScope).append(".keys), typeof("); //$NON-NLS-1$ //$NON-NLS-2$
								context.appendTypeReferenceOrObject(receiver, role, () -> parameters.get(inParamName).getType());
								receiver.append(")).getKnowledge"); //$NON-NLS-1$
							}
						} else {
							receiver.newLine().append("// Ignoring private parameter: ").append(inParamName); //$NON-NLS-1$
						}
					}
				}
				receiver.newLine().append(varEnabledMessages).append(" += new ").append(names.getProtocolMessageGenericInterface()).append("<").append(messageType3) //$NON-NLS-1$ //$NON-NLS-2$
				.append(">(").append(varSpace).append(", ").append(varMessageInstance).append(", new ").append(names.getKnowledgeIdGenericInterface()).append("(\"") //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				.append(receiverName).append("\", ").append(varScope).append(".keys).getKnowledge(typeof(").append(UUID.class).append(")))"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

				if (!outParams.isEmpty()) {
					receiver.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
				}
			}

			receiver.decreaseIndentation().newLine().append("}") //$NON-NLS-1$
			.decreaseIndentation().newLine().append("}") //$NON-NLS-1$
			.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
		}

		receiver
		.newLine().append("return ").append(varEnabledMessages) //$NON-NLS-1$
		.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
	}

	private static void generateSendFunction(ITreeAppendable receiver, ISarlTargetGeneratorContext<IProtocolNames> context, BsplProtocolRole role, String messageName, Map<String, List<BsplProtocolMessage>> sendableMessages,
			Map<String, BsplProtocolParameter> parameters) {
		final var names =  context.getNameProvider();
		final var messageQualifiedName = names.getProtocolMessageQualifiedName(context.getPackage(), context.getEnclosingTypeName(), messageName);

		final var messageType0 = context.findType(messageQualifiedName, role);
		receiver
		.newLine().append("@").append(SarlAsynchronousExecution.class).newLine() //$NON-NLS-1$
		.append("override ").append(names.getSendMessageFunctionName(messageName)).append("(message : ") //$NON-NLS-1$ //$NON-NLS-2$
		.append(names.getProtocolMessageGenericInterface()).append("<") //$NON-NLS-1$
		.append(messageType0).append(">) {").increaseIndentation(); //$NON-NLS-1$

		final Comparator<BsplProtocolArgument> comparator = (a, b) -> a.getName().compareTo(b.getName());
		final var inKeys = new TreeSet<>(comparator);
		final var outParams = new TreeSet<>(comparator);
		sendableMessages.values().stream().flatMap(List::stream).map(it -> it.getArguments()).flatMap(List::stream).forEach(it -> {
			if (it.isKey() && it.isInput()) {
				inKeys.add(it);
			} else if (it.isOutput()) {
				outParams.add(it);
			}
		});
		
		if (!outParams.isEmpty()) {
			final var variables = new TreeMap<String, Pair<String, String>>();

			for (final var outParam : outParams) {
				final var idName = receiver.declareSyntheticVariable(role, outParam.getName() + "Id"); //$NON-NLS-1$
				final var valueName = receiver.declareSyntheticVariable(role, outParam.getName());
				variables.put(outParam.getName(), Pair.of(idName, valueName));
				receiver.newLine().append("val ").append(idName).append(" = new ").append(names.getKnowledgeIdGenericInterface()).append("(\"").append(Strings.convertToJavaString(outParam.getName())).append("\""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				if (!inKeys.isEmpty()) {
					for (final var inKey : inKeys) {
						receiver.append(", "); //$NON-NLS-1$
						receiver.append("message.^event.").append(inKey.getName()); //$NON-NLS-1$
					}
				}

				receiver.append(")") //$NON-NLS-1$
				.newLine().append("var ").append(valueName).append(" : "); //$NON-NLS-1$ //$NON-NLS-2$
				context.appendTypeReferenceOrObject(receiver, role, () -> parameters.get(outParam.getName()).getType());
			}

			receiver.newLine().append("synchronized (localStateManagerLock) {").increaseIndentation(); //$NON-NLS-1$

			for (final var outParam : outParams) {
				final var outParamNames = variables.get(outParam.getName());
				final var idName = outParamNames.getKey();
				final var valueName = outParamNames.getValue();
				receiver.newLine().append(valueName).append(" = ").append(idName).append(".getKnowledge(typeof("); //$NON-NLS-1$ //$NON-NLS-2$
				context.appendTypeReferenceOrObject(receiver, role, () -> parameters.get(outParam.getName()).getType());
				receiver.append("))"); //$NON-NLS-1$
			}

			for (final var outParam : outParams) {
				final var idName = variables.get(outParam.getName()).getKey();
				receiver.newLine().append(idName).append(".bind"); //$NON-NLS-1$
			}

			receiver.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$

			for (final var outParam : outParams) {
				receiver.newLine().append("message.^event.").append(outParam.getName()).append(" = ").append(variables.get(outParam.getName()).getValue()); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

		receiver.newLine().append("emit(message.^space, message.^event) [it.ID == message.receiver]") //$NON-NLS-1$
			.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
	}

	private static void generateProtocolSkill(List<BsplProtocolMessage> messages, BsplProtocolRole role, List<BsplProtocolParameter> parameters,
			ISarlTargetGeneratorContext<IProtocolNames> context, Procedure4<String, Map<String, List<BsplProtocolMessage>>, Map<String, BsplProtocolParameter>, ITreeAppendable> generator) throws IOException {
		final var sentMessages = new TreeMap<String, Map<String, List<BsplProtocolMessage>>>();

		final var roleName = role.getName();
		Map<String, BsplProtocolParameter> parametersMapping = null;
		for (final var message : messages) {
			if (roleName.equals(message.getFrom())) {
				final var msgMapping = sentMessages.computeIfAbsent(message.getMessage(), it -> new TreeMap<>());
				final var msgList = msgMapping.computeIfAbsent(message.getTo(), it -> new ArrayList<>());
				msgList.add(message);
				if (parametersMapping == null) {
					parametersMapping = parameters.stream().collect(Collectors.toMap(it -> it.getName(), it -> it));
				}
			}
		}
		
		if (!sentMessages.isEmpty()) {
			
			final var names = context.getNameProvider();
			final var capacityType = names.getProtocolCapacity(context.getPackage(), context.getEnclosingTypeName(), role);
			final var skillName = names.getProtocolSkillName(context.getPackage(), context.getEnclosingTypeName(), role);
			final var skillPackageName = names.getProtocolAdapterPackageName(context.getPackage(), context.getEnclosingTypeName());

			final var importManager = context.newImportManager(skillPackageName, skillName);
			final var content = context.newAppendableContent(importManager);

			if (context.isPackageVisibility()) {
				content.append("package "); //$NON-NLS-1$
			} else {
				content.append("public "); //$NON-NLS-1$
			}

			content.append("skill ").append(skillName) //$NON-NLS-1$
			.append(" extends ").append(names.getProtocolSkillGenericInterface()) //$NON-NLS-1$
			.append(" implements ").append(capacityType) //$NON-NLS-1$
			.append(" {").increaseIndentation() //$NON-NLS-1$
			.newLine().append("uses ").append(names.getLocalStageManagerGenericInterface()).append(", ").append(ExternalContextAccess.class); //$NON-NLS-1$ //$NON-NLS-2$

			if (generator != null) {
				var first = true;
				for (final var message : sentMessages.entrySet()) {
					if (first) {
						first = false;
					} else {
						content.newLine();
					}
					generator.apply(message.getKey(), message.getValue(), parametersMapping, content);
				}
			}

			content.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$

			context.createSarlFile(context.getSource(), skillPackageName, skillName, importManager, content);
		}
	}

}

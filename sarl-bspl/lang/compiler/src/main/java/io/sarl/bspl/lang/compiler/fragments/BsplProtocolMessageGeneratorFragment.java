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
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.Functions.Function4;

import com.google.inject.Singleton;

import io.sarl.bspl.lang.bspl.BsplProtocolMessage;
import io.sarl.bspl.lang.bspl.BsplProtocolParameter;
import io.sarl.bspl.lang.compiler.IProtocolNames;
import io.sarl.bspl.lang.compiler.generic.ISarlTargetGeneratorContext;

/** The generator of the BSPL protocol message.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class BsplProtocolMessageGeneratorFragment {

	/** Run the pre-stage actions for the BSPL messages.
	 *
	 * @param messages the messages in the BSPL protocol.
	 * @param parameters the parameters in the BSPL protocol.
	 * @param context the generation context.
	 * @throws IOException when the file cannot be generated on the device.
	 */
	public void preGenerate(List<BsplProtocolMessage> messages, List<BsplProtocolParameter> parameters, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		//
	}

	/** Generate the SARL code that is specific to BSPL messages.
	 *
	 * @param messages the messages in the BSPL protocol.
	 * @param parameters the parameters in the BSPL protocol.
	 * @param context the generation context.
	 * @throws IOException when the file cannot be generated on the device.
	 */
	@SuppressWarnings("static-method")
	public void generate(List<BsplProtocolMessage> messages, List<BsplProtocolParameter> parameters, ISarlTargetGeneratorContext<IProtocolNames> context) throws IOException {
		generateProtocolMessages(messages, parameters, context, (message, arguments, params, receiver) -> {
			var hasContent = false;
			for (final var argument : arguments) {
				final var param = params.get(argument);
				if (param == null || !param.isPrivateVisibility()) {
					if (!hasContent) {
						hasContent = true;
						receiver.append(" {").increaseIndentation(); //$NON-NLS-1$
					}
					receiver.newLine().append("var ").append(argument).append(" : "); //$NON-NLS-1$ //$NON-NLS-2$
					context.appendTypeReferenceOrObject(receiver, param, param == null ? null : param::getType);
				}
			}
			return Boolean.valueOf(hasContent);
		});
	}
	
	private static void generateProtocolMessages(List<BsplProtocolMessage> messages, List<BsplProtocolParameter> parameters, ISarlTargetGeneratorContext<IProtocolNames> context,
			Function4<String, Set<String>, Map<String, BsplProtocolParameter>, ITreeAppendable, Boolean> generator) throws IOException {
		final var arguments = new TreeMap<String, Set<String>>();
		for (final var message : messages) {
			final var argSet = arguments.computeIfAbsent(message.getMessage(), key -> new TreeSet<>());
			final var args = message.getArguments();
			for (final var arg : args) {
				argSet.add(arg.getName());
			}
		}

		final var params = parameters.stream().collect(Collectors.toMap(it -> it.getName(), it -> it));

		for (final var messageSpec : arguments.entrySet()) {
			generateProtocolMessage(messageSpec.getKey(), messageSpec.getValue(), params, context, generator); 
		}
	}

	private static void generateProtocolMessage(String message, Set<String> arguments, Map<String, BsplProtocolParameter> params, ISarlTargetGeneratorContext<IProtocolNames> context,
			Function4<String, Set<String>, Map<String, BsplProtocolParameter>, ITreeAppendable, Boolean> generator) throws IOException {
		final var names = context.getNameProvider();
		final var messagePackageName = names.getProtocolMessagePackageName(context.getPackage(), context.getEnclosingTypeName(), message);
		final var messageBaseName = names.getProtocolMessageName(context.getPackage(), context.getEnclosingTypeName(), message);

		final var importManager = context.newImportManager(messagePackageName, messageBaseName);
		final var content = context.newAppendableContent(importManager);

		if (context.isPackageVisibility()) {
			content.append("package "); //$NON-NLS-1$
		} else {
			content.append("public "); //$NON-NLS-1$
		}

		content.append("event ").append(messageBaseName).append(" extends ").append(names.getProtocolEventGenericInterface()); //$NON-NLS-1$ //$NON-NLS-2$
		if (generator != null && !arguments.isEmpty()) {
			if (generator.apply(message, arguments, params, content).booleanValue()) {
				content.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
			}
		}
		context.createSarlFile(context.getSource(), messagePackageName, messageBaseName, importManager, content);
	}

}

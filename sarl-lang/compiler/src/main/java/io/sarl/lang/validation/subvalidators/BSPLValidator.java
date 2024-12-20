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

package io.sarl.lang.validation.subvalidators;

import static io.sarl.lang.util.BSPLConstants.KEY;
import static io.sarl.lang.util.BSPLConstants.MESSAGE_TARGET_MODIFIERS;
import static io.sarl.lang.util.BSPLConstants.MODIFIERS;
import static io.sarl.lang.validation.IssueCodes.PROTOCOL_DUPLICATE_MESSAGE;
import static io.sarl.lang.validation.IssueCodes.PROTOCOL_DUPLICATE_ROLE;
import static io.sarl.lang.validation.IssueCodes.PROTOCOL_EMPTY;
import static io.sarl.lang.validation.IssueCodes.PROTOCOL_MISSING_NAME;
import static io.sarl.lang.validation.IssueCodes.*;
import static io.sarl.lang.validation.IssueCodes.UNSUPPORTED_STATEMENT;
import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER;
import static org.eclipse.xtend.core.validation.IssueCodes.UNNECESSARY_MODIFIER;

import java.text.MessageFormat;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;

import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.inject.Inject;
import com.google.inject.Injector;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlProtocol;
import io.sarl.lang.sarl.SarlProtocolCall;
import io.sarl.lang.sarl.SarlProtocolMessage;
import io.sarl.lang.sarl.SarlProtocolParameter;
import io.sarl.lang.sarl.SarlProtocolRole;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/**
 * A specialized validator to deal with BSPL protocol components.
 *
 * <p>Hypothesis and differences with original BSPL:<ul>
 * <li>Syntax:<ol>
 * <li>Roles may be specified with one or many {@code role} statements</li>
 * <li>A parameter declaration stars with the keyword {@code var} and not {@code parameter} to avoid the addition of a keyword that has no meaning in the rest of SARL</li>
 * <li>In parameter declaration, the order of the modifiers is not important</li>
 * <li>In parameter declaration, it is possible to specify an explicit type; Default is {@code Object}</li>
 * <li>In procotol-call declaration, each call must be prefixed with the keyword {@code uses}; Otherwise the grammar becomes ambiguous</li>
 * <li>In message declaration, the modifiers {@code in} and {@code out} are accepted before the name of the receiver role</li>
 * <li>In message declaration, the list of parameters could be empty</li>
 * <li>Each statement in the protocol definition could be terminated by the character {@code ;}, which is the standard separator in SARL</li>
 * </ol></li>
 * <li>Semantic:<ol>
 * <li>No multiple definition of a role</li>
 * <li>No multiple definition of a parameter, even with different modifers</li>
 * <li>No multiple definition of a message that have the same sender, receiver and message name, even with different parameters</li>
 * <li>Modifiers {@code in}, {@code out}, {@code any}, {@code nil}, {@code opt} are mutually exclusive</li>
 * <li>Roles in a message must be defined with {@code role}</li>
 * <li>Parameters in a message must be defined with {@code var}</li>
 * <li>Modifiers {@code any}, {@code nil} and {@code opt} are not yet supported</li>
 * <li>Protocol calls are not yet supported</li>
 * </ol></li>
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
public class BSPLValidator extends AbstractSARLSubValidatorWithParentLink {

	@Inject
	private SARLGrammarKeywordAccess grammarAccess;

	@Inject
	private Injector injector;
	
	private SARLModifierValidator protocolModifierValidator;

	/** Replies the modifier validator for the protocols.
	 *
	 * @return the validator.
	 */
	protected SARLModifierValidator getProtocolModifierValidator() {
		if (this.protocolModifierValidator == null) {
			this.protocolModifierValidator = newSARLModifierValidator(Lists.newArrayList(
					this.grammarAccess.getPublicKeyword(),
					this.grammarAccess.getPackageKeyword(),
					this.grammarAccess.getAbstractKeyword(),
					this.grammarAccess.getFinalKeyword()));
		}
		return this.protocolModifierValidator;
	}

	/** Create a new validator.
	 *
	 * @return the validator.
	 * @since 0.15
	 */
	protected SARLModifierValidator newSARLModifierValidator(List<String> modifiers) {
		final var validator = new SARLModifierValidator(modifiers, getVisibilityModifiers(), this::error, this::addIssue);
		this.injector.injectMembers(validator);
		return validator;
	}

	private static String buildModifierList(Set<String> modifiers) {
		final var buffer = new StringBuilder();
		var first = false;
		for (final var modifier : modifiers) {
			if (first) {
				first = false;
			} else {
				buffer.append(", "); //$NON-NLS-1$
			}
			buffer.append(MessageFormat.format(Messages.BSPLValidator_9, modifier));
		}
		return buffer.toString();
	}

	/** Check the modifiers for the SARL protocols.
	 *
	 * @param protocol the protocol.
	 */
	@Check(CheckType.FAST)
	public void checkModifiers(SarlProtocol protocol) {
		getProtocolModifierValidator().checkModifiers(protocol,
				MessageFormat.format(Messages.SARLFeatureModifierValidator_11, protocol.getName()));
	}

	/** Check if the protocol is empty or not.
	 *
	 * @param protocol the protocol to check.
	 */
	@Check(CheckType.FAST)
	public void checkEmptyProtocol(SarlProtocol protocol) {
		if (!isIgnored(PROTOCOL_EMPTY) && protocol.getMessages().isEmpty()) {
			addIssue(
					MessageFormat.format(Messages.BSPLValidator_16, protocol.getName()),
					protocol,
					PROTOCOL_EMPTY);
		}
	}
	
	/** Protocol calls are not yet supported.
	 *
	 * @param call the protocol to be checked.
	 */
	@Check(CheckType.FAST)
	public void checkUnsupportedCall(SarlProtocolCall call) {
		error(Messages.BSPLValidator_15, call, null, ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
				UNSUPPORTED_STATEMENT);
	}

	/** Check if the name and modifiers are correctly defined for the parameter by analyzing the raw arguments from the grammar.
	 *
	 * @param parameter the parameter to check.
	 */
	@Check(CheckType.NORMAL)
	public void checkParameterNameAndModifierDeclaration(SarlProtocolParameter parameter) {
		checkIdAndModifiers(
				parameter,
				parameter.getName(),
				Messages.BSPLValidator_1,
				parameter.getModifiers(),
				MODIFIERS,
				Messages.BSPLValidator_2,
				Messages.BSPLValidator_5,
				Messages.BSPLValidator_7);
	}

	/** Check if a target is correctly defined for the message by analyzing the raw target values from the grammar.
	 *
	 * @param message the message to check.
	 */
	@Check(CheckType.NORMAL)
	public void checkMessageTargetDeclaration(SarlProtocolMessage message) {
		checkIdAndModifiers(
				message,
				message.getTo(),
				Messages.BSPLValidator_3,
				message.getModifiers(),
				MESSAGE_TARGET_MODIFIERS,
				Messages.BSPLValidator_4,
				Messages.BSPLValidator_6,
				Messages.BSPLValidator_8);
	}

	private void checkIdAndModifiers(EObject construct, String id, String idMessage, List<String> modifiers,
			Set<String> validModifiers, String modifierMessage, String unnecessaryModifierMessage,
			String exclusiveModifierMessage) {
		if (Strings.isNullOrEmpty(id)) {
			error(idMessage, construct, null, ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					PROTOCOL_MISSING_NAME);
		}
		String validModifierString = null;
		final var seenModifiers = new HashSet<String>();
		var foundExclusive = false;
		for (final var modifier : modifiers) {
			if (!validModifiers.contains(modifier)) {
				if (validModifierString == null) {
					validModifierString = buildModifierList(validModifiers);
				}
				error(MessageFormat.format(modifierMessage, modifier, validModifierString),
						construct, null, ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INVALID_MODIFIER);
			}
			if (!KEY.equals(modifier)) {
				if (foundExclusive) {
					final var exclusiveModifiers = new HashSet<>(validModifiers);
					exclusiveModifiers.remove(KEY);
					exclusiveModifiers.remove(modifier);
					final var exclusiveModifierString = buildModifierList(exclusiveModifiers);
					error(MessageFormat.format(exclusiveModifierMessage, modifier, exclusiveModifierString),
							construct, null, ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							INVALID_MODIFIER);
				} else {
					foundExclusive = true;
				}
			}
			if (!seenModifiers.add(modifier)) {
				warning(MessageFormat.format(unnecessaryModifierMessage, modifier),
						construct, null, ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						UNNECESSARY_MODIFIER);
			}
		}
	}

	/** Check if there is duplicate declarations of roles, parameters, uses and messages.
	 *
	 * @param protocol the protocol to check.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkDuplicateDeclaration(SarlProtocol protocol) {
		final var seenRoles = new HashSet<String>();
		final var seenParameters = new HashSet<String>();
		final var seenMessages = new HashSet<String>();
		for (final var member : protocol.getMembers()) {
			if (member instanceof SarlProtocolRole roles) {
				var i = 0;
				for (final var role : roles.getNames()) {
					if (!seenRoles.add(role)) {
						error(MessageFormat.format(Messages.BSPLValidator_10, role),
								roles,
								SarlPackage.eINSTANCE.getSarlProtocolRole_Names(), i,
								PROTOCOL_DUPLICATE_ROLE);
					}
					++i;
				}
			} else if (member instanceof SarlProtocolParameter parameter) {
				final var name = parameter.getName();
				if (!seenParameters.add(name)) {
					error(MessageFormat.format(Messages.BSPLValidator_11, name),
							parameter,
							null, ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							PROTOCOL_DUPLICATE_PARAMETER);
				}
			} else if (member instanceof SarlProtocolMessage message) {
				final var name = message.getIdentifier();
				if (!seenMessages.add(name)) {
					error(MessageFormat.format(Messages.BSPLValidator_12, message.getName(), message.getFrom(), message.getTo()),
							message,
							null, ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							PROTOCOL_DUPLICATE_MESSAGE);
				}
			}
		}
	}

	/** Check if there the referenced roles and parameters in the message are correct.
	 *
	 * @param protocol the protocol to check.
	 */
	@Check(CheckType.NORMAL)
	public void checkMessageReferences(SarlProtocol protocol) {
		final var allRoles = protocol.getRoleNames();
		final var allParameters = protocol.getParameterNames();
		for (final var message : protocol.getMessages()) {
			if (!allRoles.contains(message.getFrom())) {
				error(MessageFormat.format(Messages.BSPLValidator_13, message.getFrom()),
						message,
						SarlPackage.eINSTANCE.getSarlProtocolMessage_From(), ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						PROTOCOL_UNDEFINED_ROLE);
			}
			if (!allRoles.contains(message.getTo())) {
				error(MessageFormat.format(Messages.BSPLValidator_13, message.getTo()),
						message,
						SarlPackage.eINSTANCE.getSarlProtocolMessage_To(), ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						PROTOCOL_UNDEFINED_ROLE);
			}
			var i = 0;
			for (final var parameter : message.getParameters()) {
				if (!allParameters.contains(parameter.getName())) {
					error(MessageFormat.format(Messages.BSPLValidator_14, parameter.getName()),
							message,
							SarlPackage.eINSTANCE.getSarlProtocolMessage_Parameters(), i,
							PROTOCOL_UNDEFINED_PARAMETER);
				}
				++i;
			}
		}
	}

}

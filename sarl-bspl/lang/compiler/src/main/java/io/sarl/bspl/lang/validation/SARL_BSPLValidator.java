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

package io.sarl.bspl.lang.validation;

import static io.sarl.bspl.lang.validation.IssueCodes.DUPLICATE_PROTOCOL_ARGUMENT;
import static io.sarl.bspl.lang.validation.IssueCodes.DUPLICATE_PROTOCOL_MESSAGE;
import static io.sarl.bspl.lang.validation.IssueCodes.DUPLICATE_PROTOCOL_NAME;
import static io.sarl.bspl.lang.validation.IssueCodes.DUPLICATE_PROTOCOL_PARAMETER;
import static io.sarl.bspl.lang.validation.IssueCodes.DUPLICATE_PROTOCOL_ROLE;
import static io.sarl.bspl.lang.validation.IssueCodes.EMPTY_PACKAGE_DECLARATION;
import static io.sarl.bspl.lang.validation.IssueCodes.EMPTY_PROTOCOL;
import static io.sarl.bspl.lang.validation.IssueCodes.INVALID_ROLE_CARDINALITY_ORDER;
import static io.sarl.bspl.lang.validation.IssueCodes.MISSED_PARAMETER_TYPE;
import static io.sarl.bspl.lang.validation.IssueCodes.MISSED_PROTOCOL_MESSAGE;
import static io.sarl.bspl.lang.validation.IssueCodes.MISSED_PROTOCOL_ROLE;
import static io.sarl.bspl.lang.validation.IssueCodes.UNDEFINED_PROTOCOL_PARAMETER;
import static io.sarl.bspl.lang.validation.IssueCodes.UNDEFINED_PROTOCOL_ROLE;
import static io.sarl.bspl.lang.validation.IssueCodes.UNNECESSARY_ROLE_CARDINALITY;

import java.text.MessageFormat;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.util.DeprecationUtil;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociations;
import org.eclipse.xtext.xtype.XImportDeclaration;

import com.google.common.collect.HashMultimap;
import com.google.inject.Inject;
import com.google.inject.Singleton;

import io.sarl.bspl.lang.sarl_bspl.BsplProtocol;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolMessage;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolParameter;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolRole;
import io.sarl.bspl.lang.sarl_bspl.BsplProtocolSpecification;
import io.sarl.bspl.lang.sarl_bspl.Sarl_bsplPackage;

/**
 * Validator for the SARL BSPL elements.
 *
 * <p>The check type may be one of:<ul>
 * <li>{@link CheckType#FAST}: is executed after a delay of 500ms after ANY editing action (type, enter, delete);</li>
 * <li>{@link CheckType#NORMAL}: is executed after a build (manual, or automatic);</li>
 * <li>{@link CheckType#EXPENSIVE}: is executed by right clicking ANYWHERE in the editor window and chooseing "Validate".</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation"
 */
@Singleton
public class SARL_BSPLValidator extends AbstractSARL_BSPLValidator {

	@Inject
	private IJvmModelAssociations jvmModelAssociations;

	@Inject
	private IProgrammaticWarningSuppressor warningSuppressor;

	@Override
	public IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject eObject) {
		final var severities = super.getIssueSeverities(context, eObject);
		return this.warningSuppressor.getIssueSeverities(context, eObject, severities);
	}

	@Override
	@Check(CheckType.NORMAL)
	public void checkDeprecated(XImportDeclaration decl) {
		final var file = EcoreUtil2.getContainerOfType(decl, BsplProtocolSpecification.class);
		if (file != null) {
			for (final var t : file.getBsplProtocols()) {
				for (final var e : this.jvmModelAssociations.getJvmElements(t)) {
					if  (e instanceof JvmAnnotationTarget cvalue) {
						if (DeprecationUtil.isDeprecated(cvalue)) {
							return;
						}
					}
				}

				if (hasAnnotation(t, Deprecated.class)) {
					return;
				}
			}
		}
		super.checkDeprecated(decl);
	}

	private static boolean hasAnnotation(BsplProtocol source, Class<?> class1) {
		for (final var anno : source.getAnnotations()) {
			if (anno != null && anno.getAnnotationType() != null && class1.getName().equals(anno.getAnnotationType().getIdentifier())) {
				return true;
			}
		}
		return false;
	}

	/** Check the top elements within a specification are not duplicated.
	 *
	 * @param specification the SARL BSPL specification.
	 */
	@Check(CheckType.FAST)
	public void checkTopElementsAreUnique(BsplProtocolSpecification specification) {
		final var name2type = HashMultimap.<String, BsplProtocol>create();
		for (final var protocol : specification.getBsplProtocols()) {
			final var name = protocol.getName();
			if (!Strings.isEmpty(name)) {
				name2type.put(name, protocol);
			}
		}
		for (final var name: name2type.keySet()) {
			final var types = name2type.get(name);
			if (types.size() > 1) {
				for (final var type: types) {
					error(
							MessageFormat.format(Messages.SARL_BSPLValidator_0, name),
							type,
							Sarl_bsplPackage.Literals.BSPL_PROTOCOL__NAME,
							DUPLICATE_PROTOCOL_NAME);
				}
			}
		}
	}

	/** Check the protocol has at least one role and one message.
	 *
	 * @param protocol the SARL BSPL protocol.
	 */
	@Check(CheckType.FAST)
	public void checkProtocol(BsplProtocol protocol) {
		final var members = protocol.getMembers();
		if (members.isEmpty()) {
			error(
					MessageFormat.format(Messages.SARL_BSPLValidator_3, protocol.getName()),
					protocol,
					Sarl_bsplPackage.Literals.BSPL_PROTOCOL__NAME,
					EMPTY_PROTOCOL);
		} else {
			var hasRole = false;
			var hasMessage = false;
			final var iterator = members.iterator();
			while (iterator.hasNext() && (!hasRole || !hasMessage)) {
				final var member = iterator.next();
				if (member instanceof BsplProtocolRole) {
					hasRole = true;
				} else if (member instanceof BsplProtocolMessage) {
					hasMessage = true;
				}
			}
			if (!hasRole) {
				error(
						MessageFormat.format(Messages.SARL_BSPLValidator_1, protocol.getName()),
						protocol,
						Sarl_bsplPackage.Literals.BSPL_PROTOCOL__NAME,
						MISSED_PROTOCOL_ROLE);
			}
			if (!hasMessage) {
				error(
						MessageFormat.format(Messages.SARL_BSPLValidator_2, protocol.getName()),
						protocol,
						Sarl_bsplPackage.Literals.BSPL_PROTOCOL__NAME,
						MISSED_PROTOCOL_MESSAGE);
			}
		}
	}

	/** Check the protocol messages in order to not have duplicates.
	 *
	 * @param protocol the SARL BSPL protocol.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkDuplicateMessage(BsplProtocol protocol) {
		if (!isIgnored(DUPLICATE_PROTOCOL_MESSAGE)) {
			final var definedMessages = new TreeMap<String, Map<String, Set<String>>>();
			for (final var member : protocol.getMembers()) {
				if (member instanceof BsplProtocolMessage message) {
					final var from = message.getFrom();
					final var to = message.getTo();
					final var msg = message.getMessage();
					final var subset1 = definedMessages.computeIfAbsent(from, it -> new TreeMap<>());
					final var subset2 = subset1.computeIfAbsent(to, it -> new TreeSet<>());
					if (!subset2.add(msg)) {
						addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_4, from, to, msg, protocol.getName()),
								message,
								DUPLICATE_PROTOCOL_MESSAGE);
					}
				}
			}
		}
	}

	/** Check the protocol parameters in order to not have duplicates.
	 *
	 * @param protocol the SARL BSPL protocol.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkDuplicateParameter(BsplProtocol protocol) {
		final var definedParameters = new TreeSet<String>();
		for (final var member : protocol.getMembers()) {
			if (member instanceof BsplProtocolParameter parameter) {
				final var name = parameter.getName();
				if (!definedParameters.add(name)) {
					addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_9, name, protocol.getName()),
							parameter,
							DUPLICATE_PROTOCOL_PARAMETER);
				}
			}
		}
	}

	/** Check the protocol roles in order to not have duplicates.
	 *
	 * @param protocol the SARL BSPL protocol.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkDuplicateRole(BsplProtocol protocol) {
		if (!isIgnored(DUPLICATE_PROTOCOL_ROLE)) {
			final var definedRoles = new TreeSet<String>();
			for (final var member : protocol.getMembers()) {
				if (member instanceof BsplProtocolRole role) {
					final var name = role.getName();
					if (!definedRoles.add(name)) {
						addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_10, name, protocol.getName()),
								role,
								DUPLICATE_PROTOCOL_ROLE);
					}
				}
			}
		}
	}

	/** Check the protocol message has known roles.
	 *
	 * @param protocol the SARL BSPL protocol.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkMessageRoles(BsplProtocol protocol) {
		final var roles = new TreeSet<String>();
		final var messages = new LinkedList<BsplProtocolMessage>();
		for (final var member : protocol.getMembers()) {
			if (member instanceof BsplProtocolRole role) {
				roles.add(role.getName());
			} else if (member instanceof BsplProtocolMessage message) {
				messages.add(message);
			}
		}
		for (final var message : messages) {
			final var from = message.getFrom();
			if (!roles.contains(from)) {
				error(
						MessageFormat.format(Messages.SARL_BSPLValidator_5, from, protocol.getName()),
						message,
						Sarl_bsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__FROM,
						UNDEFINED_PROTOCOL_ROLE);
			}
			final var to = message.getTo();
			if (!roles.contains(to)) {
				error(
						MessageFormat.format(Messages.SARL_BSPLValidator_6, to, protocol.getName()),
						message,
						Sarl_bsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__TO,
						UNDEFINED_PROTOCOL_ROLE);
			}
		}
	}

	/** Check the protocol role cardinalities.
	 *
	 * @param role the SARL BSPL role.
	 */
	@Check(CheckType.FAST)
	public void checkRoleCardinalities(BsplProtocolRole role) {
		if (!isIgnored(UNNECESSARY_ROLE_CARDINALITY)) {
			if (role.getMin() != null && role.getMin().intValue() <= 0) {
				addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_14, role.getName()),
						role,
						Sarl_bsplPackage.Literals.BSPL_PROTOCOL_ROLE__MIN,
						UNNECESSARY_ROLE_CARDINALITY);
			}
			if (role.getMax() != null && role.getMax().intValue() <= 0) {
				addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_15, role.getName()),
						role,
						Sarl_bsplPackage.Literals.BSPL_PROTOCOL_ROLE__MAX,
						UNNECESSARY_ROLE_CARDINALITY);
			}
		}
		if (!isIgnored(INVALID_ROLE_CARDINALITY_ORDER) && role.isFixedMinMax()) {
			addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_16, role.getName(), role.getMin(), role.getMax()),
					role,
					Sarl_bsplPackage.Literals.BSPL_PROTOCOL_ROLE__MIN,
					INVALID_ROLE_CARDINALITY_ORDER);
		}
	}

	/** Check the protocol message has known parameters and arguments are not duplicated.
	 *
	 * @param protocol the SARL BSPL protocol.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkMessageParameters(BsplProtocol protocol) {
		final var parameters = new TreeSet<String>();
		final var messages = new LinkedList<BsplProtocolMessage>();
		for (final var member : protocol.getMembers()) {
			if (member instanceof BsplProtocolParameter parameter) {
				parameters.add(parameter.getName());
			} else if (member instanceof BsplProtocolMessage message) {
				messages.add(message);
			}
		}
		for (final var message : messages) {
			final var from = message.getFrom();
			final var to = message.getTo();
			final var msg = message.getMessage();
			final var definedArguments = new TreeSet<String>();
			var i = 0;
			for (final var argument : message.getArguments()) {
				final var argumentName = argument.getName();
				if (!parameters.contains(argumentName)) {
					error(
							MessageFormat.format(Messages.SARL_BSPLValidator_8, argumentName, protocol.getName()),
							message,
							Sarl_bsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__ARGUMENTS,
							i,
							UNDEFINED_PROTOCOL_PARAMETER);
				}
				if (!definedArguments.add(argumentName)) {
					error(
							MessageFormat.format(Messages.SARL_BSPLValidator_11, argumentName, from, to, msg, protocol.getName()),
							message,
							Sarl_bsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__ARGUMENTS,
							i,
							DUPLICATE_PROTOCOL_ARGUMENT);
				}
				++i;
			}
		}
	}

	/** Check the protocol parameter has type.
	 *
	 * @param parameter the SARL BSPL parameter.
	 */
	@Check(CheckType.FAST)
	public void checkParameterType(BsplProtocolParameter parameter) {
		if (!isIgnored(MISSED_PARAMETER_TYPE) && parameter.getType() == null) {
			addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_7, parameter.getName()),
					parameter,
					Sarl_bsplPackage.Literals.BSPL_PROTOCOL_PARAMETER__NAME,
					MISSED_PARAMETER_TYPE);
		}
	}
	
	/** Check the package declaration in the specification.
	 *
	 * @param specification the BSPL specification file.
	 */
	@Check(CheckType.FAST)
	public void checkPackageDeclaration(BsplProtocolSpecification specification) {
		if (Strings.isEmpty(specification.getPackage())) {
			if (!isIgnored(EMPTY_PACKAGE_DECLARATION)) {
				addIssue(Messages.SARL_BSPLValidator_12,
						specification,
						Sarl_bsplPackage.Literals.BSPL_PROTOCOL_SPECIFICATION__PACKAGE,
						EMPTY_PACKAGE_DECLARATION);
			}
		}
	}

}

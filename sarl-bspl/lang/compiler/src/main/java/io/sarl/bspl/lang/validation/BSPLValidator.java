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

import static io.sarl.bspl.lang.validation.IssueCodes.*;

import java.text.MessageFormat;
import java.util.ArrayList;
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
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.common.collect.SortedSetMultimap;
import com.google.inject.Inject;
import com.google.inject.Singleton;

import io.sarl.bspl.lang.bspl.BsplPackage;
import io.sarl.bspl.lang.bspl.BsplProtocol;
import io.sarl.bspl.lang.bspl.BsplProtocolMessage;
import io.sarl.bspl.lang.bspl.BsplProtocolParameter;
import io.sarl.bspl.lang.bspl.BsplProtocolRole;
import io.sarl.bspl.lang.bspl.BsplProtocolSpecification;

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
 * @since 0.15
 * @see "https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation"
 */
@Singleton
public class BSPLValidator extends AbstractBSPLValidator {

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
							BsplPackage.Literals.BSPL_PROTOCOL__NAME,
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
					BsplPackage.Literals.BSPL_PROTOCOL__NAME,
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
						BsplPackage.Literals.BSPL_PROTOCOL__NAME,
						MISSED_PROTOCOL_ROLE);
			}
			if (!hasMessage) {
				error(
						MessageFormat.format(Messages.SARL_BSPLValidator_2, protocol.getName()),
						protocol,
						BsplPackage.Literals.BSPL_PROTOCOL__NAME,
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
						BsplPackage.Literals.BSPL_PROTOCOL_ROLE__MIN,
						UNNECESSARY_ROLE_CARDINALITY);
			}
			if (role.getMax() != null && role.getMax().intValue() <= 0) {
				addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_15, role.getName()),
						role,
						BsplPackage.Literals.BSPL_PROTOCOL_ROLE__MAX,
						UNNECESSARY_ROLE_CARDINALITY);
			}
		}
		if (!isIgnored(INVALID_ROLE_CARDINALITY_ORDER) && role.isFixedMinMax()) {
			addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_16, role.getName(), role.getMin(), role.getMax()),
					role,
					BsplPackage.Literals.BSPL_PROTOCOL_ROLE__MIN,
					INVALID_ROLE_CARDINALITY_ORDER);
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
					BsplPackage.Literals.BSPL_PROTOCOL_PARAMETER__NAME,
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
						BsplPackage.Literals.BSPL_PROTOCOL_SPECIFICATION__PACKAGE,
						EMPTY_PACKAGE_DECLARATION);
			}
		}
	}

	/** Check the roles are declared and used.
	 *
	 * @param protocol the SARL BSPL protocol.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkRoles(BsplProtocol protocol) {
		final var allDeclaredRoles = new TreeSet<String>();
		final var declaredRoles = new TreeMap<String, BsplProtocolRole>();
		for (final var member : protocol.getMembers()) {
			if (member instanceof BsplProtocolRole role) {
				allDeclaredRoles.add(role.getName());
				declaredRoles.put(role.getName(), role);
			} else if (member instanceof BsplProtocolMessage message) {
				final var f = message.getFrom();
				final var t = message.getTo();
				if (f.equals(t)) {
					declaredRoles.remove(f);
					if (!allDeclaredRoles.contains(f)) {
						error(
								MessageFormat.format(Messages.SARL_BSPLValidator_5, f, protocol.getName()),
								message,
								BsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__FROM,
								UNDEFINED_PROTOCOL_ROLE);
						error(
								MessageFormat.format(Messages.SARL_BSPLValidator_6, t, protocol.getName()),
								message,
								BsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__TO,
								UNDEFINED_PROTOCOL_ROLE);
					}
				} else {
					declaredRoles.remove(f);
					if (!allDeclaredRoles.contains(f)) {
						error(
								MessageFormat.format(Messages.SARL_BSPLValidator_5, f, protocol.getName()),
								message,
								BsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__FROM,
								UNDEFINED_PROTOCOL_ROLE);
					}
					declaredRoles.remove(t);
					if (!allDeclaredRoles.contains(t)) {
						error(
								MessageFormat.format(Messages.SARL_BSPLValidator_6, t, protocol.getName()),
								message,
								BsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__TO,
								UNDEFINED_PROTOCOL_ROLE);
					}
				}
			}
		}
		
		if (!isIgnored(UNUSED_PROTOCOL_ROLE)) {
			for (final var declaration : declaredRoles.values()) {
				addIssue(
						MessageFormat.format(Messages.SARL_BSPLValidator_18, declaration.getName(), protocol.getName()),
						declaration,
						UNUSED_PROTOCOL_ROLE);
			}
		}
	}


	/** Check the protocol message has known parameters and arguments are not duplicated.
	 * This function also test if all the arguments have been specified, and notifies
	 * when a protocol parameter is not used.
	 *
	 * @param protocol the SARL BSPL protocol.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkParameters(BsplProtocol protocol) {
		final Multimap<String, BsplProtocolMessage> declaredMessages;
		final SortedSetMultimap<String, String> declaredArgumentsForMessage;
		if (isIgnored(MISSED_ARGUMENT_IN_MESSAGE)) {
			declaredMessages = null;
			declaredArgumentsForMessage = null;
		} else {
			declaredMessages = Multimaps.newListMultimap(new TreeMap<>(), () -> new ArrayList<>());
			declaredArgumentsForMessage = Multimaps.newSortedSetMultimap(new TreeMap<>(), () -> new TreeSet<>());
		}
		final var allDeclaredParameters = new TreeMap<String, BsplProtocolParameter>();
		final var declaredKeys = new TreeSet<String>();
		final var declaredParameters = new TreeMap<String, BsplProtocolParameter>();
		var hasProtocolKey = false;
		var hasProtocolOutParameter = false;
		var hasMessageOutParameter = false;
		final var notUsedOutParameters = new TreeMap<String, BsplProtocolParameter>();
		for (final var member : protocol.getMembers()) {
			if (member instanceof BsplProtocolParameter parameter) {
				if (parameter.isKey()) {
					declaredKeys.add(parameter.getName());
					if (!hasProtocolKey) {
						hasProtocolKey = true;
					}
				}
				if (parameter.isOutput()) {
					hasProtocolOutParameter = true;
					notUsedOutParameters.put(parameter.getName(), parameter);
					if (parameter.isPrivateVisibility()) {
						error(
								MessageFormat.format(Messages.SARL_BSPLValidator_25, parameter.getName(), protocol.getName()),
								parameter,
								BsplPackage.Literals.BSPL_PROTOCOL_PARAMETER__NAME,
								INVALID_PARAMETER_MODIFIER);
					}
				}
				if (allDeclaredParameters.putIfAbsent(parameter.getName(), parameter) != null) {
					error(
						MessageFormat.format(Messages.SARL_BSPLValidator_9, parameter.getName(), protocol.getName()),
						parameter,
						BsplPackage.Literals.BSPL_PROTOCOL_PARAMETER__NAME,
						DUPLICATE_PROTOCOL_PARAMETER);
				} else {
					declaredParameters.put(parameter.getName(), parameter);
				}
			} else if (member instanceof BsplProtocolMessage message) {
				if (declaredMessages != null) {
					declaredMessages.put(message.getMessage(), message);
				}
				final var declaredArguments = new TreeSet<String>();
				int i = 0;
				for (final var argument : message.getArguments()) {
					final var n = argument.getName();
					notUsedOutParameters.remove(n);
					if (declaredArgumentsForMessage != null) {
						declaredArgumentsForMessage.put(message.getMessage(), n);
					}
					declaredParameters.remove(n);
					if (!allDeclaredParameters.containsKey(n)) {
						error(
							MessageFormat.format(Messages.SARL_BSPLValidator_8, n, protocol.getName()),
							message,
							BsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__ARGUMENTS,
							i,
							UNDEFINED_PROTOCOL_PARAMETER);
					} else if (!declaredArguments.add(n)) {
						error(
							MessageFormat.format(Messages.SARL_BSPLValidator_11, n, message.getFrom(), message.getTo(), message.getMessage(), protocol.getName()),
							message,
							BsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__ARGUMENTS,
							i,
							DUPLICATE_PROTOCOL_ARGUMENT);
					}
					if (argument.isKey()) {
						final var declaredParameter = allDeclaredParameters.get(n);
						assert declaredParameter != null;
						if (!declaredParameter.isKey()) {
							error(
								MessageFormat.format(Messages.SARL_BSPLValidator_21, argument.getName(), protocol.getName()),
								message,
								BsplPackage.Literals.BSPL_PROTOCOL_MESSAGE__ARGUMENTS,
								i,
								INVALID_ARGUMENT_MODIFIER);
						}
					}
					if (!hasMessageOutParameter && argument.isOutput()) {
						hasMessageOutParameter = true;
					}
					++i;
				}
			}
		}

		if (!isIgnored(UNUSED_PROTOCOL_PARAMETER)) {
			for (final var parameter : declaredParameters.values()) {
				addIssue(
						MessageFormat.format(Messages.SARL_BSPLValidator_19, parameter.getName(), protocol.getName()),
						parameter,
						UNUSED_PROTOCOL_PARAMETER);
			}
		}

		if (declaredArgumentsForMessage != null && declaredMessages != null) {
			for (final var declaration : declaredMessages.entries()) {
				final var originalArguments = declaredArgumentsForMessage.get(declaration.getKey());
				if (!originalArguments.isEmpty()) {
					final var arguments = new TreeSet<>(originalArguments);
					for (final var argument : declaration.getValue().getArguments()) {
						arguments.remove(argument.getName());
					}
					if (!arguments.isEmpty()) {
						for (final var argument : arguments) {
							addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_17, argument, declaration.getKey()),
									declaration.getValue(),
									MISSED_ARGUMENT_IN_MESSAGE);
						}
					}
				}
			}
		}

		if (!isIgnored(MISSED_PROTOCOL_KEY) && !hasProtocolKey) {
			addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_20, protocol.getName()),
					protocol,
					BsplPackage.Literals.BSPL_PROTOCOL__NAME,
					MISSED_PROTOCOL_KEY);
		}

		if (!hasProtocolOutParameter) {
			error(
					MessageFormat.format(Messages.SARL_BSPLValidator_22, protocol.getName()),
					protocol,
					BsplPackage.Literals.BSPL_PROTOCOL__NAME,
					REQUIRED_OUT_PARAMETER);
		}

		if (!hasMessageOutParameter) {
			error(
					MessageFormat.format(Messages.SARL_BSPLValidator_23, protocol.getName()),
					protocol,
					BsplPackage.Literals.BSPL_PROTOCOL__NAME,
					REQUIRED_OUT_PARAMETER_IN_MESSAGES);
		}

		if (!notUsedOutParameters.isEmpty()) {
			for (final var parameter : notUsedOutParameters.values()) {
				error(
						MessageFormat.format(Messages.SARL_BSPLValidator_24, parameter.getName(), protocol.getName()),
						parameter,
						BsplPackage.Literals.BSPL_PROTOCOL_PARAMETER__NAME,
						REQUIRED_OUT_PARAMETER_IN_MESSAGES);
			}
		}
	}

	/** Check the modifiers of the protocol.
	 *
	 * @param protocol the SARL BSPL protocol.
	 */
	@Check(CheckType.FAST)
	public void checkProtocolModifiers(BsplProtocol protocol) {
		final var declaredModifier = new TreeSet<String>();
		var i = 0;
		for (final var modifier : protocol.getModifiers()) {
			if (!declaredModifier.add(modifier)) {
				if (!isIgnored(UNNECESSARY_PROTOCOL_MODIFIER)) {
					addIssue(MessageFormat.format(Messages.SARL_BSPLValidator_27, modifier, protocol.getName()),
							protocol,
							BsplPackage.Literals.BSPL_PROTOCOL__MODIFIERS,
							i,
							UNNECESSARY_PROTOCOL_MODIFIER);
				}
			} else if (i > 0 && declaredModifier.size() > 1) {
				error(
						MessageFormat.format(Messages.SARL_BSPLValidator_26, modifier, protocol.getName()),
						protocol,
						BsplPackage.Literals.BSPL_PROTOCOL__MODIFIERS,
						i,
						INVALID_PROTOCOL_MODIFIER);
			}
			++i;
		}
	}

}

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

import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_FUNCTION_NAME;
import static io.sarl.lang.validation.IssueCodes.GENERIC_TYPE_NAME_SHADOWING;
import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FIELD__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FUNCTION__NAME;
import static org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED;
import static org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING;

import java.text.MessageFormat;
import java.util.List;
import java.util.TreeMap;

import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XClosure;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.XbasePackage;

import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.SARLFeatureNameValidator;

/**
 * A specialized validator to deal with naming conventions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
public class SARLNamingValidator extends AbstractSARLSubValidator {

	/** Check if the given action has a valid name.
	 *
	 * @param action the action to test.
	 * @see SARLFeatureNameValidator
	 */
	@Check(CheckType.FAST)
	public void checkActionName(SarlAction action) {
		final var inferredOperation = getAssociations().getDirectlyInferredOperation(action);
		final var name = QualifiedName.create(inferredOperation.getQualifiedName('.').split("\\.")); //$NON-NLS-1$
		if (isReallyDisallowedName(name)) {
			final var validName = Utils.fixHiddenMember(action.getName());
			error(MessageFormat.format(Messages.SARLNamingValidator_1, action.getName()),
					action,
					XTEND_FUNCTION__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					INVALID_MEMBER_NAME,
					validName);
		} else if (!isIgnored(DISCOURAGED_FUNCTION_NAME)
				&& getFeatureNameValidator().isDiscouragedName(name)) {
			addIssue(MessageFormat.format(Messages.SARLNamingValidator_1, action.getName()),
					action,
					XTEND_FUNCTION__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					DISCOURAGED_FUNCTION_NAME);
		}
	}

	/** Check if the given field has a valid name.
	 *
	 * @param field the field to test.
	 * @see SARLFeatureNameValidator
	 */
	@Check(CheckType.FAST)
	public void checkFieldName(SarlField field) {
		final var inferredField = getAssociations().getJvmField(field);
		final QualifiedName name = Utils.getQualifiedName(inferredField);
		if (isReallyDisallowedName(name)) {
			final var validName = Utils.fixHiddenMember(field.getName());
			error(MessageFormat.format(Messages.SARLNamingValidator_2, field.getName(), Messages.SARLNamingValidator_3),
					field,
					XTEND_FIELD__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					VARIABLE_NAME_DISALLOWED,
					validName);
		} else if (getGrammarAccess().getOccurrenceKeyword().equals(field.getName())) {
			error(MessageFormat.format(Messages.SARLNamingValidator_4, getGrammarAccess().getOccurrenceKeyword(), Messages.SARLNamingValidator_3),
					field,
					XTEND_FIELD__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					VARIABLE_NAME_DISALLOWED);
		}
	}

	/** Check if the given generic type has a name that is shadowing an enclosing generic type.
	 *
	 * @param type the generic type parameter to check.
	 * @since 0.12
	 */
	@Check(CheckType.NORMAL)
	public void checkGenericTypeNameShadowing(JvmTypeParameter type) {
		final var declarator = EcoreUtil2.getContainerOfType(type.eContainer(), XtendMember.class);
		if (declarator instanceof XtendFunction && !SarlUtils.isHiddenMember(type.getName())) {
			final var enclosingType = declarator.getDeclaringType();
			List<JvmTypeParameter> params = null;
			if (enclosingType instanceof XtendClass cvalue) {
				params = cvalue.getTypeParameters();
			} else if (enclosingType instanceof XtendInterface cvalue) {
				params = cvalue.getTypeParameters();
			}
			if (params != null && !params.isEmpty()) {
				// Do not need to loop on the enclosing types since all the inner types must be declared as static
				for (final var declaredType : params) {
					if (Strings.equal(type.getSimpleName(), declaredType.getSimpleName())) {
						error(
								MessageFormat.format(Messages.SARLNamingValidator_5, type.getSimpleName(), enclosingType.getName()),
								type,
								TypesPackage.Literals.JVM_TYPE_PARAMETER__NAME,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								GENERIC_TYPE_NAME_SHADOWING);
						return;
					}
				}
			}
		}
	}


	/** Check if the given field has a name that is shadowing an inherited field.
	 *
	 * @param field the field to test.
	 */
	@Check(CheckType.NORMAL)
	public void checkFieldNameShadowing(SarlField field) {
		if (!isIgnored(VARIABLE_NAME_SHADOWING)
				&& !SarlUtils.isHiddenMember(field.getName())) {
			final var inferredField = getAssociations().getJvmField(field);
			final var inheritedFields = new TreeMap<String, JvmField>();
			Utils.populateInheritanceContext(
					inferredField.getDeclaringType(),
					null, null,
					inheritedFields,
					null, null,
					getSarlActionSignatures());

			final var inheritedField = inheritedFields.get(field.getName());
			if (inheritedField != null) {
				var nameIndex = 0;
				var newName = field.getName() + nameIndex;
				while (inheritedFields.containsKey(newName)) {
					++nameIndex;
					newName = field.getName() + nameIndex;
				}
				addIssue(MessageFormat.format(
						Messages.SARLNamingValidator_6,
						field.getName(),
						inferredField.getDeclaringType().getQualifiedName(),
						inheritedField.getQualifiedName()),
						field,
						XTEND_FIELD__NAME,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						VARIABLE_NAME_SHADOWING,
						newName);
			}
		}
	}

	/** Check if the given parameter has a valid name.
	 *
	 * @param parameter the parameter to test.
	 * @see SARLFeatureNameValidator
	 */
	@Check(CheckType.FAST)
	public void checkParameterName(SarlFormalParameter parameter) {
		final var inferredParam = getAssociations().getJvmParameter(parameter);
		final var name = Utils.getQualifiedName(inferredParam);
		if (isReallyDisallowedName(name)) {
			error(MessageFormat.format(
					Messages.SARLNamingValidator_2,
					parameter.getName(), Messages.SARLNamingValidator_7),
					parameter,
					XtendPackage.Literals.XTEND_PARAMETER__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					VARIABLE_NAME_DISALLOWED);
		} else if (getGrammarAccess().getOccurrenceKeyword().equals(parameter.getName())) {
			error(MessageFormat.format(
					Messages.SARLNamingValidator_4,
					getGrammarAccess().getOccurrenceKeyword(), Messages.SARLNamingValidator_7),
					parameter,
					XtendPackage.Literals.XTEND_PARAMETER__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					VARIABLE_NAME_DISALLOWED);
		}
	}

	/** Check if the closure parameters have a valid name.
	 *
	 * @param closure the closure to test.
	 * @see SARLFeatureNameValidator
	 */
	@Check(CheckType.FAST)
	public void checkParameterName(XClosure closure) {
		var index = 0;
		for (final var param : closure.getDeclaredFormalParameters()) {
			final var name = Utils.getQualifiedName(param);
			if (isReallyDisallowedName(name)) {
				error(MessageFormat.format(
						Messages.SARLNamingValidator_2,
						param.getName(), Messages.SARLNamingValidator_7),
						closure,
						XbasePackage.Literals.XCLOSURE__DECLARED_FORMAL_PARAMETERS,
						index,
						VARIABLE_NAME_DISALLOWED);
			} else if (getGrammarAccess().getOccurrenceKeyword().equals(param.getName())) {
				error(MessageFormat.format(
						Messages.SARLNamingValidator_4,
						getGrammarAccess().getOccurrenceKeyword(), Messages.SARLNamingValidator_7),
						closure,
						XbasePackage.Literals.XCLOSURE__DECLARED_FORMAL_PARAMETERS,
						index,
						VARIABLE_NAME_DISALLOWED);
			}
			++index;
		}
	}

	/** Check if the given local variable has a valid name.
	 *
	 * @param variable the variable to test.
	 * @see SARLFeatureNameValidator
	 */
	@Check(CheckType.FAST)
	public void checkVariableName(XVariableDeclaration variable) {
		final var name = QualifiedName.create(variable.getQualifiedName('.').split("\\.")); //$NON-NLS-1$
		if (isReallyDisallowedName(name)) {
			error(MessageFormat.format(
					Messages.SARLNamingValidator_2,
					variable.getName(), Messages.SARLNamingValidator_8),
					variable,
					XbasePackage.Literals.XVARIABLE_DECLARATION__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					VARIABLE_NAME_DISALLOWED);
		} else if (getGrammarAccess().getOccurrenceKeyword().equals(variable.getName())) {
			error(MessageFormat.format(
					Messages.SARLNamingValidator_4,
					getGrammarAccess().getOccurrenceKeyword(), Messages.SARLNamingValidator_8),
					variable,
					XbasePackage.Literals.XVARIABLE_DECLARATION__NAME,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					VARIABLE_NAME_DISALLOWED);
		}
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.bugfixes

import io.sarl.lang.validation.AbstractSARLValidator
import java.io.Serializable
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.common.types.JvmAnnotationTarget
import org.eclipse.xtext.common.types.JvmMember
import org.eclipse.xtext.common.types.JvmTypeReference
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.validation.CheckType
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.eclipse.xtext.xbase.XConstructorCall
import org.eclipse.xtext.xbase.XFeatureCall
import org.eclipse.xtext.xbase.XInstanceOfExpression
import org.eclipse.xtext.xbase.XMemberFeatureCall
import org.eclipse.xtext.xbase.XTypeLiteral
import org.eclipse.xtext.xbase.XbasePackage
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference
import org.eclipse.xtext.xtype.XImportDeclaration
import org.eclipse.xtext.xtype.XtypePackage

import static io.sarl.lang.util.ModelUtil.hasAnnotation;

/**
 * Implementation of a validator that is fixing several bugs in the Xtext API.
 * The code defined in this class should be sent to the Xtext project as patches.
 * <p>
 * <ul>
 * <li>Deprecated: {@link https://bugs.eclipse.org/bugs/show_bug.cgi?id=437689}</li>
 * </ul>
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#validation"
 */
class XtextBugFixValidator extends AbstractSARLValidator {

	protected def boolean isDeprecated(EObject t) {
			if (t instanceof JvmAnnotationTarget) {
				if (hasAnnotation(t, typeof(Deprecated))) {
					return true
				}
			}
			return false
	}
	
	protected def boolean isContainerDeprecated(EObject t) {
			var EObject visitor = t
			while (visitor instanceof JvmMember) {
				if (visitor.deprecated) {
					return true
				}
				visitor = visitor.declaringType
			}
			return false
	}
	
	/**
	 * @param type
	 */
	@Check(CheckType.NORMAL)
	public def checkDeprecated(JvmTypeReference type) {
		if (!IssueCodes::DEPRECATED_FEATURE.ignored
			&& type.type !== null) {
			if (type.type.deprecated) {
				addIssue(String::format("Deprecated type: %s. Please consider its replacement.",
							type.identifier),
							type,
							null,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
			else if (type.type.containerDeprecated) {
				addIssue(String::format("The type %s is defined inside a deprecated type. Please consider its replacement.",
							type.identifier),
							type,
							null,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
		}
	}
	
	/**
	 * @param decl
	 */
	@Check(CheckType.NORMAL)
	public def checkDeprecated(XImportDeclaration decl) {
		if (!IssueCodes::DEPRECATED_FEATURE.ignored
			&& decl.importedType !== null) {
			if (decl.importedType.deprecated) {
				addIssue(String::format("Deprecated type: %s. Please consider its replacement.",
							decl.importedType.identifier),
							decl,
							XtypePackage.Literals.XIMPORT_DECLARATION__IMPORTED_TYPE,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
			else if (decl.importedType.containerDeprecated) {
				addIssue(String::format("The type %s is defined inside a deprecated type. Please consider its replacement.",
							decl.importedType.identifier),
							decl,
							XtypePackage.Literals.XIMPORT_DECLARATION__IMPORTED_TYPE,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
		}
	}
	
	/**
	 * @param expression
	 */
	@Check(CheckType.NORMAL)
	public def checkDeprecated(XFeatureCall expression) {
		if (!IssueCodes::DEPRECATED_FEATURE.ignored) {
			if (expression.feature.deprecated) {
				addIssue(String::format("Deprecated element: %s. Please consider its replacement.",
							expression.feature.identifier),
							expression,
							null,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
			else if (expression.feature.containerDeprecated) {
				addIssue(String::format("The element %s is defined inside a deprecated type. Please consider its replacement.",
							expression.feature.identifier),
							expression,
							null,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
		}
	}

	/**
	 * @param expression
	 */
	@Check(CheckType.NORMAL)
	public def checkDeprecated(XMemberFeatureCall expression) {
		if (!IssueCodes::DEPRECATED_FEATURE.ignored) {
			if (expression.feature.deprecated) {
				addIssue(String::format("Deprecated element: %s. Please consider its replacement.",
							expression.feature.identifier),
							expression,
							XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
			else if (expression.feature.containerDeprecated) {
				addIssue(String::format("This element is defined inside a deprecated type. Please consider its replacement.",
							expression.feature.identifier),
							expression,
							XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
		}
	}

	/**
	 * @param expression
	 */
	@Check(CheckType.NORMAL)
	public def checkDeprecated(XConstructorCall expression) {
		if (!IssueCodes::DEPRECATED_FEATURE.ignored) {
			if (expression.constructor.deprecated) {
				addIssue("Deprecated constructor. Please consider its replacement.",
							expression,
							XbasePackage.Literals.XCONSTRUCTOR_CALL__CONSTRUCTOR,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
			else if (expression.constructor.containerDeprecated) {
				addIssue("This constructor is defined inside a deprecated type. Please consider its replacement.",
							expression,
							XbasePackage.Literals.XCONSTRUCTOR_CALL__CONSTRUCTOR,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
		}
	}

	/**
	 * @param expression
	 */
	@Check(CheckType.NORMAL)
	public def checkDeprecated(XTypeLiteral expression) {
		if (!IssueCodes::DEPRECATED_FEATURE.ignored) {
			if (expression.type.deprecated) {
				addIssue(String::format("Deprecated type: %s. Please consider its replacement.",
							expression.type.identifier),
							expression,
							XbasePackage.Literals.XTYPE_LITERAL__TYPE,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
			else if (expression.type.containerDeprecated) {
				addIssue(String::format("This type %s is defined inside a deprecated type. Please consider its replacement.",
							expression.type.identifier),
							expression,
							XbasePackage.Literals.XTYPE_LITERAL__TYPE,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							IssueCodes::DEPRECATED_FEATURE)
			}
		}
	}

	/**
	 * @param instanceOfExpression
	 */
	@Check(CheckType.NORMAL)
	public def checkInstanceOfOperator(XInstanceOfExpression instanceOfExpression) {
		var LightweightTypeReference leftType = getActualType(instanceOfExpression.expression)
		val LightweightTypeReference rightType = toLightweightTypeReference(instanceOfExpression.type, true)
		if (leftType === null || rightType === null || rightType.type === null || rightType.type.eIsProxy) {
			return
		}
		if (containsTypeArgs(rightType)) {
			error("Cannot perform instanceof check against parameterized type "
				+ getNameOfTypes(rightType), null, 
				ValidationMessageAcceptor::INSIGNIFICANT_INDEX,
				org.eclipse.xtext.xbase.validation.IssueCodes::INVALID_INSTANCEOF)
			return
		}
		if (leftType.any || leftType.unknown) {
			return // null / unknown is ok
		}
		if (rightType.primitive) {
			error("Cannot perform instanceof check against primitive type "
				+ this.getNameOfTypes(rightType), null, 
				ValidationMessageAcceptor::INSIGNIFICANT_INDEX,
				org.eclipse.xtext.xbase.validation.IssueCodes::INVALID_INSTANCEOF)
			return
		}
		if (leftType.primitive 
			|| rightType.array && !(leftType.array || leftType.isType(typeof(Object)) || leftType.isType(typeof(Cloneable)) || leftType.isType(typeof(Serializable)))
			|| isFinal(rightType) && !memberOfTypeHierarchy(rightType, leftType)
			|| isFinal(leftType) && !memberOfTypeHierarchy(leftType, rightType)) {
			error("Incompatible conditional operand types " + 
				getNameOfTypes(leftType)+" and "+getNameOfTypes(rightType), null, 
				ValidationMessageAcceptor::INSIGNIFICANT_INDEX,
				org.eclipse.xtext.xbase.validation.IssueCodes::INVALID_INSTANCEOF)
			return
		}
		if (!isIgnored(org.eclipse.xtext.xbase.validation.IssueCodes::OBSOLETE_INSTANCEOF)
			&& rightType.isAssignableFrom(leftType, new TypeConformanceComputationArgument(false, false, true, true, false, false))) {
			addIssueToState(
				org.eclipse.xtext.xbase.validation.IssueCodes::OBSOLETE_INSTANCEOF,
				"The expression of type " + getNameOfTypes(leftType)
					+ " is already of type " + canonicalName(rightType), null);
		}
	}

}

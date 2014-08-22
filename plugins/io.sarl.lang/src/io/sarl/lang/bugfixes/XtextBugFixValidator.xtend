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

import com.google.inject.Inject
import io.sarl.lang.validation.AbstractSARLValidator
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.EStructuralFeature
import org.eclipse.xtext.common.types.JvmMember
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.TypesPackage
import org.eclipse.xtext.validation.Check
import org.eclipse.xtext.validation.CheckType
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.eclipse.xtext.xbase.XAbstractFeatureCall
import org.eclipse.xtext.xbase.XConstructorCall
import org.eclipse.xtext.xbase.XTypeLiteral
import org.eclipse.xtext.xbase.XbasePackage
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider
import org.eclipse.xtext.xtype.XImportDeclaration
import org.eclipse.xtext.xtype.XtypePackage

import static io.sarl.lang.util.ModelUtil.hasAnnotation
import org.eclipse.xtext.xbase.XInstanceOfExpression
import java.io.Serializable
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument

/**
 * Implementation of a validator that is fixing several bugs in the Xtext API.
 * The code defined in this class should be sent to the Xtext project as patches.
 * <p>
 * <ul>
 * <li>Deprecated: {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=437689"}</li>
 * <li>InstanceOf: {@link "https://bugs.eclipse.org/bugs/show_bug.cgi?id=420959"}</li>
 * </ul>
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#validation"
 */
class XtextBugFixValidator extends AbstractSARLValidator {

	@Inject
	private ILogicalContainerProvider logicalContainerProvider;

	private static def boolean isDeprecatedMember(JvmMember t) {
			return (hasAnnotation(t, typeof(Deprecated)))
	}
	
	protected def void checkDeprecated(
			EObject object,
			String typeLabel,
			String objectLabel,
			EObject source,
			EStructuralFeature structuralFeature,
			boolean enableDeprecationInCode) {
		if (object instanceof JvmMember) {
			var String message = null;
			if (isDeprecatedMember(object)) {
				// The feature is marked deprecated.
				message = String::format("The %s %s is deprecated.", typeLabel, objectLabel)
			} else {
				// Search for a deprecated feature in the declaring type hierarchy.
				var JvmMember deprecatedContainer = null
				{
					var EObject container = this.logicalContainerProvider.getNearestLogicalContainer(object)
					if (container === null) {
						container = object.eContainer()
					}
					while (deprecatedContainer === null && container instanceof JvmMember) {
						var JvmMember enclosingType = container as JvmMember
						if (isDeprecatedMember(enclosingType)) {
							deprecatedContainer = enclosingType
						} else {
							container = enclosingType.eContainer()
						}
					}
				}
				if (deprecatedContainer !== null) {
					// The feature is deprecated by transition: one of its enclosing types
					// is deprecated.
					message = String::format(
							"The %s %s is deprecated because it is defined in a deprecated type.",
							typeLabel, objectLabel)
				}
			}
			if (message != null) {
				// Determine the correct issue code: "deprecated feature" or
				// "deprecated feature in deprecated code".
				var String code = null
				{
					var EObject container = this.logicalContainerProvider.getNearestLogicalContainer(source);
					if (container === null) {
						container = source.eContainer()
					}
					if (container instanceof JvmMember && isDeprecatedMember(container as JvmMember)) {
						if (enableDeprecationInCode) {
							code = IssueCodes::DEPRECATION_IN_DEPRECATED_CODE
						}
					} else {
						code = IssueCodes::DEPRECATED_FEATURE
					}
				}
				// Output the issue if necessary.
				if (code !== null) {
					addIssue(message, source, structuralFeature,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						code)
				}
			}
		}
	}

	@Check(CheckType::NORMAL)
	public def checkDeprecated(JvmParameterizedTypeReference type) {
		val severities = getIssueSeverities(context, currentObject)
		if (!severities.isIgnored(IssueCodes::DEPRECATED_FEATURE)) {
			val jvmType = type.type
			checkDeprecated(
					jvmType,
					"type",
					jvmType.identifier,
					type,
					TypesPackage.Literals::JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
					!severities.isIgnored(IssueCodes::DEPRECATION_IN_DEPRECATED_CODE))
		}
	}

	@Check(CheckType::NORMAL)
	public def checkDeprecated(XImportDeclaration decl) {
		val severities = getIssueSeverities(context, currentObject);
		if (!severities.isIgnored(IssueCodes::DEPRECATED_FEATURE)) {
			val jvmType = decl.importedType
			checkDeprecated(
					jvmType,
					"type",
					jvmType.identifier,
					decl,
					XtypePackage.Literals::XIMPORT_DECLARATION__IMPORTED_TYPE,
					!severities.isIgnored(IssueCodes::DEPRECATION_IN_DEPRECATED_CODE))
		}
	}

	@Check(CheckType::NORMAL)
	public def checkDeprecated(XAbstractFeatureCall expression) {
		val severities = getIssueSeverities(context, currentObject)
		if (!severities.isIgnored(IssueCodes::DEPRECATED_FEATURE)) {
			val feature = expression.feature
			checkDeprecated(
					feature,
					"feature",
					feature.identifier,
					expression,
					XbasePackage.Literals::XABSTRACT_FEATURE_CALL__FEATURE,
					!severities.isIgnored(IssueCodes::DEPRECATION_IN_DEPRECATED_CODE))
		}
	}

	@Check(CheckType::NORMAL)
	public def checkDeprecated(XConstructorCall expression) {
		val severities = getIssueSeverities(context, currentObject)
		if (!severities.isIgnored(IssueCodes::DEPRECATED_FEATURE)) {
			val constructor = expression.constructor
			checkDeprecated(
					constructor,
					"constructor",
					constructor.identifier,
					expression,
					XbasePackage.Literals::XCONSTRUCTOR_CALL__CONSTRUCTOR,
					!severities.isIgnored(IssueCodes::DEPRECATION_IN_DEPRECATED_CODE))
		}
	}

	@Check(CheckType::NORMAL)
	public def checkDeprecated(XTypeLiteral expression) {
		val severities = getIssueSeverities(context, currentObject)
		if (!severities.isIgnored(IssueCodes::DEPRECATED_FEATURE)) {
			val jvmType = expression.type
			checkDeprecated(
					jvmType,
					"type",
					jvmType.identifier,
					expression,
					XbasePackage.Literals::XTYPE_LITERAL__TYPE,
					!severities.isIgnored(IssueCodes::DEPRECATION_IN_DEPRECATED_CODE))
		}
	}

	@Check
	public override checkInstanceOf(XInstanceOfExpression instanceOfExpression) {
		var leftType = instanceOfExpression.expression.getActualType
		val rightType = instanceOfExpression.type.toLightweightTypeReference(true)
		if (leftType === null || rightType === null || rightType.type === null || rightType.type.eIsProxy) {
			return
		}
		if (rightType.containsTypeArgs) {
			error(	"Cannot perform instanceof check against parameterized type "
						+ rightType.getNameOfTypes,
					null,
					ValidationMessageAcceptor::INSIGNIFICANT_INDEX,
					org.eclipse.xtext.xbase.validation.IssueCodes::INVALID_INSTANCEOF)
			return
		}
		if (leftType.any || leftType.unknown) {
			return
		}
		if (rightType.primitive) {
			error(	"Cannot perform instanceof check against primitive type "
						+ rightType.getNameOfTypes,
					null,
					ValidationMessageAcceptor::INSIGNIFICANT_INDEX,
					org.eclipse.xtext.xbase.validation.IssueCodes::INVALID_INSTANCEOF)
			return
		}
		if (leftType.primitive 
			|| (rightType.array && !(leftType.array || leftType.isType(typeof(Object)) || leftType.isType(typeof(Cloneable)) || leftType.isType(typeof(Serializable))))
			|| (rightType.isFinal && !memberOfTypeHierarchy(rightType, leftType))
			|| (!memberOfTypeHierarchy(leftType, rightType))) {
			error(	"Incompatible conditional operand types "
						+ leftType.getNameOfTypes + " and "
						+ rightType.getNameOfTypes,
					null,
					ValidationMessageAcceptor::INSIGNIFICANT_INDEX,
					org.eclipse.xtext.xbase.validation.IssueCodes::INVALID_INSTANCEOF)
			return
		}
		if (!org.eclipse.xtext.xbase.validation.IssueCodes::OBSOLETE_INSTANCEOF.isIgnored
			&& rightType.isAssignableFrom(leftType, 
				new TypeConformanceComputationArgument(false, false, true, true, false, false))) {
			addIssueToState(
				org.eclipse.xtext.xbase.validation.IssueCodes::OBSOLETE_INSTANCEOF,
				"The expression of type " + leftType.getNameOfTypes
					+ " is already of type " + rightType.canonicalName,
				null)
		}
	}

}

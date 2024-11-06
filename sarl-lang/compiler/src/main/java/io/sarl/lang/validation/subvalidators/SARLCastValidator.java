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

import static io.sarl.lang.validation.IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION;
import static org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST;

import java.text.MessageFormat;

import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.sarl.SarlCastedExpression;
import io.sarl.lang.typesystem.FeatureCallAdapter;
import io.sarl.lang.validation.ISARLValidator;

/**
 * A specialized validator to deal with casting operations.
 *
 * <p>This validator is not a subtype of the Xtext validator API because it must
 * be directly invoked from the overridden functions in the main {@code SARLValidator}.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.14
 */
public class SARLCastValidator {

	/** Check the overridable cast operators.
	 *
	 * @param validator the validator tool.
	 * @param cast the operator.
	 * @return {@code true} if the cast was checked by this function; {@code false} if the function
	 *     did not checked the cast operator.
	 */
	public boolean checkCasts(ISARLValidator validator, XCastedExpression cast) {
		// Validation of the casting according to the SARL specification of this operator.
		if (cast instanceof SarlCastedExpression sarlCast) {
			return checkOverrideableCasts(validator, sarlCast);
		}
		// Standard check of the types.
		return false;
	}

	/** Check the type.
	 *
	 * @param validator the validator tool.
	 * @param concreteSyntax the concrete type from the SARL source code
	 * @param toType the real target type
	 * @param fromType the real from type (same as the concrete syntax type)
	 */
	public void checkCast(ISARLValidator validator, JvmTypeReference concreteSyntax, LightweightTypeReference toType, LightweightTypeReference fromType) {
		// The first test is for notifying that an unnecessary cast is found
		// The second test is for avoiding to have duplicate warning; because the super checkCast has
		// already reported a warning for the specific case when the toType and fromType are equal
		if (toType.isAssignableFrom(fromType)
				&& !toType.getIdentifier().equals(fromType.getIdentifier())
				&& !isAmbiguousCastContext(concreteSyntax, toType, fromType)) {
			// In some cases, the use of the "as" operator is mandatory to avoid ambiguity into the enclosing type
			reportCastWarnings(validator, concreteSyntax, toType, fromType);
		}
	}

	/** Test if the enclosing cast expression is used in an context that make the casting operation
	 * mandatory in order to avoid ambiguity.
	 *
	 * @param concreteSyntax the concrete type from the SARL source code
	 * @param toType the real target type
	 * @param fromType the real from type (same as the concrete syntax type)
	 * @return {@code true} if the context of a casting operation is used for
	 *      invoking a feature with some ambiguity.
	 */
	@SuppressWarnings("static-method")
	protected boolean isAmbiguousCastContext(JvmTypeReference concreteSyntax, LightweightTypeReference toType, LightweightTypeReference fromType) {
		final var cast = EcoreUtil2.getContainerOfType(concreteSyntax, SarlCastedExpression.class);
		if (cast != null && cast.eContainer() instanceof XAbstractFeatureCall call) {
			final var adapter = (FeatureCallAdapter) EcoreUtil.getAdapter(call.eAdapters(), FeatureCallAdapter.class);
			if (adapter != null) {
				return adapter.getCallCandidates().size() > 1;
			}
		}
		return false;
	}

	/** Check if the given cast expression has an associated overriding function.
	 *
	 * @param validator the validator tool.
	 * @param cast the SARL cast expression.
	 * @return {@code true} to avoid to do the "standard" cast test (because this function has already did it for example).
	 */
	protected boolean checkOverrideableCasts(ISARLValidator validator, SarlCastedExpression cast) {
		final var operation = cast.getFeature();
		if (operation != null) {
			// We have to test the unnecessary cast because because it is tested into the standard process
			final var concreteSyntax = cast.getType();
			if (concreteSyntax != null) {
				final var toType = validator.toLightweightTypeReference(cast.getType());
				reportCastWarnings(
						validator,
						concreteSyntax,
						toType,
						validator.getActualType(cast.getTarget()));

				if (!validator.isIgnored(POTENTIAL_INEFFICIENT_VALUE_CONVERSION)) {
					final var fromType = validator.toLightweightTypeReference(operation.getReturnType());
					final String message;
					if (Strings.equal(concreteSyntax.getIdentifier(), fromType.getIdentifier())) {
						message = MessageFormat.format(Messages.SARLCastValidator_1, operation.getSimpleName());
					} else {
						message = MessageFormat.format(Messages.SARLCastValidator_2, operation.getSimpleName(),
								toType.getHumanReadableName(), fromType.getHumanReadableName());
					}
					validator.addIssue(message, concreteSyntax, POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
				}
			}
			// Break to avoid the standard validation for casted expressions
			return true;
		}
		return false;
	}

	/** Report the warnings associated to the casted expressions.
	 *
	 * @param validator the validator tool.
	 * @param concreteSyntax the type specified into the casted expression.
	 * @param toType the type specified into the casted expression.
	 * @param fromType the type of the source expression.
	 */
	@SuppressWarnings("static-method")
	protected void reportCastWarnings(ISARLValidator validator, JvmTypeReference concreteSyntax, LightweightTypeReference toType, LightweightTypeReference fromType) {
		if (!validator.isIgnored(OBSOLETE_CAST) && toType.isAssignableFrom(fromType)) {
			validator.addIssue(MessageFormat.format(Messages.SARLCastValidator_3, fromType.getHumanReadableName(),
					toType.getHumanReadableName()), concreteSyntax, OBSOLETE_CAST);
		}
	}

}

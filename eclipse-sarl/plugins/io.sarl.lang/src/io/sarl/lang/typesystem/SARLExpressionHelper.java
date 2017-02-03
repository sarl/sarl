/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.typesystem;

import java.util.List;
import java.util.regex.Pattern;

import javax.inject.Inject;
import javax.inject.Singleton;

import org.eclipse.xtend.core.typing.XtendExpressionHelper;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XInstanceOfExpression;
import org.eclipse.xtext.xbase.XSynchronizedExpression;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.util.Utils;

/**
 * Helper on expressions.
 *
 * <p>This implementation extends the Xtend expression helper by assuming that any function
 * with a name starting with "get", "is", "has" is a pure function.
 * It also assumes that "equals", "hashCode", "clone" and "toString" are also pure functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#validation"
 */
@Singleton
public class SARLExpressionHelper extends XtendExpressionHelper {

	/** Regular expression patterns that matches the names of functions usually
	 * considered as pure.
	 */
	public static final String[] SPECIAL_PURE_FUNCTION_NAME_PATTERNS = {
		"is[A-Z].*", //$NON-NLS-1$
		"get(?:[A-Z1-9].*)?", //$NON-NLS-1$
		"has[A-Z1-9].*", //$NON-NLS-1$
		"to[A-Z1-9].*", //$NON-NLS-1$
		"contains(?:[A-Z1-9].*)?", //$NON-NLS-1$
		"equals", //$NON-NLS-1$
		"hashCode", //$NON-NLS-1$
		"clone", //$NON-NLS-1$
		"size", //$NON-NLS-1$
		"iterator", //$NON-NLS-1$
	};

	private Pattern pattern;

	@Inject
	private CommonTypeComputationServices services;

	/** Construct the helper.
	 *
	 * @param additionalSpecialPureFunctionNamePatterns the patterns for the functions that are considered as pure functions.
	 * @see #SPECIAL_PURE_FUNCTION_NAME_PATTERNS
	 */
	public SARLExpressionHelper(String... additionalSpecialPureFunctionNamePatterns) {
		this.pattern = buildPattern(additionalSpecialPureFunctionNamePatterns);
	}

	/** Construct the helper.
	 *
	 * @see #SPECIAL_PURE_FUNCTION_NAME_PATTERNS
	 */
	public SARLExpressionHelper() {
		this.pattern = buildPattern();
	}

	private static Pattern buildPattern(String... additionalPatterns) {
		final StringBuilder fullPattern = new StringBuilder();
		fullPattern.append("^"); //$NON-NLS-1$
		boolean hasPattern = false;
		for (final String pattern : SPECIAL_PURE_FUNCTION_NAME_PATTERNS) {
			if (hasPattern) {
				fullPattern.append("|"); //$NON-NLS-1$
			} else {
				hasPattern = true;
			}
			fullPattern.append("(?:"); //$NON-NLS-1$
			fullPattern.append(pattern);
			fullPattern.append(")"); //$NON-NLS-1$
		}
		if (additionalPatterns != null && additionalPatterns.length > 0) {
			for (final String pattern : additionalPatterns) {
				if (hasPattern) {
					fullPattern.append("|"); //$NON-NLS-1$
				} else {
					hasPattern = true;
				}
				fullPattern.append("(?:"); //$NON-NLS-1$
				fullPattern.append(pattern);
				fullPattern.append(")"); //$NON-NLS-1$
			}
		}
		fullPattern.append("$"); //$NON-NLS-1$
		return Pattern.compile(fullPattern.toString());
	}

	@Override
	public boolean hasSideEffects(XAbstractFeatureCall featureCall, boolean inspectContents) {
		if (super.hasSideEffects(featureCall, inspectContents)) {
			final JvmIdentifiableElement feature = featureCall.getFeature();
			// Several operations are not marked with @Pure but they have a clear semantic without border effects,
			// e.g. the "is", "get" functions.
			if (feature != null && !feature.eIsProxy() && feature instanceof JvmOperation) {
				final JvmOperation op = (JvmOperation) feature;
				final String name = op.getSimpleName();
				if (name != null && this.pattern.matcher(name).find() && hasPrimitiveParameters(op)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	private boolean hasPrimitiveParameters(JvmOperation op) {
		for (final JvmFormalParameter parameter : op.getParameters()) {
			final JvmTypeReference type = parameter.getParameterType();
			if (type == null || !Utils.toLightweightTypeReference(type, this.services).isPrimitive()) {
				return false;
			}
		}
		return true;
	}

	/** Replies if the given expression has a side effect in the context of a guard.
	 *
	 * <p>This function differs from {@link #hasSideEffects(XExpression)} because it explore the
	 * syntax tree for determining if one action has a side effect.
	 *
	 * @param expr the expression to test.
	 * @return <code>true</code> if a side effect was detected.
	 */
	public boolean hasDeepSideEffects(XExpression expr) {
		XExpression rawExpr = expr;
		boolean changed;
		do {
			changed = false;
			if (rawExpr instanceof XInstanceOfExpression) {
				return false;
			}
			if (rawExpr instanceof XSynchronizedExpression) {
				rawExpr = ((XSynchronizedExpression) rawExpr).getExpression();
				changed = true;
			} else if (rawExpr instanceof XCastedExpression) {
				final XCastedExpression castedExpression = (XCastedExpression) rawExpr;
				rawExpr = castedExpression.getTarget();
				changed = true;
			} else if (rawExpr instanceof XBlockExpression) {
				final List<XExpression> list = ((XBlockExpression) rawExpr).getExpressions();
				if (list != null && !list.isEmpty()) {
					for (final XExpression subExpr : list) {
						if (hasDeepSideEffects(subExpr)) {
							return true;
						}
					}
				}
				return false;
			}
		} while (changed);
		return hasSideEffects(rawExpr);
	}

	/** Check if the given operation could be annoted with "@Pure".
	 *
	 * @param operation - the operation to test.
	 * @param body - the body of the operation.
	 * @return <code>true</code> if one of the components of the given expression has a side effect;
	 *     otherwise <code>false</code>.
	 * @see Pure
	 */
	public boolean isPurableOperation(JvmOperation operation, XExpression body) {
		if (operation == null || operation.isAbstract() || body == null) {
			return false;
		}
		final String name = operation.getSimpleName();
		return (name != null && this.pattern.matcher(name).find()) || !hasSideEffects(body);
	}

}

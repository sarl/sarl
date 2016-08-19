/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import com.google.inject.Singleton;
import org.eclipse.xtend.core.typing.XtendExpressionHelper;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XInstanceOfExpression;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XReturnExpression;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XSynchronizedExpression;
import org.eclipse.xtext.xbase.XTypeLiteral;
import org.eclipse.xtext.xbase.lib.Inline;
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

	/** Regular expression pattern that matches the names of functions usually
	 * considered as pure.
	 */
	public static final String SPECIAL_PURE_FUNCTION_NAME_PATTERN =
			"^(((is)|(get)|(has))[A-Z].*)|(equals)|(hashCode)|(clone)|(toString)$"; //$NON-NLS-1$;

	private final Pattern pattern;

	@Inject
	private CommonTypeComputationServices services;

	/** Construct the helper.
	 */
	public SARLExpressionHelper() {
		this.pattern = Pattern.compile(SPECIAL_PURE_FUNCTION_NAME_PATTERN);
	}

	@Override
	public boolean hasSideEffects(XAbstractFeatureCall featureCall, boolean inspectContents) {
		if (super.hasSideEffects(featureCall, inspectContents)) {
			final JvmIdentifiableElement feature = featureCall.getFeature();
			if ((feature != null) && (!feature.eIsProxy()) && (feature instanceof JvmOperation)) {
				final JvmOperation op = (JvmOperation) feature;
				final String name = op.getSimpleName();
				if (name != null && this.pattern.matcher(name).find()
						&& hasPrimitiveParameters(op)) {
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

	/** Replies if the given expression could be inline.
	 *
	 * @param expr - the body of the operation.
	 * @return <code>true</code> if one of the components of the given expression could be inline;
	 *     otherwise <code>false</code>.
	 * @see Inline
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	public boolean isInlinableOperation(XExpression expr) {
		if (expr != null) {
			XExpression content = expr;
			while (content instanceof XBlockExpression) {
				final XBlockExpression blockExpr = (XBlockExpression) content;
				if (blockExpr.getExpressions().size() == 1) {
					content = blockExpr.getExpressions().get(0);
				} else {
					content = null;
				}
			}
			if (content instanceof XBooleanLiteral
					|| content instanceof XNullLiteral
					|| content instanceof XNumberLiteral
					|| content instanceof XStringLiteral
					|| content instanceof XTypeLiteral) {
				return true;
			}
			if (content instanceof XReturnExpression) {
				return isInlinableOperation(((XReturnExpression) content).getExpression());
			}
			if (content instanceof XCastedExpression) {
				return isInlinableOperation(((XCastedExpression) content).getTarget());
			}
			if (content instanceof XInstanceOfExpression) {
				return isInlinableOperation(((XInstanceOfExpression) content).getExpression());
			}
		}
		return false;
	}

	/** Replies if the given body could be inline.
	 *
	 * @param operation - the operation to test.
	 * @param body - the body of the operation.
	 * @return <code>true</code> if one of the components of the given expression could be inline;
	 *     otherwise <code>false</code>.
	 * @see Inline
	 */
	public boolean isInlinableOperation(JvmOperation operation, XExpression body) {
		if (operation != null && !operation.isAbstract()) {
			return isInlinableOperation(body);
		}
		return false;
	}

}

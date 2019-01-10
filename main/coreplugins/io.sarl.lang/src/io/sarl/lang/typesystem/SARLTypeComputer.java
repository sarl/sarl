/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import javax.inject.Inject;

import com.google.common.base.Throwables;
import org.eclipse.xtend.core.typesystem.XtendTypeComputer;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.validation.EObjectDiagnosticImpl;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.typesystem.computation.ILinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.computation.ITypeComputationState;
import org.eclipse.xtext.xbase.typesystem.internal.AmbiguousFeatureLinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.validation.IssueCodes;

/** Customized type computer for SARL specific expressions.
 *
 * <p>It resolves the ambiguous calls with an approach that is supporting {@link Deprecated}
 * This type computer prefers the feature candidate that is not marked with {@link Deprecated}.
 * Otherwise, its behavior is the same as the standard Xbase type computer implementation:
 * the first candidate is preferred, and a specific issue message is output.
 *
 * <p>This type computer resolves the types for the SARL keywords: break, continue and assert.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public class SARLTypeComputer extends XtendTypeComputer {

	@Inject
	private AnnotationLookup annotationLookup;

	@Override
	protected ILinkingCandidate getBestCandidate(List<? extends ILinkingCandidate> candidates) {
		// Implementation of ignorable features:
		// For example, this function is ignore the deprecated features when a not-deprecated feature is available.
		if (candidates.size() == 1) {
			return candidates.get(0);
		}
		ILinkingCandidate preferredCandidateWithoutConstraint = null;
		ILinkingCandidate preferredCandidateWithConstraint = null;
		for (final ILinkingCandidate candidate : candidates) {
			if (preferredCandidateWithoutConstraint == null) {
				preferredCandidateWithoutConstraint = candidate;
			} else {
				preferredCandidateWithoutConstraint = preferredCandidateWithoutConstraint.getPreferredCandidate(candidate);
			}
			if (preferredCandidateWithConstraint == null) {
				if (!isIgnorableCallToFeature(candidate)) {
					preferredCandidateWithConstraint = candidate;
				}
			} else {
				final ILinkingCandidate preferredCandidate = preferredCandidateWithConstraint.getPreferredCandidate(candidate);
				if (!(preferredCandidate instanceof AmbiguousFeatureLinkingCandidate)
						|| !isIgnorableCallToFeature(candidate)) {
					preferredCandidateWithConstraint = preferredCandidate;
				}
			}
		}
		if (preferredCandidateWithConstraint != null) {
			return preferredCandidateWithConstraint;
		}
		return preferredCandidateWithoutConstraint;
	}

	/** Replies if ambiguity could be removed for the given feature.
	 *
	 * @param candidate the candidate.
	 * @return {@code true} if ambiguity could be removed.
	 */
	@SuppressWarnings({"checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity"})
	protected boolean isIgnorableCallToFeature(ILinkingCandidate candidate) {
		final JvmIdentifiableElement feature = candidate.getFeature();
		//
		// @Deprecated
		//
		if (feature instanceof JvmOperation) {
			JvmAnnotationTarget target = (JvmOperation) feature;
			JvmAnnotationReference reference = this.annotationLookup.findAnnotation(target, Deprecated.class);
			if (reference == null) {
				do {
					target = EcoreUtil2.getContainerOfType(target.eContainer(), JvmAnnotationTarget.class);
					if (target != null) {
						reference = this.annotationLookup.findAnnotation(target, Deprecated.class);
					}
				} while (reference == null && target != null);
			}
			if (reference != null) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void computeTypes(XExpression expression, ITypeComputationState state) {
		if (expression instanceof SarlBreakExpression) {
			_computeTypes((SarlBreakExpression) expression, state);
		} else if (expression instanceof SarlContinueExpression) {
			_computeTypes((SarlContinueExpression) expression, state);
		} else if (expression instanceof SarlAssertExpression) {
			_computeTypes((SarlAssertExpression) expression, state);
		} else {
			try {
				super.computeTypes(expression, state);
			} catch (Throwable exception) {
				final Throwable cause = Throwables.getRootCause(exception);
				state.addDiagnostic(new EObjectDiagnosticImpl(
						Severity.ERROR,
						IssueCodes.INTERNAL_ERROR,
						cause.getLocalizedMessage(),
						expression,
						null,
						-1,
						null));
			}
		}
	}

	/** Compute the type of a break expression.
	 *
	 * @param object the expression.
	 * @param state the state of the type resolver.
	 */
	protected void _computeTypes(SarlBreakExpression object, ITypeComputationState state) {
		final LightweightTypeReference primitiveVoid = getPrimitiveVoid(state);
		state.acceptActualType(primitiveVoid);
	}

	/** Compute the type of a break expression.
	 *
	 * @param object the expression.
	 * @param state the state of the type resolver.
	 * @since 0.7
	 */
	protected void _computeTypes(SarlContinueExpression object, ITypeComputationState state) {
		final LightweightTypeReference primitiveVoid = getPrimitiveVoid(state);
		state.acceptActualType(primitiveVoid);
	}

	/** Compute the type of an assert expression.
	 *
	 * @param object the expression.
	 * @param state the state of the type resolver.
	 */
	protected void _computeTypes(SarlAssertExpression object, ITypeComputationState state) {
		state.withExpectation(getTypeForName(Boolean.class, state)).computeTypes(object.getCondition());
	}

}

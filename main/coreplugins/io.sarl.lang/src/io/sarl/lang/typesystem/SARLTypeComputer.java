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

import javax.inject.Inject;

import org.eclipse.xtend.core.typesystem.XtendTypeComputer;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.typesystem.computation.ILinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.computation.ITypeComputationState;
import org.eclipse.xtext.xbase.typesystem.conformance.ConformanceFlags;
import org.eclipse.xtext.xbase.typesystem.internal.AmbiguousFeatureLinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.sarl.SarlBreakExpression;

/** Customized type computer for SARL specific expressions.
 *
 * <p>It resolves the ambiguous calls that is supporting {@link Deprecated}.
 *
 * <p>This candidate prefers the feature that is not marked with {@link Deprecated}.
 * Otherwise, its behavior is the same as the standard Xbase candidate implementation:
 * the first candidate is preferred, and a specific issue message is output.
 *
 * <p>This type computer resolves the types for the SARL keywords: break.
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
				if (!isIgnorableCallToFeature(candidate.getFeature())) {
					preferredCandidateWithConstraint = candidate;
				}
			} else {
				final ILinkingCandidate preferredCandidate = preferredCandidateWithConstraint.getPreferredCandidate(candidate);
				if (!(preferredCandidate instanceof AmbiguousFeatureLinkingCandidate)
						|| !isIgnorableCallToFeature(candidate.getFeature())) {
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
	 * @param feature the feature.
	 * @return {@code true} if ambiguity could be removed.
	 */
	protected boolean isIgnorableCallToFeature(JvmIdentifiableElement feature) {
		if (feature instanceof JvmOperation) {
			final JvmAnnotationReference reference = this.annotationLookup.findAnnotation(
					(JvmOperation) feature, Deprecated.class);
			return reference != null;
		}
		return false;
	}

	@Override
	public void computeTypes(XExpression expression, ITypeComputationState state) {
		if (expression instanceof SarlBreakExpression) {
			_computeTypes((SarlBreakExpression) expression, state);
		} else {
			super.computeTypes(expression, state);
		}
	}

	/** Computethe type of a break expression.
	 *
	 * @param object the expression.
	 * @param state the state of the type resolver.
	 */
	protected void _computeTypes(SarlBreakExpression object, ITypeComputationState state) {
		final LightweightTypeReference primitiveVoid = getPrimitiveVoid(state);
		state.acceptActualType(primitiveVoid, ConformanceFlags.EXPLICIT_VOID_RETURN);
	}

}

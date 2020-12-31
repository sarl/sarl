/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.lang.typesystem.cast;

import static io.sarl.lang.util.Utils.setStructuralFeature;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.List;

import com.google.common.base.Objects;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.scoping.batch.IIdentifiableElementDescription;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.computation.ILinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.computation.ITypeExpectation;
import org.eclipse.xtext.xbase.typesystem.internal.AbstractPendingLinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.internal.ExpressionTypeComputationState;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.sarl.SarlPackage;

/** Linking candidate for cast operator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class CastOperatorLinkingCandidate extends AbstractPendingLinkingCandidate<XCastedExpression> implements ICastOperatorLinkingCandidate {

	private XExpression receiver;

	private XExpression argument;

	private boolean hasLinkingOperation;

	/** Constructor.
	 *
	 * @param expression the expression.
	 * @param description the description of the linked element.
	 * @param expectation the type expectation.
	 * @param state the state of the type computation.
	 */
	public CastOperatorLinkingCandidate(XCastedExpression expression,
			IIdentifiableElementDescription description, ITypeExpectation expectation,
			ExpressionTypeComputationState state) {
		super(expression, description, expectation, state);
	}

	@Override
	protected ExpressionTypeComputationState getState() {
		// Caution: enable access to this function from within this package.
		return super.getState();
	}

	@Override
	public ILinkingCandidate getPreferredCandidate(ILinkingCandidate other) {
		final CastOperatorLinkingCandidate right = (CastOperatorLinkingCandidate) other;
		final CandidateCompareResult candidateCompareResult = compareTo(right);
		switch (candidateCompareResult) {
		case AMBIGUOUS:
			return createAmbiguousLinkingCandidate(right);
		case SUSPICIOUS_OTHER:
			return createSuspiciousLinkingCandidate(right);
		case EQUALLY_INVALID:
		case THIS:
			return this;
		case OTHER:
			return other;
		default:
			throw new IllegalStateException();
		}
	}

	private static boolean isSame(LightweightTypeReference first, LightweightTypeReference second) {
		return Objects.equal(first.getIdentifier(), second.getIdentifier());
	}

	@SuppressWarnings("checkstyle:magicnumber")
	private static int computeCompliance(LightweightTypeReference target, LightweightTypeReference current) {
		if (isSame(target, current)) {
			return 0;
		}
		final LightweightTypeReference target2 = target.getWrapperTypeIfPrimitive();
		if (target2.isSubtypeOf(Number.class)) {
			final LightweightTypeReference current2 = current.getWrapperTypeIfPrimitive();
			if (current2.isSubtypeOf(Number.class)) {
				return 1 + Math.max(getNumberPrecision(target) - getNumberPrecision(current), 0);
			}
			if (target.isAssignableFrom(current)) {
				return 8;
			}
			return 9;
		}
		if (target.isAssignableFrom(current)) {
			return 1;
		}
		return 2;
	}

	@SuppressWarnings("checkstyle:magicnumber")
	private static int getNumberPrecision(LightweightTypeReference type) {
		switch (type.getPrimitiveIfWrapperType().getSimpleName()) {
		case "byte": //$NON-NLS-1$
			return 0;
		case "short": //$NON-NLS-1$
			return 1;
		case "int": //$NON-NLS-1$
		case "AtomicInteger": //$NON-NLS-1$
			return 2;
		case "long": //$NON-NLS-1$
		case "AtomicLong": //$NON-NLS-1$
			return 3;
		case "float": //$NON-NLS-1$
			return 4;
		case "double": //$NON-NLS-1$
		case "AtomicDouble": //$NON-NLS-1$
			return 5;
		default:
			return 6;
		}
	}

	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity", "checkstyle:returncount"})
	private CandidateCompareResult compareTo(CastOperatorLinkingCandidate right) {
		// FIXME: Override super#compareTo() when super's CandidateCompareResult is visible.
		boolean invalid = false;

		CandidateCompareResult result = compareWithObjectType(right);
		switch (result) {
		case SUSPICIOUS_OTHER:
			throw new IllegalStateException();
		case EQUALLY_INVALID:
			invalid = true;
			break;
		case OTHER:
		case THIS:
			return result;
		case AMBIGUOUS:
		default:
		}

		final LightweightTypeReference sourceType = getActualType(getExpression().getTarget());
		final LightweightTypeReference leftSourceType = getOperationParameterType(this);
		final LightweightTypeReference rightSourceType = getOperationParameterType(right);

		final LightweightTypeReference targetType = getState().getReferenceOwner().toLightweightTypeReference(getExpression().getType());
		final LightweightTypeReference leftTargetType = getOperationReturnType(this);
		final LightweightTypeReference rightTargetType = getOperationReturnType(right);

		/*if ("boolean".equals(sourceType.getSimpleName()) && "String".equals(targetType.getSimpleName())) {
			System.out.println(sourceType.getHumanReadableName() + " -> " + targetType.getHumanReadableName());
			System.out.println("\t" + this.getValidationDescription() + " / (" + leftSourceType.getHumanReadableName()
				+ ") -> " + leftTargetType.getHumanReadableName());
			System.out.println("\t" + right.getValidationDescription() + " / (" + rightSourceType.getHumanReadableName()
				+ ") -> " + rightTargetType.getHumanReadableName());
		}*/

		final int leftTargetCompliance = computeCompliance(targetType, leftTargetType);
		final int rightTargetCompliance = computeCompliance(targetType, rightTargetType);

		if (leftTargetCompliance == 0) {
			if (rightTargetCompliance != 0) {
				return CandidateCompareResult.THIS;
			}
		} else if (rightTargetCompliance == 0) {
			return CandidateCompareResult.OTHER;
		}

		final int leftSourceCompliance = computeCompliance(sourceType, leftSourceType);
		final int leftCompliance = leftSourceCompliance + leftTargetCompliance;

		final int rightSourceCompliance = computeCompliance(sourceType, rightSourceType);
		final int rightCompliance = rightSourceCompliance + rightTargetCompliance;

		if (leftCompliance < rightCompliance) {
			return CandidateCompareResult.THIS;
		}
		if (leftCompliance > rightCompliance) {
			return CandidateCompareResult.OTHER;
		}

		result = compareByArityOverride(getArityMismatch(), right.getArityMismatch());
		switch (result) {
		case SUSPICIOUS_OTHER:
			throw new IllegalStateException();
		case EQUALLY_INVALID:
			invalid = true;
			break;
		case OTHER:
		case THIS:
			return result;
		case AMBIGUOUS:
		default:
		}

		return invalid ? CandidateCompareResult.EQUALLY_INVALID : CandidateCompareResult.AMBIGUOUS;
	}

	private CandidateCompareResult compareWithObjectType(CastOperatorLinkingCandidate right) {
		// This code prefer any candidate that is not provided by the Object type than
		// the candidate from Object.
		JvmOperation operation = right.getOperation();
		final boolean otherIsObject = operation != null && Objects.equal(operation.getDeclaringType().getIdentifier(), Object.class.getName());
		operation = getOperation();
		final boolean meIsObject = operation != null && Objects.equal(operation.getDeclaringType().getIdentifier(), Object.class.getName());
		if (otherIsObject != meIsObject) {
			if (otherIsObject) {
				return CandidateCompareResult.THIS;
			}
			return CandidateCompareResult.OTHER;
		}
		return CandidateCompareResult.AMBIGUOUS;
	}

	private static CandidateCompareResult compareByArityOverride(int leftArityMismatch, int rightArityMismatch) {
		// FIXME: Remove this function when the super type CandidateCompareResult is visible
		if (leftArityMismatch != rightArityMismatch) {
			if (leftArityMismatch == 0) {
				return CandidateCompareResult.THIS;
			}
			if (rightArityMismatch == 0) {
				return CandidateCompareResult.OTHER;
			}
			if (Math.abs(leftArityMismatch) < Math.abs(rightArityMismatch)) {
				return CandidateCompareResult.THIS;
			}
			if (Math.abs(leftArityMismatch) > Math.abs(rightArityMismatch)) {
				return CandidateCompareResult.OTHER;
			}
			if (leftArityMismatch < 0) {
				return CandidateCompareResult.THIS;
			}
			if (rightArityMismatch < 0) {
				return CandidateCompareResult.OTHER;
			}
		}
		return leftArityMismatch == 0 ? CandidateCompareResult.AMBIGUOUS : CandidateCompareResult.EQUALLY_INVALID;
	}

	/**
	 * The result of the comparison of two linking candidates.
	 * FIXME: Remove when the CandidateCompareResult into the super type is visible.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	private enum CandidateCompareResult {
		/**
		 * Indicates that the current candidate is a better match than the other one.
		 */
		THIS,
		/**
		 * Indicates that the current candidate is worse than the other one.
		 */
		OTHER,
		/**
		 * Indicates that the current candidate is worse than the other one due
		 * to less specific parameter types, but the other one is on a different
		 * implicit receiver than this one.
		 *
		 * <p>This result may only be returned when the expected argument types are compared.
		 */
		SUSPICIOUS_OTHER,
		/**
		 * Indicates that both candidates are equally valid. The situation may be ambiguous.
		 */
		AMBIGUOUS,
		/**
		 * Indicates that both candidates are equally invalid. Pick the first one.
		 */
		EQUALLY_INVALID
	}

	private LightweightTypeReference getOperationReturnType(CastOperatorLinkingCandidate candidate) {
		return getState().getReferenceOwner().toLightweightTypeReference(candidate.getOperation().getReturnType());
	}

	private LightweightTypeReference getOperationParameterType(CastOperatorLinkingCandidate candidate) {
		final JvmOperation operation = candidate.getOperation();
		if (operation.getParameters().isEmpty()) {
			return getState().getReferenceOwner().toLightweightTypeReference(operation.getDeclaringType());
		}
		return getState().getReferenceOwner().toLightweightTypeReference(operation.getParameters().get(0).getParameterType());
	}

	@Override
	public boolean isExtension() {
		return this.description.isExtension();
	}

	@Override
	public boolean isTypeLiteral() {
		return this.description.isTypeLiteral();
	}

	@Override
	protected boolean hasReceiver() {
		return !this.description.isStatic();
	}

	@Override
	public XExpression getReceiver() {
		ensureLinkingOperation();
		return this.receiver;
	}

	@Override
	public XExpression getArgument() {
		ensureLinkingOperation();
		return this.argument;
	}

	@Override
	protected String getFeatureTypeName() {
		return "cast"; //$NON-NLS-1$
	}

	@Override
	protected List<XExpression> getArguments() {
		// Do not reply the value of getArgument() because it cause type errors.
		return Collections.emptyList();
	}

	@Override
	protected List<JvmTypeReference> getPlainSyntacticTypeArguments() {
		return Collections.emptyList();
	}

	@Override
	protected ILinkingCandidate createAmbiguousLinkingCandidate(AbstractPendingLinkingCandidate<?> second) {
		return new AmbiguousCastOperatorLinkingCandidate(this, second);
	}

	@Override
	protected ILinkingCandidate createSuspiciousLinkingCandidate(AbstractPendingLinkingCandidate<?> chosenCandidate) {
		return new SuspiciousOverloadedCastOperatorLinkingCandidate((CastOperatorLinkingCandidate) chosenCandidate, this);
	}

	@Override
	public void applyToModel(IResolvedTypes resolvedTypes) {
		final XCastedExpression expr = getExpression();
		if (expr.eClass().isSuperTypeOf(SarlPackage.eINSTANCE.getSarlCastedExpression())) {
			// Feature
			setStructuralFeature(expr, SarlPackage.Literals.SARL_CASTED_EXPRESSION__FEATURE, getFeature());
			// Receiver
			setStructuralFeature(expr, SarlPackage.Literals.SARL_CASTED_EXPRESSION__RECEIVER, getReceiver());
			// Argument
			setStructuralFeature(expr, SarlPackage.Literals.SARL_CASTED_EXPRESSION__ARGUMENT, getArgument());
		}
	}

	@Override
	public JvmOperation getOperation() {
		return (JvmOperation) getFeature();
	}

	/** Replies the string representation of the candidate.
	 *
	 * @return the description of the candidate.
	 */
	public String getValidationDescription() {
		final JvmOperation feature = getOperation();
		String message = null;
		if (!getDeclaredTypeParameters().isEmpty()) {
			message = MessageFormat.format(Messages.CastOperatorLinkingCandidate_0,
					getFeatureTypeParametersAsString(true),
					feature.getSimpleName(),
					getFeatureParameterTypesAsString(),
					feature.getDeclaringType().getSimpleName());
		} else {
			message = MessageFormat.format(Messages.CastOperatorLinkingCandidate_1,
					feature.getSimpleName(),
					getFeatureParameterTypesAsString(),
					feature.getDeclaringType().getSimpleName());
		}
		return message;
	}

	/** Ensure that the values that should be replied by {@link #getReceiver()} and
	 * {@link #getArgument()} are correctly computed.
	 */
	protected void ensureLinkingOperation() {
		if (!this.hasLinkingOperation) {
			this.hasLinkingOperation = true;
			final XExpression target = getExpression().getTarget();
			if (!hasReceiver()) {
				// Static call
				this.receiver = null;
				this.argument = target;
			} else if (getOperation().getParameters().isEmpty()) {
				// Call on the receiver instance
				this.receiver = target;
				this.argument = null;
			} else {
				// Call to an object that is not the target, with the target as arugment
				this.argument = target;

				final XExpression obj1 = this.description.getSyntacticReceiver();
				final XExpression obj2 = this.description.getImplicitFirstArgument();
				final XExpression obj3 = this.description.getImplicitReceiver();

				if (obj1 != null && obj1 != target) {
					this.receiver = obj1;
				} else if (obj2 != null && obj2 != target) {
					this.receiver = obj2;
				} else if (obj3 != target) {
					this.receiver = obj3;
				} else {
					this.receiver = null;
				}
			}
		}
	}

}

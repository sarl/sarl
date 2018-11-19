/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

import java.util.Collections;
import java.util.EnumSet;
import java.util.List;

import javax.inject.Inject;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import org.eclipse.xtend.core.typesystem.XtendTypeComputer;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.EObjectDiagnosticImpl;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;
import org.eclipse.xtext.xbase.scoping.batch.IFeatureNames;
import org.eclipse.xtext.xbase.scoping.batch.IIdentifiableElementDescription;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.computation.IApplicableCandidate;
import org.eclipse.xtext.xbase.typesystem.computation.ILinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.computation.ITypeComputationResult;
import org.eclipse.xtext.xbase.typesystem.computation.ITypeComputationState;
import org.eclipse.xtext.xbase.typesystem.computation.ITypeExpectation;
import org.eclipse.xtext.xbase.typesystem.conformance.ConformanceHint;
import org.eclipse.xtext.xbase.typesystem.internal.AbstractPendingLinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.internal.AbstractTypeComputationState;
import org.eclipse.xtext.xbase.typesystem.internal.AmbiguousFeatureLinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.internal.ExpressionAwareStackedResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.internal.ExpressionTypeComputationState;
import org.eclipse.xtext.xbase.typesystem.internal.ForwardingResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.internal.ForwardingTypeComputationState;
import org.eclipse.xtext.xbase.typesystem.internal.ScopeProviderAccess;
import org.eclipse.xtext.xbase.typesystem.internal.StackedResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlCastedExpression;
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
		// For example, this function is ignoring the deprecated features when a not-deprecated feature is available.
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

	/** Compute the type of an assert expression.
	 *
	 * @param object the expression.
	 * @param state the state of the type resolver.
	 */
	@Override
	protected void _computeTypes(XCastedExpression object, ITypeComputationState state) {
		// Set the linked feature
		try {
			if (object instanceof SarlCastedExpression && state instanceof AbstractTypeComputationState) {
				final CastedExpressionTypeComputationState astate = new CastedExpressionTypeComputationState((AbstractTypeComputationState) state);
				final SarlCastedExpression cast = (SarlCastedExpression) object;
				final List<? extends ILinkingCandidate> candidates = astate.getLinkingCandidates(cast);
				if (!candidates.isEmpty()) {
					final ILinkingCandidate best = getBestCandidate(candidates);
					if (best != null) {
						best.applyToComputationState();
						return;
					}
				}
			}
		} catch (Throwable exception) {
			exception.printStackTrace();
		}
		// Standard type computation
		super._computeTypes(object, state);
	}

	/** State for type computation associated to the cast operator.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	private static class CastedExpressionTypeComputationState extends ForwardingTypeComputationState {

		private final ReflectExtensions ref = new ReflectExtensions();

		/** Constructor.
		 * @param delegate the original state.
		 */
		CastedExpressionTypeComputationState(AbstractTypeComputationState delegate) {
			super(delegate);
		}

		@SuppressWarnings("unchecked")
		private <T> T invoke(Object receiver, String functionName, Object... args) {
			try {
				return (T) this.ref.invoke(receiver, functionName, args);
			} catch (Throwable exception) {
				throw new Error(exception);
			}
		}

		@Override
		protected AbstractTypeComputationState getDelegate() {
			return (AbstractTypeComputationState) super.getDelegate();
		}

		/** Compute the best candidate for the feature behind the cast operator.
		 *
		 * @param cast the cast operator.
		 * @return the candidates.
		 */
		public List<? extends ILinkingCandidate> getLinkingCandidates(SarlCastedExpression cast) {
			final AbstractTypeComputationState astate = getDelegate();
			final StackedResolvedTypes demandComputedTypes = invoke(astate.getResolvedTypes(), "pushTypes"); //$NON-NLS-1$
			final AbstractTypeComputationState forked = invoke(astate, "withNonVoidExpectation", demandComputedTypes); //$NON-NLS-1$
			final ForwardingResolvedTypes demandResolvedTypes = new ForwardingResolvedTypes() {
				@Override
				protected IResolvedTypes delegate() {
					return forked.getResolvedTypes();
				}

				@Override
				public LightweightTypeReference getActualType(XExpression expression) {
					final LightweightTypeReference type = super.getActualType(expression);
					if (type == null) {
						final ITypeComputationResult result = forked.computeTypes(expression);
						return result.getActualExpressionType();
					}
					return type;
				}
			};
			final IScope scope = astate.getFeatureScopeSession().getScope(cast.getTarget(),
					XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE, demandResolvedTypes);
			final List<ILinkingCandidate> resultList = Lists.newArrayList();
			final JvmTypeReference type = cast.getType();
			final String featureName = "to" + Strings.toFirstUpper(type.getSimpleName()); //$NON-NLS-1$
			final Iterable<IEObjectDescription> descriptions = scope.getElements(QualifiedName.create(featureName));
			for (final IEObjectDescription description: descriptions) {
				final IIdentifiableElementDescription idesc = invoke(astate, "toIdentifiableDescription", description);  //$NON-NLS-1$
				final ILinkingCandidate candidate = createCandidate(cast, astate, type, idesc);
				if (candidate != null) {
					resultList.add(candidate);
				}
			}
			return resultList;
		}

		protected ILinkingCandidate createCandidate(SarlCastedExpression cast,
				AbstractTypeComputationState astate, JvmTypeReference type,
				IIdentifiableElementDescription description) {
			final ExpressionAwareStackedResolvedTypes resolvedTypes = invoke(astate.getResolvedTypes(), "pushTypes", cast); //$NON-NLS-1$
			final ExpressionTypeComputationState state = invoke(astate, "createExpressionComputationState", cast, resolvedTypes); //$NON-NLS-1$
			if (!(description instanceof ScopeProviderAccess.ErrorDescription) && (description.getNumberOfParameters() == 0)) {
				final JvmIdentifiableElement element = description.getElementOrProxy();
				final LightweightTypeReference returnTypeReference = astate.getResolvedTypes().getActualType(element);
				if (returnTypeReference != null && returnTypeReference.isSubtypeOf(type.getType())) {
					return new CastOperatorLinkingCandidate(cast, description,
							invoke(getDelegate(), "getSingleExpectation", state), //$NON-NLS-1$
							state);
				}
			}
			return null;
		}

		@Override
		public void acceptCandidate(XExpression expression, IApplicableCandidate candidate) {
			getDelegate().acceptCandidate(expression, candidate);
		}

		@Override
		public void acceptActualType(LightweightTypeReference type, ConformanceHint... hints) {
			getDelegate().acceptActualType(type, hints);
		}

		@Override
		public void acceptActualType(LightweightTypeReference type, EnumSet<ConformanceHint> hints) {
			getDelegate().acceptActualType(type, hints);
		}

		@Override
		protected ForwardingTypeComputationState newForwardingTypeComputationState(ITypeComputationState delegate) {
			if (delegate instanceof AbstractTypeComputationState) {
				return new CastedExpressionTypeComputationState((AbstractTypeComputationState) delegate);
			}
			throw new IllegalArgumentException();
		}

	}

	/** Linking candidate for cast operator.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	private static class CastOperatorLinkingCandidate extends AbstractPendingLinkingCandidate<XCastedExpression> implements IFeatureNames {

		/** Constructor.
		 *
		 * @param expression the expression.
		 * @param description the description of the linked element.
		 * @param expectation the type expectation.
		 * @param state the state of the type computation.
		 */
		CastOperatorLinkingCandidate(XCastedExpression expression,
				IIdentifiableElementDescription description, ITypeExpectation expectation,
				ExpressionTypeComputationState state) {
			super(expression, description, expectation, state);
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
		protected String getFeatureTypeName() {
			return "cast"; //$NON-NLS-1$
		}

		@Override
		protected List<XExpression> getArguments() {
			return Collections.emptyList();
		}

		@Override
		protected List<JvmTypeReference> getPlainSyntacticTypeArguments() {
			return Collections.emptyList();
		}

		@Override
		protected ILinkingCandidate createAmbiguousLinkingCandidate(AbstractPendingLinkingCandidate<?> second) {
			return this;
		}

		@Override
		public void applyToModel(IResolvedTypes resolvedTypes) {
			resolveLinkingProxy(XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE, XbasePackage.XABSTRACT_FEATURE_CALL__FEATURE);
			final XCastedExpression expr = getExpression();
			if (expr instanceof SarlCastedExpression) {
				((SarlCastedExpression) expr).setFeature(getFeature());
			}
		}

	}

}

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

import java.util.List;

import com.google.common.collect.Lists;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.scoping.batch.AbstractFeatureScopeSession;
import org.eclipse.xtext.xbase.scoping.batch.IIdentifiableElementDescription;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.computation.ILinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.computation.ITypeComputationResult;
import org.eclipse.xtext.xbase.typesystem.internal.AbstractTypeComputationState;
import org.eclipse.xtext.xbase.typesystem.internal.ExpressionAwareStackedResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.internal.ExpressionTypeComputationState;
import org.eclipse.xtext.xbase.typesystem.internal.ForwardingResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.internal.ResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.internal.StackedResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.sarl.SarlCastedExpression;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.typesystem.cast.ICastOperationCandidateSelector.ISelector;
import io.sarl.lang.util.ReflectMethod;

/** State for type computation associated to the cast operator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class CastedExpressionTypeComputationState extends ExpressionTypeComputationState {

	private static ReflectMethod<ResolvedTypes, StackedResolvedTypes> pushTypesMethod
			= ReflectMethod.of(ResolvedTypes.class, StackedResolvedTypes.class, "pushTypes"); //$NON-NLS-1$

	private final CastScopeSession castScopeSession;

	private final ICastOperationCandidateSelector candidateValidator;

	/** Constructor.
	 *
	 * @param expression the cast expression to consider.
	 * @param delegate the original state.
	 * @param candidateValidator the validator for the candidate functions to be cast operation.
	 */
	public CastedExpressionTypeComputationState(SarlCastedExpression expression,
			AbstractTypeComputationState delegate, ICastOperationCandidateSelector candidateValidator) {
		super(
				(StackedResolvedTypes) delegate.getResolvedTypes(),
				delegate.getFeatureScopeSession(),
				delegate,
				expression);
		this.candidateValidator = candidateValidator;
		this.castScopeSession = new CastScopeSession((AbstractFeatureScopeSession) delegate.getFeatureScopeSession());
	}

	private StackedResolvedTypes pushTypes() {
		return pushTypesMethod.invoke(getStackedResolvedTypes());
	}

	/** Replies if the linking to the cast operator functions is enabled.
	 *
	 * @param cast the cast operator.
	 * @return {@code true} if the linking is enabled.
	 */
	public boolean isCastOperatorLinkingEnabled(SarlCastedExpression cast) {
		final LightweightTypeReference sourceType = getStackedResolvedTypes().getReturnType(cast.getTarget());
		final LightweightTypeReference destinationType = getReferenceOwner().toLightweightTypeReference(cast.getType());
		if (sourceType.isPrimitiveVoid() || destinationType.isPrimitiveVoid()) {
			return false;
		}
		if (sourceType.isPrimitive() && destinationType.isPrimitive()) {
			return false;
		}
		return !sourceType.isSubtypeOf(destinationType.getType());
	}

	/** Compute the best candidates for the feature behind the cast operator.
	 *
	 * @param cast the cast operator.
	 * @return the candidates.
	 */
	public List<? extends ILinkingCandidate> getLinkingCandidates(SarlCastedExpression cast) {
		// Prepare the type resolver.
		final StackedResolvedTypes demandComputedTypes = pushTypes();
		final AbstractTypeComputationState forked = withNonVoidExpectation(demandComputedTypes);
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

		// Create the scope
		final IScope scope = getCastScopeSession().getScope(cast,
				// Must be the feature of the AbstractFeatureCall in order to enable the scoping for a function call.
				//XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE,
				SarlPackage.Literals.SARL_CASTED_EXPRESSION__FEATURE,
				demandResolvedTypes);

		// Search for the features into the scope
		final LightweightTypeReference targetType = getReferenceOwner().toLightweightTypeReference(cast.getType());
		final List<ILinkingCandidate> resultList = Lists.newArrayList();
		final LightweightTypeReference expressionType = getStackedResolvedTypes().getActualType(cast.getTarget());
		final ISelector validator = this.candidateValidator.prepare(
				getParent(), targetType, expressionType);
		// FIXME: The call to getAllElements() is not efficient; find another way in order to be faster.
		for (final IEObjectDescription description : scope.getAllElements()) {
			final IIdentifiableElementDescription idesc = toIdentifiableDescription(description);
			if (validator.isCastOperatorCandidate(idesc)) {
				final ExpressionAwareStackedResolvedTypes descriptionResolvedTypes = pushTypes(cast);
				final ExpressionTypeComputationState descriptionState = createExpressionComputationState(cast, descriptionResolvedTypes);
				final ILinkingCandidate candidate = createCandidate(cast, descriptionState, idesc);
				if (candidate != null) {
					resultList.add(candidate);
				}
			}
		}

		return resultList;
	}

	/** Replies the scope to be used for the cast operator.
	 *
	 * @return the scope.
	 */
	public CastScopeSession getCastScopeSession() {
		return this.castScopeSession;
	}

	/** Create a candidate from the given description.
	 *
	 * @param cast the cast operator.
	 * @param state the state
	 * @param description the description of the cast linked operation.
	 * @return the linking candidate.
	 */
	protected ILinkingCandidate createCandidate(SarlCastedExpression cast,
			ExpressionTypeComputationState state,
			IIdentifiableElementDescription description) {
		return new CastOperatorLinkingCandidate(cast, description,
				getSingleExpectation(state),
				state);
	}

	/** Reset the properties of the given feature in order to have casted expression that is not linked
	 * to an operation.
	 *
	 * @param object the expression to reset.
	 */
	@SuppressWarnings("static-method")
	public void resetFeature(SarlCastedExpression object) {
		setStructuralFeature(object, SarlPackage.Literals.SARL_CASTED_EXPRESSION__FEATURE, null);
		setStructuralFeature(object, SarlPackage.Literals.SARL_CASTED_EXPRESSION__RECEIVER, null);
		setStructuralFeature(object, SarlPackage.Literals.SARL_CASTED_EXPRESSION__ARGUMENT, null);
	}

}

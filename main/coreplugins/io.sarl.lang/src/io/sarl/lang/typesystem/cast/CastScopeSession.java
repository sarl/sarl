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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.scoping.batch.AbstractFeatureScopeSession;
import org.eclipse.xtext.xbase.scoping.batch.AbstractNestedFeatureScopeSession;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;

import io.sarl.lang.sarl.SarlCastedExpression;
import io.sarl.lang.sarl.SarlPackage;

/** Scope session dedicated to the cast operator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class CastScopeSession extends AbstractNestedFeatureScopeSession {

	/** Constructor.
	 *
	 * @param parent the parent session.
	 */
	public CastScopeSession(AbstractFeatureScopeSession parent) {
		super(parent);
	}

	/** Replies if the given reference is related to the cast operator scope.
	 *
	 * @param reference the reference to test.
	 * @return {@code true} if the reference is for a cast operator.
	 */
	@SuppressWarnings("static-method")
	public boolean isCastOperatorScope(EReference reference) {
		return reference == SarlPackage.Literals.SARL_CASTED_EXPRESSION__FEATURE;
	}

	@Override
	public IScope getScope(EObject context, EReference reference, IResolvedTypes types) {
		if (isCastOperatorScope(reference)) {
			return createCastOperatorScope(context, reference, types);
		}
		return super.getScope(context, reference, types);
	}

	/** create a scope for cast operator.
	 *
	 * @param context the context.
	 * @param reference the reference to the internal feature.
	 * @param resolvedTypes the resolved types.
	 * @return the scope.
	 */
	protected IScope createCastOperatorScope(EObject context, EReference reference, IResolvedTypes resolvedTypes) {
		if (!(context instanceof SarlCastedExpression)) {
			return IScope.NULLSCOPE;
		}
		final SarlCastedExpression call = (SarlCastedExpression) context;
		final XExpression receiver = call.getTarget();
		if (receiver == null) {
			return IScope.NULLSCOPE;
		}
		return getFeatureScopes().createFeatureCallScopeForReceiver(call, receiver, getParent(), resolvedTypes);
	}

}

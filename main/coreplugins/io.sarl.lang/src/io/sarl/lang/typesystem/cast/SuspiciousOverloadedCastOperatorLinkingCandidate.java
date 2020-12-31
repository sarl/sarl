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

import java.text.MessageFormat;
import java.util.List;

import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.diagnostics.AbstractDiagnostic;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.validation.EObjectDiagnosticImpl;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.computation.ILinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.computation.ISuspiciouslyOverloadedCandidate;
import org.eclipse.xtext.xbase.typesystem.internal.ResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.validation.IssueCodes;

/** Ambiguous linking candidate for cast operator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class SuspiciousOverloadedCastOperatorLinkingCandidate implements ISuspiciouslyOverloadedCandidate, ICastOperatorLinkingCandidate {

	private final CastOperatorLinkingCandidate rejectedCandidate;

	private final CastOperatorLinkingCandidate chosenCandidate;

	private final ReflectExtensions reflect = new ReflectExtensions();

	/** Constructor.
	 *
	 * @param chosenCandidate the candidate that is chosen.
	 * @param rejectedCandidate the candidate that is chosen.
	 */
	protected SuspiciousOverloadedCastOperatorLinkingCandidate(CastOperatorLinkingCandidate chosenCandidate,
			CastOperatorLinkingCandidate rejectedCandidate) {
		this.chosenCandidate = chosenCandidate;
		this.rejectedCandidate = rejectedCandidate;
	}

	@Override
	public CastOperatorLinkingCandidate getChosenCandidate() {
		return this.chosenCandidate;
	}

	@Override
	public CastOperatorLinkingCandidate getRejectedCandidate() {
		return this.rejectedCandidate;
	}

	@Override
	public void applyToComputationState() {
		final ResolvedTypes types = getChosenCandidate().getState().getResolvedTypes();
		try {
			this.reflect.invoke(types, "reassignLinkingInformation", this.chosenCandidate.getExpression(), this); //$NON-NLS-1$
		} catch (Throwable exception) {
			throw new Error(exception);
		}
		getChosenCandidate().applyToComputationState();
	}

	@Override
	public void applyToModel(IResolvedTypes resolvedTypes) {
		getChosenCandidate().applyToModel(resolvedTypes);
	}

	@Override
	public ILinkingCandidate getPreferredCandidate(ILinkingCandidate other) {
		return getChosenCandidate().getPreferredCandidate(other);
	}

	@Override
	public JvmIdentifiableElement getFeature() {
		return getChosenCandidate().getFeature();
	}

	@Override
	public XExpression getExpression() {
		return getChosenCandidate().getExpression();
	}

	@Override
	public List<LightweightTypeReference> getTypeArguments() {
		return getChosenCandidate().getTypeArguments();
	}

	@Override
	public boolean validate(IAcceptor<? super AbstractDiagnostic> result) {
		final CastOperatorLinkingCandidate ccandidate = getChosenCandidate();
		if (ccandidate.validate(result)) {
			final String message = MessageFormat.format(
					Messages.SuspiciousOverloadedCastOperatorLinkingCandidate_0,
					ccandidate.getValidationDescription(), getRejectedCandidate().toString());
			final AbstractDiagnostic diagnostic = new EObjectDiagnosticImpl(
					ccandidate.getState().getSeverity(IssueCodes.SUSPICIOUSLY_OVERLOADED_FEATURE),
					IssueCodes.SUSPICIOUSLY_OVERLOADED_FEATURE, message, getExpression(),
					XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE, -1, null);
			result.accept(diagnostic);
		}
		return false;
	}

	@Override
	public JvmOperation getOperation() {
		return getChosenCandidate().getOperation();
	}

	@Override
	public XExpression getReceiver() {
		return getChosenCandidate().getReceiver();
	}

	@Override
	public XExpression getArgument() {
		return getChosenCandidate().getArgument();
	}

}

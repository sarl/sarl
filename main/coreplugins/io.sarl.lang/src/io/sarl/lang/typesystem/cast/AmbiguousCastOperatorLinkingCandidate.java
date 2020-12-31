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

import java.util.Set;

import com.google.common.collect.Sets;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.typesystem.computation.ILinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.internal.AbstractAmbiguousLinkingCandidate;
import org.eclipse.xtext.xbase.typesystem.internal.AbstractPendingLinkingCandidate;

/** Ambiguous linking candidate for cast operator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class AmbiguousCastOperatorLinkingCandidate extends AbstractAmbiguousLinkingCandidate<CastOperatorLinkingCandidate>
		implements ICastOperatorLinkingCandidate {

	/** Constructor.
	 *
	 * @param first the first candidate.
	 * @param second the second candidate.
	 */
	protected AmbiguousCastOperatorLinkingCandidate(CastOperatorLinkingCandidate first, AbstractPendingLinkingCandidate<?> second) {
		super(first, second);
	}

	@Override
	protected String getSyntaxDescriptions() {
		return "cast"; //$NON-NLS-1$
	}

	@Override
	protected String getFeatureTypeName() {
		if (getPrimaryCandidate().isExtension()) {
			return "extension method"; //$NON-NLS-1$
		}
		return super.getFeatureTypeName();
	}

	@Override
	public XExpression getReceiver() {
		return getPrimaryCandidate().getReceiver();
	}

	@Override
	public XExpression getArgument() {
		return getPrimaryCandidate().getArgument();
	}

	@Override
	public JvmOperation getOperation() {
		return getPrimaryCandidate().getOperation();
	}

	@Override
	protected EStructuralFeature getFeatureToMark() {
		return XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE;
	}

	@Override
	protected String[] getDiagnosticData() {
		final Set<String> data = Sets.newLinkedHashSet();
		for (final ILinkingCandidate candidate : getAlternatives()) {
			final JvmIdentifiableElement feature = candidate.getFeature();
			final String simpleName = feature.getSimpleName();
			data.add(simpleName + "()"); //$NON-NLS-1$
		}
		return data.toArray(new String[data.size()]);
	}

}

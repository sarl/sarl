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

package io.sarl.lang.ui.contentassist.general;

import com.google.inject.Provider;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.ui.editor.contentassist.ConfigurableCompletionProposal;

/** Completion proposal that provides helpfull accessors.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SARLCompletionProposal extends ConfigurableCompletionProposal {

	private Object additionalInfo;

	private Resource contextResource;

	/**
	 * Creates a new completion proposal based on the provided information. The replacement string is
	 * considered being the display string too. All remaining fields are set to <code>null</code>.
	 *
	 * @param replacementString the actual string to be inserted into the document
	 * @param replacementOffset the offset of the text to be replaced
	 * @param replacementLength the length of the text to be replaced
	 * @param cursorPosition the position of the cursor following the insert relative to replacementOffset
	 */
	public SARLCompletionProposal(String replacementString, int replacementOffset, int replacementLength, int cursorPosition) {
		super(replacementString, replacementOffset, replacementLength, cursorPosition);
	}

	/**
	 * Creates a new completion proposal. All fields are initialized based on the provided information.
	 *
	 * @param replacementString the actual string to be inserted into the document
	 * @param replacementOffset the offset of the text to be replaced
	 * @param replacementLength the length of the text to be replaced
	 * @param cursorPosition the position of the cursor following the insert relative to replacementOffset
	 * @param image the image to display for this proposal
	 * @param displayString the string to be displayed for the proposal
	 * @param contextInformation the context information associated with this proposal
	 * @param additionalProposalInfo the additional information associated with this proposal
	 */
	public SARLCompletionProposal(String replacementString, int replacementOffset, int replacementLength,
			int cursorPosition, Image image, StyledString displayString, IContextInformation contextInformation,
			String additionalProposalInfo) {
		super(replacementString, replacementOffset, replacementLength, cursorPosition, image, displayString,
				contextInformation, additionalProposalInfo);
	}

	@Override
	public void setAdditionalProposalInfo(Object additionalProposalInfo) {
		super.setAdditionalProposalInfo(additionalProposalInfo);
		this.additionalInfo = additionalProposalInfo;
	}

	@Override
	public void setProposalContextResource(Resource contextResource) {
		super.setProposalContextResource(contextResource);
		this.contextResource = contextResource;
	}

	/** Replies the feature in the proposal.
	 *
	 * @return the feature.
	 */
	public EObject getReferencedFeature() {
		EObject eObject = null;
		if (this.additionalInfo instanceof EObject) {
			eObject = (EObject) this.additionalInfo;
		} else {
			if (this.additionalInfo instanceof Provider) {
				final Object o = ((Provider<?>) this.additionalInfo).get();
				if (o instanceof EObject) {
					eObject = (EObject) o;
				}
			}
		}
		if (eObject != null) {
			if (eObject.eIsProxy()) {
				eObject = EcoreUtil.resolve(eObject, this.contextResource);
			}
			return eObject;
		}
		return null;
	}

}

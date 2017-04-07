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

import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.xtext.xbase.ui.contentassist.XbaseContentProposalPriorities;

/** Update the priorities of the content proposals.
 *
 * <p>This specific implementation compute the priorities in that order:<ul>
 * <li>SARL elements in the same resource,</li>
 * <li>SARL elements outside the current resource but in the project,</li>
 * <li>SARL elements ouside the project</li>
 * <li>Other elements according to the super implementation</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SARLContentProposalProrities extends XbaseContentProposalPriorities {

	/** Priority for the contextual keywords, such as "it".
	 */
	public static final int CONTEXTUAL_KEYWORD_PRIORITY = 1000;

	/** Priority for the cross references from local SARL elements.
	 */
	public static final int SARL_LOCAL_CROSSREFERENCE_PRIORITY = 900;

	/** Priority for the cross references from inherited SARL elements.
	 */
	public static final int SARL_INHERITED_CROSSREFERENCE_PRIORITY = 800;

	/** Priority for the cross references from SARL elements in the same resource.
	 */
	public static final int SARL_RESOURCE_CROSSREFERENCE_PRIORITY = 700;

	/** Priority for the cross references from SARL elements outside the same resource.
	 */
	public static final int SARL_CROSSREFERENCE_PRIORITY = 600;

	/** Priority for the cross references (from Java).
	 */
	public static final int CROSSREFERENCE_PRIORITY = 500;

	/** Priority for keywords.
	 */
	public static final int DEFAULT_PRIORITY = 400;

	/** Priority for keywords.
	 */
	public static final int KEYWORD_PRIORITY = 300;

	/** Priority for contextual keywords.
	 */
	protected int contextualKeywordPriority = CONTEXTUAL_KEYWORD_PRIORITY;

	/** Priority for SARL elements in the current type.
	 */
	protected int localSarlElementPriority = SARL_LOCAL_CROSSREFERENCE_PRIORITY;

	/** Priority for SARL elements in the inherited type.
	 */
	protected int inheritedSarlElementPriority = SARL_INHERITED_CROSSREFERENCE_PRIORITY;

	/** Priority for SARL elements in the current resource.
	 */
	protected int resourceSarlElementPriority = SARL_RESOURCE_CROSSREFERENCE_PRIORITY;

	/** Priority for SARL elements ouside the current resource.
	 */
	protected int sarlElementPriority = SARL_CROSSREFERENCE_PRIORITY;

	/**
	 * Constructor.
	 */
	public SARLContentProposalProrities() {
		// Override the standard priorities.
		this.crossReferencePriority = CROSSREFERENCE_PRIORITY;
		this.keywordPriority = KEYWORD_PRIORITY;
		this.defaultPriority = DEFAULT_PRIORITY;
	}

	@Override
	public void adjustCrossReferencePriority(ICompletionProposal proposal, String prefix) {
		//		if (proposal instanceof SARLCompletionProposal) {
		//			final SARLCompletionProposal configurableProposal = (SARLCompletionProposal) proposal;
		//			final EObject eobject = configurableProposal.getReferencedFeature();
		//			if (eobject instanceof JvmIdentifiableElement) {
		//				//final JvmIdentifiableElement idElement = (JvmIdentifiableElement) eobject;
		//			}
		//		}
		super.adjustCrossReferencePriority(proposal, prefix);
	}

}

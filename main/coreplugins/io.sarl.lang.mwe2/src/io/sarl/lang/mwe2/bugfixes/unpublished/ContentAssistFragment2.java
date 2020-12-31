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

package io.sarl.lang.mwe2.bugfixes.unpublished;

import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.xtext.generator.model.TypeReference;

/**
 * Fixing the invalid package name for the Xtend proposal provider.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ContentAssistFragment2 extends org.eclipse.xtext.xtext.generator.ui.contentAssist.ContentAssistFragment2 {

	private static final String XTEND_GRAMMAR_NAME = "Xtend"; //$NON-NLS-1$

	private static final String XTEND_PROPOSAL_PROVIDER =
			"org.eclipse.xtend.ide.contentassist.XtendProposalProvider"; //$NON-NLS-1$

	@Override
	protected TypeReference getProposalProviderClass(Grammar grammar) {
		final String simpleName = GrammarUtil.getSimpleName(grammar);
		if (XTEND_GRAMMAR_NAME.equals(simpleName)) {
			return new TypeReference(XTEND_PROPOSAL_PROVIDER);
		}
		return super.getProposalProviderClass(grammar);
	}

}

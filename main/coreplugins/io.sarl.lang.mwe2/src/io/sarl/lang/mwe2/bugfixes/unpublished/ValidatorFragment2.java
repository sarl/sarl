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

import org.eclipse.xtend.core.validation.XtendConfigurableIssueCodes;
import org.eclipse.xtext.xtext.generator.model.TypeReference;
import org.eclipse.xtext.xtext.generator.util.GrammarUtil2;

/**
 * Fixing the invalid package name for the Xtend configurable issue provider.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class ValidatorFragment2 extends org.eclipse.xtext.xtext.generator.validation.ValidatorFragment2 {

	private static final String XTEND_GRAMMAR = "org.eclipse.xtend.core.Xtend"; //$NON-NLS-1$

	@Override
	protected TypeReference getSuperConfigurableIssueCodesProviderClass() {
		if (GrammarUtil2.inherits(getGrammar(), XTEND_GRAMMAR)) {
			return new TypeReference(XtendConfigurableIssueCodes.class);
		}
		return super.getSuperConfigurableIssueCodesProviderClass();
	}

}

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

package io.sarl.lang.ui.hover;

import javax.inject.Inject;

import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.xbase.ui.hover.HoverUiStrings;

import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.ui.validation.SARLUIStrings;

/**
 * UI Strings for hovers.
 *
 * <p>This class extends the standard Xtend serializer by replacing the example of code, written
 * in Java or Xtend, by the same example with the SARL syntax.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public class SARLHoverUIStrings extends HoverUiStrings {

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Inject
	private AnnotationLookup annotationFinder;

	@Override
	protected String parametersToString(Iterable<? extends JvmFormalParameter> elements, boolean isVarArgs, boolean includeName) {
		return SARLUIStrings.getParameterString(elements, isVarArgs, includeName, this.keywords, this.annotationFinder, this);
	}

}

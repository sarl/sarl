/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.mwe2.external;

import org.eclipse.xtext.generator.IGeneratorFragment;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the Google prettify library.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class LaTeXBeamerGenerator extends LaTeXListingsGenerator {

	/** Constructs a generator for LaTeX Beamer.
	 */
	public LaTeXBeamerGenerator() {
		clearRequirements();
		setFloatBasicStyle("\\usebeamertemplate{code basic style}"); //$NON-NLS-1$
		setInlineBasicStyle("\\usebeamertemplate{code inline style}"); //$NON-NLS-1$
		setIdentifierStyle("\\usebeamertemplate{code identifier style}"); //$NON-NLS-1$
		setCommentStyle("\\usebeamercolor[fg]{code comment}"); //$NON-NLS-1$
		setStringStyle("\\usebeamercolor[fg]{code string}"); //$NON-NLS-1$
		setKeywordStyle("\\usebeamertemplate*{code keyword style}\\usebeamercolor[fg]{code keyword}"); //$NON-NLS-1$
	}

	@Override
	protected String computeDefaultStyBasename(String languageName) {
		return languageName.toLowerCase() + "-beamer-listing"; //$NON-NLS-1$
	}

	@Override
	protected String getHumanReadableSpecificationName() {
		return "LaTeX Beamer style"; //$NON-NLS-1$
	}

}


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

package io.sarl.lang.mwe2.externalspec.latex;

import java.text.MessageFormat;

import com.google.inject.Injector;

import io.sarl.lang.mwe2.externalspec.IStyleAppendable;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the LaTeX Beamer.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class LaTeXBeamerGenerator2 extends LaTeXListingsGenerator2 {

	/** The default basename pattern for {@link MessageFormat}.
	 */
	public static final String BEAMER_BASENAME_PATTERN = "{0}-beamer-listing.sty"; //$NON-NLS-1$

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		clearRequirements();
		setLineNumbers(false);
		setFloatBasicStyle("\\usebeamertemplate{code basic style}"); //$NON-NLS-1$
		setInlineBasicStyle("\\usebeamertemplate{code inline style}"); //$NON-NLS-1$
		setIdentifierStyle("\\usebeamertemplate{code identifier style}"); //$NON-NLS-1$
		setCommentStyle("\\usebeamercolor[fg]{code comment}"); //$NON-NLS-1$
		setStringStyle("\\usebeamercolor[fg]{code string}"); //$NON-NLS-1$
		setKeywordStyle("\\usebeamertemplate*{code keyword style}\\usebeamercolor[fg]{code keyword}"); //$NON-NLS-1$
		setBasenameTemplate(BEAMER_BASENAME_PATTERN);
	}

	@Override
	protected void generateExtension(IStyleAppendable it) {
		it.append("\\ifusesarlcolors"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.appendNl("\\setbeamercolor*{code keyword}{fg=SARLkeyword}"); //$NON-NLS-1$
		it.appendNl("\\setbeamercolor*{code string}{fg=SARLstring}"); //$NON-NLS-1$
		it.append("\\setbeamercolor*{code comment}{fg=SARLcomment}"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.appendNl("\\fi"); //$NON-NLS-1$
	}

	@Override
	public String toString() {
		return "LaTeX Beamer"; //$NON-NLS-1$
	}

}


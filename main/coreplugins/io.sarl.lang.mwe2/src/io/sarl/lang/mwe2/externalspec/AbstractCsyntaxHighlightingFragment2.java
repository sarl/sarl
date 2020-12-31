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

package io.sarl.lang.mwe2.externalspec;

import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.CodeConfig;

/**
 * A {@link AbstractXtextGeneratorFragment} that enables to create the highlighting in
 * a C-syntax-based external tool.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public abstract class AbstractCsyntaxHighlightingFragment2 extends AbstractExternalHighlightingFragment2<IStyleAppendable> {

	@Override
	protected IStyleAppendable newStyleAppendable() {
		return new CsyntaxAppendable(getCodeConfig(), getLanguageSimpleName(), getLanguageVersion());
	}

	/** Appendable for c-syntax-based styles.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class CsyntaxAppendable extends AbstractAppendable {

		/** Constructor.
		 *
		 * @param codeConfig the code configuration.
		 * @param languageName the language name.
		 * @param languageVersion the language version.
		 */
		protected CsyntaxAppendable(CodeConfig codeConfig, String languageName, String languageVersion) {
			super(codeConfig, languageName, languageVersion);
		}

		@Override
		public void appendComment(String text, Object... parameters) {
			final String comment = applyFormat(text, parameters);
			appendNl("/* "); //$NON-NLS-1$
			for (final String line : comment.split("[\n\r]")) { //$NON-NLS-1$
				appendNl(" * " + line.trim()); //$NON-NLS-1$
			}
			appendNl("*/"); //$NON-NLS-1$
		}

		@Override
		public void appendHeader() {
			final String[] header = Strings.emptyIfNull(getCodeConfig().getFileHeader()).split("[\n\r]+"); //$NON-NLS-1$
			appendNl("/* "); //$NON-NLS-1$
			for (final String headerLine : header) {
				appendNl(headerLine.replaceFirst("^\\s*[/]?[*][/]?", " * ")); //$NON-NLS-1$//$NON-NLS-2$
			}
			appendNl("*/"); //$NON-NLS-1$
			appendNl("/* Style for {0} {1} */", getLanguageSimpleName(), getLanguageVersion()); //$NON-NLS-1$
			newLine();
		}

	}

}

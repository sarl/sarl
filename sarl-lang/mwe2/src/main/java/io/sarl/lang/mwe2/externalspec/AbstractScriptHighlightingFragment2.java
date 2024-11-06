/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
 * a script-based external tool.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version mwe2 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid mwe2
 * @since 0.6
 */
public abstract class AbstractScriptHighlightingFragment2 extends AbstractExternalHighlightingFragment2<IStyleAppendable> {

	@Override
	protected IStyleAppendable newStyleAppendable() {
		return new ScriptAppendable(getCodeConfig(), getLanguageSimpleName(), getLanguageVersion());
	}

	/** Appendable for script-based styles.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version mwe2 0.14.0 20241106-161406
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid mwe2
	 * @since 0.6
	 */
	protected static class ScriptAppendable extends AbstractAppendable {

		/** Constructor.
		 *
		 * @param codeConfig the code configuration.
		 * @param languageName the language name.
		 * @param languageVersion the language version.
		 */
		protected ScriptAppendable(CodeConfig codeConfig, String languageName, String languageVersion) {
			super(codeConfig, languageName, languageVersion);
		}

		@Override
		public void appendComment(String text, Object... parameters) {
			final var comment = applyFormat(text, parameters);
			for (final var line : comment.split("[\n\r]")) { //$NON-NLS-1$
				appendNl("# " + line.trim()); //$NON-NLS-1$
			}
		}

		@Override
		public void appendHeader() {
			final var header = Strings.emptyIfNull(getCodeConfig().getFileHeader()).split("[\n\r]+"); //$NON-NLS-1$
			for (final var headerLine : header) {
				appendNl(headerLine.replaceFirst("^\\s*[/]?[*][/]?", "#")); //$NON-NLS-1$//$NON-NLS-2$
			}
			appendNl("# Style for {0} {1}", getLanguageSimpleName(), getLanguageVersion()); //$NON-NLS-1$
			newLine();
		}

	}

}

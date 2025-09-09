/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.eclipse.javadoc;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.xtext.ui.editor.utils.EditorUtils;
import org.eclipse.xtext.ui.refactoring.ui.SyncUtil;

import io.sarl.lang.ui.contentassist.javadoc.ISarlDocumentationProvider;

/**
 * Generate the JavaDoc comment for the selected element.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.12
 */
@SuppressWarnings("restriction")
public class GenerateJavadocHandler extends AbstractHandler {

	@Inject
	private Provider<SyncUtil> syncUtil;

	@Inject
	private Provider<ISarlDocumentationProvider> documentProvider;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			// Save current editor
			this.syncUtil.get().totalSync(false);
			final var editor = EditorUtils.getActiveXtextEditor(event);
			if (editor != null) {
				final var selection = (ITextSelection) editor.getSelectionProvider().getSelection();
				final var document = editor.getDocument();
				this.documentProvider.get().generateDocumentationIfPossible(document, selection);
			}
			return null;
		} catch (Exception exception) {
			throw new ExecutionException(exception.getLocalizedMessage(), exception);
		}
	}

}

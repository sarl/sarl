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

package io.sarl.lang.ui.editor;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Region;
import org.eclipse.xtext.ui.editor.formatting2.ContentFormatter;
import org.eclipse.xtext.ui.editor.utils.EditorUtils;

/**
 * Handler for correct the indentation within a SARL code.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.7
 */
@SuppressWarnings("restriction")
public class CorrectIndentationHandler extends AbstractHandler {

	@Inject
	private Provider<ContentFormatter> formatterProvider;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		final var activeXtextEditor = EditorUtils.getActiveXtextEditor(event);
		if (activeXtextEditor == null) {
			return null;
		}
		final var doc = activeXtextEditor.getDocument();
		final var selection = (ITextSelection) activeXtextEditor.getSelectionProvider().getSelection();
		final var region = new Region(selection.getOffset(), selection.getLength());
		this.formatterProvider.get().format(doc, region);
		return null;
	}

}

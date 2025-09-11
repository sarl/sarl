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
import org.eclipse.core.resources.IStorage;
import org.eclipse.xtext.builder.nature.ToggleXtextNatureCommand;
import org.eclipse.xtext.ui.editor.IXtextEditorCallback;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.ui.resource.UriValidator;

/** Callback associated to the editor for configuring the SARL editor.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.8
 */
@SuppressWarnings("restriction")
public class SARLNatureAddingEditorCallback extends IXtextEditorCallback.NullImpl {

	@Inject
	private IStorage2UriMapper mapper;

	@Inject
	private UriValidator uriValidator;

	@Inject
	private ToggleXtextNatureCommand toggleNature;

	@Override
	public void afterCreatePartControl(XtextEditor editor) {
		final var resource = editor.getResource();
		if (resource != null && !this.toggleNature.hasNature(resource.getProject())
				&& resource.getProject().isAccessible() && !resource.getProject().isHidden() && canBuild(editor)) {
			this.toggleNature.toggleNature(resource.getProject());
		}
	}

	private boolean canBuild(XtextEditor editor) {
		final var resource = editor.getResource();
		if (resource instanceof IStorage storage) {
			final var uri = this.mapper.getUri(storage);
			return this.uriValidator.canBuild(uri, storage);
		}
		return false;
	}

}

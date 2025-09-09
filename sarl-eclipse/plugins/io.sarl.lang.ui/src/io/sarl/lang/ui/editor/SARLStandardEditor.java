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

import org.eclipse.jface.text.source.ISourceViewerExtension5;
import org.eclipse.jface.util.PropertyChangeEvent;

import io.sarl.lang.ui.codemining.SARLCodeminingPreferenceAccess;

/** Editor for SARL code.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.8
 */
@SuppressWarnings("restriction")
public class SARLStandardEditor extends SARLEditor {

	@Override
	protected void handlePreferenceStoreChanged(PropertyChangeEvent event) {
		if (SARLCodeminingPreferenceAccess.CODEMINING_PROPERTY.equals(event.getProperty())) {
			final var viewer = getSourceViewer();
			if (viewer instanceof ISourceViewerExtension5 cvalue) {
				cvalue.updateCodeMinings();
			}
		}
		super.handlePreferenceStoreChanged(event);
	}

}

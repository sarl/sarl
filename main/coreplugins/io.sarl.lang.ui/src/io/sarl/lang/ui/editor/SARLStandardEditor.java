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

package io.sarl.lang.ui.editor;

import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.ISourceViewerExtension5;
import org.eclipse.jface.util.PropertyChangeEvent;

import io.sarl.lang.ui.codemining.SARLCodeminingPreferenceAccess;

/** Editor for SARL code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class SARLStandardEditor extends SARLEditor {

	@Override
	protected void handlePreferenceStoreChanged(PropertyChangeEvent event) {
		if (SARLCodeminingPreferenceAccess.CODEMINING_PROPERTY.equals(event.getProperty())) {
			final ISourceViewer viewer = getSourceViewer();
			if (viewer instanceof ISourceViewerExtension5) {
				((ISourceViewerExtension5) viewer).updateCodeMinings();
			}
		}
		super.handlePreferenceStoreChanged(event);
	}

}

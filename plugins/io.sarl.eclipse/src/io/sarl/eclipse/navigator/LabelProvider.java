/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.navigator;

import io.sarl.eclipse.SARLEclipsePlugin;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

/**
 * SARL custom project navigator label provider.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class LabelProvider implements ILabelProvider {

	@Override
	public Image getImage(Object element) {
		Image image = null;

		if (ISARLProjectElement.class.isInstance(element)) {
			image = ((ISARLProjectElement) element).getImage();
		}
		// else ignore the element
		return image;
	}

	@Override
	public String getText(Object element) {
		String text = ""; //$NON-NLS-1$
		if (ISARLProjectElement.class.isInstance(element)) {
			text = ((ISARLProjectElement) element).getText();
		}
		// else ignore the element
		return text;
	}

	@Override
	public void addListener(ILabelProviderListener listener) {
		SARLEclipsePlugin.getDefault().logDebugMessage(
				"LabelProvider.addListener: " + listener.getClass().getName()); //$NON-NLS-1$
	}

	@Override
	public void dispose() {
		SARLEclipsePlugin.getDefault().logDebugMessage("LabelProvider.dispose"); //$NON-NLS-1$
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		SARLEclipsePlugin.getDefault().logDebugMessage(
				"LabelProvider.isLabelProperty: " + element.getClass().getName()); //$NON-NLS-1$
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener listener) {
		SARLEclipsePlugin.getDefault().logDebugMessage(
				"LabelProvider.removeListener: " + listener.getClass().getName()); //$NON-NLS-1$
	}

}

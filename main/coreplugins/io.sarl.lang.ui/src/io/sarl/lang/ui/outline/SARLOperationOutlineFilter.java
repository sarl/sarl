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

package io.sarl.lang.ui.outline;

import static io.sarl.lang.ui.outline.SARLOutlineNodeComparator.isAction;

import com.google.inject.Inject;
import org.eclipse.jface.action.Action;
import org.eclipse.xtext.ui.IImageHelper.IImageDescriptorHelper;
import org.eclipse.xtext.ui.editor.outline.IOutlineNode;
import org.eclipse.xtext.ui.editor.outline.actions.AbstractFilterOutlineContribution;
import org.eclipse.xtext.ui.editor.outline.impl.EObjectNode;
import org.eclipse.xtext.ui.editor.outline.impl.EStructuralFeatureNode;

/**
 * Customize the filter/sorter of the outline.
 *
 * <p>This filter permits to hide/show the actions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLOperationOutlineFilter extends AbstractFilterOutlineContribution {

	/** Name of the key in the preference store for this filter class.
	 */
	public static final String PREFERENCE_KEY = "io.sarl.lang.ui.outline.filterOperations"; //$NON-NLS-1$

	/** Basename for the action icon.
	 */
	public static final String ICON_BASENAME = "hide_operations.png"; //$NON-NLS-1$

	@Inject
	private IImageDescriptorHelper imageHelper;

	@Override
	protected boolean apply(IOutlineNode node) {
		if (node instanceof EObjectNode) {
			return !isAction(((EObjectNode) node).getEClass());
		}
		if (node instanceof EStructuralFeatureNode) {
			return !isAction(((EStructuralFeatureNode) node).getEStructuralFeature().eClass());
		}
		return true;
	}

	@Override
	protected void configureAction(Action action) {
		action.setText(Messages.SARLOperationOutlineFilter_0);
		action.setDescription(Messages.SARLOperationOutlineFilter_0);
		action.setToolTipText(Messages.SARLOperationOutlineFilter_0);
		action.setImageDescriptor(this.imageHelper.getImageDescriptor(ICON_BASENAME));
	}

	@Override
	public String getPreferenceKey() {
		return PREFERENCE_KEY;
	}

}

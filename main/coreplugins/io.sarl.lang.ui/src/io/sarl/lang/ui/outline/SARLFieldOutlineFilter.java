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

import static io.sarl.lang.ui.outline.SARLOutlineNodeComparator.isField;

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
 * <p>This filter permits to hide/show the fields.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLFieldOutlineFilter extends AbstractFilterOutlineContribution {

	/** Name of the preference key for the outline filter.
	 */
	public static final String PREFERENCE_KEY = "io.sarl.lang.ui.outline.filterFields"; //$NON-NLS-1$

	/** Basename of the action icon.
	 */
	public static final String ICON_BASENAME = "hide_fields.png"; //$NON-NLS-1$

	@Inject
	private IImageDescriptorHelper imageHelper;

	@Override
	protected boolean apply(IOutlineNode node) {
		if (node instanceof EObjectNode) {
			return !isField(((EObjectNode) node).getEClass());
		}
		if (node instanceof EStructuralFeatureNode) {
			return !isField(((EStructuralFeatureNode) node).getEStructuralFeature().eClass());
		}
		return true;
	}

	@Override
	protected void configureAction(Action action) {
		action.setText(Messages.SARLFieldOutlineFilter_0);
		action.setDescription(Messages.SARLFieldOutlineFilter_0);
		action.setToolTipText(Messages.SARLFieldOutlineFilter_0);
		action.setImageDescriptor(this.imageHelper.getImageDescriptor(ICON_BASENAME));
	}

	@Override
	public String getPreferenceKey() {
		return PREFERENCE_KEY;
	}

}

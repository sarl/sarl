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

package io.sarl.lang.ui.outline;

import static io.sarl.lang.ui.outline.SARLOutlineNodeComparator.isBehaviorUnit;

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
 * <p>This filter permits to hide/show the behavior units.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 */
public class SARLBehaviorUnitOutlineFilter extends AbstractFilterOutlineContribution {

	/** Key in the preference storage for outline components.
	 */
	public static final String PREFERENCE_KEY = "io.sarl.lang.ui.outline.filterBehaviorUnits"; //$NON-NLS-1$

	/** Basename of the icon of the action.
	 */
	public static final String ICON_BASENAME = "hide_behavior_units.png"; //$NON-NLS-1$

	@Inject
	private IImageDescriptorHelper imageHelper;

	@Override
	protected boolean apply(IOutlineNode node) {
		if (node instanceof EObjectNode cvalue) {
			return !isBehaviorUnit(cvalue.getEClass());
		}
		if (node instanceof EStructuralFeatureNode cvalue) {
			return !isBehaviorUnit(cvalue.getEStructuralFeature().eClass());
		}
		return true;
	}

	@Override
	protected void configureAction(Action action) {
		action.setText(Messages.SARLBehaviorUnitOutlineFilter_0);
		action.setDescription(Messages.SARLBehaviorUnitOutlineFilter_0);
		action.setToolTipText(Messages.SARLBehaviorUnitOutlineFilter_0);
		action.setImageDescriptor(this.imageHelper.getImageDescriptor(ICON_BASENAME));
	}

	@Override
	public String getPreferenceKey() {
		return PREFERENCE_KEY;
	}

}

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
package io.sarl.lang.ui.outline

import com.google.inject.Inject
import io.sarl.lang.sarl.SarlPackage
import org.eclipse.emf.ecore.EClass
import org.eclipse.jface.action.Action
import org.eclipse.xtext.ui.IImageHelper.IImageDescriptorHelper
import org.eclipse.xtext.ui.editor.outline.IOutlineNode
import org.eclipse.xtext.ui.editor.outline.actions.AbstractFilterOutlineContribution
import org.eclipse.xtext.ui.editor.outline.impl.EObjectNode
import org.eclipse.xtext.ui.editor.outline.impl.EStructuralFeatureNode

/**
 * Customize the filter/sorter of the outline.
 * 
 * This filter permits to hide/show the actions.
 */
public class SARLOperationOutlineFilter extends AbstractFilterOutlineContribution {
	
	public static final String PREFERENCE_KEY = "io.sarl.lang.ui.outline.filterOperations"
	
	@Inject private IImageDescriptorHelper imageHelper;
	
	protected def isOperation(EClass type) {
		type == SarlPackage.Literals.ACTION
		|| type == SarlPackage.Literals.ACTION_SIGNATURE
		|| type == SarlPackage.Literals.CONSTRUCTOR
	}
	
	override protected apply(IOutlineNode node) {
		if (node instanceof EObjectNode) {
			! node.EClass.operation
		}
		else if (node instanceof EStructuralFeatureNode) {
			! node.EStructuralFeature.eClass.operation
		}
		else {
			true
		}
	}
	
	override protected configureAction(Action action) {
		action.setText(Messages::SARLOperationOutlineFilter_0)
	    action.setDescription(Messages::SARLOperationOutlineFilter_0)
	    action.setToolTipText(Messages::SARLOperationOutlineFilter_0)
	    action.setImageDescriptor(imageHelper.getImageDescriptor("hide_operations.png"))
	}
	
	override getPreferenceKey() {
		PREFERENCE_KEY
	}
	
}
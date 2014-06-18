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
 * This filter permits to hide/show the behavior units.
 */
public class SARLBehaviorUnitOutlineFilter extends AbstractFilterOutlineContribution {
	
	public static final String PREFERENCE_KEY = "io.sarl.lang.ui.outline.filterBehaviorUnits"
	
	@Inject private IImageDescriptorHelper imageHelper;
	
	protected def isBehaviorUnit(EClass type) {
		type == SarlPackage.Literals.BEHAVIOR_UNIT
	}
	
	override protected apply(IOutlineNode node) {
		if (node instanceof EObjectNode) {
			! node.EClass.behaviorUnit
		}
		else if (node instanceof EStructuralFeatureNode) {
			! node.EStructuralFeature.eClass.behaviorUnit
		}
		else {
			true
		}
	}
	
	override protected configureAction(Action action) {
		action.setText("Hide behavior units")
	    action.setDescription("Hide behavior units")
	    action.setToolTipText("Hide behavior units")
	    action.setImageDescriptor(imageHelper.getImageDescriptor("hide_behavior_units.png"))
	}
	
	override getPreferenceKey() {
		PREFERENCE_KEY
	}
	
}
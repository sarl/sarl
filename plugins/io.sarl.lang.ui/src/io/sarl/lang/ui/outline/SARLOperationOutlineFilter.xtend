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
		action.setText("Hide operations")
	    action.setDescription("Hide constructors and actions")
	    action.setToolTipText("Hide constructors and actions")
	    action.setImageDescriptor(imageHelper.getImageDescriptor("hide_operations.png"))
	}
	
	override getPreferenceKey() {
		PREFERENCE_KEY
	}
	
}
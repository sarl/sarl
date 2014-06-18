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
 * This filter permits to hide/show the fields.
 */
public class SARLFieldOutlineFilter extends AbstractFilterOutlineContribution {
	
	public static final String PREFERENCE_KEY = "io.sarl.lang.ui.outline.filterFields"
	
	@Inject private IImageDescriptorHelper imageHelper;
	
	protected def isField(EClass type) {
		type == SarlPackage.Literals.ATTRIBUTE
	}
	
	override protected apply(IOutlineNode node) {
		if (node instanceof EObjectNode) {
			! node.EClass.field
		}
		else if (node instanceof EStructuralFeatureNode) {
			! node.EStructuralFeature.eClass.field
		}
		else {
			true
		}
	}
	
	override protected configureAction(Action action) {
		action.setText("Hide fields")
	    action.setDescription("Hide fields")
	    action.setToolTipText("Hide fields")
	    action.setImageDescriptor(imageHelper.getImageDescriptor("hide_fields.png"))
	}
	
	override getPreferenceKey() {
		PREFERENCE_KEY
	}
	
}
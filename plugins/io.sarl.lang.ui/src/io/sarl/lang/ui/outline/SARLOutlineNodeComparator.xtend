package io.sarl.lang.ui.outline

import org.eclipse.xtext.ui.editor.outline.actions.SortOutlineContribution.DefaultComparator
import org.eclipse.xtext.ui.editor.outline.IOutlineNode
import org.eclipse.xtext.ui.editor.outline.impl.EStructuralFeatureNode
import io.sarl.lang.sarl.SarlPackage
import org.eclipse.xtext.ui.editor.outline.impl.EObjectNode
import org.eclipse.xtext.xtype.XtypePackage

/**
 * Comparator of nodes in the SARL outline.
 */
public class SARLOutlineNodeComparator extends DefaultComparator {
		
	new() {
	}
	
	override int getCategory(IOutlineNode node) {
		if (node instanceof EStructuralFeatureNode) {
			var feature = node.EStructuralFeature
			if (feature == SarlPackage.Literals.SARL_SCRIPT__NAME) return 0
			return 30
		}
		if (node instanceof EObjectNode) {
			var eClass = node.EClass
			if (XtypePackage.Literals.XIMPORT_SECTION.isSuperTypeOf(eClass)) return 10
			if (SarlPackage.Literals.CAPACITY_USES.isSuperTypeOf(eClass)) return 40
			if (SarlPackage.Literals.REQUIRED_CAPACITY.isSuperTypeOf(eClass)) return 50
			if (SarlPackage.Literals.ATTRIBUTE.isSuperTypeOf(eClass)) return 60
			if (SarlPackage.Literals.CONSTRUCTOR.isSuperTypeOf(eClass)) return 70
			if (SarlPackage.Literals.BEHAVIOR_UNIT.isSuperTypeOf(eClass)) return 80
			if (SarlPackage.Literals.ACTION.isSuperTypeOf(eClass)) return 90
			if (SarlPackage.Literals.ACTION_SIGNATURE.isSuperTypeOf(eClass)) return 100
		}
		return Integer.MAX_VALUE;
	}
		
}
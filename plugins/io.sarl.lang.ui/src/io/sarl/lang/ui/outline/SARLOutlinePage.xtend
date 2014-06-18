package io.sarl.lang.ui.outline

import java.util.List
import org.eclipse.xtext.ui.editor.outline.IOutlineNode
import org.eclipse.xtext.ui.editor.outline.impl.EStructuralFeatureNode
import org.eclipse.xtext.ui.editor.outline.impl.OutlinePage

/**
 * Customize the outline page.
 * The outline page is expanding the nodes at the startup.
 */
public class SARLOutlinePage extends OutlinePage {

	protected override List<IOutlineNode> getInitiallyExpandedNodes() {
		var IOutlineNode rootNode = getTreeProvider().createRoot(getXtextDocument());
		var List<IOutlineNode> result = newArrayList(rootNode);
		
		for(IOutlineNode firstLevelNode: rootNode.getChildren()) {
			if(firstLevelNode instanceof EStructuralFeatureNode) { 
				result.add(firstLevelNode)
			}
		} 
		
		return result;
	}
	
}
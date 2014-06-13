package io.sarl.lang.ui.outline

import org.eclipse.xtext.ui.editor.outline.impl.OutlineFilterAndSorter

/**
 * Customize the filter/sorter of the outline.
 */
public class SARLOutlineFilterAndSorter extends OutlineFilterAndSorter {

	public new() {
		comparator = new SARLOutlineNodeComparator
	}
	
}
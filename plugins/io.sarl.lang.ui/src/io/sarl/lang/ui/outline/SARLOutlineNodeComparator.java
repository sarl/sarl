/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import static io.sarl.lang.sarl.SarlPackage.Literals.ACTION;
import static io.sarl.lang.sarl.SarlPackage.Literals.ACTION_SIGNATURE;
import static io.sarl.lang.sarl.SarlPackage.Literals.ATTRIBUTE;
import static io.sarl.lang.sarl.SarlPackage.Literals.BEHAVIOR_UNIT;
import static io.sarl.lang.sarl.SarlPackage.Literals.CAPACITY_USES;
import static io.sarl.lang.sarl.SarlPackage.Literals.CONSTRUCTOR;
import static io.sarl.lang.sarl.SarlPackage.Literals.REQUIRED_CAPACITY;
import static org.eclipse.xtext.xtype.XtypePackage.Literals.XIMPORT_SECTION;
import io.sarl.lang.sarl.SarlPackage;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtext.ui.editor.outline.IOutlineNode;
import org.eclipse.xtext.ui.editor.outline.actions.SortOutlineContribution.DefaultComparator;
import org.eclipse.xtext.ui.editor.outline.impl.EObjectNode;
import org.eclipse.xtext.ui.editor.outline.impl.EStructuralFeatureNode;

/**
 * Comparator of nodes in the SARL outline.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLOutlineNodeComparator extends DefaultComparator {

	private static final int SCRIPT_PRIORITY = 0;
	private static final int IMPORT_PRIORITY = 10;
	private static final int TOPELEMENT_PRIORITY = 20;

	private static final int PRIORITY_STEP = 10;

	private final EClass[] types = new EClass[] {
			CAPACITY_USES,
			REQUIRED_CAPACITY,
			ATTRIBUTE,
			CONSTRUCTOR,
			BEHAVIOR_UNIT,
			ACTION,
			ACTION_SIGNATURE,
	};

	/**
	 */
	public SARLOutlineNodeComparator() {
		//
	}

	@Override
	public int getCategory(IOutlineNode node) {
		if (node instanceof EStructuralFeatureNode) {
			EStructuralFeature feature = ((EStructuralFeatureNode) node).getEStructuralFeature();
			if (feature == SarlPackage.Literals.SARL_SCRIPT__NAME) {
				return SCRIPT_PRIORITY;
			}
			return TOPELEMENT_PRIORITY;
		}
		if (node instanceof EObjectNode) {
			EClass eClass = ((EObjectNode) node).getEClass();
			if (XIMPORT_SECTION.isSuperTypeOf(eClass)) {
				return IMPORT_PRIORITY;
			}
			int priority = 0;
			for (EClass type : this.types) {
				priority += PRIORITY_STEP;
				if (type.isSuperTypeOf(eClass)) {
					return priority;
				}
			}
		}
		return Integer.MAX_VALUE;
	}

}

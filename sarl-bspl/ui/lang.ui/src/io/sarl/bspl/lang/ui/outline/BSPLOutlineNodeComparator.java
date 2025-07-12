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

package io.sarl.bspl.lang.ui.outline;

import static io.sarl.bspl.lang.bspl.BsplPackage.Literals.BSPL_PROTOCOL_MESSAGE;
import static io.sarl.bspl.lang.bspl.BsplPackage.Literals.BSPL_PROTOCOL_PARAMETER;
import static io.sarl.bspl.lang.bspl.BsplPackage.Literals.BSPL_PROTOCOL_ROLE;
import static io.sarl.bspl.lang.bspl.BsplPackage.Literals.BSPL_PROTOCOL_SPECIFICATION__PACKAGE;
import static org.eclipse.xtext.xtype.XtypePackage.Literals.XIMPORT_SECTION;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.xtext.ui.editor.outline.IOutlineNode;
import org.eclipse.xtext.ui.editor.outline.actions.SortOutlineContribution.DefaultComparator;
import org.eclipse.xtext.ui.editor.outline.impl.EObjectNode;
import org.eclipse.xtext.ui.editor.outline.impl.EStructuralFeatureNode;

/**
 * Comparator of nodes in the BSPL outline.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class BSPLOutlineNodeComparator extends DefaultComparator {

	private static final int SPECIFICATION_PRIORITY = 0;

	private static final int IMPORT_PRIORITY = 10;

	private static final int PROTOCOL_PRIORITY = 20;

	private static final int ROLE_PRIORITY = 30;

	private static final int KEY_PRIORITY = 40;

	private static final int PARAMETER_PRIORITY = 50;

	private static final int MESSAGE_PRIORITY = 60;

	/** Construct a comparator.
	 */
	public BSPLOutlineNodeComparator() {
		//
	}

	/** Replies if the given type corresponds to a role.
	 *
	 * @param type the type.
	 * @return {@code true} if the type corresponds to a BSPL role.
	 */
	public static boolean isRole(EClass type) {
		return BSPL_PROTOCOL_ROLE.isSuperTypeOf(type);
	}

	/** Replies if the given type corresponds to a parameter.
	 *
	 * @param type the type.
	 * @return {@code true} if the type corresponds to a BSPL parameter.
	 */
	public static boolean isParameter(EClass type) {
		return BSPL_PROTOCOL_PARAMETER.isSuperTypeOf(type);
	}

	/** Replies if the given type corresponds to a message.
	 *
	 * @param type the type.
	 * @return {@code true} if the type corresponds to a BSPL message.
	 */
	public static boolean isMessage(EClass type) {
		return BSPL_PROTOCOL_MESSAGE.isSuperTypeOf(type);
	}

	/** Replies if the given type corresponds to a key parameter.
	 *
	 * @param node the node to test.
	 * @return {@code true} if the type corresponds to a key parameter.
	 */
	public static boolean isKey(EObjectNode node) {
		if (node instanceof BSPLEObjectNode cvalue) {
			return cvalue.isKey();
		}
		return false;
	}

	@Override
	public int getCategory(IOutlineNode node) {
		if (node instanceof EStructuralFeatureNode cvalue) {
			final var feature = cvalue.getEStructuralFeature();
			if (feature == BSPL_PROTOCOL_SPECIFICATION__PACKAGE) {
				return SPECIFICATION_PRIORITY;
			}
			return PROTOCOL_PRIORITY;
		}
		if (node instanceof EObjectNode objectNode) {
			final var objectNodeType = objectNode.getEClass();
			if (XIMPORT_SECTION.isSuperTypeOf(objectNodeType)) {
				return IMPORT_PRIORITY;
			}
			if (isRole(objectNodeType)) {
				return ROLE_PRIORITY;
			}
			if (isParameter(objectNodeType)) {
				if (isKey(objectNode)) {
					return KEY_PRIORITY;
				}
				return PARAMETER_PRIORITY;
			}
			if (isMessage(objectNodeType)) {
				return MESSAGE_PRIORITY;
			}
		}
		return Integer.MAX_VALUE;
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_BEHAVIOR_UNIT;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_CAPACITY_USES;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_REQUIRED_CAPACITY;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_CONSTRUCTOR;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FIELD;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FILE__PACKAGE;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FUNCTION;
import static org.eclipse.xtext.common.types.TypesPackage.Literals.JVM_CONSTRUCTOR;
import static org.eclipse.xtext.common.types.TypesPackage.Literals.JVM_FIELD;
import static org.eclipse.xtext.common.types.TypesPackage.Literals.JVM_OPERATION;
import static org.eclipse.xtext.xtype.XtypePackage.Literals.XIMPORT_SECTION;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.common.types.TypesPackage;
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

	private static final int CAPACITY_USE_PRIORITY = 30;

	private static final int CAPACITY_REQUIREMENT_PRIORITY = 40;

	private static final int STATIC_INNER_TYPE_PRIORITY = 50;

	private static final int INNER_TYPE_PRIORITY = 60;

	private static final int STATIC_FIELD_PRIORITY = 70;

	private static final int STATIC_METHOD_PRIORITY = 80;

	private static final int STATIC_CONSTRUCTOR = 90;

	private static final int FIELD_PRIORITY = 100;

	private static final int CONSTRUCTOR_PRIORITY = 110;

	private static final int BEHAVIOR_UNIT_PRIORITY = 120;

	private static final int METHOD_PRIORITY = 130;

	/** Construct a comparator.
	 */
	public SARLOutlineNodeComparator() {
		//
	}

	/** Replies if the given type corresponds to a field.
	 *
	 * @param type the type.
	 * @return {@code true} if the type corresponds to a SARL, Xtend or JVM field.
	 * @since 0.9
	 */
	public static boolean isField(EClass type) {
		return XTEND_FIELD.isSuperTypeOf(type) || JVM_FIELD.isSuperTypeOf(type);
	}

	/** Replies if the given type corresponds to an action.
	 *
	 * @param type the type.
	 * @return {@code true} if the type corresponds to a SARL, Xtend or JVM action/method/operation.
	 * @since 0.9
	 */
	public static boolean isAction(EClass type) {
		return XTEND_FUNCTION.isSuperTypeOf(type) || JVM_OPERATION.isSuperTypeOf(type);
	}

	/** Replies if the given type corresponds to a constructor.
	 *
	 * @param type the type.
	 * @return {@code true} if the type corresponds to a SARL, Xtend or JVM constructor.
	 * @since 0.9
	 */
	public static boolean isConstructor(EClass type) {
		return XTEND_CONSTRUCTOR.isSuperTypeOf(type) || JVM_CONSTRUCTOR.isSuperTypeOf(type);
	}

	/** Replies if the given type corresponds to a behavior unit.
	 *
	 * @param type the type.
	 * @return {@code true} if the type corresponds to a SARL behavior unit.
	 * @since 0.9
	 */
	public static boolean isBehaviorUnit(EClass type) {
		return SARL_BEHAVIOR_UNIT.isSuperTypeOf(type);
	}

	/** Replies if the given type corresponds to a capacity use.
	 *
	 * @param type the type.
	 * @return {@code true} if the type corresponds to a SARL capacity use.
	 * @since 0.9
	 */
	public static boolean isCapacityUses(EClass type) {
		return SARL_CAPACITY_USES.isSuperTypeOf(type);
	}

	@SuppressWarnings({"checkstyle:npathcomplexity", "checkstyle:returncount",
			"checkstyle:cyclomaticcomplexity"})
	@Override
	public int getCategory(IOutlineNode node) {
		if (node instanceof EStructuralFeatureNode) {
			final EStructuralFeature feature = ((EStructuralFeatureNode) node).getEStructuralFeature();
			if (feature == XTEND_FILE__PACKAGE) {
				return SCRIPT_PRIORITY;
			}
			return TOPELEMENT_PRIORITY;
		}
		if (node instanceof EObjectNode) {
			final EObjectNode objectNode = (EObjectNode) node;
			final EClass objectNodeType = objectNode.getEClass();
			if (XIMPORT_SECTION.isSuperTypeOf(objectNodeType)) {
				return IMPORT_PRIORITY;
			}
			if (XtendPackage.Literals.XTEND_TYPE_DECLARATION.isSuperTypeOf(objectNodeType)
					|| TypesPackage.Literals.JVM_DECLARED_TYPE.isSuperTypeOf(objectNodeType)
					|| TypesPackage.Literals.JVM_ENUMERATION_LITERAL.isSuperTypeOf(objectNodeType)) {
				if (isStatic(objectNode)) {
					return STATIC_INNER_TYPE_PRIORITY;
				}
				return INNER_TYPE_PRIORITY;
			}
			if (isCapacityUses(objectNodeType)) {
				return CAPACITY_USE_PRIORITY;
			}
			if (SARL_REQUIRED_CAPACITY.isSuperTypeOf(objectNodeType)) {
				return CAPACITY_REQUIREMENT_PRIORITY;
			}
			if (isField(objectNodeType)) {
				if (isStatic(objectNode)) {
					return STATIC_FIELD_PRIORITY;
				}
				return FIELD_PRIORITY;
			}
			if (isAction(objectNodeType)) {
				if (isStatic(objectNode)) {
					return STATIC_METHOD_PRIORITY;
				}
				return METHOD_PRIORITY;
			}
			if (isConstructor(objectNodeType)) {
				if (isStatic(objectNode)) {
					return STATIC_CONSTRUCTOR;
				}
				return CONSTRUCTOR_PRIORITY;
			}
			if (isBehaviorUnit(objectNodeType)) {
				return BEHAVIOR_UNIT_PRIORITY;
			}
		}
		return Integer.MAX_VALUE;
	}

	private static boolean isStatic(EObjectNode eobjectNode) {
		if (eobjectNode instanceof SARLEObjectNode) {
			return ((SARLEObjectNode) eobjectNode).isStatic();
		}
		return false;
	}

}

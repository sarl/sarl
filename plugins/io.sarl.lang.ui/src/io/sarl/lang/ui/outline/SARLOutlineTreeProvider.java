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

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.ui.editor.outline.impl.DocumentRootNode;
import org.eclipse.xtext.ui.editor.outline.impl.EObjectNode;
import org.eclipse.xtext.ui.editor.outline.impl.EStructuralFeatureNode;
import org.eclipse.xtext.xbase.annotations.ui.outline.XbaseWithAnnotationsOutlineTreeProvider;

import com.google.common.base.Strings;

/**
 * Customization of the default outline structure.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#outline"
 */
public class SARLOutlineTreeProvider extends XbaseWithAnnotationsOutlineTreeProvider {

	/** Create a node for the SARL script.
	 *
	 * @param parentNode - the parent node.
	 * @param modelElement - the feature container for which a node should be created.
	 */
	protected void _createChildren(DocumentRootNode parentNode, XtendFile modelElement) {
		if (!Strings.isNullOrEmpty(modelElement.getPackage())) {
			createEStructuralFeatureNode(
					parentNode, modelElement,
					XtendPackage.Literals.XTEND_FILE__PACKAGE,
					this.imageDispatcher.invoke(getClass().getPackage()),
					// Do not use the text dispatcher below for avoiding to obtain
					// the filename of the script.
					modelElement.getPackage(),
					true);
		}
		if (modelElement.getImportSection() != null && !modelElement.getImportSection().getImportDeclarations().isEmpty()) {
			createNode(parentNode, modelElement.getImportSection());
		}
		for (XtendTypeDeclaration topElement : modelElement.getXtendTypes()) {
			createNode(parentNode, topElement);
		}
	}

	/** Create a node for the given feature container.
	 *
	 * @param parentNode - the parent node.
	 * @param modelElement - the feature container for which a node should be created.
	 */
	protected void _createNode(DocumentRootNode parentNode, XtendTypeDeclaration modelElement) {
		EStructuralFeatureNode elementNode = createEStructuralFeatureNode(
				parentNode,
				modelElement,
				XtendPackage.Literals.XTEND_TYPE_DECLARATION__NAME,
				this.imageDispatcher.invoke(modelElement),
				this.textDispatcher.invoke(modelElement),
				modelElement.getMembers().isEmpty());
		if (!modelElement.getMembers().isEmpty()) {
			EObjectNode capacityUseNode = null;
			EObjectNode capacityRequirementNode = null;

			for (EObject feature : modelElement.getMembers()) {
				if (feature instanceof XtendField) {
					createNode(elementNode, feature);
				} else if (feature instanceof SarlAction) {
					createNode(elementNode, feature);
				} else if (feature instanceof SarlBehaviorUnit) {
					createNode(elementNode, feature);
				} else if (feature instanceof XtendConstructor) {
					createNode(elementNode, feature);
				} else if (feature instanceof SarlCapacityUses) {
					capacityUseNode = createCapacityUseNode(elementNode, (SarlCapacityUses) feature, capacityUseNode);
				} else if (feature instanceof SarlRequiredCapacity) {
					capacityRequirementNode = createRequiredCapacityNode(elementNode,
							(SarlRequiredCapacity) feature, capacityRequirementNode);
				}
			}
		}
	}

	private EObjectNode createCapacityUseNode(EStructuralFeatureNode elementNode, SarlCapacityUses feature,
			EObjectNode oldCapacityUseNode) {
		EObjectNode capacityUseNode = oldCapacityUseNode;
		if (capacityUseNode == null) {
			capacityUseNode = createEObjectNode(
					elementNode, feature,
					this.imageDispatcher.invoke(feature),
					this.textDispatcher.invoke(feature),
					false);
		}
		for (JvmParameterizedTypeReference item : feature.getCapacities()) {
			createEObjectNode(
					capacityUseNode, item,
					this.imageDispatcher.invoke(item),
					this.textDispatcher.invoke(item),
					true);
		}
		return capacityUseNode;
	}

	private EObjectNode createRequiredCapacityNode(EStructuralFeatureNode elementNode, SarlRequiredCapacity feature,
			EObjectNode oldCapacityRequirementNode) {
		EObjectNode capacityRequirementNode = oldCapacityRequirementNode;
		if (capacityRequirementNode == null) {
			capacityRequirementNode = createEObjectNode(
					elementNode, feature,
					this.imageDispatcher.invoke(feature),
					this.textDispatcher.invoke(feature),
					false);
		}
		for (JvmParameterizedTypeReference item : feature.getCapacities()) {
			createEObjectNode(
					capacityRequirementNode, item,
					this.imageDispatcher.invoke(item),
					this.textDispatcher.invoke(item),
					true);
		}
		return capacityRequirementNode;
	}

	/** Replies if the agent element is a leaf in the outline.
	 *
	 * @param modelElement - the model element.
	 * @return <code>true</code> if it is a leaf, <code>false</code> otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(SarlAgent modelElement) {
		return modelElement.getMembers().isEmpty();
	}

	/** Replies if the capacity element is a leaf in the outline.
	 *
	 * @param modelElement - the model element.
	 * @return <code>true</code> if it is a leaf, <code>false</code> otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(SarlCapacity modelElement) {
		return modelElement.getMembers().isEmpty();
	}

	/** Replies if the skill element is a leaf in the outline.
	 *
	 * @param modelElement - the model element.
	 * @return <code>true</code> if it is a leaf, <code>false</code> otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(SarlSkill modelElement) {
		return modelElement.getMembers().isEmpty();
	}

	/** Replies if the event element is a leaf in the outline.
	 *
	 * @param modelElement - the model element.
	 * @return <code>true</code> if it is a leaf, <code>false</code> otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(SarlEvent modelElement) {
		return modelElement.getMembers().isEmpty();
	}

	/** Replies if the behavior element is a leaf in the outline.
	 *
	 * @param modelElement - the model element.
	 * @return <code>true</code> if it is a leaf, <code>false</code> otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(SarlBehavior modelElement) {
		return modelElement.getMembers().isEmpty();
	}

	/** Replies if the action element is a leaf in the outline.
	 *
	 * @param modelElement - the model element.
	 * @return <code>true</code> if it is a leaf, <code>false</code> otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(SarlAction modelElement) {
		return true;
	}

	/** Replies if the constructor element is a leaf in the outline.
	 *
	 * @param modelElement - the model element.
	 * @return <code>true</code> if it is a leaf, <code>false</code> otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(XtendConstructor modelElement) {
		return true;
	}

	/** Replies if the behabior unit element is a leaf in the outline.
	 *
	 * @param modelElement - the model element.
	 * @return <code>true</code> if it is a leaf, <code>false</code> otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(SarlBehaviorUnit modelElement) {
		return true;
	}

	/** Replies if the attribute element is a leaf in the outline.
	 *
	 * @param modelElement - the model element.
	 * @return <code>true</code> if it is a leaf, <code>false</code> otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(XtendField modelElement) {
		return true;
	}

}

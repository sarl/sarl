/*
 * $Id$
 *
 * File is automatically generated by the Xtext language generator.
 * Do not change it.
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

import com.google.common.base.Strings;
import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.ui.editor.outline.IOutlineNode;
import org.eclipse.xtext.ui.editor.outline.impl.DefaultOutlineTreeProvider;
import org.eclipse.xtext.ui.editor.outline.impl.DocumentRootNode;
import org.eclipse.xtext.ui.editor.outline.impl.EObjectNode;
import org.eclipse.xtext.ui.editor.outline.impl.EStructuralFeatureNode;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociations;

import io.sarl.bspl.lang.bspl.BsplPackage;
import io.sarl.bspl.lang.bspl.BsplProtocol;
import io.sarl.bspl.lang.bspl.BsplProtocolMember;
import io.sarl.bspl.lang.bspl.BsplProtocolParameter;
import io.sarl.bspl.lang.bspl.BsplProtocolSpecification;

/**
 * Customization of the default outline structure.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class BSPLOutlineTreeProvider extends DefaultOutlineTreeProvider {

	@Inject
	private IJvmModelAssociations associations;

	@Override
	protected EObjectNode createEObjectNode(
			IOutlineNode parentNode,
			EObject modelElement, Image image, Object text,
			boolean isLeaf) {
		final var objectNode = new BSPLEObjectNode(modelElement, parentNode, image, text, isLeaf);
		configureNode(parentNode, modelElement, objectNode);
		return objectNode;
	}

	private void configureNode(IOutlineNode parentNode, EObject modelElement, BSPLEObjectNode objectNode) {
		final var primarySourceElement = this.associations.getPrimarySourceElement(modelElement);
		final var parserNode = NodeModelUtils.getNode(
				(primarySourceElement == null) ? modelElement : primarySourceElement);

		if (parserNode != null) {
			objectNode.setTextRegion(parserNode.getTextRegion());
		}

		if (isLocalElement(parentNode, modelElement)) {
			objectNode.setShortTextRegion(this.locationInFileProvider.getSignificantTextRegion(modelElement));
		}

		objectNode.setKey(isKey(modelElement));
	}

	private static boolean isKey(EObject element) {
		if (element instanceof BsplProtocolParameter cvalue) {
			return cvalue.isKey();
		}
		return false;
	}

	/** Create a node for the BSPL specification.
	 *
	 * @param parentNode the parent node.
	 * @param modelElement the feature container for which a node should be created.
	 */
	protected void _createChildren(DocumentRootNode parentNode, BsplProtocolSpecification modelElement) {
		if (!Strings.isNullOrEmpty(modelElement.getPackage())) {
			// Create the node for the package declaration.
			createEStructuralFeatureNode(
					parentNode, modelElement,
					BsplPackage.Literals.BSPL_PROTOCOL_SPECIFICATION__PACKAGE,
					this.imageDispatcher.invoke(getClass().getPackage()),
					// Do not use the text dispatcher below for avoiding to obtain
					// the filename of the script.
					modelElement.getPackage(),
					true);
		}
		// Create the nodes for the import declarations.
		/*if (modelElement.getImportSection() != null && !modelElement.getImportSection().getImportDeclarations().isEmpty()) {
			createNode(parentNode, modelElement.getImportSection());
		}*/
		// Create a node per type declaration.
		for (final var topElement : modelElement.getBsplProtocols()) {
			createNode(parentNode, topElement);
		}
	}

	/** Create a node for the given feature container at the root level.
	 *
	 * @param parentNode the parent node.
	 * @param modelElement the feature container for which a node should be created.
	 */
	protected void _createNode(DocumentRootNode parentNode, BsplProtocol modelElement) {
		createProtocolNode(parentNode, modelElement);
	}

	/** Create a node for the given feature container at the root level.
	 *
	 * @param parentNode the parent node.
	 * @param modelElement the feature container for which a node should be created.
	 */
	protected void _createNode(EObjectNode parentNode, BsplProtocol modelElement) {
		createProtocolNode(parentNode, modelElement);
	}

	/** Create a node for the given feature container at the root level.
	 *
	 * @param parentNode the parent node.
	 * @param modelElement the feature container for which a node should be created.
	 */
	protected void _createNode(EStructuralFeatureNode parentNode, BsplProtocol modelElement) {
		createProtocolNode(parentNode, modelElement);
	}

	private void createProtocolNode(IOutlineNode parentNode, BsplProtocol modelElement) {
		//
		// The text region is set to the model element, not to the model element's name as in the
		// default implementation of createStructuralFeatureNode().
		// The text region computation is overridden in order to have a correct link to the editor.
		//
		final var isFeatureSet = modelElement.eIsSet(BsplPackage.Literals.BSPL_PROTOCOL__NAME);
		final var elementNode = new EStructuralFeatureNode(
				modelElement,
				BsplPackage.Literals.BSPL_PROTOCOL__NAME,
				parentNode,
				this.imageDispatcher.invoke(modelElement),
				this.textDispatcher.invoke(modelElement),
				modelElement.getMembers().isEmpty() || !isFeatureSet);
		final var primarySourceElement = this.associations.getPrimarySourceElement(modelElement);
		final var parserNode = NodeModelUtils.getNode(
				(primarySourceElement == null) ? modelElement : primarySourceElement);
		elementNode.setTextRegion(parserNode.getTextRegion());
		//
		if (!modelElement.getMembers().isEmpty()) {
			for (final var feature : modelElement.getMembers()) {
				createNode(elementNode, feature);
			}
		}
	}

	/** Replies if the type declaration element is a leaf in the outline.
	 *
	 * @param modelElement the model element.
	 * @return {@code true} if it is a leaf, {@code false} otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(BsplProtocolSpecification modelElement) {
		return modelElement.getBsplProtocols().isEmpty();
	}

	/** Replies if the type declaration element is a leaf in the outline.
	 *
	 * @param modelElement the model element.
	 * @return {@code true} if it is a leaf, {@code false} otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(BsplProtocol modelElement) {
		return modelElement.getMembers().isEmpty();
	}

	/** Replies if the member element is a leaf in the outline.
	 *
	 * @param modelElement the model element.
	 * @return {@code true} if it is a leaf, {@code false} otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(BsplProtocolMember modelElement) {
		return true;
	}

	/** Replies if the JVM elements are leafs in the outline.
	 *
	 * @param modelElement the model element.
	 * @return {@code true} if it is a leaf, {@code false} otherwise.
	 */
	@SuppressWarnings("static-method")
	protected boolean _isLeaf(JvmIdentifiableElement modelElement) {
		return true;
	}

}

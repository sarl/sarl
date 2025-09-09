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

package io.sarl.lang.ui.labeling;

import java.lang.reflect.Method;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.Diagnostician;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.BaseLabelProvider;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtext.diagnostics.AbstractDiagnostic;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.ui.IImageHelper;
import org.eclipse.xtext.ui.IImageHelper.IImageDescriptorHelper;

/**
 * A decorator that add the diagnostic results on Ecore elements.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 */
@Singleton
@SuppressWarnings("restriction")
public class SARLDiagnosticLabelDecorator extends BaseLabelProvider implements ILabelDecorator {

	private static final Method GET_NODE_METHOD;

	static {
		try {
			GET_NODE_METHOD = AbstractDiagnostic.class.getDeclaredMethod("getNode"); //$NON-NLS-1$
			GET_NODE_METHOD.setAccessible(true);
		} catch (Exception e) {
			throw new Error(e);
		}
	}

	@Inject
	private IImageHelper imageHelper;

	@Inject
	private IImageDescriptorHelper imageDescriptorHelper;

	private static boolean inside(INode node, ICompositeNode parentCandidate) {
		var current = node;
		while (current != null) {
			if (current.equals(parentCandidate)) {
				return true;
			}
			current = current.getParent();
		}
		return false;
	}

	/** Replies the image that corresponds to the given object.
	 *
	 * @param imageDescription
	 *            a {@link String}, an {@link ImageDescriptor} or an {@link Image}
	 * @return the {@link Image} associated with the description or {@code null}
	 */
	protected Image convertToImage(Object imageDescription) {
		if (imageDescription instanceof Image cvalue) {
			return cvalue;
		} else if (imageDescription instanceof ImageDescriptor cvalue) {
			return this.imageHelper.getImage(cvalue);
		} else if (imageDescription instanceof String cvalue) {
			return this.imageHelper.getImage(cvalue);
		}
		return null;
	}

	private static boolean hasParserIssue(ICompositeNode node, Iterable<Resource.Diagnostic> issues) {
		for (final var resourceDiagnostic : issues) {
			if (resourceDiagnostic instanceof AbstractDiagnostic diag) {
				final INode diagNode;
				try {
					diagNode = (INode) GET_NODE_METHOD.invoke(diag);
				} catch (Exception e) {
					throw new Error(e);
				}
				if (inside(diagNode, node)) {
					return true;
				}
			}
        }
		return false;
	}

	/** Replies the diagnotic adornment for the given element.
	 *
	 * @param element the model element.
	 * @return the adornment.
	 */
	@SuppressWarnings("static-method")
	protected int getIssueAdornment(XtendMember element) {
		final var node = NodeModelUtils.getNode(element);
		if (node == null) {
			return 0;
		}
		// Error markers are more important than warning markers.
		// Order of checks:
		// - parser error (from the resource) or semantic error (from Diagnostician)
		// - parser warning or semantic warning
		final var resource = element.eResource();
		if (!resource.getURI().isArchive()) {
			if (hasParserIssue(node, resource.getErrors())) {
				return JavaElementImageDescriptor.ERROR;
			}
			final var diagnostic = Diagnostician.INSTANCE.validate(element);
	        switch (diagnostic.getSeverity()) {
	        case Diagnostic.ERROR:
	        	return JavaElementImageDescriptor.ERROR;
	        case Diagnostic.WARNING:
	        	return JavaElementImageDescriptor.WARNING;
	        default:
	        }
			if (hasParserIssue(node, resource.getWarnings())) {
				return JavaElementImageDescriptor.WARNING;
			}
		}
		return 0;
	}

	/**Replies the size of the images.
	 *
	 * @return {@link JavaElementImageProvider#SMALL_SIZE} or {@link JavaElementImageProvider#BIG_SIZE}
	 */
	@SuppressWarnings("static-method")
	protected Point imagesSize() {
		return JavaElementImageProvider.BIG_SIZE;
	}

	@Override
	public Image decorateImage(Image image, Object element) {
		if (element instanceof XtendMember cvalue) {
			final var adornment = getIssueAdornment(cvalue);
			if (adornment != 0) {
				final var descriptor = this.imageDescriptorHelper.getImageDescriptor(image);
				final var newDescriptor = new JavaElementImageDescriptor(descriptor, adornment, imagesSize());
				return convertToImage(newDescriptor);
			}
		}
		return image;
	}

	@Override
	public String decorateText(String text, Object element) {
		return text;
	}

}

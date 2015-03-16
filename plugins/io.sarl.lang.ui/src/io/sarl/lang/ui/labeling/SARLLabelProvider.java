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
package io.sarl.lang.ui.labeling;

import java.util.Collections;
import java.util.concurrent.locks.ReentrantLock;

import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.ActionSignature;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.Behavior;
import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.CapacityUses;
import io.sarl.lang.sarl.Constructor;
import io.sarl.lang.sarl.Event;
import io.sarl.lang.sarl.RequiredCapacity;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.ui.images.SARLImages;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.util.Exceptions;
import org.eclipse.xtext.util.PolymorphicDispatcher;
import org.eclipse.xtext.util.PolymorphicDispatcher.ErrorHandler;
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider;
import org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.ui.labeling.XbaseLabelProvider;
import org.eclipse.xtext.xbase.validation.UIStrings;

import com.google.common.base.Strings;
import com.google.inject.Inject;

/**
 * Provides labels for a EObjects.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#labelProvider"
 */
public class SARLLabelProvider extends XbaseLabelProvider {

	/** Max length of the text for the behavior units.
	 */
	public static final int BEHAVIOR_UNIT_TEXT_LENGTH = 7;

	@Inject
	private UIStrings uiStrings;

	@Inject
	private OperatorMapping operatorMapping;

	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private ILogicalContainerProvider logicalContainerProvider;

	@Inject
	private SARLImages images;

	private final PolymorphicDispatcher<ImageDescriptor> imageDescriptorDispatcher;
	private final ReentrantLock imageDescriptorLock = new ReentrantLock();

	/**
	 * @param delegate - the original provider.
	 */
	@Inject
	public SARLLabelProvider(AdapterFactoryLabelProvider delegate) {
		super(delegate);
		this.imageDescriptorDispatcher = new PolymorphicDispatcher<>(
				"imageDescriptor", //$NON-NLS-1$
				1, 1,
				Collections.singletonList(this),
				new ErrorHandler<ImageDescriptor>() {
					@Override
					public ImageDescriptor handle(Object[] params, Throwable e) {
						return handleImageDescriptorError(params, e);
					}
				});
	}

	/** Get the image descriptor for the given element.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 */
	protected ImageDescriptor doGetImageDescriptor(Object element) {
		return this.imageDescriptorDispatcher.invoke(element);
	}

	/** Invoked when an image descriptor cannot be found.
	 *
	 * @param params - the parameters given to the method polymorphic dispatcher.
	 * @param e - the cause of the error.
	 * @return the image descriptor for the error.
	 */
	protected ImageDescriptor handleImageDescriptorError(Object[] params, Throwable e) {
		if (e instanceof NullPointerException) {
			Object o = getDefaultImage();
			if (o instanceof ImageDescriptor) {
				return (ImageDescriptor) o;
			}
			if (o instanceof Image) {
				return ImageDescriptor.createFromImage((Image) o);
			}
			return super.imageDescriptor(params[0]);
		}
		return Exceptions.throwUncheckedException(e);
	}

	/** Create a string representation of a signature without the return type.
	 *
	 * @param simpleName - the action name.
	 * @param element - the executable element.
	 * @return the signature.
	 */
	protected StyledString signatureWithoutReturnType(StyledString simpleName, JvmExecutable element) {
		return simpleName.append(this.uiStrings.parameters(element));
	}

	/** Create a string representation of the given element.
	 *
	 * @param reference - the element.
	 * @return the string representation.
	 */
	protected StyledString getHumanReadableName(JvmTypeReference reference) {
		if (reference == null) {
			return new StyledString("Object"); //$NON-NLS-1$
		}
		String name = this.uiStrings.referenceToString(reference, "Object"); //$NON-NLS-1$
		//
		// FIXME: https://bugs.eclipse.org/bugs/show_bug.cgi?id=443131
		JvmType type = reference.getType();
		if (type != null && type.eIsProxy() && reference.eResource() != null) {
			// This case occurs when the reference is unknown:
			// the found "name" is the fully qualified name of the type.
			// So we should extract the simple name
			int index = name.length() - 1;
			char dot = '.';
			char doll = '$';
			char dies = '#';
			char ch;
			while (index >= 0) {
				ch = name.charAt(index);
				if (ch == dot || ch == doll || ch == dies) {
					name = name.substring(index + 1);
					// break the loop
					index = -1;
				} else {
					index--;
				}
			}
		}
		// END OF FIX
		//
		return convertToStyledString(name);
	}

	/** Replies the JVM element of the given type associated to the given SARL element.
	 *
	 * @param <T> - the type of the JVM element to search for.
	 * @param element - the SARL element.
	 * @param type - the type of the JVM element to search for.
	 * @return the JVM element, or <code>null</code> if not found.
	 */
	protected <T> T getJvmElement(EObject element, Class<T> type) {
		for (EObject obj : this.services.getJvmModelAssociations().getJvmElements(element)) {
			if (type.isInstance(obj)) {
				return type.cast(obj);
			}
		}
		return null;
	}

	// Descriptors

	@Override
	protected ImageDescriptor imageDescriptor(Object element) {
		if (this.imageDescriptorLock.isLocked()) {
			return super.imageDescriptor(element);
		}
		this.imageDescriptorLock.lock();
		try {
			return doGetImageDescriptor(element);
		} finally {
			this.imageDescriptorLock.unlock();
		}
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(Package element) {
		// Mostly used by the outline
		return this.images.forPackage();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlScript element) {
		return this.images.forFile();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(Agent element) {
		return this.images.forAgent();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(Event element) {
		return this.images.forEvent();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(Capacity element) {
		return this.images.forCapacity();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(Skill element) {
		return this.images.forSkill();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(Behavior element) {
		return this.images.forBehavior();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(Attribute element) {
		return this.images.forAttribute(element.isWriteable());
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(Constructor element) {
		return this.images.forConstructor(JvmVisibility.PUBLIC, 0);
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(Action element) {
		return this.images.forAction();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(ActionSignature element) {
		return this.images.forActionSignature();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(CapacityUses element) {
		return this.images.forCapacityUses();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(RequiredCapacity element) {
		return this.images.forCapacityRequirements();
	}

	/** Replies the image for the given element.
	 *
	 * This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element - the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(BehaviorUnit element) {
		return this.images.forBehaviorUnit();
	}

	// Texts

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(JvmTypeReference element) {
		return getHumanReadableName(element);
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(SarlScript element) {
		return convertToStyledString(element.eResource().getURI().trimFileExtension().lastSegment());
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(Agent element) {
		return convertToStyledString(element.getName());
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(Event element) {
		return convertToStyledString(element.getName());
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(Capacity element) {
		return convertToStyledString(element.getName());
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(Skill element) {
		return convertToStyledString(element.getName());
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(Behavior element) {
		return convertToStyledString(element.getName());
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(Attribute element) {
		StyledString label = convertToStyledString(element.getName());
		JvmTypeReference theType = null;
		if (element.getType() != null) {
			theType = element.getType();
		} else {
			JvmField jvmElement = getJvmElement(element, JvmField.class);
			if (jvmElement != null) {
				theType = jvmElement.getType();
			}
		}
		if (theType != null) {
			label.append(" : " + getHumanReadableName(theType), StyledString.COUNTER_STYLER); //$NON-NLS-1$
		}
		return label;
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(Constructor element) {
		JvmIdentifiableElement container = this.logicalContainerProvider.getNearestLogicalContainer(element);
		StyledString name;
		if (container != null) {
			name = convertToStyledString(container.getSimpleName());
		} else {
			name = new StyledString("new", StyledString.DECORATIONS_STYLER); //$NON-NLS-1$
		}
		JvmExecutable jvmElement = getJvmElement(element, JvmExecutable.class);
		if (jvmElement != null) {
			return signatureWithoutReturnType(name, jvmElement);
		}
		return signatureWithoutReturnType(name, jvmElement);
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(Action element) {
		String simpleName = element.getName();
		if (simpleName != null) {
			QualifiedName qnName = QualifiedName.create(simpleName);
			QualifiedName operator = this.operatorMapping.getOperator(qnName);
			if (operator != null) {
				StyledString result = signature(operator.getFirstSegment(), getJvmElement(element, JvmExecutable.class));
				result.append(" (" + simpleName + ")", StyledString.COUNTER_STYLER); //$NON-NLS-1$//$NON-NLS-2$
				return result;
			}
		}
		return signature(element.getName(), getJvmElement(element, JvmExecutable.class));
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(ActionSignature element) {
		String simpleName = element.getName();
		if (simpleName != null) {
			QualifiedName qnName = QualifiedName.create(simpleName);
			QualifiedName operator = this.operatorMapping.getOperator(qnName);
			if (operator != null) {
				StyledString result = signature(operator.getFirstSegment(), getJvmElement(element, JvmExecutable.class));
				result.append(" (" + simpleName + ")", StyledString.COUNTER_STYLER); //$NON-NLS-1$//$NON-NLS-2$
				return result;
			}
		}
		return signature(element.getName(), getJvmElement(element, JvmExecutable.class));
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	@SuppressWarnings("static-method")
	protected StyledString text(CapacityUses element) {
		return new StyledString(Messages.SARLLabelProvider_0, StyledString.QUALIFIER_STYLER);
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	@SuppressWarnings("static-method")
	protected StyledString text(RequiredCapacity element) {
		return new StyledString(Messages.SARLLabelProvider_1, StyledString.QUALIFIER_STYLER);
	}

	/** Replies the text for the given element.
	 *
	 * @param element - the element.
	 * @return the text.
	 */
	protected StyledString text(BehaviorUnit element) {
		StyledString s = new StyledString("on ", StyledString.DECORATIONS_STYLER); //$NON-NLS-1$
		s.append(getHumanReadableName(element.getName()));
		if (element.getGuard() != null) {
			String txt = null;
			ICompositeNode node = NodeModelUtils.getNode(element.getGuard());
			if (node != null) {
				txt = node.getText().trim();
			}
			if (Strings.isNullOrEmpty(txt)) {
				txt = "[" + Messages.SARLLabelProvider_2 + "]"; //$NON-NLS-1$//$NON-NLS-2$
			} else {
				assert (txt != null);
				String dots = "..."; //$NON-NLS-1$
				if (txt.length() > BEHAVIOR_UNIT_TEXT_LENGTH + dots.length()) {
					txt = "[" + txt.substring(0, BEHAVIOR_UNIT_TEXT_LENGTH) + dots + "]"; //$NON-NLS-1$//$NON-NLS-2$
				} else {
					txt = "[" + txt + "]"; //$NON-NLS-1$//$NON-NLS-2$
				}
			}
			s.append(" "); //$NON-NLS-1$
			s.append(txt, StyledString.DECORATIONS_STYLER);
		}
		return s;
	}

}

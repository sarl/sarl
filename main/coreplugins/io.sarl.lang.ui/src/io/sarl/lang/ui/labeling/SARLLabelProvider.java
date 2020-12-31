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

package io.sarl.lang.ui.labeling;

import java.util.Collections;
import java.util.concurrent.locks.ReentrantLock;
import javax.inject.Singleton;

import com.google.common.base.Strings;
import com.google.inject.Inject;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtend.core.jvmmodel.IXtendJvmAssociations;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.ide.labeling.XtendLabelProvider;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmEnumerationType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.util.Exceptions;
import org.eclipse.xtext.util.PolymorphicDispatcher;
import org.eclipse.xtext.util.PolymorphicDispatcher.ErrorHandler;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.ui.labeling.XbaseImageAdornments;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.typesystem.InheritanceHelper;
import io.sarl.lang.ui.validation.SARLUIStrings;

/**
 * Provides labels for a EObjects.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#label-provider"
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
@Singleton
public class SARLLabelProvider extends XtendLabelProvider implements IQualifiedNameImageProvider {

	/** Max length of the text for the behavior units.
	 */
	public static final int BEHAVIOR_UNIT_TEXT_LENGTH = 7;

	@Inject
	private SARLUIStrings uiStrings;

	@Inject
	private OperatorMapping operatorMapping;

	@Inject
	private IXtendJvmAssociations jvmModelAssociations;

	@Inject
	private SARLImages images;

	@Inject
	private XbaseImageAdornments adornments;

	private final PolymorphicDispatcher<ImageDescriptor> imageDescriptorDispatcher;

	private final ReentrantLock imageDescriptorLock = new ReentrantLock();

	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Inject
	private InheritanceHelper inheritanceHelper;

	/** Constructor.
	 * @param delegate the original provider.
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
					public ImageDescriptor handle(Object[] params, Throwable exception) {
						return handleImageDescriptorError(params, exception);
					}
				});
	}

	/** Get the image descriptor for the given element.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 */
	protected ImageDescriptor doGetImageDescriptor(Object element) {
		return this.imageDescriptorDispatcher.invoke(element);
	}

	/** Invoked when an image descriptor cannot be found.
	 *
	 * @param params the parameters given to the method polymorphic dispatcher.
	 * @param exception the cause of the error.
	 * @return the image descriptor for the error.
	 */
	protected ImageDescriptor handleImageDescriptorError(Object[] params, Throwable exception) {
		if (exception instanceof NullPointerException) {
			final Object defaultImage = getDefaultImage();
			if (defaultImage instanceof ImageDescriptor) {
				return (ImageDescriptor) defaultImage;
			}
			if (defaultImage instanceof Image) {
				return ImageDescriptor.createFromImage((Image) defaultImage);
			}
			return super.imageDescriptor(params[0]);
		}
		return Exceptions.throwUncheckedException(exception);
	}

	/** Create a string representation of a signature without the return type.
	 *
	 * @param simpleName the action name.
	 * @param element the executable element.
	 * @return the signature.
	 */
	protected StyledString signatureWithoutReturnType(StyledString simpleName, JvmExecutable element) {
		return simpleName.append(this.uiStrings.styledParameters(element));
	}

	@Override
	protected StyledString signature(String simpleName, JvmIdentifiableElement element) {
		final JvmTypeReference returnType;
		if (element instanceof JvmOperation) {
			returnType = ((JvmOperation) element).getReturnType();
		} else if (element instanceof JvmField) {
			returnType = ((JvmField) element).getType();
		} else {
			returnType = null;
		}
		final StandardTypeReferenceOwner owner = new StandardTypeReferenceOwner(this.services, element);
		final String returnTypeString = (returnType == null) ? this.keywords.getVoidKeyword()
			: owner.toLightweightTypeReference(returnType).getHumanReadableName();
		String decoratedPart = " : " + returnTypeString; //$NON-NLS-1$
		final String typeParam = Strings.nullToEmpty(this.uiStrings.typeParameters(element));
		if (!Strings.isNullOrEmpty(typeParam)) {
			decoratedPart = " " + typeParam + " : " + returnTypeString; //$NON-NLS-1$ //$NON-NLS-2$
		}
		final StyledString str = new StyledString();
		str.append(simpleName);
		str.append(this.uiStrings.styledParameters(element));
		str.append(decoratedPart, StyledString.DECORATIONS_STYLER);
		return str;
	}

	/** Create a string representation of the given element.
	 *
	 * @param reference the element.
	 * @return the string representation.
	 */
	protected StyledString getHumanReadableName(JvmTypeReference reference) {
		if (reference == null) {
			return new StyledString("Object"); //$NON-NLS-1$
		}
		final String name = this.uiStrings.referenceToString(reference, "Object"); //$NON-NLS-1$
		return convertToStyledString(name);
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
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(Package element) {
		// Mostly used by the outline
		return this.images.forPackage();
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlScript element) {
		return this.images.forFile();
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlAgent element) {
		final JvmDeclaredType jvmElement = this.jvmModelAssociations.getInferredType(element);
		return this.images.forAgent(
				element.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlEvent element) {
		final JvmDeclaredType jvmElement = this.jvmModelAssociations.getInferredType(element);
		return this.images.forEvent(
				element.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlCapacity element) {
		final JvmDeclaredType jvmElement = this.jvmModelAssociations.getInferredType(element);
		return this.images.forCapacity(
				element.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlSkill element) {
		final JvmDeclaredType jvmElement = this.jvmModelAssociations.getInferredType(element);
		return this.images.forSkill(
				element.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlBehavior element) {
		final JvmDeclaredType jvmElement = this.jvmModelAssociations.getInferredType(element);
		return this.images.forBehavior(
				element.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlField element) {
		return this.images.forField(
				element.getVisibility(),
				this.adornments.get(this.jvmModelAssociations.getJvmField(element)));
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlConstructor element) {
		if (element.isStatic()) {
			return this.images.forStaticConstructor();
		}
		return this.images.forConstructor(
				element.getVisibility(),
				this.adornments.get(this.jvmModelAssociations.getInferredConstructor(element)));
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlAction element) {
		final JvmOperation jvmElement = this.jvmModelAssociations.getDirectlyInferredOperation(element);
		return this.images.forOperation(
				element.getVisibility(),
				this.adornments.get(jvmElement));
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlCapacityUses element) {
		return this.images.forCapacityUses();
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlRequiredCapacity element) {
		return this.images.forCapacityRequirements();
	}

	/** Replies the image for the given element.
	 *
	 * <p>This function is a Xtext dispatch function for {@link #imageDescriptor(Object)}.
	 *
	 * @param element the element.
	 * @return the image descriptor.
	 * @see #imageDescriptor(Object)
	 */
	protected ImageDescriptor imageDescriptor(SarlBehaviorUnit element) {
		return this.images.forBehaviorUnit();
	}

	// Texts

	/** Replies the text for the given element.
	 *
	 * @param element the element.
	 * @return the text.
	 */
	protected StyledString text(JvmTypeReference element) {
		return getHumanReadableName(element);
	}

	/** Replies the text for the given element.
	 *
	 * @param element the element.
	 * @return the text.
	 */
	protected StyledString text(SarlAgent element) {
		return convertToStyledString(element.getName());
	}

	/** Replies the text for the given element.
	 *
	 * @param element the element.
	 * @return the text.
	 */
	protected StyledString text(SarlEvent element) {
		return convertToStyledString(element.getName());
	}

	/** Replies the text for the given element.
	 *
	 * @param element the element.
	 * @return the text.
	 */
	protected StyledString text(SarlCapacity element) {
		return convertToStyledString(element.getName());
	}

	/** Replies the text for the given element.
	 *
	 * @param element the element.
	 * @return the text.
	 */
	protected StyledString text(SarlSkill element) {
		return convertToStyledString(element.getName());
	}

	/** Replies the text for the given element.
	 *
	 * @param element the element.
	 * @return the text.
	 */
	protected StyledString text(SarlBehavior element) {
		return convertToStyledString(element.getName());
	}

	/** Replies the text for the given element.
	 *
	 * @param element the element.
	 * @return the text.
	 */
	protected StyledString text(SarlAction element) {
		final JvmIdentifiableElement jvmElement = this.jvmModelAssociations.getDirectlyInferredOperation(element);
		final String simpleName = element.getName();
		if (simpleName != null) {
			final QualifiedName qnName = QualifiedName.create(simpleName);
			final QualifiedName operator = this.operatorMapping.getOperator(qnName);
			if (operator != null) {
				final StyledString result = signature(operator.getFirstSegment(), jvmElement);
				result.append(" (" + simpleName + ")", StyledString.COUNTER_STYLER); //$NON-NLS-1$//$NON-NLS-2$
				return result;
			}
		}
		return signature(element.getName(), jvmElement);
	}

	/** Replies the text for the given element.
	 *
	 * @param element the element.
	 * @return the text.
	 */
	@SuppressWarnings("static-method")
	protected StyledString text(SarlCapacityUses element) {
		return new StyledString(Messages.SARLLabelProvider_0, StyledString.QUALIFIER_STYLER);
	}

	/** Replies the text for the given element.
	 *
	 * @param element the element.
	 * @return the text.
	 */
	@SuppressWarnings("static-method")
	protected StyledString text(SarlRequiredCapacity element) {
		return new StyledString(Messages.SARLLabelProvider_1, StyledString.QUALIFIER_STYLER);
	}

	/** Replies the text for the given element.
	 *
	 * @param element the element.
	 * @return the text.
	 */
	protected StyledString text(SarlBehaviorUnit element) {
		final StyledString text = new StyledString("on ", StyledString.DECORATIONS_STYLER); //$NON-NLS-1$
		text.append(getHumanReadableName(element.getName()));
		if (element.getGuard() != null) {
			String txt = null;
			final ICompositeNode node = NodeModelUtils.getNode(element.getGuard());
			if (node != null) {
				txt = node.getText().trim();
			}
			if (Strings.isNullOrEmpty(txt)) {
				txt = "[" + Messages.SARLLabelProvider_2 + "]"; //$NON-NLS-1$//$NON-NLS-2$
			} else {
				assert txt != null;
				final String dots = "..."; //$NON-NLS-1$
				if (txt.length() > BEHAVIOR_UNIT_TEXT_LENGTH + dots.length()) {
					txt = "[" + txt.substring(0, BEHAVIOR_UNIT_TEXT_LENGTH) + dots + "]"; //$NON-NLS-1$//$NON-NLS-2$
				} else {
					txt = "[" + txt + "]"; //$NON-NLS-1$//$NON-NLS-2$
				}
			}
			text.append(" "); //$NON-NLS-1$
			text.append(txt, StyledString.DECORATIONS_STYLER);
		}
		return text;
	}

	@Override
	protected String text(XtendConstructor element) {
		if (element.isStatic()) {
			return Messages.SARLLabelProvider_3;
		}
		return super.text(element);
	}

	@Override
	protected String text(XVariableDeclaration variableDeclaration) {
		final IResolvedTypes resolvedTypes = getTypeResolver().resolveTypes(variableDeclaration);
		final LightweightTypeReference type = resolvedTypes.getActualType((JvmIdentifiableElement) variableDeclaration);
		if (type != null) {
			return variableDeclaration.getName() + " " + this.keywords.getColonKeyword() //$NON-NLS-1$
				+ " " + type.getHumanReadableName(); //$NON-NLS-1$
		}
		return variableDeclaration.getName();
	}

	@Override
	public Image getImageForQualifiedName(String qualifiedName, Notifier context, IJvmTypeProvider jvmTypeProvider) {
		return convertToImage(getImageDescriptorForQualifiedName(qualifiedName, context, jvmTypeProvider));
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
	@Override
	public ImageDescriptor getImageDescriptorForQualifiedName(String qualifiedName, Notifier context,
			IJvmTypeProvider typeProvider) {
		JvmType type = null;
		if (typeProvider != null) {
			type = typeProvider.findTypeByName(qualifiedName);
		}
		if (type == null && context != null) {
			type = this.services.getTypeReferences().findDeclaredType(qualifiedName, context);
		}
		int adornments = this.adornments.get(type);
		JvmVisibility visibility = JvmVisibility.DEFAULT;
		if (type != null) {
			if (type.eClass() == TypesPackage.Literals.JVM_GENERIC_TYPE) {
				final JvmGenericType gtype = (JvmGenericType) type;
				visibility = gtype.getVisibility();
				final int ecoreCode = this.inheritanceHelper.getSarlElementEcoreType(gtype);
				switch (ecoreCode) {
				case SarlPackage.SARL_AGENT:
					return this.images.forAgent(visibility, this.adornments.get(gtype));
				case SarlPackage.SARL_BEHAVIOR:
					return this.images.forBehavior(visibility, this.adornments.get(gtype));
				case SarlPackage.SARL_CAPACITY:
					// Remove the "abstract" ornment because capacities are always abstract.
					adornments = (adornments & JavaElementImageDescriptor.ABSTRACT) ^ adornments;
					return this.images.forCapacity(visibility, adornments);
				case SarlPackage.SARL_EVENT:
					return this.images.forEvent(visibility, this.adornments.get(gtype));
				case SarlPackage.SARL_SKILL:
					return this.images.forSkill(visibility, this.adornments.get(gtype));
				default:
					if (gtype.isInterface()) {
						return this.images.forInterface(visibility, this.adornments.get(gtype));
					}
				}
			} else if (type.eClass() == TypesPackage.Literals.JVM_ENUMERATION_TYPE) {
				final JvmEnumerationType etype = (JvmEnumerationType) type;
				visibility = etype.getVisibility();
				return this.images.forEnum(visibility, adornments);
			} else if (type.eClass() == TypesPackage.Literals.JVM_ANNOTATION_TYPE) {
				final JvmAnnotationType atype = (JvmAnnotationType) type;
				visibility = atype.getVisibility();
				return this.images.forAnnotation(visibility, adornments);
			} else {
				visibility = JvmVisibility.DEFAULT;
			}
		}
		// Default icon is the class icon.
		return this.images.forClass(visibility, adornments);
	}

}

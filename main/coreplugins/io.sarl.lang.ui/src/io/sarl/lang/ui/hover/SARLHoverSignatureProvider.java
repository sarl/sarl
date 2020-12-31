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

package io.sarl.lang.ui.hover;

import java.text.MessageFormat;
import javax.inject.Inject;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.ide.hover.XtendHoverSignatureProvider;
import org.eclipse.xtext.common.types.JvmAnyTypeReference;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.typesystem.references.ITypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlArtifact;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCastedExpression;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/**
 * Provider of signatures for hovers.
 *
 * <p>This class extends the standard Xtend serializer by replacing the example of code, written
 * in Java or Xtend, by the same example with the SARL syntax.
 *
 * <p>This class enables the hovers on the XCastedExpression.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public class SARLHoverSignatureProvider extends XtendHoverSignatureProvider {

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Override
	public String getSignature(EObject object) {
		return internalGetSignature(object, true);
	}

	/** Replies the signature for a SARL agent.
	 *
	 * @param agent the SARL element.
	 * @param typeAtEnd ignored
	 * @return the signature
	 */
	@SuppressWarnings("static-method")
	protected String _signature(SarlAgent agent, boolean typeAtEnd) {
		return agent.getName();
	}

	/** Replies the signature for a SARL behavior.
	 *
	 * @param behavior the SARL element.
	 * @param typeAtEnd ignored
	 * @return the signature
	 */
	@SuppressWarnings("static-method")
	protected String _signature(SarlBehavior behavior, boolean typeAtEnd) {
		return behavior.getName();
	}

	/** Replies the signature for a SARL capacity.
	 *
	 * @param capacity the SARL element.
	 * @param typeAtEnd ignored
	 * @return the signature
	 */
	@SuppressWarnings("static-method")
	protected String _signature(SarlCapacity capacity, boolean typeAtEnd) {
		return capacity.getName();
	}

	/** Replies the signature for a SARL skill.
	 *
	 * @param skill the SARL element.
	 * @param typeAtEnd ignored
	 * @return the signature
	 */
	@SuppressWarnings("static-method")
	protected String _signature(SarlSkill skill, boolean typeAtEnd) {
		return skill.getName();
	}

	/** Replies the signature for a SARL event.
	 *
	 * @param event the SARL element.
	 * @param typeAtEnd ignored
	 * @return the signature
	 */
	@SuppressWarnings("static-method")
	protected String _signature(SarlEvent event, boolean typeAtEnd) {
		return event.getName();
	}

	/** Replies the signature for a SARL space.
	 *
	 * @param space the SARL element.
	 * @param typeAtEnd ignored
	 * @return the signature
	 */
	@SuppressWarnings("static-method")
	protected String _signature(SarlSpace space, boolean typeAtEnd) {
		return space.getName();
	}

	/** Replies the signature for a SARL artifact.
	 *
	 * @param artifact the SARL element.
	 * @param typeAtEnd ignored
	 * @return the signature
	 */
	@SuppressWarnings("static-method")
	protected String _signature(SarlArtifact artifact, boolean typeAtEnd) {
		return artifact.getName();
	}

	@Override
	protected String _signature(XtendField field, boolean typeAtEnd) {
		if (field.getName() == null && field.isExtension()) {
			return getTypeName(field.getType());
		}
		final JvmField jvmField = this.associations.getJvmField(field);
		if (jvmField != null) {
			return _signature(jvmField, typeAtEnd);
		}
		if (field.getName() == null) {
			return ""; //$NON-NLS-1$
		}
		return field.getName();
	}

	@Override
	protected String _signature(XtendFunction function, boolean typeAtEnd) {
		final JvmOperation inferredOperation = this.associations.getDirectlyInferredOperation(function);
		if (inferredOperation != null) {
			return _signature(inferredOperation, typeAtEnd);
		}
		return function.getName() + this.keywords.getLeftParenthesisKeyword() + this.keywords.getRightParenthesisKeyword();
	}

	@Override
	protected String _signature(JvmConstructor constructor, boolean typeAtEnd) {
		return this.keywords.getNewKeyword() + this.hoverUiStrings.typeParameters(constructor.getDeclaringType())
			+ this.hoverUiStrings.parameters(constructor) + getThrowsDeclaration(constructor);
	}

	@Override
	protected String _signature(XtendConstructor constructor, boolean typeAtEnd) {
		final JvmConstructor inferredConstructor = this.associations.getInferredConstructor(constructor);
		return _signature(inferredConstructor, typeAtEnd);
	}

	@Override
	protected String _signature(XAbstractFeatureCall featureCall, boolean typeAtEnd) {
		final JvmIdentifiableElement feature = featureCall.getFeature();
		if (feature != null) {
			return internalGetSignature(feature, typeAtEnd);
		}
		return ""; //$NON-NLS-1$
	}

	@Override
	protected String _signature(XConstructorCall featureCall, boolean typeAtEnd) {
		final JvmIdentifiableElement feature = featureCall.getConstructor();
		if (feature != null) {
			return internalGetSignature(feature, typeAtEnd);
		}
		return ""; //$NON-NLS-1$
	}

	@Override
	protected String _signature(JvmOperation jvmOperation, boolean typeAtEnd) {
		String returnTypeString = this.keywords.getVoidKeyword();
		final JvmTypeReference returnType = jvmOperation.getReturnType();
		if (returnType != null) {
			if (returnType instanceof JvmAnyTypeReference) {
				throw new IllegalStateException();
			}
			returnTypeString = returnType.getSimpleName();
		}

		final String signature = jvmOperation.getSimpleName() + this.hoverUiStrings.parameters(jvmOperation);
		final String postSignature = getThrowsDeclaration(jvmOperation);
		final String typeParameter = this.uiStrings.typeParameters(jvmOperation.getTypeParameters());
		if (typeParameter != null && typeParameter.length() > 0) {
			if (typeAtEnd) {
				return signature + " " + typeParameter + " " //$NON-NLS-1$ //$NON-NLS-2$
					+ this.keywords.getColonKeyword() + " " + returnTypeString + postSignature; //$NON-NLS-1$
			}
			return typeParameter + " " + returnTypeString + " " + signature + postSignature; //$NON-NLS-1$ //$NON-NLS-2$
		}
		if (typeAtEnd) {
			return signature + " " + this.keywords.getColonKeyword() + " " + returnTypeString + postSignature; //$NON-NLS-1$ //$NON-NLS-2$
		}
		return returnTypeString + " " + enrichWithDeclarator(signature, jvmOperation) + postSignature; //$NON-NLS-1$
	}

	/** Replies the hover for a SARL casted expression.
	 *
	 * @param castExpression the casted expression.
	 * @param typeAtEnd indicates if the type should be put at end.
	 * @return the string representation into the hover.
	 */
	protected String _signature(XCastedExpression castExpression, boolean typeAtEnd) {
		if (castExpression instanceof SarlCastedExpression) {
			final JvmOperation delegate = ((SarlCastedExpression) castExpression).getFeature();
			if (delegate != null) {
				return _signature(delegate, typeAtEnd);
			}
		}
		return MessageFormat.format(Messages.SARLHoverSignatureProvider_0,
				getTypeName(castExpression.getType()));
	}

	@Override
	public String getImageTag(final EObject object) {
		if (object instanceof SarlCastedExpression) {
			final SarlCastedExpression expr = (SarlCastedExpression) object;
			final JvmOperation delegate = expr.getFeature();
			if (delegate != null) {
				return getImageTag(delegate);
			}
		}
		return super.getImageTag(object);
	}

	/** Replies the type name for the given type.
	 *
	 * @param type the type.
	 * @return the string representation of the given type.
	 */
	protected String getTypeName(JvmType type) {
		if (type != null) {
			if (type instanceof JvmDeclaredType) {
				final ITypeReferenceOwner owner = new StandardTypeReferenceOwner(this.services, type);
				return owner.toLightweightTypeReference(type).getHumanReadableName();
			}
			return type.getSimpleName();
		}
		return Messages.SARLHoverSignatureProvider_1;
	}

}

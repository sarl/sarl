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

package io.sarl.lang.typesystem.cast;

import java.util.List;

import com.google.inject.Singleton;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.scoping.batch.IIdentifiableElementDescription;
import org.eclipse.xtext.xbase.typesystem.internal.AbstractTypeComputationState;
import org.eclipse.xtext.xbase.typesystem.internal.ScopeProviderAccess;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/** Select the functions that should be called in place of a classic cast operator.
 *
 * <p>This selector searches for the functions with:
 * <ul>
 * <li>the prefix text {@code to} for the object types, e.g. {@code toString}; and</li>
 * <li>the post-fix text {@code Value} for the primitive types, e.g. ({@code intValue}.</li>
 * <li>the text after the prefix text or before the post-fix text is the simple name of the return value's type.
 * </ul>
 *
 * <p>This selector also applies the following criteria, assuming {@code E} is the type of the
 * expression to cast and {@code T} is the destination type:
 * <ul>
 * <li>{@code T} is assignable from the the return value's type.</li>
 * <li>{@code E} is assignable from the argument's type.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
@Singleton
public class ObjectAndPrimitiveBasedCastOperationCandidateSelector implements ICastOperationCandidateSelector {

	@Override
	public ISelector prepare(AbstractTypeComputationState state,
			LightweightTypeReference castType, LightweightTypeReference expressionType) {
		return new ObjectAndPrimitiveBasedSelector(state, castType, expressionType);
	}

	/** Select the functions that should be called in place of a classic cast operator.
	 *
	 * <p>This selector searches for the functions with:
	 * <ul>
	 * <li>the prefix text {@code to} for the object types, e.g. {@code toString}; and</li>
	 * <li>the post-fix text {@code Value} for the primitive types, e.g. ({@code intValue}.</li>
	 * <li>the text after the prefix text or before the post-fix text is the simple name of the return value's type.
	 * </ul>
	 *
	 * <p>This selector also applies the following criteria, assuming {@code E} is the type of the
	 * expression to cast and {@code T} is the destination type:
	 * <ul>
	 * <li>{@code T} is assignable from the the return value's type.</li>
	 * <li>{@code E} is assignable from the argument's type.</li>
	 * </ul>
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.9
	 */
	protected static class ObjectAndPrimitiveBasedSelector implements ISelector {

		private static final String CLASS_PREFIX = "to"; //$NON-NLS-1$

		private static final String PRIMITIVE_POSTFIX = "Value"; //$NON-NLS-1$

		private final AbstractTypeComputationState state;

		private final LightweightTypeReference castType;

		private final LightweightTypeReference primitiveCastType;

		private final LightweightTypeReference expressionType;

		private final boolean primitiveCast;

		/** Constructor.
		 *
		 * @param state the current state of the type computation.
		 * @param castType the target type.
		 * @param expressionType the type of the expression to cast.
		 */
		protected ObjectAndPrimitiveBasedSelector(AbstractTypeComputationState state,
				LightweightTypeReference castType, LightweightTypeReference expressionType) {
			this.state = state;
			this.castType = castType.getWrapperTypeIfPrimitive();
			this.primitiveCastType = castType.getPrimitiveIfWrapperType();
			this.expressionType = expressionType;
			this.primitiveCast = castType.isPrimitive() || castType.isWrapper();
		}

		/** Validate the simple name for class type.
		 *
		 * @param name the simple name to validate.
		 * @return the type name part of the name, with the first letter upper case. or {@code null} if the name is invalid.
		 */
		@SuppressWarnings("static-method")
		protected String getValidClassSimpleName(String name) {
			if (name.startsWith(CLASS_PREFIX)) {
				final String nm = Strings.toFirstUpper(name.substring(CLASS_PREFIX.length()));
				if (!Strings.isEmpty(nm)) {
					return nm;
				}
			}
			return null;
		}

		/** Validate the simple name for primitive type.
		 *
		 * @param name the simple name to validate.
		 * @return the type name part of the name, all lower case. or {@code null} if the name is invalid.
		 */
		protected String getValidPrimitiveSimpleName(String name) {
			if (this.primitiveCast && name.endsWith(PRIMITIVE_POSTFIX)) {
				final String nm = name.substring(0, name.length() - PRIMITIVE_POSTFIX.length()).toLowerCase();
				if (!Strings.isEmpty(nm)) {
					return nm;
				}
			}
			return null;
		}

		/** Validate the return type of the operation.
		 *
		 * @param expectedTypeName the expected simple name of the return type.
		 * @param type the return type.
		 * @return {@code true} if the return type is valid; otherwise {@code false}.
		 */
		protected boolean isValidReturnType(String expectedTypeName, LightweightTypeReference type) {
			if (type != null
					&& (type.isSubtypeOf(this.castType.getType())
					|| (this.primitiveCast && type.isSubtypeOf(this.primitiveCastType.getType())))) {
				return type.getSimpleName().equals(expectedTypeName);
			}
			return false;
		}

		/** Validate the parameters of the operation.
		 *
		 * @param operation the operation from which the parameters are extracted.
		 * @return {@code true} if the return type is valid; otherwise {@code false}.
		 */
		protected boolean isValidParameters(JvmOperation operation) {
			final List<JvmFormalParameter> parameters = operation.getParameters();
			if (parameters.size() == 0) {
				final JvmType originType = operation.getDeclaringType();
				return this.expressionType.isSubtypeOf(originType);
			} else if (parameters.size() == 1) {
				final JvmTypeReference parameterType = parameters.get(0).getParameterType();
				final LightweightTypeReference paramType = this.state.getReferenceOwner().toLightweightTypeReference(parameterType);
				if (parameterType != null) {
					return paramType.isAssignableFrom(this.expressionType);
				}
			}
			return false;
		}

		private boolean validatePrototype(String expectedTypeName, JvmOperation operation) {
			final LightweightTypeReference concreteReturnType = this.state.getResolvedTypes().getActualType(operation);
			if (isValidReturnType(expectedTypeName, concreteReturnType)) {
				return isValidParameters(operation);
			}
			return false;
		}

		@Override
		public boolean isCastOperatorCandidate(IIdentifiableElementDescription description) {
			if (description instanceof ScopeProviderAccess.ErrorDescription || !description.isVisible()) {
				return false;
			}
			final JvmIdentifiableElement operatorFunction = description.getElementOrProxy();
			if (!(operatorFunction instanceof JvmOperation)) {
				return false;
			}
			final JvmOperation executable = (JvmOperation) operatorFunction;
			final String objectTypeName = getValidClassSimpleName(executable.getSimpleName());
			if (objectTypeName != null) {
				return validatePrototype(objectTypeName, executable);
			}
			final String primitiveTypeName = getValidPrimitiveSimpleName(executable.getSimpleName());
			if (primitiveTypeName != null) {
				return validatePrototype(primitiveTypeName, executable);
			}
			return false;
		}

	}

}

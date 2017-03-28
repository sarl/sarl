/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.compiler;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.compiler.XtendCompiler;
import org.eclipse.xtext.common.types.JvmAnnotationAnnotationValue;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationValue;
import org.eclipse.xtext.common.types.JvmBooleanAnnotationValue;
import org.eclipse.xtext.common.types.JvmByteAnnotationValue;
import org.eclipse.xtext.common.types.JvmCharAnnotationValue;
import org.eclipse.xtext.common.types.JvmCustomAnnotationValue;
import org.eclipse.xtext.common.types.JvmDoubleAnnotationValue;
import org.eclipse.xtext.common.types.JvmEnumAnnotationValue;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFloatAnnotationValue;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmIntAnnotationValue;
import org.eclipse.xtext.common.types.JvmLongAnnotationValue;
import org.eclipse.xtext.common.types.JvmShortAnnotationValue;
import org.eclipse.xtext.common.types.JvmStringAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XTypeLiteral;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.util.XExpressionHelper;

import io.sarl.lang.sarl.SarlBreakExpression;


/** The compiler from SARL to the target language.
 *
 * <p>This compiler provide a specific support for inline annotations. Indeed, the Xbase inline evaluation does
 * not support variadic parameters. This SARL compiler provides a support for variadic feature calls.
 *
 * <p>Additionally, this compiler supports the Inline annotation for non-static calls, by skipping the left
 * operand of a member feature call when the inline expression is constant.
 *
 * <p>The compiler supports the SARL keywords too: break.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.4
 */
public class SarlCompiler extends XtendCompiler {

	private static final String INLINE_VARIABLE_PREFIX = "$"; //$NON-NLS-1$

	private static final String INLINE_VALUE_NAME = "value"; //$NON-NLS-1$

	private static final String INLINE_IMPORTED_NAME = "imported"; //$NON-NLS-1$

	private static final String CONSTANT_EXPRESSION_NAME = "constantExpression"; //$NON-NLS-1$

	private static final Pattern INLINE_VARIABLE_PATTERN = Pattern.compile("\\" + INLINE_VARIABLE_PREFIX //$NON-NLS-1$
			+ "(\\" + INLINE_VARIABLE_PREFIX + "|[0-9]+)"); //$NON-NLS-1$ //$NON-NLS-2$

	@Inject
	private XExpressionHelper expressionHelper;

	@Inject
	private IBatchTypeResolver batchTypeResolver;

	@SuppressWarnings({"checkstyle:returncount", "checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity"})
	private static String getAnnotationStringValue(JvmAnnotationValue value) {
		if (value instanceof JvmAnnotationAnnotationValue) {
			return ((JvmAnnotationAnnotationValue) value).getValues().get(0).getAnnotation().getIdentifier();
		}
		if (value instanceof JvmBooleanAnnotationValue) {
			return ((JvmBooleanAnnotationValue) value).getValues().get(0).toString();
		}
		if (value instanceof JvmByteAnnotationValue) {
			return ((JvmByteAnnotationValue) value).getValues().get(0).toString();
		}
		if (value instanceof JvmCharAnnotationValue) {
			return ((JvmCharAnnotationValue) value).getValues().get(0).toString();
		}
		if (value instanceof JvmCustomAnnotationValue) {
			final EObject evalue = ((JvmCustomAnnotationValue) value).getValues().get(0);
			if (evalue instanceof XStringLiteral) {
				return ((XStringLiteral) evalue).getValue();
			}
			if (evalue instanceof XNumberLiteral) {
				return ((XNumberLiteral) evalue).getValue();
			}
			if (evalue instanceof XBooleanLiteral) {
				return ((XNumberLiteral) evalue).getValue();
			}
			if (evalue instanceof XTypeLiteral) {
				return ((XTypeLiteral) evalue).getType().getIdentifier();
			}
		}
		if (value instanceof JvmDoubleAnnotationValue) {
			return ((JvmDoubleAnnotationValue) value).getValues().get(0).toString();
		}
		if (value instanceof JvmEnumAnnotationValue) {
			return ((JvmEnumAnnotationValue) value).getValues().get(0).getSimpleName();
		}
		if (value instanceof JvmFloatAnnotationValue) {
			return ((JvmFloatAnnotationValue) value).getValues().get(0).toString();
		}
		if (value instanceof JvmIntAnnotationValue) {
			return ((JvmIntAnnotationValue) value).getValues().get(0).toString();
		}
		if (value instanceof JvmLongAnnotationValue) {
			return ((JvmLongAnnotationValue) value).getValues().get(0).toString();
		}
		if (value instanceof JvmShortAnnotationValue) {
			return ((JvmShortAnnotationValue) value).getValues().get(0).toString();
		}
		if (value instanceof JvmStringAnnotationValue) {
			return ((JvmStringAnnotationValue) value).getValues().get(0);
		}
		if (value instanceof JvmTypeAnnotationValue) {
			return ((JvmTypeAnnotationValue) value).getValues().get(0).getIdentifier();
		}
		return null;
	}

	private static Collection<JvmTypeReference> getAnnotationTypeValue(JvmAnnotationValue value) {
		if (value instanceof JvmTypeAnnotationValue) {
			return ((JvmTypeAnnotationValue) value).getValues();
		}
		return Collections.emptyList();
	}

	@Override
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	protected synchronized void appendInlineFeatureCall(XAbstractFeatureCall call, ITreeAppendable target) {
		final JvmAnnotationReference inlineAnnotation = this.expressionHelper.findInlineAnnotation(call);

		String formatString = null;
		final List<JvmTypeReference> importedTypes = Lists.newArrayListWithCapacity(2);
		for (final JvmAnnotationValue annotationValue: inlineAnnotation.getValues()) {
			final String valueName = annotationValue.getValueName();
			if (Strings.isNullOrEmpty(valueName)) {
				// Special case: the annotation value as no associated operation.
				// If it appends, we could assumes that the operation is "value()"
				if (!Strings.isNullOrEmpty(formatString)) {
					throw new IllegalStateException();
				}
				formatString = getAnnotationStringValue(annotationValue);
			} else if (INLINE_VALUE_NAME.equals(valueName)) {
				if (!Strings.isNullOrEmpty(formatString)) {
					throw new IllegalStateException();
				}
				formatString = getAnnotationStringValue(annotationValue);
			} else if (INLINE_IMPORTED_NAME.equals(valueName)) {
				importedTypes.addAll(getAnnotationTypeValue(annotationValue));
			}
		}

		if (formatString == null) {
			throw new IllegalStateException();
		}

		final IResolvedTypes resolvedTypes = this.batchTypeResolver.resolveTypes(call);

		final List<XExpression> arguments = getActualArguments(call);
		final JvmIdentifiableElement calledFeature = call.getFeature();
		int numberVariadicParameter = 0;
		final int numberFormalParameters;
		JvmFormalParameter formalVariadicParameter = null;
		if (calledFeature instanceof JvmExecutable) {
			final JvmExecutable jvmexec = (JvmExecutable) calledFeature;
			numberFormalParameters = jvmexec.getParameters().size();
			if (numberFormalParameters > 0) {
				formalVariadicParameter = jvmexec.getParameters().get(numberFormalParameters - 1);
				if (jvmexec.isVarArgs()) {
					numberVariadicParameter = 1;
				}
			}
		} else {
			numberFormalParameters = arguments.size();
		}
		final Matcher matcher = INLINE_VARIABLE_PATTERN.matcher(formatString);
		int prevEnd = 0;


		while (matcher.find()) {
			final int start = matcher.start();
			if (start != prevEnd) {
				target.append(formatString.substring(prevEnd, start));
			}
			final String indexOrDollar = matcher.group(1);
			if (INLINE_VARIABLE_PREFIX.equals(indexOrDollar)) {
				target.append(INLINE_VARIABLE_PREFIX);
			} else {
				final int index = Integer.parseInt(indexOrDollar) - 1;
				final int numberImports = importedTypes.size();
				final int numberFormalParametersImports = numberFormalParameters + numberImports;
				if (numberVariadicParameter != 0 && index < arguments.size() && index == (numberFormalParameters - 1)) {
					XExpression argument = arguments.get(index);
					appendArgument(argument, target, index > 0);
					for (int i = index + 1; i < arguments.size(); ++i) {
						target.append(", "); //$NON-NLS-1$
						argument = arguments.get(i);
						appendArgument(argument, target, true);
					}
				} else if (index > numberFormalParametersImports) {
					final List<LightweightTypeReference> typeArguments = resolvedTypes.getActualTypeArguments(call);
					final LightweightTypeReference typeArgument = typeArguments.get(index - numberFormalParametersImports - 1);
					serialize(typeArgument.getRawTypeReference().toTypeReference(), call, target);
				} else if (index >= numberFormalParameters && index < numberFormalParametersImports) {
					serialize(importedTypes.get(index - numberFormalParameters), call, target);
				} else if (index == numberFormalParametersImports) {
					appendTypeArguments(call, target);
				} else if (index < arguments.size()) {
					final XExpression argument = arguments.get(index);
					appendArgument(argument, target, index > 0);
				} else if (formalVariadicParameter != null) {
					appendNullValue(formalVariadicParameter.getParameterType(), calledFeature, target);
				} else {
					throw new IllegalStateException();
				}
			}
			prevEnd = matcher.end();
		}
		if (prevEnd != formatString.length()) {
			target.append(formatString.substring(prevEnd));
		}
	}

	private static boolean isConstantExpression(JvmAnnotationReference reference) {
		//TODO: Remove when Xtext issue is fixed; https://github.com/eclipse/xtext-extras/issues/43
		for (final JvmAnnotationValue annotationValue: reference.getValues()) {
			if (CONSTANT_EXPRESSION_NAME.equals(annotationValue.getValueName())) {
				return ((JvmBooleanAnnotationValue) annotationValue).getValues().get(0).booleanValue();
			}
		}
		return false;
	}

	@Override
	protected void featureCalltoJavaExpression(final XAbstractFeatureCall call, ITreeAppendable output,
			boolean isExpressionContext) {
		//TODO: Remove when Xtext issue is fixed; https://github.com/eclipse/xtext-extras/issues/43
		if (call instanceof XAssignment) {
			assignmentToJavaExpression((XAssignment) call, output, isExpressionContext);
		} else {
			if (needMultiAssignment(call)) {
				appendLeftOperand(call, output, isExpressionContext).append(" = "); //$NON-NLS-1$
			}

			ITreeAppendable child = output;
			final JvmAnnotationReference annotationRef = this.expressionHelper.findInlineAnnotation(call);
			if (annotationRef == null || !isConstantExpression(annotationRef)) {
				final boolean hasReceiver = appendReceiver(call, output, isExpressionContext);
				if (hasReceiver) {
					output.append("."); //$NON-NLS-1$
					child = appendTypeArguments(call, output);
				}
			}
			appendFeatureCall(call, child);
		}
	}

	@Override
	public void doInternalToJavaStatement(XExpression obj, ITreeAppendable appendable, boolean isReferenced) {
		if (obj instanceof SarlBreakExpression) {
			_toJavaStatement((SarlBreakExpression) obj, appendable, isReferenced);
		} else {
			super.doInternalToJavaStatement(obj, appendable, isReferenced);
		}
	}

	/** Generate the JAva code for the break keyword.
	 *
	 * @param breakExpression the expression.
	 * @param appendable the output.
	 * @param isReferenced indicates if the expression is referenced.
	 */
	@SuppressWarnings("static-method")
	protected void _toJavaStatement(SarlBreakExpression breakExpression, ITreeAppendable appendable, boolean isReferenced) {
		appendable.newLine().append("break;"); //$NON-NLS-1$
	}

}

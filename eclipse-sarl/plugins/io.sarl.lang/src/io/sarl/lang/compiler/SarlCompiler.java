/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.collect.Lists;
import com.google.inject.Inject;
import org.eclipse.xtend.core.compiler.XtendCompiler;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationValue;
import org.eclipse.xtext.common.types.JvmBooleanAnnotationValue;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmStringAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.util.XExpressionHelper;


/** The compiler from SARL to the target language.
 *
 * <p>This compiler provide a specific support for inline annotations. Indeed, the Xbase inline evaluation does
 * not support variadic parameters. This SARL compiler provides a support for variadic feature calls.
 *
 * <p>Additionally, this compiler supports the Inline annotation for non-static calls, by skipping the left
 * operand of a memver feature call when the inline expression is constant.
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

	@Override
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	protected synchronized void appendInlineFeatureCall(XAbstractFeatureCall call, ITreeAppendable target) {
		final JvmAnnotationReference inlineAnnotation = this.expressionHelper.findInlineAnnotation(call);

		String formatString = null;
		final List<JvmTypeReference> importedTypes = Lists.newArrayListWithCapacity(2);
		for (final JvmAnnotationValue annotationValue: inlineAnnotation.getValues()) {
			if (INLINE_VALUE_NAME.equals(annotationValue.getValueName())) {
				formatString = ((JvmStringAnnotationValue) annotationValue).getValues().get(0);
			} else if (INLINE_IMPORTED_NAME.equals(annotationValue.getValueName())) {
				final JvmTypeAnnotationValue typeAnnotationValue = (JvmTypeAnnotationValue) annotationValue;
				importedTypes.addAll(typeAnnotationValue.getValues());
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

}

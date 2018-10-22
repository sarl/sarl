/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.collect.Lists;
import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.compiler.XtendCompiler;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationAnnotationValue;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationValue;
import org.eclipse.xtext.common.types.JvmBooleanAnnotationValue;
import org.eclipse.xtext.common.types.JvmByteAnnotationValue;
import org.eclipse.xtext.common.types.JvmCharAnnotationValue;
import org.eclipse.xtext.common.types.JvmCustomAnnotationValue;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmDoubleAnnotationValue;
import org.eclipse.xtext.common.types.JvmEnumAnnotationValue;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFloatAnnotationValue;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmIntAnnotationValue;
import org.eclipse.xtext.common.types.JvmLongAnnotationValue;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmShortAnnotationValue;
import org.eclipse.xtext.common.types.JvmStringAnnotationValue;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.linking.ILinker;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XCollectionLiteral;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XSetLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XTypeLiteral;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.compiler.IGeneratorConfigProvider;
import org.eclipse.xtext.xbase.compiler.Later;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.featurecalls.IdentifiableSimpleNameProvider;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.util.XExpressionHelper;

import io.sarl.lang.controlflow.ISarlEarlyExitComputer;
import io.sarl.lang.jvmmodel.Messages;
import io.sarl.lang.jvmmodel.SARLJvmModelInferrer;
import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlCastedExpression;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.typesystem.SARLExpressionHelper;


/** The compiler from SARL to the target language.
 *
 * <p>This compiler provide a specific support for inline annotations. Indeed, the Xbase inline evaluation does
 * not support variadic parameters. This SARL compiler provides a support for variadic feature calls.
 *
 * <p>Additionally, this compiler supports the Inline annotation for non-static calls, by skipping the left
 * operand of a member feature call when the inline expression is constant.
 * See https://github.com/eclipse/xtext-extras/pull/62.
 *
 * <p>This compiler supports also the "$0" parameter in inline expression. This parameter represents the
 * current receiver, e.g. "this.".
 *
 * <p>The compiler supports the SARL keywords: break.
 *
 * <p>This compiler catches exceptions when generating statements for expressions in order to let the compiler
 * to generate as much as possible.
 *
 * <p>The compiler adds a return statement when the early exit statement in SARL is not an early exist statement
 * in Java. In this case a Java "return" statement must be added impliclitly.
 *
 * <p>The roles of the different generation tools are:<ul>
 * <li>{@link SARLJvmModelInferrer}: Generating the expected Java Ecore model from the SARL Ecore model.</li>
 * <li>{@link ILinker}: Create links among the SARL Ecore models.<li>
 * <li>{@link SARLJvmGenerator}: Generate the Java code from the Java Ecore model.</li>
 * <li>{@link SarlCompiler}: Generate the Java code for the XExpression objects.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.4
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public class SarlCompiler extends XtendCompiler {

	private static final String INLINE_VARIABLE_PREFIX = "$"; //$NON-NLS-1$

	private static final String INLINE_VALUE_NAME = "value"; //$NON-NLS-1$

	private static final String INLINE_IMPORTED_NAME = "imported"; //$NON-NLS-1$

	private static final Pattern INLINE_VARIABLE_PATTERN = Pattern.compile("\\" + INLINE_VARIABLE_PREFIX //$NON-NLS-1$
			+ "(\\" + INLINE_VARIABLE_PREFIX + "|[0-9]+)"); //$NON-NLS-1$ //$NON-NLS-2$

	@Inject
	private Logger log;

	@Inject
	private XExpressionHelper expressionHelper;

	@Inject
	private IBatchTypeResolver batchTypeResolver;

	@Inject
	private IGeneratorConfigProvider generatorConfigProvider;

	@Inject
	private SARLExpressionHelper sarlExpressionHelper;

	@Inject
	private ISarlEarlyExitComputer earlyExit;

	@Inject
	private IdentifiableSimpleNameProvider featureNameProvider;

	private volatile boolean isOnJavaEarlyExit;

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
		// Overridden for fixing the @Inline behavior
		appendInlineFeatureCall(call, call, target);
	}

	/** Append the inline version for the given call.
	 *
	 * <p>This function supports the specific semantic of the inline expression that is defined into the SARL specification.
	 *
	 * @param featureCall the feature to call.
	 * @param context the context for finding types.
	 * @param target the receiver.
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	protected synchronized void appendInlineFeatureCall(XAbstractFeatureCall featureCall,
			EObject context, ITreeAppendable target) {
		// Overridden for fixing the @Inline behavior
		final JvmAnnotationReference inlineAnnotation = this.expressionHelper.findInlineAnnotation(featureCall);

		String formatString = null;
		final List<JvmTypeReference> importedTypes = Lists.newArrayListWithCapacity(2);
		for (final JvmAnnotationValue annotationValue: inlineAnnotation.getValues()) {
			final String valueName = annotationValue.getValueName();
			if (Strings.isEmpty(valueName)) {
				// Special case: the annotation value as no associated operation.
				// If it appends, we could assumes that the operation is "value()"
				if (!Strings.isEmpty(formatString)) {
					throw new IllegalStateException();
				}
				formatString = getAnnotationStringValue(annotationValue);
			} else if (INLINE_VALUE_NAME.equals(valueName)) {
				if (!Strings.isEmpty(formatString)) {
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

		final IResolvedTypes resolvedTypes = this.batchTypeResolver.resolveTypes(context);

		final List<XExpression> arguments = getActualArguments(featureCall);
		final JvmIdentifiableElement calledFeature = featureCall.getFeature();
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
				// Treat the $0 parameter in the inline expression
				if (index < 0) {
					final boolean hasReceiver = appendReceiver(featureCall, target, true);
					if (hasReceiver) {
						target.append("."); //$NON-NLS-1$
					}
				} else {
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
						final List<LightweightTypeReference> typeArguments = resolvedTypes.getActualTypeArguments(featureCall);
						final LightweightTypeReference typeArgument = typeArguments.get(index - numberFormalParametersImports - 1);
						serialize(typeArgument.getRawTypeReference().toTypeReference(), context, target);
					} else if (index >= numberFormalParameters && index < numberFormalParametersImports) {
						serialize(importedTypes.get(index - numberFormalParameters), context, target);
					} else if (index == numberFormalParametersImports) {
						appendTypeArguments(featureCall, target);
					} else if (index < arguments.size()) {
						final XExpression argument = arguments.get(index);
						appendArgument(argument, target, index > 0);
					} else if (formalVariadicParameter != null) {
						appendNullValue(formalVariadicParameter.getParameterType(), calledFeature, target);
					} else {
						throw new IllegalStateException();
					}
				}
			}
			prevEnd = matcher.end();
		}
		if (prevEnd != formatString.length()) {
			target.append(formatString.substring(prevEnd));
		}
	}

	@Override
	public void doInternalToJavaStatement(XExpression obj, ITreeAppendable appendable, boolean isReferenced) {
		// Overridden for enabling the expressions that are specific to SARL
		if (obj instanceof SarlBreakExpression) {
			_toJavaStatement((SarlBreakExpression) obj, appendable, isReferenced);
		} else if (obj instanceof SarlContinueExpression) {
			_toJavaStatement((SarlContinueExpression) obj, appendable, isReferenced);
		} else if (obj instanceof SarlAssertExpression) {
			_toJavaStatement((SarlAssertExpression) obj, appendable, isReferenced);
		} else {
			try {
				super.doInternalToJavaStatement(obj, appendable, isReferenced);
			} catch (IllegalStateException exception) {
				// Log the exception but do not fail the generation.
				logInternalError(exception);
			}
		}
	}

	@Override
	public void internalToConvertedExpression(XExpression obj, ITreeAppendable appendable) {
		// Overridden for enabling the expressions that are specific to SARL
		if (obj instanceof SarlBreakExpression) {
			_toJavaExpression((SarlBreakExpression) obj, appendable);
		} else if (obj instanceof SarlContinueExpression) {
			_toJavaExpression((SarlContinueExpression) obj, appendable);
		} else if (obj instanceof SarlAssertExpression) {
			_toJavaExpression((SarlAssertExpression) obj, appendable);
		} else {
			try {
				super.internalToConvertedExpression(obj, appendable);
			} catch (IllegalStateException exception) {
				// Log the exception but do not fail the generation.
				logInternalError(exception);
			}
		}
	}

	/** Generate the Java code related to the preparation statements for the break keyword.
	 *
	 * @param breakExpression the expression.
	 * @param appendable the output.
	 * @param isReferenced indicates if the expression is referenced.
	 */
	@SuppressWarnings("static-method")
	protected void _toJavaStatement(SarlBreakExpression breakExpression, ITreeAppendable appendable, boolean isReferenced) {
		appendable.newLine().append("break;"); //$NON-NLS-1$
	}

	/** Generate the Java code related to the preparation statements for the break keyword.
	 *
	 * @param breakExpression the expression.
	 * @param appendable the output.
	 * @param isReferenced indicates if the expression is referenced.
	 * @since 0.7
	 */
	@SuppressWarnings("static-method")
	protected void _toJavaStatement(SarlContinueExpression breakExpression, ITreeAppendable appendable, boolean isReferenced) {
		appendable.newLine().append("continue;"); //$NON-NLS-1$
	}

	/** Generate the Java code to the preparation statements for the assert keyword.
	 *
	 * @param assertExpression the expression.
	 * @param appendable the output.
	 * @param isReferenced indicates if the expression is referenced.
	 */
	protected void _toJavaStatement(SarlAssertExpression assertExpression, ITreeAppendable appendable, boolean isReferenced) {
		if (!assertExpression.isIsStatic() && assertExpression.getCondition() != null && isAtLeastJava8(assertExpression)) {
			final XExpression condition = assertExpression.getCondition();
			final LightweightTypeReference actualType = getLightweightType(condition);
			if (actualType != null) {
				final Boolean booleanConstant = this.sarlExpressionHelper.toBooleanPrimitiveWrapperConstant(condition);
				if (booleanConstant != null) {
					appendable.newLine().append("assert "); //$NON-NLS-1$
					appendable.append(booleanConstant.toString());
				} else {
					// Get the local variables.
					final Map<XVariableDeclaration, XFeatureCall> localVariables = getReferencedLocalVariable(condition, true);

					final String className = appendable.declareUniqueNameVariable(assertExpression, "$AssertEvaluator$"); //$NON-NLS-1$

					appendable.openScope();
					try {
						reassignThisInClosure(appendable, findKnownTopLevelType(Object.class, assertExpression));

						appendable.newLine().append("class ").append(className).append(" {"); //$NON-NLS-1$ //$NON-NLS-2$
						appendable.increaseIndentation().newLine();
						appendable.append("final boolean $$result;").newLine(); //$NON-NLS-1$
						appendable.append(className).append("("); //$NON-NLS-1$
						boolean first = true;
						for (final Entry<XVariableDeclaration, XFeatureCall> localVariable : localVariables.entrySet()) {
							if (first) {
								first = false;
							} else {
								appendable.append(", "); //$NON-NLS-1$
							}
							final JvmTypeReference localVariableType = getType(localVariable.getValue());
							appendable.append("final ").append(toLightweight(localVariableType, assertExpression)); //$NON-NLS-1$
							String referenceName = getReferenceName(localVariable.getValue(), appendable);
							if (Strings.isEmpty(referenceName)) {
								referenceName = localVariable.getKey().getName();
							}
							appendable.append(" ").append(referenceName); //$NON-NLS-1$
						}
						appendable.append(") {"); //$NON-NLS-1$
						appendable.increaseIndentation();
						internalToJavaStatement(condition, appendable, true);
						appendable.newLine();
						appendable.append("this.$$result = "); //$NON-NLS-1$
						internalToConvertedExpression(condition, appendable, actualType);
						appendable.append(";"); //$NON-NLS-1$
						appendable.decreaseIndentation().newLine();
						appendable.append("}"); //$NON-NLS-1$
						appendable.decreaseIndentation().newLine();
						appendable.append("}"); //$NON-NLS-1$
						appendable.newLine();
						appendable.append("assert new ").append(className).append("("); //$NON-NLS-1$ //$NON-NLS-2$
						first = true;
						for (final Entry<XVariableDeclaration, XFeatureCall> localVariable : localVariables.entrySet()) {
							if (first) {
								first = false;
							} else {
								appendable.append(", "); //$NON-NLS-1$
							}
							String referenceName = getReferenceName(localVariable.getValue(), appendable);
							if (Strings.isEmpty(referenceName)) {
								referenceName = localVariable.getKey().getName();
							}
							appendable.append(referenceName);
						}
						appendable.append(").$$result"); //$NON-NLS-1$
					} finally {
						appendable.closeScope();
					}
				}
				if (!Strings.isEmpty(assertExpression.getMessage())) {
					appendable.append(" : \""); //$NON-NLS-1$
					appendable.append(Strings.convertToJavaString(assertExpression.getMessage()));
					appendable.append("\""); //$NON-NLS-1$
				}
				appendable.append(";"); //$NON-NLS-1$
			}
		}
	}

	@Override
	protected void _toJavaStatement(XCastedExpression expr, ITreeAppendable appendable, boolean isReferenced) {
		internalToJavaStatement(expr.getTarget(), appendable, isReferenced);
	}

	/** Replies all the variables that are referenced into the given expression.
	 *
	 * @param expression the expression.
	 * @param onlyWritable if {@code true} only the writable variables are replied. Otherwise, all variables are replied.
	 * @return the referenced variables.
	 */
	@SuppressWarnings("static-method")
	protected Map<XVariableDeclaration, XFeatureCall> getReferencedLocalVariable(XExpression expression, boolean onlyWritable) {
		final Map<XVariableDeclaration, XFeatureCall> localVariables = new TreeMap<>((k1, k2) -> {
			return k1.getIdentifier().compareTo(k2.getIdentifier());
		});
		for (final XFeatureCall featureCall : EcoreUtil2.getAllContentsOfType(expression, XFeatureCall.class)) {
			if (featureCall.getFeature() instanceof XVariableDeclaration) {
				final XVariableDeclaration localVariable = (XVariableDeclaration) featureCall.getFeature();
				if ((!onlyWritable || localVariable.isWriteable()) && !localVariables.containsKey(localVariable)) {
					localVariables.put(localVariable, featureCall);
				}
			}
		}
		return localVariables;
	}

	/** Replies if the generation is for Java version 8 at least.
	 *
	 * @param context the context.
	 * @return {@code true} if Java 8 or newer.
	 */
	protected boolean isAtLeastJava8(EObject context) {
		return this.generatorConfigProvider.get(EcoreUtil.getRootContainer(context)).getJavaSourceVersion().isAtLeast(JavaVersion.JAVA8);
	}

	/** Generate the Java code related to the expression for the break keyword.
	 *
	 * @param breakExpression the expression.
	 * @param appendable the output.
	 */
	@SuppressWarnings("static-method")
	protected void _toJavaExpression(SarlBreakExpression breakExpression, ITreeAppendable appendable) {
		appendable.append("/* error - couldn't compile nested break */"); //$NON-NLS-1$
	}

	/** Generate the Java code related to the expression for the continue keyword.
	 *
	 * @param breakExpression the expression.
	 * @param appendable the output.
	 * @since 0.7
	 */
	@SuppressWarnings("static-method")
	protected void _toJavaExpression(SarlContinueExpression breakExpression, ITreeAppendable appendable) {
		appendable.append("/* error - couldn't compile nested continue */"); //$NON-NLS-1$
	}

	/** Generate the Java code related to the expression for the assert keyword.
	 *
	 * @param assertExpression the expression.
	 * @param appendable the output.
	 */
	protected void _toJavaExpression(SarlAssertExpression assertExpression, ITreeAppendable appendable) {
		if (!assertExpression.isIsStatic() && isAtLeastJava8(assertExpression)) {
			appendable.append("/* error - couldn't compile nested assert */"); //$NON-NLS-1$
		}
	}

	@Override
	@SuppressWarnings({"checkstyle:npathcomplexity", "checkstyle:nestedifdepth"})
	protected void _toJavaExpression(XCastedExpression expr, ITreeAppendable appendable) {
		final LightweightTypeReference targetType = toLightweight(getType(expr.getTarget()), expr);
		final LightweightTypeReference expectedType = toLightweight(expr.getType(), expr);
		if (expr instanceof SarlCastedExpression) {
			final SarlCastedExpression cast = (SarlCastedExpression) expr;
			final JvmOperation operation = cast.getFeature();
			if (operation != null) {
				final boolean hasNullInputTest = !targetType.isPrimitive() && !targetType.isPrimitiveVoid()
						&& !isLiteral(expr.getTarget());

				final XExpression receiver = cast.getReceiver();
				final XExpression argument = cast.getArgument();

				if (hasNullInputTest) {
					appendable.append("("); //$NON-NLS-1$
					internalToConvertedExpression(expr.getTarget(), appendable, targetType);
					appendable.append(" == null ? "); //$NON-NLS-1$
					appendDefaultLiteral(appendable, expectedType);
					appendable.append(" : "); //$NON-NLS-1$
				}

				final Later callGeneration = it -> {
					final JvmAnnotationReference inlineAnnotation = this.expressionHelper.findInlineAnnotation(operation);
					if (inlineAnnotation != null) {
						final XAbstractFeatureCall call;
						if (operation.isStatic()) {
							final XFeatureCall mcall = XbaseFactory.eINSTANCE.createXFeatureCall();
							mcall.setFeature(operation);
							mcall.getActualArguments().add(EcoreUtil.copy(argument));
							mcall.eContainer();
							call = mcall;
						} else {
							final XMemberFeatureCall mcall = XbaseFactory.eINSTANCE.createXMemberFeatureCall();
							mcall.setFeature(operation);
							if (operation.getParameters().isEmpty()) {
								mcall.setMemberCallTarget(EcoreUtil.copy(argument));
								call = mcall;
							} else {
								mcall.setMemberCallTarget(receiver);
								mcall.getActualArguments().add(EcoreUtil.copy(argument));
								call = mcall;
							}
						}
						appendInlineFeatureCall(call, cast, appendable);
					} else {
						if (operation.isStatic()) {
							final JvmDeclaredType operationContainer = operation.getDeclaringType();
							final JvmTypeReference operationContainerType = getTypeComputationServices()
									.getTypeReferences().createTypeRef(operationContainer);
							serialize(operationContainerType, expr, it);
							it.append("."); //$NON-NLS-1$
						} else if (receiver != null) {
							final LightweightTypeReference receiverType;
							if (receiver == expr.getTarget()) {
								receiverType = targetType;
							} else {
								receiverType = toLightweight(getType(receiver), expr);
							}
							if (receiverType.isPrimitive() || receiverType.isPrimitiveVoid()) {
								internalToConvertedExpression(receiver, it, receiverType.getWrapperTypeIfPrimitive());
							} else {
								internalToJavaExpression(receiver, it);
							}
							it.append("."); //$NON-NLS-1$
						}

						String name = null;
						if (it.hasName(operation)) {
							name = it.getName(operation);
						} else {
							name = this.featureNameProvider.getSimpleName(operation);
						}
						if (name == null) {
							name = "/* name is null */"; //$NON-NLS-1$
						}

						it.trace(expr, XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE, 0).append(name);
						it.append("("); //$NON-NLS-1$

						if (argument != null) {
							if (argument == expr.getTarget()) {
								internalToConvertedExpression(expr.getTarget(), it, targetType);
							} else {
								appendArgument(argument, it, false);
							}
						}

						it.append(")"); //$NON-NLS-1$
					}
				};

				final LightweightTypeReference concreteType = toLightweight(operation.getReturnType(), expr);
				doConversion(expectedType, concreteType, appendable, expr, callGeneration);

				if (hasNullInputTest) {
					appendable.append(")"); //$NON-NLS-1$
				}

				return;
			}
		}
		// Generate the standard Java cast operator
		if (expectedType.isAssignableFrom(targetType)) {
			internalToConvertedExpression(expr.getTarget(), appendable, expectedType);
		} else {
			super._toJavaExpression(expr, appendable);
		}
	}

	private static boolean isLiteral(XExpression expr) {
		return expr instanceof XBooleanLiteral || expr instanceof XStringLiteral
				|| expr instanceof XNumberLiteral || expr instanceof XCollectionLiteral
				|| expr instanceof XSetLiteral || expr instanceof XNullLiteral || expr instanceof XTypeLiteral;
	}

	/** Generate the Java expression for the given JVM operation.
	 *
	 * @param sourceObject the object into the source tree that is the source for the call.
	 * @param operation the JVM operation to call.
	 * @param receiver the receiver of the call.
	 * @param arguments the arguments to pass to the called operation.
	 * @param appendable the receiver of the Java code.
	 */
	protected void jvmOperationCallToJavaExpression(final XExpression sourceObject, final JvmOperation operation,
			XExpression receiver, List<XExpression> arguments, ITreeAppendable appendable) {
		String name = null;
		assert operation != null;
		if (appendable.hasName(operation)) {
			name = appendable.getName(operation);
		} else {
			name = this.featureNameProvider.getSimpleName(operation);
		}
		if (name == null) {
			name = "/* name is null */"; //$NON-NLS-1$
		}
		if (operation.isStatic()) {
			final JvmDeclaredType operationContainer = operation.getDeclaringType();
			final JvmIdentifiableElement container = getLogicalContainerProvider().getNearestLogicalContainer(sourceObject);
			final JvmType containerType = EcoreUtil2.getContainerOfType(container, JvmType.class);
			final LightweightTypeReference reference = newTypeReferenceOwner(sourceObject)
					.toLightweightTypeReference(operationContainer);
			if (!reference.isAssignableFrom(containerType)) {
				appendable.append(operationContainer);
				appendable.append("."); //$NON-NLS-1$
			}
		} else if (receiver != null) {
			internalToJavaExpression(receiver, appendable);
			appendable.append("."); //$NON-NLS-1$
		} else {
			appendable.append("this."); //$NON-NLS-1$
		}
		appendable.trace(sourceObject, XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE, 0).append(name);
		appendable.append("("); //$NON-NLS-1$
		if (arguments != null && !arguments.isEmpty()) {
			appendArguments(arguments, appendable, true);
		}
		appendable.append(")"); //$NON-NLS-1$
	}

	@Override
	protected boolean internalCanCompileToJavaExpression(XExpression expression, ITreeAppendable appendable) {
		// Overridden for enabling the expressions that are specific to SARL
		if (expression instanceof SarlBreakExpression) {
			return true;
		}
		if (expression instanceof SarlContinueExpression) {
			return true;
		}
		if (expression instanceof SarlAssertExpression) {
			return true;
		}
		return super.internalCanCompileToJavaExpression(expression, appendable);
	}

	@Override
	protected boolean isVariableDeclarationRequired(XExpression expression, ITreeAppendable appendable, boolean recursive) {
		// Overridden for enabling the expressions that are specific to SARL
		final EObject container = expression.eContainer();
		if (expression instanceof SarlBreakExpression) {
			return false;
		}
		if (expression instanceof SarlContinueExpression) {
			return false;
		}
		if (container instanceof SarlAssertExpression) {
			return false;
		}
		return super.isVariableDeclarationRequired(expression, appendable, recursive);
	}

	/** Log an internal error but do not fail.
	 *
	 * @param exception the exception to log.
	 */
	protected void logInternalError(Throwable exception) {
		if (exception != null && this.log.isLoggable(Level.FINEST)) {
			this.log.log(Level.FINEST, Messages.SARLJvmModelInferrer_0, exception);
		}
	}

	@Override
	protected boolean isEarlyExit(XExpression expr) {
		// This function is redefined in order to take care about the SARL early exit statements
		// that are not Java early exit statements.
		// In this case, a Java "return" statement must be applied.
		if (this.isOnJavaEarlyExit) {
			this.isOnJavaEarlyExit = false;
			return this.earlyExit.isEarlyExitInJava(expr);
		}
		return this.earlyExit.isEarlyExit(expr);
	}

	@Override
	public ITreeAppendable compile(XExpression expr, ITreeAppendable parentAppendable,
			LightweightTypeReference expectedReturnType, Set<JvmTypeReference> declaredExceptions) {
		// This function is redefined in order to take care about the SARL early exit statements
		// that are not Java early exit statements.
		// In this case, a Java "return" statement must be applied.
		final boolean isPrimitiveVoidExpected = expectedReturnType.isPrimitiveVoid();
		if (!isPrimitiveVoidExpected && expr instanceof XBlockExpression) {
			this.isOnJavaEarlyExit = true;
		}
		return super.compile(expr, parentAppendable, expectedReturnType, declaredExceptions);
	}

	/** Replies if the given annotation reference has an associated boolean value defined into
	 * {@code constantExpression}.
	 *
	 * @param reference the annotation reference.
	 * @return {@code true} if the annotation has the {@code constantExpression} value evaluted to {@code true};
	 *     otherwise {@code false}.
	 */
	private static boolean isConstantExpression(JvmAnnotationReference reference) {
		// FIXME: Remove when Issue is fixed (https://github.com/eclipse/xtext-extras/pull/324)
		for (final JvmAnnotationValue annotationValue: reference.getValues()) {
			if ("constantExpression".equals(annotationValue.getValueName())) { //$NON-NLS-1$
				if (annotationValue instanceof JvmBooleanAnnotationValue) {
					return ((JvmBooleanAnnotationValue) annotationValue).getValues().get(0).booleanValue();
				} else if (annotationValue instanceof JvmCustomAnnotationValue) {
					final EObject value = ((JvmCustomAnnotationValue) annotationValue).getValues().get(0);
					if (value instanceof XBooleanLiteral) {
						return ((XBooleanLiteral) value).isIsTrue();
					}
				}
			}
		}
		return false;
	}

	@Override
	protected void featureCalltoJavaExpression(final XAbstractFeatureCall call, ITreeAppendable appendable, boolean isExpressionContext) {
		// FIXME: Remove when Issue is fixed (https://github.com/eclipse/xtext-extras/pull/324)
		if (call instanceof XAssignment) {
			assignmentToJavaExpression((XAssignment) call, appendable, isExpressionContext);
		} else {
			if (needMultiAssignment(call)) {
				appendLeftOperand(call, appendable, isExpressionContext).append(" = "); //$NON-NLS-1$
			}
			final JvmAnnotationReference annotationRef = this.expressionHelper.findInlineAnnotation(call);
			ITreeAppendable app = appendable;
			if (annotationRef == null || !isConstantExpression(annotationRef)) {
				final boolean hasReceiver = appendReceiver(call, app, isExpressionContext);
				if (hasReceiver) {
					app.append("."); //$NON-NLS-1$
					app = appendTypeArguments(call, app);
				}
			}
			appendFeatureCall(call, app);
		}
	}

}

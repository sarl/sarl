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

package io.sarl.lang.compiler;

import static com.google.common.collect.Sets.newHashSet;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;
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
import org.eclipse.xtext.common.types.JvmDoubleAnnotationValue;
import org.eclipse.xtext.common.types.JvmEnumAnnotationValue;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmField;
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
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XClosure;
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
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.compiler.Later;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.featurecalls.IdentifiableSimpleNameProvider;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;
import org.eclipse.xtext.xbase.scoping.batch.IFeatureNames;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.util.XExpressionHelper;

import io.sarl.lang.controlflow.ISarlEarlyExitComputer;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.core.util.SerializableProxy;
import io.sarl.lang.jvmmodel.Messages;
import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlCastedExpression;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.typesystem.SARLExpressionHelper;
import io.sarl.lang.util.ContextAwareTreeAppendable;
import io.sarl.lang.util.Utils;


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
 * in Java. In this case a Java "return" statement must be added implicitly.
 *
 * <p>The roles of the different generation tools are:<ul>
 * <li>{@link io.sarl.lang.jvmmodel.SARLJvmModelInferrer}: Generating the expected Java Ecore model from the SARL Ecore model.</li>
 * <li>{@link org.eclipse.xtext.linking.ILinker}: Create links among the SARL Ecore models.<li>
 * <li>{@link io.sarl.lang.compiler.SARLJvmGenerator}: Generate the Java code from the Java Ecore model.</li>
 * <li>{@link io.sarl.lang.compiler.SarlCompiler}: Generate the Java code for the XExpression objects.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.4
 */
public class SarlCompiler extends XtendCompiler {

	private static final String INLINE_VARIABLE_PREFIX = "$"; //$NON-NLS-1$

	private static final String INLINE_VALUE_NAME = "value"; //$NON-NLS-1$

	private static final String INLINE_IMPORTED_NAME = "imported"; //$NON-NLS-1$

	private static final String SERIALIZABLE_CLOSURE_LOCAL_REFERENCES = "serializableClosure.localReferences"; //$NON-NLS-1$

	private static final Pattern INLINE_VARIABLE_PATTERN = Pattern.compile("\\" + INLINE_VARIABLE_PREFIX //$NON-NLS-1$
			+ "((?:\\" + INLINE_VARIABLE_PREFIX + ")|(?:\\-?[0-9]+))"); //$NON-NLS-1$ //$NON-NLS-2$

	@Inject
	private Logger log;

	@Inject
	private XExpressionHelper expressionHelper;

	@Inject
	private IBatchTypeResolver batchTypeResolver;

	@Inject
	private SARLExpressionHelper sarlExpressionHelper;

	@Inject
	private ISarlEarlyExitComputer earlyExit;

	@Inject
	private IdentifiableSimpleNameProvider featureNameProvider;

	private volatile boolean isOnJavaEarlyExit;

	// FIXME: Remove when PR is merged.
	@Inject
	private ReflectExtensions reflect;

	private static String getAnnotationStringValue(JvmAnnotationValue value) {
		if (value instanceof JvmAnnotationAnnotationValue cvalue) {
			return cvalue.getValues().get(0).getAnnotation().getIdentifier();
		}
		if (value instanceof JvmBooleanAnnotationValue cvalue) {
			return cvalue.getValues().get(0).toString();
		}
		if (value instanceof JvmByteAnnotationValue cvalue) {
			return cvalue.getValues().get(0).toString();
		}
		if (value instanceof JvmCharAnnotationValue cvalue) {
			return cvalue.getValues().get(0).toString();
		}
		if (value instanceof JvmCustomAnnotationValue cvalue) {
			final var evalue = cvalue.getValues().get(0);
			if (evalue instanceof XStringLiteral cvalue0) {
				return cvalue0.getValue();
			}
			if (evalue instanceof XNumberLiteral cvalue0) {
				return cvalue0.getValue();
			}
			if (evalue instanceof XBooleanLiteral cvalue0) {
				return Boolean.toString(cvalue0.isIsTrue());
			}
			if (evalue instanceof XTypeLiteral cvalue0) {
				return cvalue0.getType().getIdentifier();
			}
		}
		if (value instanceof JvmDoubleAnnotationValue cvalue) {
			return cvalue.getValues().get(0).toString();
		}
		if (value instanceof JvmEnumAnnotationValue cvalue) {
			return cvalue.getValues().get(0).getSimpleName();
		}
		if (value instanceof JvmFloatAnnotationValue cvalue) {
			return cvalue.getValues().get(0).toString();
		}
		if (value instanceof JvmIntAnnotationValue cvalue) {
			return cvalue.getValues().get(0).toString();
		}
		if (value instanceof JvmLongAnnotationValue cvalue) {
			return cvalue.getValues().get(0).toString();
		}
		if (value instanceof JvmShortAnnotationValue cvalue) {
			return cvalue.getValues().get(0).toString();
		}
		if (value instanceof JvmStringAnnotationValue cvalue) {
			return cvalue.getValues().get(0);
		}
		if (value instanceof JvmTypeAnnotationValue cvalue) {
			return cvalue.getValues().get(0).getIdentifier();
		}
		return null;
	}

	private static Collection<JvmTypeReference> getAnnotationTypeValue(JvmAnnotationValue value) {
		if (value instanceof JvmTypeAnnotationValue cvalue) {
			return cvalue.getValues();
		}
		return Collections.emptyList();
	}

	@Override
	protected synchronized void appendInlineFeatureCall(XAbstractFeatureCall call, ITreeAppendable target) {
		// Overridden for fixing the @Inline behavior
		final var inlineAnnotation = this.expressionHelper.findInlineAnnotation(call);

		String formatString = null;
		final var importedTypes = Lists.<JvmTypeReference>newArrayListWithCapacity(2);
		for (final var annotationValue: inlineAnnotation.getValues()) {
			final var valueName = annotationValue.getValueName();
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

		final var resolvedTypes = this.batchTypeResolver.resolveTypes(call);

		final var arguments = getActualArguments(call);
		final var calledFeature = call.getFeature();
		var numberVariadicParameter = 0;
		final int numberFormalParameters;
		JvmFormalParameter formalVariadicParameter = null;
		if (calledFeature instanceof JvmExecutable jvmexec) {
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
		final var matcher = INLINE_VARIABLE_PATTERN.matcher(formatString);
		var prevEnd = 0;


		while (matcher.find()) {
			final var start = matcher.start();
			if (start != prevEnd) {
				target.append(formatString.substring(prevEnd, start));
			}
			final var indexOrDollar = matcher.group(1);
			if (INLINE_VARIABLE_PREFIX.equals(indexOrDollar)) {
				target.append(INLINE_VARIABLE_PREFIX);
			} else {
				final var index = Integer.parseInt(indexOrDollar) - 1;
				// Treat the $-1 and $0 parameters in the inline expression
				if (index < 0) {
					final var hasReceiver = appendReceiver(call, target, true);
					if (hasReceiver && index != -2) {
						target.append("."); //$NON-NLS-1$
					}
				} else {
					final var numberImports = importedTypes.size();
					final var numberFormalParametersImports = numberFormalParameters + numberImports;
					if (numberVariadicParameter != 0 && index < arguments.size() && index == (numberFormalParameters - 1)) {
						var argument = arguments.get(index);
						appendArgument(argument, target, index > 0);
						for (var i = index + 1; i < arguments.size(); ++i) {
							target.append(", "); //$NON-NLS-1$
							argument = arguments.get(i);
							appendArgument(argument, target, true);
						}
					} else if (index > numberFormalParametersImports) {
						final var typeArguments = resolvedTypes.getActualTypeArguments(call);
						final var typeArgument = typeArguments.get(index - numberFormalParametersImports - 1);
						serialize(typeArgument.getRawTypeReference().toTypeReference(), call, target);
					} else if (index >= numberFormalParameters && index < numberFormalParametersImports) {
						serialize(importedTypes.get(index - numberFormalParameters), call, target);
					} else if (index == numberFormalParametersImports) {
						appendTypeArguments(call, target);
					} else if (index < arguments.size()) {
						final var argument = arguments.get(index);
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

	/** Append the inline version for the given call.
	 *
	 * <p>This function supports the specific semantic of the inline expression that is defined into the SARL specification.
	 *
	 * @param inlineAnnotation the inline annotation.
	 * @param calledFeature the called feature.
	 * @param receiver the receiver of the call: {@code getActualReceiver(call)}.
	 * @param arguments the call's argument: {@code getActualArguments(call)}.
	 * @param context the context for finding types.
	 * @param appendable the receiver.
	 */
	private void appendInlineFeatureCallForCastedExpression(
			JvmAnnotationReference inlineAnnotation,
			JvmIdentifiableElement calledFeature,
			XExpression receiver,
			List<XExpression> arguments,
			EObject context,
			ITreeAppendable appendable) {
		// Overridden for fixing the @Inline behavior
		String formatString = null;
		final var importedTypes = Lists.<JvmTypeReference>newArrayListWithCapacity(2);
		for (final var annotationValue: inlineAnnotation.getValues()) {
			final var valueName = annotationValue.getValueName();
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

		var numberVariadicParameter = 0;
		final int numberFormalParameters;
		JvmFormalParameter formalVariadicParameter = null;
		if (calledFeature instanceof JvmExecutable jvmexec) {
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

		final var matcher = INLINE_VARIABLE_PATTERN.matcher(formatString);
		var prevEnd = 0;

		while (matcher.find()) {
			final var start = matcher.start();
			if (start != prevEnd) {
				appendable.append(formatString.substring(prevEnd, start));
			}
			final var indexOrDollar = matcher.group(1);
			if (INLINE_VARIABLE_PREFIX.equals(indexOrDollar)) {
				appendable.append(INLINE_VARIABLE_PREFIX);
			} else {
				final var index = Integer.parseInt(indexOrDollar) - 1;
				// Treat the $0 parameter in the inline expression
				if (index < 0) {
					var hasReceiver = true;
					if (receiver != null) {
						internalToJavaExpression(receiver, appendable);
						if (receiver instanceof XAbstractFeatureCall cvalue) {
							if (cvalue.getFeature() instanceof JvmType) {
								final var referenceName = getReferenceName(receiver, appendable);
								if (referenceName != null && referenceName.length() == 0) {
									hasReceiver = false;
								}
							}
						}
					} else {
						hasReceiver = false;
					}
					if (hasReceiver) {
						appendable.append("."); //$NON-NLS-1$
					}
				} else {
					final var numberImports = importedTypes.size();
					final var numberFormalParametersImports = numberFormalParameters + numberImports;
					if (numberVariadicParameter != 0 && index < arguments.size() && index == (numberFormalParameters - 1)) {
						XExpression argument = arguments.get(index);
						appendArgument(argument, appendable, index > 0);
						for (var i = index + 1; i < arguments.size(); ++i) {
							appendable.append(", "); //$NON-NLS-1$
							argument = arguments.get(i);
							appendArgument(argument, appendable, true);
						}
					} else if (index > numberFormalParametersImports) {
						throw new IllegalStateException();
					} else if (index >= numberFormalParameters && index < numberFormalParametersImports) {
						serialize(importedTypes.get(index - numberFormalParameters), context, appendable);
					} else if (index == numberFormalParametersImports) {
						throw new IllegalStateException();
					} else if (index < arguments.size()) {
						final XExpression argument = arguments.get(index);
						appendArgument(argument, appendable, index > 0);
					} else if (formalVariadicParameter != null) {
						appendNullValue(formalVariadicParameter.getParameterType(), calledFeature, appendable);
					} else {
						throw new IllegalStateException();
					}
				}
			}
			prevEnd = matcher.end();
		}
		if (prevEnd != formatString.length()) {
			appendable.append(formatString.substring(prevEnd));
		}
	}

	@Override
	public void doInternalToJavaStatement(XExpression obj, ITreeAppendable appendable, boolean isReferenced) {
		// Overridden for enabling the expressions that are specific to SARL
		if (obj instanceof SarlBreakExpression cvalue) {
			_toJavaStatement(cvalue, appendable, isReferenced);
		} else if (obj instanceof SarlContinueExpression cvalue) {
			_toJavaStatement(cvalue, appendable, isReferenced);
		} else if (obj instanceof SarlAssertExpression cvalue) {
			_toJavaStatement(cvalue, appendable, isReferenced);
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
		if (obj instanceof SarlBreakExpression cvalue) {
			_toJavaExpression(cvalue, appendable);
		} else if (obj instanceof SarlContinueExpression cvalue) {
			_toJavaExpression(cvalue, appendable);
		} else if (obj instanceof SarlAssertExpression cvalue) {
			_toJavaExpression(cvalue, appendable);
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

	@Override
	protected void _toJavaStatement(XClosure closure, ITreeAppendable appendable, boolean isReferenced) {
		// This function is implemented in order to generate static inner class when the closure
		// cannot be represented neither by a Java 8 lambda nor a not-static inner class.
		// It solves the issues related to the serialization and deserialization of the closures.
		final var type = getLightweightType(closure);
		final var operation = findImplementingOperation(type);
		if (!canCompileToJavaLambda(closure, type, operation) && !canBeNotStaticAnonymousClass(closure, type, operation)) {
			toSerializableAnonymousClassProxyDefinition(closure, appendable, type, operation);
		}
		super._toJavaStatement(closure, appendable, isReferenced);
	}

	/** Generate the Java code to the preparation statements for the assert keyword.
	 *
	 * @param assertExpression the expression.
	 * @param appendable the output.
	 * @param isReferenced indicates if the expression is referenced.
	 */
	protected void _toJavaStatement(SarlAssertExpression assertExpression, ITreeAppendable appendable, boolean isReferenced) {
		if (!assertExpression.isIsStatic() && assertExpression.getCondition() != null) {
			final var condition = assertExpression.getCondition();
			final var actualType = getLightweightType(condition);
			if (actualType != null) {
				final var booleanConstant = this.sarlExpressionHelper.toBooleanPrimitiveWrapperConstant(condition);
				if (booleanConstant != null) {
					appendable.newLine().append("assert "); //$NON-NLS-1$
					appendable.append(booleanConstant.toString());
				} else {
					// Get the local variables.
					final var localVariables = getReferencedLocalVariable(condition, true);

					final var className = appendable.declareUniqueNameVariable(assertExpression, "$AssertEvaluator$"); //$NON-NLS-1$

					appendable.openScope();
					try {
						reassignThisInClosure(appendable, findKnownTopLevelType(Object.class, assertExpression));

						appendable.newLine().append("class ").append(className).append(" {"); //$NON-NLS-1$ //$NON-NLS-2$
						appendable.increaseIndentation().newLine();
						appendable.append("final boolean $$result;").newLine(); //$NON-NLS-1$
						appendable.append(className).append("("); //$NON-NLS-1$
						var first = true;
						for (final Entry<XVariableDeclaration, XFeatureCall> localVariable : localVariables.entrySet()) {
							if (first) {
								first = false;
							} else {
								appendable.append(", "); //$NON-NLS-1$
							}
							final var localVariableType = getType(localVariable.getValue());
							appendable.append("final ").append(toLightweight(localVariableType, assertExpression)); //$NON-NLS-1$
							var referenceName = getReferenceName(localVariable.getValue(), appendable);
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
						for (final var localVariable : localVariables.entrySet()) {
							if (first) {
								first = false;
							} else {
								appendable.append(", "); //$NON-NLS-1$
							}
							var referenceName = getReferenceName(localVariable.getValue(), appendable);
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
		final var localVariables = new TreeMap<XVariableDeclaration, XFeatureCall>((k1, k2) -> {
			return k1.getIdentifier().compareTo(k2.getIdentifier());
		});
		for (final var featureCall : EcoreUtil2.getAllContentsOfType(expression, XFeatureCall.class)) {
			if (featureCall.getFeature() instanceof XVariableDeclaration localVariable) {
				if ((!onlyWritable || localVariable.isWriteable()) && !localVariables.containsKey(localVariable)) {
					localVariables.put(localVariable, featureCall);
				}
			}
		}
		return localVariables;
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
	@SuppressWarnings("static-method")
	protected void _toJavaExpression(SarlAssertExpression assertExpression, ITreeAppendable appendable) {
		if (!assertExpression.isIsStatic()) {
			appendable.append("/* error - couldn't compile nested assert */"); //$NON-NLS-1$
		}
	}

	@Override
	protected void _toJavaExpression(XCastedExpression expr, ITreeAppendable appendable) {
		final var target = expr.getTarget();
		final var fromType = toLightweight(getType(target), expr);
		final var toType = toLightweight(expr.getType(), expr);
		if (expr instanceof SarlCastedExpression cast) {
			final var operation = cast.getFeature();
			if (operation != null) {
				final var hasNullInputTest = !fromType.isPrimitive() && !fromType.isPrimitiveVoid()
						&& !isLiteral(target);

				final var receiver = cast.getReceiver();
				final var argument = cast.getArgument();

				if (hasNullInputTest) {
					appendable.append("("); //$NON-NLS-1$
					internalToConvertedExpression(target, appendable, fromType);
					appendable.append(" == null ? "); //$NON-NLS-1$
					appendDefaultLiteral(appendable, toType);
					appendable.append(" : "); //$NON-NLS-1$
				}

				final Later callGeneration = it -> {
					final var inlineAnnotation = this.expressionHelper.findInlineAnnotation(operation);
					if (inlineAnnotation != null) {
						final XExpression concreteReceiver;
						if (operation.isStatic()) {
							concreteReceiver = null;
						} else {
							if (operation.getParameters().isEmpty()) {
								concreteReceiver = argument;
							} else {
								concreteReceiver = receiver;
							}
						}
						appendInlineFeatureCallForCastedExpression(inlineAnnotation, operation,
								concreteReceiver,
								Collections.singletonList(argument),
								cast, appendable);
					} else {
						if (operation.isStatic()) {
							final var operationContainer = operation.getDeclaringType();
							final var operationContainerType = getTypeComputationServices()
									.getTypeReferences().createTypeRef(operationContainer);
							serialize(operationContainerType, expr, it);
							it.append("."); //$NON-NLS-1$
						} else if (receiver != null) {
							final LightweightTypeReference receiverType;
							if (receiver == target) {
								receiverType = fromType;
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
							if (argument == target) {
								internalToConvertedExpression(target, it, fromType);
							} else {
								appendArgument(argument, it, false);
							}
						}

						it.append(")"); //$NON-NLS-1$
					}
				};

				final var concreteType = toLightweight(operation.getReturnType(), expr);
				doConversion(toType, concreteType, appendable, expr, callGeneration);

				if (hasNullInputTest) {
					appendable.append(")"); //$NON-NLS-1$
				}

				return;
			}
		}
		// Generate the standard Java cast operator
		if (toType.getType().equals(fromType.getType()) && fromType.isRawType() && !toType.isRawType()) {
			// Base type are compatible; but we need to check generic types.
			// Eg: Map to Map<A, B>
			// In this case the case is mandatory for Java
			super._toJavaExpression(expr, appendable);
		} else if (toType.isAssignableFrom(fromType)) {
			// Force the cast when the input value is of any type (e.g. null)
			if (fromType.isAny()) {
				doCastConversion(toType, appendable, it -> {
					final ITreeAppendable it1 = it.trace(expr, true);
					internalToConvertedExpression(target, it1);
				});
			} else {
				// Force cast when the type is refined, e.g. into an "instanceof" block
				final var resolvedTypes = getResolvedTypes(expr);
				final var refined = resolvedTypes.isRefinedType(target);
				if (refined) {
					doCastConversion(toType, appendable, it -> {
						final ITreeAppendable it1 = it.trace(expr, true);
						internalToConvertedExpression(target, it1);
					});
				} else {
					internalToConvertedExpression(target, appendable, toType);
				}
			}
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
			final var operationContainer = operation.getDeclaringType();
			final var container = getLogicalContainerProvider().getNearestLogicalContainer(sourceObject);
			final var containerType = EcoreUtil2.getContainerOfType(container, JvmType.class);
			final var reference = newTypeReferenceOwner(sourceObject)
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
		// Add the following test for avoiding to create an variable declaration when the expression has already a name.
		final var refName = getReferenceName(expression, appendable);
		if (!Strings.isEmpty(refName)) {
			return false;
		}
		// Overridden for enabling the expressions that are specific to SARL
		if (expression instanceof SarlBreakExpression) {
			return false;
		}
		if (expression instanceof SarlContinueExpression) {
			return false;
		}
		final var container = expression.eContainer();
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
		final var isPrimitiveVoidExpected = expectedReturnType.isPrimitiveVoid();
		if (!isPrimitiveVoidExpected && expr instanceof XBlockExpression) {
			this.isOnJavaEarlyExit = true;
		}
		return super.compile(expr, parentAppendable, expectedReturnType, declaredExceptions);
	}

	private List<XAbstractFeatureCall> getReferencedExternalCalls(XExpression expression,
			List<JvmFormalParameter> exclusion, boolean enableExpressionNaming, ITreeAppendable appendable) {
		// This function is implemented in order to generate static inner class when the closure
		// cannot be represented neither by a Java 8 lambda nor a not-static inner class.
		// It solves the issues related to the serialization and deserialization of the closures.
		final var references = new ArrayList<XAbstractFeatureCall>();
		final var identifiers = new TreeSet<String>();
		for (final var featureCall : EcoreUtil2.getAllContentsOfType(expression, XAbstractFeatureCall.class)) {
			if (featureCall instanceof XMemberFeatureCall || featureCall instanceof XFeatureCall) {
				final var feature = featureCall.getFeature();
				XAbstractFeatureCall selected = null;
				var forceNaming = false;
				XAbstractFeatureCall root = null;
				JvmOperation operationToTest = null;
				if (feature instanceof JvmFormalParameter) {
					if (!exclusion.contains(feature)) {
						selected = featureCall;
						forceNaming = true;
					}
				} else if (feature instanceof XVariableDeclaration) {
					if (!EcoreUtil.isAncestor(expression, feature)) {
						selected = featureCall;
						forceNaming = true;
					}
				} else if (feature instanceof JvmField) {
					selected = featureCall;
					forceNaming = true;
				} else if (featureCall instanceof XFeatureCall cvalue) {
					// Special case: the XFeatureCall could be considered as a XMemberFeatureCall
					// with an implicit "it". In this case, the standard treatment
					// for XMemberFeatureCall must be applied
					if (isReferenceToIt(cvalue)) {
						if (feature instanceof JvmOperation cvalue0) {
							operationToTest = cvalue0;
						}
					} else {
						selected = featureCall;
					}
				} else if (featureCall instanceof XMemberFeatureCall && feature instanceof JvmOperation cvalue) {
					operationToTest = cvalue;
				}
				if (operationToTest != null && operationToTest.isStatic()) {
					root = Utils.getRootFeatureCall(featureCall, expression, exclusion);
					if (root != null && root != featureCall) {
						selected = featureCall;
					}
				}
				if (selected != null) {
					if (root == null) {
						root = Utils.getRootFeatureCall(selected, expression, exclusion);
					}
					if (root != null) {
						if (enableExpressionNaming) {
							final var refName = getReferenceName(root, appendable);
							if (!forceNaming || Strings.isEmpty(refName)) {
								final var proposedName = SarlUtils.HIDDEN_MEMBER_CHARACTER + makeJavaIdentifier(getFavoriteVariableName(root));
								appendable.declareSyntheticVariable(root, proposedName);
							}
						}
						updateReferenceList(references, identifiers, root);
					}
				}
			}
		}
		return references;
	}

	/** Replies if the given feature call has an implicit reference to the {@code it} variable.
	 *
	 * @param featureCall the feature call to test.
	 * @return {@code true} if the given feature call has an implicit reference to the
	 *     {@code it} variable.
	 * @since 0.9
	 */
	@SuppressWarnings("static-method")
	protected boolean isReferenceToIt(XFeatureCall featureCall) {
		assert featureCall != null;
		if (!featureCall.isTypeLiteral() && !featureCall.isPackageFragment()) {
			final var itKeyword = IFeatureNames.IT.getFirstSegment();
			var theFeatureCall = featureCall;
			do {
				final var name =  theFeatureCall.getFeature().getSimpleName();
				if (Strings.equal(itKeyword, name)) {
					return true;
				}
				final var expr = theFeatureCall.getImplicitReceiver();
				if (expr instanceof XFeatureCall cvalue) {
					theFeatureCall = cvalue;
				} else {
					theFeatureCall = null;
				}
			} while (theFeatureCall != null);
		}
		return false;
	}

	private static void updateReferenceList(List<XAbstractFeatureCall> references, Set<String> identifiers,
			XAbstractFeatureCall featureCall) {
		final var feature = featureCall.getFeature();
		if (feature instanceof JvmFormalParameter || feature instanceof XVariableDeclaration || feature instanceof JvmField) {
			if (identifiers.add(feature.getIdentifier())) {
				references.add(featureCall);
			}
		} else if (!references.contains(featureCall)) {
			references.add(featureCall);
		}
	}

	@Override
	protected void prepareExpression(XExpression arg, ITreeAppendable appendable) {
		// This function is implemented in order to generate static inner class when the closure
		// cannot be represented neither by a Java 8 lambda nor a not-static inner class.
		// It solves the issues related to the serialization and deserialization of the closures.
		if (arg instanceof XAbstractFeatureCall featureCall && appendable instanceof ContextAwareTreeAppendable cappendable) {
			final List<XAbstractFeatureCall> localReferences = cappendable.getContextualValue(SERIALIZABLE_CLOSURE_LOCAL_REFERENCES);
			if (localReferences != null) {
				final var root = Utils.getRootFeatureCall(featureCall);
				if (localReferences.contains(root)) {
					return;
				}
			}
		}
		super.prepareExpression(arg, appendable);
	}

	private ITreeAppendable toSerializableAnonymousClassProxyDefinition(XClosure closure, ITreeAppendable appendable,
			LightweightTypeReference type, JvmOperation operation) {
		// This function is implemented in order to generate static inner class when the closure
		// cannot be represented neither by a Java 8 lambda nor a not-static inner class.
		// It solves the issues related to the serialization and deserialization of the closures.
		final var implementationType = appendable.declareUniqueNameVariable(type, "$SerializableClosureProxy"); //$NON-NLS-1$
		appendable.newLine().append("class "); //$NON-NLS-1$
		appendable.append(implementationType);
		if (type.isInterfaceType()) {
			appendable.append(" implements "); //$NON-NLS-1$
		} else {
			appendable.append(" extends "); //$NON-NLS-1$
		}
		appendable.append(type);
		appendable.append(" {"); //$NON-NLS-1$
		appendable.increaseIndentation();

		appendable.openPseudoScope();
		try {
			final var closureParams = closure.getFormalParameters();
			final var localReferences = getReferencedExternalCalls(
					closure.getExpression(), closureParams, true, appendable);
			try {
				appendable.openScope();

				for (final var call : localReferences) {
					final var exprType = toLightweight(getType(call), closure);
					final var paramName = getReferenceName(call, appendable);
					appendable.newLine().newLine().append("private final ").append(exprType); //$NON-NLS-1$
					appendable.append(" ").append(paramName).append(";"); //$NON-NLS-1$ //$NON-NLS-2$
				}

				appendable.newLine().newLine().append("public ").append(implementationType).append("("); //$NON-NLS-1$//$NON-NLS-2$
				var first = true;
				for (final var call : localReferences) {
					if (first) {
						first = false;
					} else {
						appendable.append(", "); //$NON-NLS-1$
					}
					final var exprType = toLightweight(getType(call), closure);
					final var paramName = getReferenceName(call, appendable);
					appendable.append("final ").append(exprType).append(" ").append(paramName); //$NON-NLS-1$ //$NON-NLS-2$
				}

				appendable.append(") {").increaseIndentation(); //$NON-NLS-1$
				for (final var call : localReferences) {
					final var varName = getReferenceName(call, appendable);
					appendable.newLine().append("this.").append(varName).append(" = ");  //$NON-NLS-1$//$NON-NLS-2$
					appendable.append(varName).append(";"); //$NON-NLS-1$
				}
				appendable.decreaseIndentation().newLine().append("}").newLine(); //$NON-NLS-1$

				final var returnType = getClosureOperationReturnType(type, operation);
				appendOperationVisibility(appendable, operation);
				if (!operation.getTypeParameters().isEmpty()) {
					appendTypeParameters(appendable, operation, type);
				}
				appendable.append(returnType);
				appendable.append(" ").append(operation.getSimpleName()); //$NON-NLS-1$
				appendable.append("("); //$NON-NLS-1$
				final var isVarArgs = operation.isVarArgs();
				for (var i = 0; i < closureParams.size(); i++) {
					final var closureParam = closureParams.get(i);
					final var parameterType = getClosureOperationParameterType(type, operation, i);
					if (isVarArgs && i == closureParams.size() - 1 && parameterType.isArray()) {
						appendClosureParameterVarArgs(closureParam, parameterType.getComponentType(), appendable);
					} else {
						appendClosureParameter(closureParam, parameterType, appendable);
					}
					if (i != closureParams.size() - 1) {
						appendable.append(", "); //$NON-NLS-1$
					}
				}
				appendable.append(")"); //$NON-NLS-1$
				if (!operation.getExceptions().isEmpty()) {
					appendable.append(" throws "); //$NON-NLS-1$
					for (var i = 0; i < operation.getExceptions().size(); ++i) {
						serialize(operation.getExceptions().get(i), closure, appendable, false, false, false, false);
						if (i != operation.getExceptions().size() - 1) {
							appendable.append(", "); //$NON-NLS-1$
						}
					}
				}
				appendable.append(" {"); //$NON-NLS-1$
				appendable.increaseIndentation();
				reassignThisInClosure(appendable, null);
				final var contextAppendable = new ContextAwareTreeAppendable(appendable);
				contextAppendable.defineContextualValue(SERIALIZABLE_CLOSURE_LOCAL_REFERENCES, localReferences);
				compile(closure.getExpression(),
						contextAppendable,
						returnType, newHashSet(operation.getExceptions()));
				closeBlock(appendable);
			} catch (Exception exception) {
				throw new RuntimeException(exception);
			} finally {
				appendable.closeScope();
			}
			appendable.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
		} finally {
			appendable.closeScope();
		}
		return appendable;
	}

	@Override
	protected ITreeAppendable toAnonymousClass(XClosure closure, ITreeAppendable appendable, LightweightTypeReference type,
			JvmOperation operation) {
		// This function is implemented in order to generate static inner class when the closure
		// cannot be represented neither by a Java 8 lambda nor a not-static inner class.
		// It solves the issues related to the serialization and deserialization of the closures.
		if (canBeNotStaticAnonymousClass(closure, type, operation)
				|| Strings.isEmpty(getVarName(type, appendable))) {
			return super.toAnonymousClass(closure, appendable, type, operation);
		}
		return toSerializableAnonymousClass(closure, appendable, type, operation);
	}

	private ITreeAppendable toSerializableAnonymousClass(XClosure closure, ITreeAppendable appendable,
			LightweightTypeReference type, JvmOperation operation) {
		// This function is implemented in order to generate static inner class when the closure
		// cannot be represented neither by a Java 8 lambda nor a not-static inner class.
		// It solves the issues related to the serialization and deserialization of the closures.
		final var closureParams = closure.getFormalParameters();
		appendable.openPseudoScope();
		try {
			final var localReferences = getReferencedExternalCalls(closure.getExpression(), closureParams, false, appendable);
			final var returnType = getClosureOperationReturnType(type, operation);
			appendable.append("new ").append(type).append("() {"); //$NON-NLS-1$ //$NON-NLS-2$
			appendable.increaseIndentation();
			String selfVariable = null;
			try {
				appendable.openScope();
				if (needSyntheticSelfVariable(closure, type)) {
					appendable.newLine().append("final "); //$NON-NLS-1$
					appendable.append(type).append(" "); //$NON-NLS-1$
					selfVariable = appendable.declareVariable(type.getType(), "_self"); //$NON-NLS-1$
					appendable.append(selfVariable);
					appendable.append(" = this;"); //$NON-NLS-1$
				}
				appendOperationVisibility(appendable, operation);
				if (!operation.getTypeParameters().isEmpty()) {
					appendTypeParameters(appendable, operation, type);
				}
				appendable.append(returnType);
				appendable.append(" ").append(operation.getSimpleName()); //$NON-NLS-1$
				appendable.append("("); //$NON-NLS-1$
				final var isVarArgs = operation.isVarArgs();
				for (var i = 0; i < closureParams.size(); i++) {
					final var closureParam = closureParams.get(i);
					final var parameterType = getClosureOperationParameterType(type, operation, i);
					if (isVarArgs && i == closureParams.size() - 1 && parameterType.isArray()) {
						appendClosureParameterVarArgs(closureParam, parameterType.getComponentType(), appendable);
					} else {
						appendClosureParameter(closureParam, parameterType, appendable);
					}
					if (i != closureParams.size() - 1) {
						appendable.append(", "); //$NON-NLS-1$
					}
				}
				appendable.append(")"); //$NON-NLS-1$
				if (!operation.getExceptions().isEmpty()) {
					appendable.append(" throws "); //$NON-NLS-1$
					for (var i = 0; i < operation.getExceptions().size(); ++i) {
						serialize(operation.getExceptions().get(i), closure, appendable, false, false, false, false);
						if (i != operation.getExceptions().size() - 1) {
							appendable.append(", "); //$NON-NLS-1$
						}
					}
				}
				appendable.append(" {"); //$NON-NLS-1$
				appendable.increaseIndentation();
				if (selfVariable == null) {
					reassignThisInClosure(appendable, type.getType());
				} else {
					// We have already assigned the closure type to _self, so don't assign it again
					reassignThisInClosure(appendable, null);
				}
				compile(closure.getExpression(), appendable, returnType, newHashSet(operation.getExceptions()));
				closeBlock(appendable);
			} catch (Exception exception) {
				throw new RuntimeException(exception);
			} finally {
				appendable.closeScope();
			}
			appendable.newLine().append("private ").append(Object.class).append(" writeReplace() throws ");  //$NON-NLS-1$//$NON-NLS-2$
			appendable.append(ObjectStreamException.class).append(" {").increaseIndentation().newLine(); //$NON-NLS-1$
			if (selfVariable == null) {
				reassignThisInClosure(appendable, type.getType());
			} else {
				// We have already assigned the closure type to _self, so don't assign it again
				reassignThisInClosure(appendable, null);
			}
			appendable.append("return new ").append(SerializableProxy.class); //$NON-NLS-1$
			appendable.append("(").append(appendable.getName(type)).append(".class"); //$NON-NLS-1$ //$NON-NLS-2$
			for (final var call : localReferences) {
				appendable.append(", "); //$NON-NLS-1$
				compileAsJavaExpression(call, appendable, returnType);
			}

			appendable.append(");").decreaseIndentation().newLine().append("}"); //$NON-NLS-1$ //$NON-NLS-2$
			appendable.decreaseIndentation().newLine().append("}"); //$NON-NLS-1$
		} finally {
			appendable.closeScope();
		}
		return appendable;
	}

	/** Replies if the given closure could be represented by an not static anonymous class.
	 *
	 * @param closure the closure.
	 * @param typeRef the type of the closure.
	 * @param operation the operation to implement.
	 * @return {@code true} if the given closure could be represented by a not-static anonymous class.
	 * @since 0.8.6
	 */
	@SuppressWarnings("static-method")
	protected boolean canBeNotStaticAnonymousClass(XClosure closure, LightweightTypeReference typeRef,
			JvmOperation operation) {
		return !typeRef.isSubtypeOf(Serializable.class);
	}

	@Override
	protected boolean canCompileToJavaLambda(XClosure closure, LightweightTypeReference typeRef,
			JvmOperation operation) {
		// This function overrides the super one in order to avoid the generation of a Java 8 lambda when
		// the implemented interface is serializable. In this case, it will avoid issue for serialization and
		// deserialization of  the closure.
		return !typeRef.isSubtypeOf(Serializable.class) && super.canCompileToJavaLambda(closure, typeRef, operation);
	}

	@Override
	protected void doConversion(LightweightTypeReference left, LightweightTypeReference right,
			ITreeAppendable appendable, XExpression context, Later expression) {
		// This function overrides the super one in order to enables a NULL-SAFE conversion
		// from a wrapper to a primitive type.
		if (left.isPrimitive() && !right.isPrimitive()) {
			if (right.isAny()) {
				convertNullSafeWrapperToPrimitive(left, left, context, appendable, expression);
			} else {
				convertNullSafeWrapperToPrimitive(right, right.getPrimitiveIfWrapperType(), context, appendable, expression);
			}
			return;
		}
		// Standard behavior
		super.doConversion(left, right, appendable, context, expression);
	}

	// TODO: See Xtext PR, Copy-paste of the convertSafeWrapperToPrimitive function from the super type.
	// Rename when the super function access is fixed (https://github.com/eclipse/xtext-extras/pull/411)
	private void convertNullSafeWrapperToPrimitive(
			LightweightTypeReference wrapper,
			LightweightTypeReference primitive,
			XExpression context,
			ITreeAppendable appendable,
			Later expression) {
		// BEGIN Specific
		final var defaultValue = primitive.isType(boolean.class) ? "false" : "0"; //$NON-NLS-1$ //$NON-NLS-2$
		// END Specific
		final var normalized = normalizeBlockExpression(context);
		if (normalized instanceof XAbstractFeatureCall featureCall && !(context.eContainer() instanceof XAbstractFeatureCall)) {
			// Avoid javac bug
			// https://bugs.eclipse.org/bugs/show_bug.cgi?id=410797
			// TODO make that dependent on the compiler version (javac 1.7 fixed that bug)
			if (featureCall.isStatic()) {
				final var feature = featureCall.getFeature();
				if (feature instanceof JvmOperation cvalue) {
					if (!(cvalue.getTypeParameters().isEmpty())) {
						// BEGIN Specific
						appendable.append("(("); //$NON-NLS-1$
						expression.exec(appendable);
						appendable.append(") == null ? "); //$NON-NLS-1$
						appendable.append(defaultValue);
						appendable.append(" : "); //$NON-NLS-1$
						// END Specific
						appendable.append("("); //$NON-NLS-1$
						appendable.append(primitive);
						appendable.append(") "); //$NON-NLS-1$
						expression.exec(appendable);
						// BEGIN Specific
						appendable.append(") "); //$NON-NLS-1$
						// END Specific
						return;
					}
				}
			}
		}

		// BEGIN Specific
		appendable.append("(("); //$NON-NLS-1$
		expression.exec(appendable);
		appendable.append(") == null ? "); //$NON-NLS-1$
		appendable.append(defaultValue);
		appendable.append(" : "); //$NON-NLS-1$
		// END Specific
		final boolean mustInsertTypeCast;
		try {
			mustInsertTypeCast = ((Boolean) this.reflect.invoke(this, "mustInsertTypeCast", context, wrapper)).booleanValue(); //$NON-NLS-1$
		} catch (Exception exception) {
			throw new Error(exception);
		}
		if (mustInsertTypeCast) {
			appendable.append("("); //$NON-NLS-1$
			appendable.append(wrapper);
			appendable.append(") "); //$NON-NLS-1$
		}
		// BEGIN Specific
		appendable.append("("); //$NON-NLS-1$
		expression.exec(appendable);
		appendable.append(")"); //$NON-NLS-1$
		// END Specific
		appendable.append("."); //$NON-NLS-1$
		appendable.append(primitive);
		appendable.append("Value())"); //$NON-NLS-1$
	}

}

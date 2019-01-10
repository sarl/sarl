/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
import org.eclipse.xtext.common.types.JvmTypeAnnotationValue;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.linking.ILinker;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XClosure;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XTypeLiteral;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.compiler.IGeneratorConfigProvider;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.util.XExpressionHelper;

import io.sarl.lang.controlflow.ISarlEarlyExitComputer;
import io.sarl.lang.jvmmodel.Messages;
import io.sarl.lang.jvmmodel.SARLJvmModelInferrer;
import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.typesystem.SARLExpressionHelper;
import io.sarl.lang.util.ContextAwareTreeAppendable;
import io.sarl.lang.util.SerializableProxy;
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

	private static final String SERIALIZABLE_CLOSURE_LOCAL_REFERENCES = "serializableClosure.localReferences"; //$NON-NLS-1$

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

	//FIXME: Remove according to https://github.com/eclipse/xtext-extras/pull/331
	@Inject
	private ReflectExtensions reflect;

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
		final JvmAnnotationReference inlineAnnotation = this.expressionHelper.findInlineAnnotation(call);

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
				// Treat the $0 parameter in the inline expression
				if (index < 0) {
					final boolean hasReceiver = appendReceiver(call, target, true);
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

	@Override
	protected void _toJavaStatement(XClosure closure, ITreeAppendable appendable, boolean isReferenced) {
		// This function is implemented in order to generate static inner class when the closure
		// cannot be represented neither by a Java 8 lambda nor a not-static inner class.
		// It solves the issues related to the serialization and deserialization of the closures.
		final LightweightTypeReference type = getLightweightType(closure);
		final JvmOperation operation = findImplementingOperation(type);
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
		final String refName = getReferenceName(expression, appendable);
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
		final EObject container = expression.eContainer();
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

	@SuppressWarnings({"checkstyle:nestedifdepth", "checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	private List<XAbstractFeatureCall> getReferencedExternalCalls(XExpression expression,
			List<JvmFormalParameter> exclusion, boolean enableExpressionNaming, ITreeAppendable appendable) {
		// This function is implemented in order to generate static inner class when the closure
		// cannot be represented neither by a Java 8 lambda nor a not-static inner class.
		// It solves the issues related to the serialization and deserialization of the closures.
		final List<XAbstractFeatureCall> references = new ArrayList<>();
		final Set<String> identifiers = new TreeSet<>();
		for (final XAbstractFeatureCall featureCall : EcoreUtil2.getAllContentsOfType(expression, XAbstractFeatureCall.class)) {
			if (featureCall instanceof XMemberFeatureCall || featureCall instanceof XFeatureCall) {
				final JvmIdentifiableElement feature = featureCall.getFeature();
				XAbstractFeatureCall selected = null;
				boolean forceNaming = false;
				XAbstractFeatureCall root = null;
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
				} else if (featureCall instanceof XFeatureCall) {
					selected = featureCall;
				} else if (featureCall instanceof XMemberFeatureCall && feature instanceof JvmOperation) {
					final JvmOperation operation = (JvmOperation) feature;
					if (operation.isStatic()) {
						root = Utils.getRootFeatureCall(featureCall, expression, exclusion);
						if (root != null && root != featureCall) {
							selected = featureCall;
						}
					}
				}
				if (selected != null) {
					if (root == null) {
						root = Utils.getRootFeatureCall(selected, expression, exclusion);
					}
					if (root != null) {
						if (enableExpressionNaming) {
							final String refName = getReferenceName(root, appendable);
							if (!forceNaming || Strings.isEmpty(refName)) {
								final String proposedName = "$" + makeJavaIdentifier(getFavoriteVariableName(root)); //$NON-NLS-1$
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

	private static void updateReferenceList(List<XAbstractFeatureCall> references, Set<String> identifiers,
			XAbstractFeatureCall featureCall) {
		final JvmIdentifiableElement feature = featureCall.getFeature();
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
		if (arg instanceof XAbstractFeatureCall && appendable instanceof ContextAwareTreeAppendable) {
			final XAbstractFeatureCall featureCall = (XAbstractFeatureCall) arg;
			final ContextAwareTreeAppendable cappendable = (ContextAwareTreeAppendable) appendable;
			final List<XAbstractFeatureCall> localReferences = cappendable.getContextualValue(SERIALIZABLE_CLOSURE_LOCAL_REFERENCES);
			if (localReferences != null) {
				final XAbstractFeatureCall root = Utils.getRootFeatureCall(featureCall);
				if (localReferences.contains(root)) {
					return;
				}
			}
		}
		super.prepareExpression(arg, appendable);
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
	private ITreeAppendable toSerializableAnonymousClassProxyDefinition(XClosure closure, ITreeAppendable appendable,
			LightweightTypeReference type, JvmOperation operation) {
		// This function is implemented in order to generate static inner class when the closure
		// cannot be represented neither by a Java 8 lambda nor a not-static inner class.
		// It solves the issues related to the serialization and deserialization of the closures.
		final String implementationType = appendable.declareUniqueNameVariable(type, "$SerializableClosureProxy"); //$NON-NLS-1$
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
			final List<JvmFormalParameter> closureParams = closure.getFormalParameters();
			final List<XAbstractFeatureCall> localReferences = getReferencedExternalCalls(
					closure.getExpression(), closureParams, true, appendable);
			try {
				appendable.openScope();

				for (final XAbstractFeatureCall call : localReferences) {
					final LightweightTypeReference exprType = toLightweight(getType(call), closure);
					final String paramName = getReferenceName(call, appendable);
					appendable.newLine().newLine().append("private final ").append(exprType); //$NON-NLS-1$
					appendable.append(" ").append(paramName).append(";"); //$NON-NLS-1$ //$NON-NLS-2$
				}

				appendable.newLine().newLine().append("public ").append(implementationType).append("("); //$NON-NLS-1$//$NON-NLS-2$
				boolean first = true;
				for (final XAbstractFeatureCall call : localReferences) {
					if (first) {
						first = false;
					} else {
						appendable.append(", "); //$NON-NLS-1$
					}
					final LightweightTypeReference exprType = toLightweight(getType(call), closure);
					final String paramName = getReferenceName(call, appendable);
					appendable.append("final ").append(exprType).append(" ").append(paramName); //$NON-NLS-1$ //$NON-NLS-2$
				}

				appendable.append(") {").increaseIndentation(); //$NON-NLS-1$
				for (final XAbstractFeatureCall call : localReferences) {
					final String varName = getReferenceName(call, appendable);
					appendable.newLine().append("this.").append(varName).append(" = ");  //$NON-NLS-1$//$NON-NLS-2$
					appendable.append(varName).append(";"); //$NON-NLS-1$
				}
				appendable.decreaseIndentation().newLine().append("}").newLine(); //$NON-NLS-1$

				final LightweightTypeReference returnType = getClosureOperationReturnType(type, operation);
				appendOperationVisibility(appendable, operation);
				if (!operation.getTypeParameters().isEmpty()) {
					//FIXME: Remove reflect according to https://github.com/eclipse/xtext-extras/pull/331
					this.reflect.invoke(this, "appendTypeParameters", appendable, operation, type); //$NON-NLS-1$
				}
				appendable.append(returnType);
				appendable.append(" ").append(operation.getSimpleName()); //$NON-NLS-1$
				appendable.append("("); //$NON-NLS-1$
				final boolean isVarArgs = operation.isVarArgs();
				for (int i = 0; i < closureParams.size(); i++) {
					final JvmFormalParameter closureParam = closureParams.get(i);
					final LightweightTypeReference parameterType = getClosureOperationParameterType(type, operation, i);
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
					for (int i = 0; i < operation.getExceptions().size(); ++i) {
						serialize(operation.getExceptions().get(i), closure, appendable, false, false, false, false);
						if (i != operation.getExceptions().size() - 1) {
							appendable.append(", "); //$NON-NLS-1$
						}
					}
				}
				appendable.append(" {"); //$NON-NLS-1$
				appendable.increaseIndentation();
				reassignThisInClosure(appendable, null);
				final ContextAwareTreeAppendable contextAppendable = new ContextAwareTreeAppendable(appendable);
				contextAppendable.defineContextualValue(SERIALIZABLE_CLOSURE_LOCAL_REFERENCES, localReferences);
				compile(closure.getExpression(),
						contextAppendable,
						returnType, newHashSet(operation.getExceptions()));
				//FIXME: Remove reflect according to https://github.com/eclipse/xtext-extras/pull/331
				this.reflect.invoke(this, "closeBlock", appendable); //$NON-NLS-1$
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

	@SuppressWarnings("checkstyle:npathcomplexity")
	private ITreeAppendable toSerializableAnonymousClass(XClosure closure, ITreeAppendable appendable,
			LightweightTypeReference type, JvmOperation operation) {
		// This function is implemented in order to generate static inner class when the closure
		// cannot be represented neither by a Java 8 lambda nor a not-static inner class.
		// It solves the issues related to the serialization and deserialization of the closures.
		final List<JvmFormalParameter> closureParams = closure.getFormalParameters();
		appendable.openPseudoScope();
		try {
			final List<XAbstractFeatureCall> localReferences = getReferencedExternalCalls(
					closure.getExpression(), closureParams, false, appendable);
			final LightweightTypeReference returnType = getClosureOperationReturnType(type, operation);
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
					//FIXME: Remove reflect according to https://github.com/eclipse/xtext-extras/pull/331
					this.reflect.invoke(this, "appendTypeParameters", appendable, operation, type); //$NON-NLS-1$
				}
				appendable.append(returnType);
				appendable.append(" ").append(operation.getSimpleName()); //$NON-NLS-1$
				appendable.append("("); //$NON-NLS-1$
				final boolean isVarArgs = operation.isVarArgs();
				for (int i = 0; i < closureParams.size(); i++) {
					final JvmFormalParameter closureParam = closureParams.get(i);
					final LightweightTypeReference parameterType = getClosureOperationParameterType(type, operation, i);
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
					for (int i = 0; i < operation.getExceptions().size(); ++i) {
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
				//FIXME: Remove reflect according to https://github.com/eclipse/xtext-extras/pull/331
				this.reflect.invoke(this, "closeBlock", appendable); //$NON-NLS-1$
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
			for (final XAbstractFeatureCall call : localReferences) {
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

}

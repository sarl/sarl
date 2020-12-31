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

package io.sarl.pythongenerator.generator.generator;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.regex.Pattern;
import javax.inject.Inject;

import org.eclipse.xtend.core.xtend.AnonymousClass;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XBasicForLoopExpression;
import org.eclipse.xtext.xbase.XBinaryOperation;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XCasePart;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XCatchClause;
import org.eclipse.xtext.xbase.XClosure;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.XDoWhileExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XForLoopExpression;
import org.eclipse.xtext.xbase.XIfExpression;
import org.eclipse.xtext.xbase.XInstanceOfExpression;
import org.eclipse.xtext.xbase.XListLiteral;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XPostfixOperation;
import org.eclipse.xtext.xbase.XReturnExpression;
import org.eclipse.xtext.xbase.XSetLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XSwitchExpression;
import org.eclipse.xtext.xbase.XSynchronizedExpression;
import org.eclipse.xtext.xbase.XThrowExpression;
import org.eclipse.xtext.xbase.XTryCatchFinallyExpression;
import org.eclipse.xtext.xbase.XTypeLiteral;
import org.eclipse.xtext.xbase.XUnaryOperation;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.XWhileExpression;
import org.eclipse.xtext.xbase.compiler.IAppendable;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.typesystem.references.FunctionTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.extralanguage.compiler.AbstractExpressionGenerator;
import io.sarl.lang.extralanguage.compiler.ExtraLanguageAppendable;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageConversionInitializer;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageGeneratorContext;
import io.sarl.lang.extralanguage.compiler.IRootGenerator;
import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.util.Utils;

/** Generator of XExpression for Python 3.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public class PyExpressionGenerator extends AbstractExpressionGenerator {

	@Inject
	private IQualifiedNameProvider qualifiedNameProvider;

	@Inject
	private CommonTypeComputationServices typeServices;

	/** Constructor.
	 *
	 * @param keywordProvider the provider of Python keywords.
	 */
	@Inject
	public PyExpressionGenerator(PyKeywordProvider keywordProvider) {
		super(keywordProvider);
	}

	@Override
	protected IExtraLanguageConversionInitializer getTypeConverterInitializer() {
		return PyInitializers.getTypeConverterInitializer();
	}

	@Override
	protected IExtraLanguageConversionInitializer getFeatureNameConverterInitializer() {
		return PyInitializers.getFeatureNameConverterInitializer();
	}

	private static void appendReturnIfExpectedReturnedExpression(IAppendable it, IExtraLanguageGeneratorContext context) {
		if (context.getExpectedExpressionType() != null) {
			it.append("return "); //$NON-NLS-1$
		}
	}

	@Override
	protected void before(XExpression expression, IAppendable output, IExtraLanguageGeneratorContext context) {
		if (!(expression instanceof XClosure) && !(expression instanceof AnonymousClass)) {
			// Generate the closure definitions before their usage in the expressions
			for (final XClosure closure : EcoreUtil2.getAllContentsOfType(expression, XClosure.class)) {
				generateClosureDefinition(closure, output, context);
			}
			// Generate the closure definitions before their usage in the expressions
			for (final AnonymousClass anonClass : EcoreUtil2.getAllContentsOfType(expression, AnonymousClass.class)) {
				generateAnonymousClassDefinition(anonClass, output, context);
			}
		}
	}

	/** Generate the closure definition.
	 *
	 * @param closure the closure.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	protected void generateClosureDefinition(XClosure closure, IAppendable it, IExtraLanguageGeneratorContext context) {
		if (!it.hasName(closure)) {
			final LightweightTypeReference closureType0 = getExpectedType(closure);
			LightweightTypeReference closureType = closureType0;
			if (closureType0.isFunctionType()) {
				final FunctionTypeReference fctRef = closureType0.tryConvertToFunctionTypeReference(true);
				if (fctRef != null) {
					closureType = Utils.toLightweightTypeReference(fctRef.getType(), this.typeServices).getRawTypeReference();
				}
			}
			final String closureName = it.declareSyntheticVariable(closure, "__Jclosure_" //$NON-NLS-1$
					+ closureType.getSimpleName());
			final JvmDeclaredType rawType = (JvmDeclaredType) closureType.getType();
			final JvmOperation function = rawType.getDeclaredOperations().iterator().next();
			// Add the object type as super type because of an issue in the Python language.
			final JvmTypeReference objType = getTypeReferences().getTypeForName(Object.class, closure);
			it.openPseudoScope();
			it.append("class ").append(closureName).append("(") //$NON-NLS-1$//$NON-NLS-2$
				.append(closureType).append(",").append(objType.getType()).append("):") //$NON-NLS-1$ //$NON-NLS-2$
				.increaseIndentation().newLine().append("def ") //$NON-NLS-1$
				.append(function.getSimpleName()).append("(") //$NON-NLS-1$
				.append(getExtraLanguageKeywordProvider().getThisKeywordLambda().apply());
			for (final JvmFormalParameter param : closure.getFormalParameters()) {
				it.append(", "); //$NON-NLS-1$
				final String name = it.declareUniqueNameVariable(param, param.getName());
				it.append(name);
			}
			it.append("):"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			if (closure.getExpression() != null) {
				LightweightTypeReference returnType = closureType0;
				if (returnType.isFunctionType()) {
					final FunctionTypeReference fctRef = returnType.tryConvertToFunctionTypeReference(true);
					if (fctRef != null) {
						returnType = fctRef.getReturnType();
					} else {
						returnType = null;
					}
				} else {
					returnType = null;
				}
				//LightweightTypeReference returnType = getClosureOperationReturnType(type, operation);
				generate(closure.getExpression(), returnType, it, context);
			} else {
				it.append("pass"); //$NON-NLS-1$
			}
			it.decreaseIndentation().decreaseIndentation().newLine();
			it.closeScope();
		}
	}

	/** Generate the anonymous class definition.
	 *
	 * @param anonClass the anonymous class.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	protected void generateAnonymousClassDefinition(AnonymousClass anonClass, IAppendable it, IExtraLanguageGeneratorContext context) {
		if (!it.hasName(anonClass) && it instanceof PyAppendable) {
			final LightweightTypeReference jvmAnonType = getExpectedType(anonClass);
			final String anonName = it.declareSyntheticVariable(anonClass, jvmAnonType.getSimpleName());
			QualifiedName anonQualifiedName = QualifiedName.create(
					jvmAnonType.getType().getQualifiedName().split(Pattern.quote("."))); //$NON-NLS-1$
			anonQualifiedName = anonQualifiedName.skipLast(1);
			if (anonQualifiedName.isEmpty()) {
				// The type resolver does not include the enclosing class.
				assert anonClass.getDeclaringType() == null : "The Xtend API has changed the AnonymousClass definition!"; //$NON-NLS-1$
				final XtendTypeDeclaration container = EcoreUtil2.getContainerOfType(anonClass.eContainer(), XtendTypeDeclaration.class);
				anonQualifiedName = anonQualifiedName.append(this.qualifiedNameProvider.getFullyQualifiedName(container));
			}
			anonQualifiedName = anonQualifiedName.append(anonName);
			it.openPseudoScope();
			final IRootGenerator rootGenerator = context.getRootGenerator();
			assert rootGenerator instanceof PyGenerator;
			final List<JvmTypeReference> types = new ArrayList<>();
			for (final JvmTypeReference superType : anonClass.getConstructorCall().getConstructor().getDeclaringType().getSuperTypes()) {
				if (!Object.class.getCanonicalName().equals(superType.getIdentifier())) {
					types.add(superType);
				}
			}
			((PyGenerator) rootGenerator).generateTypeDeclaration(
					anonQualifiedName.toString(),
					anonName,
					false,
					types,
					getTypeBuilder().getDocumentation(anonClass),
					false,
					anonClass.getMembers(),
					(PyAppendable) it,
					context,
					null);
			it.closeScope();
		}
	}

	/** Generate the given object.
	 *
	 * @param anonClass the anonymous class.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the class definition.
	 */
	protected XExpression _generate(AnonymousClass anonClass, IAppendable it, IExtraLanguageGeneratorContext context) {
		if (it.hasName(anonClass)) {
			appendReturnIfExpectedReturnedExpression(it, context);
			it.append(it.getName(anonClass)).append("("); //$NON-NLS-1$
			boolean firstArg = true;
			for (final XExpression arg : anonClass.getConstructorCall().getArguments()) {
				if (firstArg) {
					firstArg = false;
				} else {
					it.append(", "); //$NON-NLS-1$
				}
				generate(arg, it, context);
			}
			it.append(")"); //$NON-NLS-1$
		}
		return anonClass;
	}

	/** Generate the given object.
	 *
	 * @param closure the closure.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the closure.
	 */
	@SuppressWarnings("static-method")
	protected XExpression _generate(XClosure closure, IAppendable it, IExtraLanguageGeneratorContext context) {
		if (it.hasName(closure)) {
			appendReturnIfExpectedReturnedExpression(it, context);
			it.append(it.getName(closure)).append("()"); //$NON-NLS-1$
		}
		return closure;
	}

	/** Generate the given object.
	 *
	 * @param block the block expression.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the last expression in the block or {@code null}.
	 */
	protected XExpression _generate(XBlockExpression block, IAppendable it, IExtraLanguageGeneratorContext context) {
		XExpression last = block;
		if (block.getExpressions().isEmpty()) {
			it.append("pass"); //$NON-NLS-1$
		} else {
			it.openScope();
			if (context.getExpectedExpressionType() == null) {
				boolean first = true;
				for (final XExpression expression : block.getExpressions()) {
					if (first) {
						first = false;
					} else {
						it.newLine();
					}
					last = generate(expression, it, context);
				}
			} else {
				final List<XExpression> exprs = block.getExpressions();
				if (!exprs.isEmpty()) {
					for (int i = 0; i < exprs.size() - 1; ++i) {
						if (i > 0) {
							it.newLine();
						}
						last = generate(exprs.get(i), it, context);
					}
					last = generate(exprs.get(exprs.size() - 1), context.getExpectedExpressionType(), it, context);
				}
			}
			it.closeScope();
		}
		return last;
	}

	/** Generate the given object.
	 *
	 * @param literal the literal.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the literal.
	 */
	@SuppressWarnings("static-method")
	protected XExpression _generate(XNumberLiteral literal, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		it.append(literal.getValue());
		return literal;
	}

	/** Generate the given object.
	 *
	 * @param literal the literal.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the literal.
	 */
	@SuppressWarnings("static-method")
	protected XExpression _generate(XStringLiteral literal, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		it.append("u\"").append(Strings.convertToJavaString(literal.getValue())).append("\""); //$NON-NLS-1$//$NON-NLS-2$
		return literal;
	}

	/** Generate the given object.
	 *
	 * @param breakStatement the break statement.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	@SuppressWarnings("static-method")
	protected XExpression _generate(SarlBreakExpression breakStatement, IAppendable it, IExtraLanguageGeneratorContext context) {
		if (context.getExpectedExpressionType() == null) {
			it.append("break"); //$NON-NLS-1$
		} else {
			it.append("return ").append(toDefaultValue(context.getExpectedExpressionType().toJavaCompliantTypeReference())); //$NON-NLS-1$
		}
		return breakStatement;
	}

	/** Generate the given object.
	 *
	 * @param continueStatement the continue statement.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	@SuppressWarnings("static-method")
	protected XExpression _generate(SarlContinueExpression continueStatement, IAppendable it, IExtraLanguageGeneratorContext context) {
		if (context.getExpectedExpressionType() == null) {
			it.append("continue"); //$NON-NLS-1$
		} else {
			it.append("return ").append(toDefaultValue(context.getExpectedExpressionType().toJavaCompliantTypeReference())); //$NON-NLS-1$
		}
		return continueStatement;
	}

	/** Generate the given object.
	 *
	 * @param assertStatement the assert statement.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	protected XExpression _generate(SarlAssertExpression assertStatement, IAppendable it, IExtraLanguageGeneratorContext context) {
		final boolean haveAssert = !assertStatement.isIsStatic() && assertStatement.getCondition() != null;
		if (haveAssert) {
			it.append("assert (lambda:"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generate(assertStatement.getCondition(), it, context);
			it.decreaseIndentation().newLine();
			it.append(")()"); //$NON-NLS-1$
		}
		if (context.getExpectedExpressionType() != null) {
			if (haveAssert) {
				it.newLine();
			}
			it.append("return ").append(toDefaultValue(context.getExpectedExpressionType().toJavaCompliantTypeReference())); //$NON-NLS-1$
		}
		return assertStatement;
	}

	/** Generate the given object.
	 *
	 * @param assignment the assignment operator.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the assignment.
	 */
	protected XExpression _generate(XAssignment assignment, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		it.append("("); //$NON-NLS-1$
		newFeatureCallGenerator(context, it).generate(assignment);
		it.append(" = "); //$NON-NLS-1$
		generate(assignment.getValue(), it, context);
		it.append(")"); //$NON-NLS-1$
		return assignment;
	}

	/** Generate the given object.
	 *
	 * @param operation the binary operation.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the operation.
	 */
	@SuppressWarnings("checkstyle:cyclomaticcomplexity")
	protected XExpression _generate(XBinaryOperation operation, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		final String operator = getOperatorSymbol(operation);
		if (operator != null) {
			it.append("("); //$NON-NLS-1$
			generate(operation.getLeftOperand(), it, context);
			switch (operator) {
			case "-": //$NON-NLS-1$
			case "+": //$NON-NLS-1$
			case "*": //$NON-NLS-1$
			case "/": //$NON-NLS-1$
			case "%": //$NON-NLS-1$
			case "-=": //$NON-NLS-1$
			case "+=": //$NON-NLS-1$
			case "*=": //$NON-NLS-1$
			case "/=": //$NON-NLS-1$
			case "%=": //$NON-NLS-1$
			case "<": //$NON-NLS-1$
			case ">": //$NON-NLS-1$
			case "<=": //$NON-NLS-1$
			case ">=": //$NON-NLS-1$
			case "==": //$NON-NLS-1$
			case "!=": //$NON-NLS-1$
			case "<<": //$NON-NLS-1$
			case ">>": //$NON-NLS-1$
				it.append(" ").append(operator).append(" "); //$NON-NLS-1$ //$NON-NLS-2$
				break;
			case "&&": //$NON-NLS-1$
				it.append(" and "); //$NON-NLS-1$
				break;
			case "||": //$NON-NLS-1$
				it.append(" or "); //$NON-NLS-1$
				break;
			case "===": //$NON-NLS-1$
				it.append(" is "); //$NON-NLS-1$
				break;
			case "!==": //$NON-NLS-1$
				it.append(" is not "); //$NON-NLS-1$
				break;
			default:
				throw new IllegalArgumentException(MessageFormat.format(Messages.PyExpressionGenerator_0, operator));
			}
			generate(operation.getRightOperand(), it, context);
			it.append(")"); //$NON-NLS-1$
		}
		return operation;
	}

	/** Generate the given object.
	 *
	 * @param call the feature call.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the feature call.
	 */
	protected XExpression _generate(XFeatureCall call, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		newFeatureCallGenerator(context, it).generate(call);
		return call;
	}

	/** Generate the given object.
	 *
	 * @param call the member feature call.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the feature call.
	 */
	protected XExpression _generate(XMemberFeatureCall call, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		newFeatureCallGenerator(context, it).generate(call);
		return call;
	}

	/** Generate the given object.
	 *
	 * @param operation the postfix operator.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the operation.
	 */
	protected XExpression _generate(XPostfixOperation operation, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		final String operator = getOperatorSymbol(operation);
		if (operator != null) {
			it.append("("); //$NON-NLS-1$
			switch (operator) {
			case "++": //$NON-NLS-1$
				generate(operation.getOperand(), it, context);
				it.append(" += 1"); //$NON-NLS-1$
				break;
			case "--": //$NON-NLS-1$
				generate(operation.getOperand(), it, context);
				it.append(" -= 1"); //$NON-NLS-1$
				break;
			default:
				throw new IllegalArgumentException(MessageFormat.format(Messages.PyExpressionGenerator_0, operator));
			}
			it.append(")"); //$NON-NLS-1$
		}
		return operation;
	}

	/** Generate the given object.
	 *
	 * @param operation the unary operation.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the operation.
	 */
	protected XExpression _generate(XUnaryOperation operation, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		final String operator = getOperatorSymbol(operation);
		if (operator != null) {
			it.append("("); //$NON-NLS-1$
			switch (operator) {
			case "+": //$NON-NLS-1$
				generate(operation.getOperand(), it, context);
				break;
			case "-": //$NON-NLS-1$
				it.append("-"); //$NON-NLS-1$
				generate(operation.getOperand(), it, context);
				break;
			default:
				throw new IllegalArgumentException(MessageFormat.format(Messages.PyExpressionGenerator_0, operator));
			}
			it.append(")"); //$NON-NLS-1$
		}
		return operation;
	}

	/** Generate the given object.
	 *
	 * @param call the constructor call.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the constructor call.
	 */
	protected XExpression _generate(XConstructorCall call, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		newFeatureCallGenerator(context, it).generate(call);
		return call;
	}

	/** Generate the given object.
	 *
	 * @param whileLoop the while-loop.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the last statement in the loop or {@code null}.
	 */
	protected XExpression _generate(XWhileExpression whileLoop, IAppendable it, IExtraLanguageGeneratorContext context) {
		it.append("while "); //$NON-NLS-1$
		generate(whileLoop.getPredicate(), it, context);
		it.append(":"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		final XExpression last = generate(whileLoop.getBody(), it, context);
		it.decreaseIndentation();
		return last;
	}

	/** Generate the given object.
	 *
	 * @param whileLoop the while-loop.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the last statement in the loop or {@code null}.
	 */
	protected XExpression _generate(XDoWhileExpression whileLoop, IAppendable it, IExtraLanguageGeneratorContext context) {
		generate(whileLoop.getBody(), it, context);
		it.newLine();
		it.append("while "); //$NON-NLS-1$
		generate(whileLoop.getPredicate(), it, context);
		it.append(":"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		final XExpression last = generate(whileLoop.getBody(), it, context);
		it.decreaseIndentation();
		return last;
	}

	/** Generate the given object.
	 *
	 * @param forLoop the for-loop.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	protected XExpression _generate(XForLoopExpression forLoop, IAppendable it, IExtraLanguageGeneratorContext context) {
		it.append("for "); //$NON-NLS-1$
		final String varName = it.declareUniqueNameVariable(forLoop.getDeclaredParam(), forLoop.getDeclaredParam().getSimpleName());
		it.append(varName);
		it.append(" in "); //$NON-NLS-1$
		generate(forLoop.getForExpression(), it, context);
		it.append(":"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		final XExpression last = generate(forLoop.getEachExpression(), it, context);
		it.decreaseIndentation();
		return last;
	}

	/** Generate the given object.
	 *
	 * @param forLoop the for-loop.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the last statement in the loop or {@code null}.
	 */
	protected XExpression _generate(XBasicForLoopExpression forLoop, IAppendable it, IExtraLanguageGeneratorContext context) {
		for (final XExpression expr : forLoop.getInitExpressions()) {
			generate(expr, it, context);
			it.newLine();
		}
		it.append("while "); //$NON-NLS-1$
		generate(forLoop.getExpression(), it, context);
		it.append(":"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		final XExpression last = generate(forLoop.getEachExpression(), it, context);
		for (final XExpression expr : forLoop.getUpdateExpressions()) {
			it.newLine();
			generate(expr, it, context);
		}
		it.decreaseIndentation();
		return last;
	}

	/** Generate the given object.
	 *
	 * @param literal the boolean literal.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the literal.
	 */
	@SuppressWarnings("static-method")
	protected XExpression _generate(XBooleanLiteral literal, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		it.append(literal.isIsTrue() ? "True" : "False"); //$NON-NLS-1$ //$NON-NLS-2$
		return literal;
	}

	/** Generate the given object.
	 *
	 * @param castOperator the cast operator.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the expression.
	 */
	protected XExpression _generate(XCastedExpression castOperator, IAppendable it, IExtraLanguageGeneratorContext context) {
		return generate(castOperator.getTarget(), context.getExpectedExpressionType(), it, context);
	}

	/** Generate the given object.
	 *
	 * @param literal the list literal.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the literal.
	 */
	protected XExpression _generate(XListLiteral literal, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		it.append("["); //$NON-NLS-1$
		boolean first = true;
		for (final XExpression value : literal.getElements()) {
			if (first) {
				first = false;
			} else {
				it.append(", "); //$NON-NLS-1$
			}
			generate(value, it, context);
		}
		it.append("]"); //$NON-NLS-1$
		return literal;
	}

	/** Generate the given object.
	 *
	 * @param literal the set literal.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the literal.
	 */
	protected XExpression _generate(XSetLiteral literal, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		it.append("{"); //$NON-NLS-1$
		boolean first = true;
		for (final XExpression value : literal.getElements()) {
			if (first) {
				first = false;
			} else {
				it.append(", "); //$NON-NLS-1$
			}
			generate(value, it, context);
		}
		it.append("}"); //$NON-NLS-1$
		return literal;
	}

	/** Generate the given object.
	 *
	 * @param ifStatement the if-then-else statement.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	protected XExpression _generate(XIfExpression ifStatement, IAppendable it, IExtraLanguageGeneratorContext context) {
		it.append("if "); //$NON-NLS-1$
		generate(ifStatement.getIf(), it, context);
		it.append(":"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		if (ifStatement.getThen() != null) {
			generate(ifStatement.getThen(), context.getExpectedExpressionType(), it, context);
		} else if (context.getExpectedExpressionType() == null) {
			it.append("pass"); //$NON-NLS-1$
		} else {
			it.append("return ").append(toDefaultValue(context.getExpectedExpressionType().toJavaCompliantTypeReference())); //$NON-NLS-1$
		}
		it.decreaseIndentation();
		if (ifStatement.getElse() != null) {
			it.newLine().append("else:"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generate(ifStatement.getElse(), context.getExpectedExpressionType(), it, context);
			it.decreaseIndentation();
		} else if (context.getExpectedExpressionType() != null) {
			it.newLine().append("else:"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append("return ").append(toDefaultValue(context.getExpectedExpressionType().toJavaCompliantTypeReference())); //$NON-NLS-1$
			it.decreaseIndentation();
		}
		return ifStatement;
	}

	/** Generate the given object.
	 *
	 * @param operator the instance-of operator.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the expression.
	 */
	protected XExpression _generate(XInstanceOfExpression operator, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		it.append("isinstance("); //$NON-NLS-1$
		generate(operator.getExpression(), it, context);
		it.append(", "); //$NON-NLS-1$
		it.append(operator.getType().getType());
		it.append(")"); //$NON-NLS-1$
		return operator;
	}


	/** Generate the given object.
	 *
	 * @param literal the null literal.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the literal.
	 */
	@SuppressWarnings("static-method")
	protected XExpression _generate(XNullLiteral literal, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		it.append("None"); //$NON-NLS-1$
		return literal;
	}

	/** Generate the given object.
	 *
	 * @param returnStatement the return statement.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	protected XExpression _generate(XReturnExpression returnStatement, IAppendable it, IExtraLanguageGeneratorContext context) {
		it.append("return "); //$NON-NLS-1$
		generate(returnStatement.getExpression(), it, context);
		return returnStatement;
	}

	/** Generate the given object.
	 *
	 * @param switchStatement the switch statement.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	@SuppressWarnings({ "checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity" })
	protected XExpression _generate(XSwitchExpression switchStatement, IAppendable it, IExtraLanguageGeneratorContext context) {
		final String varName;
		if (switchStatement.getDeclaredParam() != null) {
			varName = it.declareUniqueNameVariable(switchStatement.getDeclaredParam(),
					switchStatement.getDeclaredParam().getSimpleName());
		} else {
			varName = it.declareSyntheticVariable(switchStatement, "___expression"); //$NON-NLS-1$
		}
		it.openPseudoScope();
		it.append(varName).append(" = "); //$NON-NLS-1$
		generate(switchStatement.getSwitch(), it, context);
		it.newLine();
		boolean first = true;
		boolean fallThrough = false;
		for (final XCasePart caseExpression : switchStatement.getCases()) {
			if (fallThrough) {
				it.append(") or ("); //$NON-NLS-1$
			} else if (first) {
				it.append("if ("); //$NON-NLS-1$
				first = false;
			} else {
				it.append("elif ("); //$NON-NLS-1$
			}
			if (caseExpression.getTypeGuard() != null) {
				it.append("isinstance(").append(varName); //$NON-NLS-1$
				it.append(", ").append(caseExpression.getTypeGuard().getType()); //$NON-NLS-1$
				it.append(")"); //$NON-NLS-1$
				if (caseExpression.getCase() != null) {
					it.append(" and ("); //$NON-NLS-1$
					generate(caseExpression.getCase(), it, context);
					it.append(")"); //$NON-NLS-1$
					final LightweightTypeReference convertedType = getExpectedType(caseExpression.getCase());
					if (!convertedType.isType(Boolean.TYPE) && !convertedType.isType(Boolean.class)) {
						it.append(" == ").append(varName); //$NON-NLS-1$
					}
				}
			} else if (caseExpression.getCase() != null) {
				it.append("("); //$NON-NLS-1$
				generate(caseExpression.getCase(), it, context);
				it.append(")"); //$NON-NLS-1$
				final LightweightTypeReference convertedType = getExpectedType(caseExpression.getCase());
				if (!convertedType.isType(Boolean.TYPE) && !convertedType.isType(Boolean.class)) {
					it.append(" == ").append(varName); //$NON-NLS-1$
				}
			}
			fallThrough = caseExpression.isFallThrough();
			if (!fallThrough) {
				it.append("):"); //$NON-NLS-1$
				it.increaseIndentation().newLine();
				if (caseExpression.getThen() != null) {
					generate(caseExpression.getThen(), it, context);
				} else {
					it.append("pass"); //$NON-NLS-1$
				}
				it.decreaseIndentation().newLine();
			}
		}
		if (switchStatement.getDefault() != null) {
			if (first) {
				generate(switchStatement.getDefault(), it, context);
				it.newLine();
			} else {
				it.append("else:"); //$NON-NLS-1$
				it.increaseIndentation().newLine();
				generate(switchStatement.getDefault(), it, context);
				it.decreaseIndentation().newLine();
			}
		}
		it.closeScope();
		return switchStatement;
	}

	/** Generate the given object.
	 *
	 * @param synchronizedStatement the synchronized statement.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	protected XExpression _generate(XSynchronizedExpression synchronizedStatement, IAppendable it, IExtraLanguageGeneratorContext context) {
		return generate(synchronizedStatement.getExpression(), context.getExpectedExpressionType(), it, context);
	}

	/** Generate the given object.
	 *
	 * @param throwStatement the throw statement.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	protected XExpression _generate(XThrowExpression throwStatement, IAppendable it, IExtraLanguageGeneratorContext context) {
		it.append("raise "); //$NON-NLS-1$
		generate(throwStatement.getExpression(), it, context);
		return throwStatement;
	}

	/** Generate the given object.
	 *
	 * @param tryStatement the try-catch-finally statement.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	protected XExpression _generate(XTryCatchFinallyExpression tryStatement, IAppendable it, IExtraLanguageGeneratorContext context) {
		it.append("try:"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		generate(tryStatement.getExpression(), context.getExpectedExpressionType(), it, context);
		it.decreaseIndentation().newLine();
		for (final XCatchClause clause : tryStatement.getCatchClauses()) {
			it.append("except "); //$NON-NLS-1$
			it.append(clause.getDeclaredParam().getParameterType().getType());
			it.append(", "); //$NON-NLS-1$
			it.append(it.declareUniqueNameVariable(clause.getDeclaredParam(), clause.getDeclaredParam().getSimpleName()));
			it.append(":"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generate(clause.getExpression(), context.getExpectedExpressionType(), it, context);
			it.decreaseIndentation().newLine();
		}
		if (tryStatement.getFinallyExpression() != null) {
			it.append("finally:"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generate(tryStatement.getFinallyExpression(), it, context);
			it.decreaseIndentation();
		}
		return tryStatement;
	}

	/** Generate the given object.
	 *
	 * @param literal the type literal.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the literal.
	 */
	@SuppressWarnings("static-method")
	protected XExpression _generate(XTypeLiteral literal, IAppendable it, IExtraLanguageGeneratorContext context) {
		appendReturnIfExpectedReturnedExpression(it, context);
		it.append(literal.getType());
		return literal;
	}

	/** Generate the given object.
	 *
	 * @param varDeclaration the variable declaration.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @return the statement.
	 */
	protected XExpression _generate(XVariableDeclaration varDeclaration, IAppendable it, IExtraLanguageGeneratorContext context) {
		final String name = it.declareUniqueNameVariable(varDeclaration, varDeclaration.getName());
		it.append(name);
		it.append(" = "); //$NON-NLS-1$
		if (varDeclaration.getRight() != null) {
			generate(varDeclaration.getRight(), it, context);
		} else if (varDeclaration.getType() != null) {
			it.append(toDefaultValue(varDeclaration.getType()));
		} else {
			it.append("None"); //$NON-NLS-1$
		}
		if (context.getExpectedExpressionType() != null) {
			it.newLine();
			it.append("return ").append(name); //$NON-NLS-1$
		}
		return varDeclaration;
	}

	/** Replies the Python default value for the given type.
	 *
	 * @param type the type.
	 * @return the default value.
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity",
		"checkstyle:booleanexpressioncomplexity", "checkstyle:npathcomplexity"})
	public static String toDefaultValue(JvmTypeReference type) {
		final String id = type.getIdentifier();
		if (!"void".equals(id)) { //$NON-NLS-1$
			if (Strings.equal(Boolean.class.getName(), id) || Strings.equal(Boolean.TYPE.getName(), id)) {
				return "False"; //$NON-NLS-1$
			}
			if (Strings.equal(Float.class.getName(), id) || Strings.equal(Float.TYPE.getName(), id)
				|| Strings.equal(Double.class.getName(), id) || Strings.equal(Double.TYPE.getName(), id)) {
				return "0.0"; //$NON-NLS-1$
			}
			if (Strings.equal(Integer.class.getName(), id) || Strings.equal(Integer.TYPE.getName(), id)
				|| Strings.equal(Long.class.getName(), id) || Strings.equal(Long.TYPE.getName(), id)
				|| Strings.equal(Byte.class.getName(), id) || Strings.equal(Byte.TYPE.getName(), id)
				|| Strings.equal(Short.class.getName(), id) || Strings.equal(Short.TYPE.getName(), id)) {
				return "0"; //$NON-NLS-1$
			}
			if (Strings.equal(Character.class.getName(), id) || Strings.equal(Character.TYPE.getName(), id)) {
				return "\"\\0\""; //$NON-NLS-1$
			}
		}
		return "None"; //$NON-NLS-1$
	}

	/** Generate a feature call.
	 *
	 * @param context the generation context.
	 * @param it the code receiver.
	 * @return the generator
	 */
	protected PyFeatureCallGenerator newFeatureCallGenerator(IExtraLanguageGeneratorContext context, IAppendable it) {
		return new PyFeatureCallGenerator(context, (ExtraLanguageAppendable) it);
	}

	/** Feature call generator for Python.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public class PyFeatureCallGenerator extends FeatureCallGenerator {

		/** Constructor.
		 * @param context the generation context.
		 * @param codeReceiver the code receiver.
		 */
		protected PyFeatureCallGenerator(IExtraLanguageGeneratorContext context, ExtraLanguageAppendable codeReceiver) {
			super(context, codeReceiver);
		}

		private void appendCallPrefix(Collection<?> elements, String postfix) {
			if (elements != null && !elements.isEmpty()) {
				boolean first = true;
				for (final Object element : elements) {
					if (first) {
						first = false;
					} else {
						this.codeReceiver.append("."); //$NON-NLS-1$
					}
					if (element instanceof XExpression) {
						PyExpressionGenerator.this.generate((XExpression) element, this.codeReceiver, this.context);
					} else if (element instanceof JvmType) {
						this.codeReceiver.append((JvmType) element);
					} else if (element instanceof LightweightTypeReference) {
						this.codeReceiver.append((LightweightTypeReference) element);
					} else {
						this.codeReceiver.append(element.toString());
					}
				}
				this.codeReceiver.append(postfix);
			}
		}

		@Override
		protected void appendCall(JvmIdentifiableElement calledFeature, List<Object> leftOperand,
				List<Object> receiver, String name,
				List<XExpression> args, Function0<? extends XExpression> beginOfBlock) {
			if (beginOfBlock != null) {
				this.codeReceiver.append("if "); //$NON-NLS-1$
				PyExpressionGenerator.this.generate(beginOfBlock.apply(), this.codeReceiver, this.context);
				this.codeReceiver.append(" != None:"); //$NON-NLS-1$
				this.codeReceiver.increaseIndentation().newLine();
			}
			//
			appendCallPrefix(leftOperand, " = "); //$NON-NLS-1$
			appendCallPrefix(receiver, "."); //$NON-NLS-1$
			if (args != null) {
				this.codeReceiver.append(name);
				this.codeReceiver.append("("); //$NON-NLS-1$
				boolean first = true;
				for (final XExpression arg : args) {
					if (first) {
						first = false;
					} else {
						this.codeReceiver.append(", "); //$NON-NLS-1$
					}
					PyExpressionGenerator.this.generate(arg, this.codeReceiver, this.context);
				}
				this.codeReceiver.append(")"); //$NON-NLS-1$
			} else {
				this.codeReceiver.append(name);
			}
			//
			if (beginOfBlock != null) {
				this.codeReceiver.decreaseIndentation().newLine();
			}
		}

	}

}

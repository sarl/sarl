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

package io.sarl.lang.generator.extra.python3;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.inject.Inject;

import org.eclipse.xtend.core.xtend.AnonymousClass;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
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

import io.sarl.lang.generator.extra.AbstractExtraGenerator;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlScript;

/** The generator from SARL to the Python language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public class Python3Generator extends AbstractExtraGenerator {

	@Inject
	private IQualifiedNameProvider qualifiedNameProvider;

	private static String toModifierPrefix(XtendMember member) {
		final JvmVisibility visibility = member.getVisibility();
		if (visibility != null) {
			switch (visibility) {
			case PRIVATE:
				return "__"; //$NON-NLS-1$
			case DEFAULT:
			case PROTECTED:
				return "_"; //$NON-NLS-1$
			case PUBLIC:
			default:
			}
		}
		return ""; //$NON-NLS-1$
	}

	private static String toDefaultValue(JvmTypeReference type) {
		final String id = type.getIdentifier();
		if (!"void".equals(id)) { //$NON-NLS-1$
			switch (id) {
			case "boolean": //$NON-NLS-1$
				return "True"; //$NON-NLS-1$
			case "float": //$NON-NLS-1$
			case "double": //$NON-NLS-1$
				return "0.0"; //$NON-NLS-1$
			case "int": //$NON-NLS-1$
			case "long": //$NON-NLS-1$
			case "byte": //$NON-NLS-1$
			case "short": //$NON-NLS-1$
				return "0"; //$NON-NLS-1$
			case "char": //$NON-NLS-1$
				return "'\\0'"; //$NON-NLS-1$
			default:
			}
		}
		return "None"; //$NON-NLS-1$
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
	private void generatePythonClass(XtendTypeDeclaration type, GeneratorContext context, String name,
			List<JvmTypeReference> superTypes, IAppendable it) {
		it.append("class ").append(name).append("("); //$NON-NLS-1$//$NON-NLS-2$
		if (!superTypes.isEmpty()) {
			boolean first = true;
			for (final JvmTypeReference reference : superTypes) {
				if (first) {
					first = false;
				} else {
					it.append(", "); //$NON-NLS-1$
				}
				it.append(reference.getType());
			}
		} else {
			it.append("object"); //$NON-NLS-1$
		}
		it.append("):"); //$NON-NLS-1$
		it.increaseIndentation().newLine();

		boolean foundMember = false;

		final List<SarlField> fields = new ArrayList<>();
		final List<SarlConstructor> constructors = new ArrayList<>();

		for (final XtendMember member : type.getMembers()) {
			if (member instanceof XtendTypeDeclaration) {
				generate(member, context);
			} else if (member instanceof SarlField) {
				fields.add((SarlField) member);
				foundMember = true;
			} else if (member instanceof SarlConstructor) {
				constructors.add((SarlConstructor) member);
				foundMember = true;
			} else {
				generate(member, context, it);
				foundMember = true;
			}
		}
		if (!foundMember) {
			it.newLine();
			it.append("pass"); //$NON-NLS-1$
		} else if (constructors.isEmpty()) {
			if (!fields.isEmpty()) {
				it.append("def __init__(self):"); //$NON-NLS-1$
				it.increaseIndentation();
				generateFieldInitialization(fields, context, it);
			}
		} else {
			for (final SarlConstructor constructor : constructors) {
				it.append("def __init__"); //$NON-NLS-1$
				generateParameters(constructor.getParameters(), context, it);
				it.append(":"); //$NON-NLS-1$
				it.increaseIndentation();
				generateFieldInitialization(fields, context, it);
				if (constructor.getExceptions() != null) {
					generate(constructor.getExpression(), context, it);
				} else if (fields.isEmpty()) {
					it.newLine();
					it.append("pass"); //$NON-NLS-1$
				}
			}
		}

		it.decreaseIndentation().newLine();
	}

	private void generateParameters(List<? extends XtendParameter> parameters, GeneratorContext context, IAppendable it) {
		it.append("(self"); //$NON-NLS-1$
		for (final XtendParameter param : parameters) {
			it.append(", "); //$NON-NLS-1$
			if (param.isVarArg()) {
				it.append("*"); //$NON-NLS-1$
			}
			it.append(param.getName());
			if (param instanceof SarlFormalParameter) {
				final SarlFormalParameter sarlParameter = (SarlFormalParameter) param;
				if (sarlParameter.getDefaultValue() != null) {
					it.append(" = "); //$NON-NLS-1$
					generate(sarlParameter.getDefaultValue(), context, it);
				}
			}
		}
		it.append(")"); //$NON-NLS-1$
	}

	private void generateFieldInitialization(Iterable<SarlField> fields, GeneratorContext context, IAppendable it) {
		for (final SarlField field : fields) {
			it.newLine();
			it.append("self."); //$NON-NLS-1$
			it.append(toModifierPrefix(field));
			it.append(field.getName());
			it.append(" = "); //$NON-NLS-1$
			if (field.getInitialValue() != null) {
				generate(field.getInitialValue(), context, it);
			} else {
				it.append(toDefaultValue(field.getType()));
			}
		}
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 */
	protected void _generate(SarlScript object, GeneratorContext context) {
		for (final XtendTypeDeclaration type : object.getXtendTypes()) {
			if (context.getCancelIndicator().isCanceled()) {
				return;
			}
			final IAppendable appendable = new ExtraLanguageAppendable("\t", "\n"); //$NON-NLS-1$ //$NON-NLS-2$
			generate(type, context, appendable);
			if (context.getCancelIndicator().isCanceled()) {
				return;
			}
			final String content = appendable.getContent();
			if (!Strings.isEmpty(content)) {
				final QualifiedName qualifiedName = this.qualifiedNameProvider.getFullyQualifiedName(type);
				final String fileName = qualifiedName.toString(File.separator) + ".py"; //$NON-NLS-1$
				context.getFileSystemAccess().generateFile(fileName, content);
			}
		}
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(SarlClass object, GeneratorContext context, IAppendable it) {
		if (!Strings.isEmpty(object.getName())) {
			final List<JvmTypeReference> superTypes = new ArrayList<>();
			if (object.getExtends() != null
					&& !Objects.equals(object.getExtends().getIdentifier(), Object.class.getName())) {
				superTypes.add(object.getExtends());
			}
			for (final JvmTypeReference reference : object.getImplements()) {
				if (!Objects.equals(reference.getIdentifier(), Object.class.getName())) {
					superTypes.add(reference);
				}
			}
			generatePythonClass(object, context, object.getName(), superTypes, it);
		}
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(SarlInterface object, GeneratorContext context, IAppendable it) {
		if (!Strings.isEmpty(object.getName())) {
			final List<JvmTypeReference> superTypes = new ArrayList<>();
			for (final JvmTypeReference reference : object.getExtends()) {
				if (!Objects.equals(reference.getIdentifier(), Object.class.getName())) {
					superTypes.add(reference);
				}
			}
			generatePythonClass(object, context, object.getName(), superTypes, it);
		}
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(SarlEnumeration object, GeneratorContext context, IAppendable it) {
		if (!Strings.isEmpty(object.getName())) {
			generatePythonClass(object, context, object.getName(), Collections.emptyList(), it);
		}
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(SarlAnnotationType object, GeneratorContext context, IAppendable it) {
		if (!Strings.isEmpty(object.getName())) {
			generatePythonClass(object, context, object.getName(), Collections.emptyList(), it);
		}
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	@SuppressWarnings("static-method")
	protected void _generate(SarlBreakExpression object, GeneratorContext context, IAppendable it) {
		it.append("break"); //$NON-NLS-1$
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XAssignment object, GeneratorContext context, IAppendable it) {
		//TODO
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XBinaryOperation object, GeneratorContext context, IAppendable it) {
		//TODO
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XFeatureCall object, GeneratorContext context, IAppendable it) {
		//TODO
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XMemberFeatureCall object, GeneratorContext context, IAppendable it) {
		//TODO
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XPostfixOperation object, GeneratorContext context, IAppendable it) {
		//TODO
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XUnaryOperation object, GeneratorContext context, IAppendable it) {
		//TODO
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XConstructorCall object, GeneratorContext context, IAppendable it) {
		//TODO
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XClosure object, GeneratorContext context, IAppendable it) {
		//TODO
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(AnonymousClass object, GeneratorContext context, IAppendable it) {
		//TODO
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XDoWhileExpression object, GeneratorContext context, IAppendable it) {
		if (object.getBody() != null) {
			it.newLine();
			it.append("___condition = "); //$NON-NLS-1$
			generate(object.getPredicate(), context, it);
			it.newLine();
			it.append("while ___condition:"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generate(object.getBody(), context, it);
			it.newLine();
			it.append("___condition = "); //$NON-NLS-1$
			generate(object.getPredicate(), context, it);
			it.decreaseIndentation();
		}
	}


	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XWhileExpression object, GeneratorContext context, IAppendable it) {
		if (object.getBody() != null) {
			it.newLine();
			it.append("while "); //$NON-NLS-1$
			generate(object.getPredicate(), context, it);
			it.append(":"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generate(object.getBody(), context, it);
			it.decreaseIndentation();
		}
	}


	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XBasicForLoopExpression object, GeneratorContext context, IAppendable it) {
		if (object.getExpression() != null) {
			for (final XExpression initExpr : object.getInitExpressions()) {
				generate(initExpr, context, it);
			}
			it.newLine();
			it.append("while "); //$NON-NLS-1$
			generate(object.getEachExpression(), context, it);
			it.append(":"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generate(object.getExpression(), context, it);
			for (final XExpression initExpr : object.getUpdateExpressions()) {
				generate(initExpr, context, it);
			}
			it.decreaseIndentation();
		}
	}


	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XBlockExpression object, GeneratorContext context, IAppendable it) {
		for (final XExpression expr : object.getExpressions()) {
			it.increaseIndentation();
			generate(expr, context, it);
			it.decreaseIndentation();
		}
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	@SuppressWarnings("static-method")
	protected void _generate(XBooleanLiteral object, GeneratorContext context, IAppendable it) {
		it.append(object.isIsTrue() ? "True" : "False"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XCastedExpression object, GeneratorContext context, IAppendable it) {
		generate(object.getTarget(), context, it);
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XListLiteral object, GeneratorContext context, IAppendable it) {
		it.append("["); //$NON-NLS-1$
		boolean first = true;
		for (final XExpression value : object.getElements()) {
			if (first) {
				first = false;
			} else {
				it.append(", "); //$NON-NLS-1$
			}
			generate(value, context, it);
		}
		it.append("["); //$NON-NLS-1$
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XSetLiteral object, GeneratorContext context, IAppendable it) {
		it.append("{"); //$NON-NLS-1$
		boolean first = true;
		for (final XExpression value : object.getElements()) {
			if (first) {
				first = false;
			} else {
				it.append(", "); //$NON-NLS-1$
			}
			generate(value, context, it);
		}
		it.append("}"); //$NON-NLS-1$
	}


	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XForLoopExpression object, GeneratorContext context, IAppendable it) {
		//TODO
	}


	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XIfExpression object, GeneratorContext context, IAppendable it) {
		if (object.getElse() != null || object.getThen() != null) {
			it.append("if "); //$NON-NLS-1$
			generate(object.getIf(), context, it);
			it.append(":"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			if (object.getThen() != null) {
				generate(object.getThen(), context, it);
			} else {
				it.append("pass"); //$NON-NLS-1$
			}
			it.decreaseIndentation();
			if (object.getElse() != null) {
				it.append("else:"); //$NON-NLS-1$
				it.increaseIndentation().newLine();
				generate(object.getElse(), context, it);
				it.decreaseIndentation();
			}
		}
	}


	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XInstanceOfExpression object, GeneratorContext context, IAppendable it) {
		it.append("isinstance("); //$NON-NLS-1$
		generate(object.getExpression(), context, it);
		it.append(", "); //$NON-NLS-1$
		it.append(object.getType().getType());
		it.append(")"); //$NON-NLS-1$
	}


	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	@SuppressWarnings("static-method")
	protected void _generate(XNullLiteral object, GeneratorContext context, IAppendable it) {
		it.append("None"); //$NON-NLS-1$
	}


	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	@SuppressWarnings("static-method")
	protected void _generate(XNumberLiteral object, GeneratorContext context, IAppendable it) {
		it.append(object.getValue());
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XReturnExpression object, GeneratorContext context, IAppendable it) {
		it.append("return "); //$NON-NLS-1$
		generate(object.getExpression(), context, it);
	}


	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	@SuppressWarnings("static-method")
	protected void _generate(XStringLiteral object, GeneratorContext context, IAppendable it) {
		it.append("\"").append(Strings.convertToJavaString(object.getValue())).append("\""); //$NON-NLS-1$//$NON-NLS-2$
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XSwitchExpression object, GeneratorContext context, IAppendable it) {
		it.append("___expression = "); //$NON-NLS-1$
		generate(object.getSwitch(), context, it);
		boolean first = true;
		for (final XCasePart caseExpression : object.getCases()) {
			if (first) {
				it.append("if "); //$NON-NLS-1$
				first = false;
			} else {
				it.append("elif "); //$NON-NLS-1$
			}
			it.append("___expression == "); //$NON-NLS-1$
			generate(caseExpression.getCase(), context, it);
			it.append(":"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generate(caseExpression.getThen(), context, it);
			it.decreaseIndentation().newLine();
		}
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XSynchronizedExpression object, GeneratorContext context, IAppendable it) {
		generate(object.getExpression(), context, it);
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XThrowExpression object, GeneratorContext context, IAppendable it) {
		it.append("raise "); //$NON-NLS-1$
		generate(object.getExpression(), context, it);
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XTryCatchFinallyExpression object, GeneratorContext context, IAppendable it) {
		if (!object.getCatchClauses().isEmpty() || object.getFinallyExpression() != null) {
			it.append("try:"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generate(object.getExpression(), context, it);
			it.decreaseIndentation();
			for (final XCatchClause clause : object.getCatchClauses()) {
				it.newLine();
				it.append("except "); //$NON-NLS-1$
				generate(clause.getExpression(), context, it);
				it.append(":"); //$NON-NLS-1$
				it.increaseIndentation().newLine();
				generate(clause.getExpression(), context, it);
				it.decreaseIndentation();
			}
		}
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	@SuppressWarnings("static-method")
	protected void _generate(XTypeLiteral object, GeneratorContext context, IAppendable it) {
		it.append(object.getType());
	}

	/** Generate the given object.
	 *
	 * @param object the object.
	 * @param context the context.
	 * @param it the target for the generated content.
	 */
	protected void _generate(XVariableDeclaration object, GeneratorContext context, IAppendable it) {
		it.append(object.getName());
		it.append(" = "); //$NON-NLS-1$
		if (object.getRight() != null) {
			generate(object.getRight(), context, it);
		} else if (object.getType() != null) {
			it.append(toDefaultValue(object.getType()));
		} else {
			it.append("None"); //$NON-NLS-1$
		}
	}

}

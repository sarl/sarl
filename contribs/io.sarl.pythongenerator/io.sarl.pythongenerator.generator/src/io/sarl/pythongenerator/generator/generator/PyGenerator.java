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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import javax.inject.Inject;

import org.eclipse.xtend.core.xtend.XtendEnumLiteral;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.extralanguage.compiler.AbstractExtraLanguageGenerator;
import io.sarl.lang.extralanguage.compiler.ExtraLanguageAppendable;
import io.sarl.lang.extralanguage.compiler.ExtraLanguageTypeConverter;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageGeneratorContext;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.sarl.actionprototype.InferredPrototype;
import io.sarl.lang.sarl.actionprototype.InferredStandardParameter;
import io.sarl.lang.sarl.actionprototype.InferredValuedParameter;
import io.sarl.lang.sarl.actionprototype.QualifiedActionName;
import io.sarl.lang.util.Utils;
import io.sarl.pythongenerator.generator.PyGeneratorPlugin;
import io.sarl.pythongenerator.generator.configuration.IPyGeneratorConfigurationProvider;
import io.sarl.pythongenerator.generator.configuration.PyGeneratorConfiguration;
import io.sarl.pythongenerator.generator.configuration.PyOutputConfigurationProvider;

/** The generator from SARL to the Python language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public class PyGenerator extends AbstractExtraLanguageGenerator {

	/** Header for a Python file.
	 */
	public static final String PYTHON_FILE_HEADER = "#!/usr/bin/env python3"; //$NON-NLS-1$

	private static final String PYTHON_FILENAME_EXTENSION = ".py"; //$NON-NLS-1$

	private static final String LIBRARY_CONTENT = "__all__ = []"; //$NON-NLS-1$

	private static final String LIBRARY_FILENAME = "__init__"; //$NON-NLS-1$

	private static final String INSTANCE_VARIABLES_MEMENTO = "INSTANCE_VARIABLES"; //$NON-NLS-1$

	private static final String EVENT_GUARDS_MEMENTO = "EVENT_GUARDS"; //$NON-NLS-1$

	@Inject
	private IQualifiedNameProvider qualifiedNameProvider;

	private PyExpressionGenerator expressionGenerator;

	private IPyGeneratorConfigurationProvider configuration;

	private Map<JvmOperation, String> useCapacityMapping = new HashMap<>();

	/** Change the provider of the generator's configuration.
	 *
	 * @param provider the provider.
	 */
	@Inject
	public void setPyGeneratorConfigurationProvider(IPyGeneratorConfigurationProvider provider) {
		this.configuration = provider;
	}

	/** Change the generator of XExpression.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setExpressionGenerator(PyExpressionGenerator generator) {
		this.expressionGenerator = generator;
	}

	/** Replies the generator of XExpression.
	 *
	 * @return the generator.
	 */
	@Override
	public PyExpressionGenerator getExpressionGenerator() {
		return this.expressionGenerator;
	}

	@Override
	protected void initializeContext(IExtraLanguageGeneratorContext generatorContext) {
		final PyGeneratorConfiguration config = this.configuration.get(generatorContext.getResource(), true);
		final ExtraLanguageTypeConverter converter = getExpressionGenerator().getTypeConverter(generatorContext);
		converter.setImplicitJvmTypes(config.isImplicitJvmTypes());
	}

	@Override
	protected PyAppendable createAppendable(JvmDeclaredType thisType, IExtraLanguageGeneratorContext context) {
		final ExtraLanguageTypeConverter converter = getTypeConverter(context);
		final PyAppendable appendable = new PyAppendable(thisType, converter);
		markCapacityFunctions(appendable);
		return appendable;
	}

	@Override
	protected String getOutputConfigurationName() {
		return PyOutputConfigurationProvider.OUTPUT_CONFIGURATION_NAME;
	}

	@Override
	protected String getFilenameExtension() {
		return PYTHON_FILENAME_EXTENSION;
	}

	@Override
	protected String getPreferenceID() {
		return PyGeneratorPlugin.PREFERENCE_ID;
	}

	@Override
	protected boolean writeFile(QualifiedName name, ExtraLanguageAppendable appendable,
			IExtraLanguageGeneratorContext context) {
		if (super.writeFile(name, appendable, context)) {
			// Generate the package files for the Python library
			writePackageFiles(name, appendable.getLineSeparator(), context);
			return true;
		}
		return false;
	}

	/** Generate the Python package files.
	 *
	 * <p>This function generates the "__init__.py" files for all the packages.
	 *
	 * @param name the name of the generated type.
	 * @param lineSeparator the line separator.
	 * @param context the generation context.
	 */
	protected void writePackageFiles(QualifiedName name, String lineSeparator,
			IExtraLanguageGeneratorContext context) {
		final IFileSystemAccess2 fsa = context.getFileSystemAccess();
		final String outputConfiguration = getOutputConfigurationName();
		QualifiedName libraryName = null;
		for (final String segment : name.skipLast(1).getSegments()) {
			if (libraryName == null) {
				libraryName = QualifiedName.create(segment, LIBRARY_FILENAME);
			} else {
				libraryName = libraryName.append(segment).append(LIBRARY_FILENAME);
			}
			final String fileName = toFilename(libraryName);
			if (!fsa.isFile(fileName)) {
				final String content = PYTHON_FILE_HEADER + lineSeparator
						+ getGenerationComment(context) + lineSeparator
						+ LIBRARY_CONTENT;
				if (Strings.isEmpty(outputConfiguration)) {
					fsa.generateFile(fileName, content);
				} else {
					fsa.generateFile(fileName, outputConfiguration, content);
				}
			}
			libraryName = libraryName.skipLast(1);
		}
	}

	/** Replies a string representing a comment with the generation information.
	 *
	 * @param context the generation context.
	 * @return the comment text.
	 */
	@SuppressWarnings("static-method")
	protected String getGenerationComment(IExtraLanguageGeneratorContext context) {
		return "# Generated by the SARL compiler the " + context.getGenerationDate().toString() //$NON-NLS-1$
				+ ". Do not change this file."; //$NON-NLS-1$
	}

	/** Generate the type declaration for a Python class.
	 *
	 * @param typeName name of the type.
	 * @param isAbstract indicates if the type is abstract (interface included).
	 * @param superTypes the super types.
	 * @param comment the type declaration's comment.
	 * @param ignoreObjectType ignores the "Object" type if the super types.
	 * @param it the output.
	 * @param context the generation context.
	 * @return {@code true} if the declaration was generated. {@code false} if the declaration was not generated.
	 */
	@SuppressWarnings("static-method")
	protected boolean generatePythonClassDeclaration(String typeName, boolean isAbstract,
			List<? extends JvmTypeReference> superTypes, String comment, boolean ignoreObjectType, PyAppendable it,
			IExtraLanguageGeneratorContext context) {
		if (!Strings.isEmpty(typeName)) {
			it.append("class ").append(typeName).append("("); //$NON-NLS-1$ //$NON-NLS-2$
			boolean isOtherSuperType = false;
			boolean first = true;
			for (final JvmTypeReference reference : superTypes) {
				if (!ignoreObjectType || !Strings.equal(reference.getQualifiedName(), Object.class.getCanonicalName())) {
					isOtherSuperType = true;
					if (first) {
						first = false;
					} else {
						it.append(","); //$NON-NLS-1$
					}
					it.append(reference.getType());
				}
			}
			if (isOtherSuperType) {
				it.append(","); //$NON-NLS-1$
			}
			// Add "object to avoid a bug within the Python interpreter.
			it.append("object):"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generateDocString(comment, it);
			return true;
		}
		return false;
	}

	/** Generate a Python docstring with the given comment.
	 *
	 * @param comment the comment.
	 * @param it the receiver of the docstring.
	 * @return {@code true} if the docstring is added, {@code false} otherwise.
	 */
	protected static boolean generateDocString(String comment, PyAppendable it) {
		final String cmt = comment == null ? null : comment.trim();
		if (!Strings.isEmpty(cmt)) {
			assert cmt != null;
			it.append("\"\"\"").increaseIndentation(); //$NON-NLS-1$
			for (final String line : cmt.split("[\n\r\f]+")) {  //$NON-NLS-1$
				it.newLine().append(line);
			}
			it.decreaseIndentation().newLine();
			it.append("\"\"\"").newLine(); //$NON-NLS-1$
			return true;
		}
		return false;
	}

	/** Generate a Python block comment with the given comment.
	 *
	 * @param comment the comment.
	 * @param it the receiver of the block comment.
	 * @return {@code true} if the block comment is added, {@code false} otherwise.
	 */
	protected static boolean generateBlockComment(String comment, PyAppendable it) {
		final String cmt = comment == null ? null : comment.trim();
		if (!Strings.isEmpty(cmt)) {
			assert cmt != null;
			for (final String line : cmt.split("[\n\r\f]+")) {  //$NON-NLS-1$
				it.append("# ").append(line).newLine(); //$NON-NLS-1$
			}
			return true;
		}
		return false;
	}

	/** Generate the given type.
	 *
	 * @param fullyQualifiedName the fully qualified name of the type.
	 * @param name the name of the type.
	 * @param isAbstract indicates if the type is abstract.
	 * @param superTypes the super types.
	 * @param comment the comment.
	 * @param ignoreObjectType ignores the "Object" type if the super types.
	 * @param members the members.
	 * @param it the output.
	 * @param context the context.
	 * @param memberGenerator the generator of members.
	 * @return {@code true} if the type declaration was generated.
	 */
	@SuppressWarnings({ "checkstyle:parameternumber" })
	protected boolean generateTypeDeclaration(
			String fullyQualifiedName,
			String name, boolean isAbstract,
			List<? extends JvmTypeReference> superTypes,
			String comment,
			boolean ignoreObjectType, List<? extends XtendMember> members, PyAppendable it,
			IExtraLanguageGeneratorContext context, Procedure2<? super PyAppendable, ? super IExtraLanguageGeneratorContext> memberGenerator) {
		if (!Strings.isEmpty(name)) {
			if (!generatePythonClassDeclaration(name, isAbstract, superTypes, comment, ignoreObjectType, it, context)
				|| context.getCancelIndicator().isCanceled()) {
				return false;
			}
			//
			it.openScope();
			//
			if (!generateSarlMembers(members, it, context)
				|| context.getCancelIndicator().isCanceled()) {
				return false;
			}
			//
			if (memberGenerator != null) {
				memberGenerator.apply(it,  context);
			}
			//
			if (!generatePythonConstructors(fullyQualifiedName, members, it, context)
				|| context.getCancelIndicator().isCanceled()) {
				return false;
			}
			//
			it.decreaseIndentation().newLine().newLine();
			//
			it.closeScope();
			//
			if (context.getCancelIndicator().isCanceled()) {
				return false;
			}
			return true;
		}
		return false;
	}

	/** Generate the given enumeration declaration.
	 *
	 * @param enumeration the enumeration.
	 * @param it the receiver of the generated code.
	 * @param context the context.
	 * @return {@code true} if a declaration was generated. {@code false} if no enumeration was generated.
	 * @since 0.8
	 */
	protected boolean generateEnumerationDeclaration(SarlEnumeration enumeration, PyAppendable it, IExtraLanguageGeneratorContext context) {
		if (!Strings.isEmpty(enumeration.getName())) {
			it.append("class ").append(enumeration.getName()); //$NON-NLS-1$
			it.append("(Enum"); //$NON-NLS-1$
			it.append(newType("enum.Enum")); //$NON-NLS-1$
			it.append("):"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			generateDocString(getTypeBuilder().getDocumentation(enumeration), it);
			int i = 0;
			for (final XtendMember item : enumeration.getMembers()) {
				if (context.getCancelIndicator().isCanceled()) {
					return false;
				}
				if (item instanceof XtendEnumLiteral) {
					final XtendEnumLiteral literal = (XtendEnumLiteral) item;
					it.append(literal.getName()).append(" = "); //$NON-NLS-1$
					it.append(Integer.toString(i));
					it.newLine();
					++i;
				}
			}
			//
			it.decreaseIndentation().newLine().newLine();
			return true;
		}
		return false;
	}

	/** Generate the constructors for a Python class.
	 *
	 * @param container the fully qualified name of the container.
	 * @param members the members to be added.
	 * @param it the output.
	 * @param context the generation context.
	 * @return {@code true} if a constructor was generated. {@code false} if no constructor was generated.
	 */
	protected boolean generatePythonConstructors(String container, List<? extends XtendMember> members,
			PyAppendable it, IExtraLanguageGeneratorContext context) {
		// Prepare field initialization
		boolean hasConstructor = false;
		for (final XtendMember member : members) {
			if (context.getCancelIndicator().isCanceled()) {
				return false;
			}
			if (member instanceof SarlConstructor) {
				hasConstructor = true;
				generate(member, it, context);
				it.newLine();
			}
		}
		if (context.getCancelIndicator().isCanceled()) {
			return false;
		}
		if (!hasConstructor) {
			it.append("def __init__(self):"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			final List<SarlField> fields = context.getMultimapValues(INSTANCE_VARIABLES_MEMENTO, container);
			if (fields.isEmpty()) {
				it.append("pass"); //$NON-NLS-1$
			} else {
				for (final SarlField field : fields) {
					generatePythonField(field, it, context);
				}
			}
			it.decreaseIndentation().newLine();
		}
		return true;
	}

	/** Create a JvmType for a Python type.
	 *
	 * @param pythonName the python type name.
	 * @return the type.
	 */
	@SuppressWarnings("static-method")
	protected JvmType newType(String pythonName) {
		final JvmGenericType type = TypesFactory.eINSTANCE.createJvmGenericType();
		final int index = pythonName.indexOf("."); //$NON-NLS-1$
		if (index <= 0) {
			type.setSimpleName(pythonName);
		} else {
			type.setPackageName(pythonName.substring(0, index - 1));
			type.setSimpleName(pythonName.substring(index + 1));
		}
		return type;
	}

	/** Create a field declaration.
	 *
	 * @param field the field to generate.
	 * @param it the output
	 * @param context the generation context.
	 */
	protected void generatePythonField(SarlField field, PyAppendable it, IExtraLanguageGeneratorContext context) {
		generateBlockComment(getTypeBuilder().getDocumentation(field), it);
		if (!field.isStatic()) {
			it.append("self."); //$NON-NLS-1$
		}
		final String fieldName = it.declareUniqueNameVariable(field, field.getName());
		it.append(fieldName);
		it.append(" = "); //$NON-NLS-1$
		if (field.getInitialValue() != null) {
			generate(field.getInitialValue(), null, it, context);
		} else {
			it.append(PyExpressionGenerator.toDefaultValue(field.getType()));
		}
		it.newLine();
	}

	/** Generate the Python code for an executable statement.
	 *
	 * @param name the name of the executable.
	 * @param executable the executable statement.
	 * @param appendSelf indicates if the "self" parameter should be added.
	 * @param isAbstract indicates if the executable is abstract.
	 * @param returnType the type of the value to be returned, or {@code null} if void.
	 * @param comment the comment associated to the function.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	@SuppressWarnings({"checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity"})
	protected void generateExecutable(String name, XtendExecutable executable, boolean appendSelf, boolean isAbstract,
			JvmTypeReference returnType, String comment, PyAppendable it, IExtraLanguageGeneratorContext context) {
		it.append("def ").append(name); //$NON-NLS-1$
		it.append("("); //$NON-NLS-1$
		boolean firstParam = true;
		if (appendSelf) {
			firstParam = false;
			it.append(getExpressionGenerator().getExtraLanguageKeywordProvider().getThisKeywordLambda().apply());
		}
		for (final XtendParameter parameter : executable.getParameters()) {
			if (firstParam) {
				firstParam = false;
			} else {
				it.append(", "); //$NON-NLS-1$
			}
			if (parameter.isVarArg()) {
				it.append("*"); //$NON-NLS-1$
			}
			final String pname = it.declareUniqueNameVariable(parameter, parameter.getName());
			it.append(pname).append(" : ").append(parameter.getParameterType().getType()); //$NON-NLS-1$
		}
		final LightweightTypeReference actualReturnType = getExpectedType(executable, returnType);
		it.append(")"); //$NON-NLS-1$
		if (actualReturnType != null) {
			it.append(" -> ").append(actualReturnType); //$NON-NLS-1$
		}
		it.append(":"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		generateDocString(comment, it);
		if (executable.getExpression() != null) {
			it.openScope();
			generate(executable.getExpression(), actualReturnType, it, context);
			it.closeScope();
		} else if (isAbstract) {
			it.append("raise Exception(\"Unimplemented function\")"); //$NON-NLS-1$
		} else {
			it.append("pass"); //$NON-NLS-1$
		}
		it.decreaseIndentation().newLine();
		//
		// Generate the additional functions
		final IActionPrototypeProvider prototypeProvider = getActionPrototypeProvider();
		final QualifiedActionName actionName = prototypeProvider.createQualifiedActionName(
				(JvmIdentifiableElement) getJvmModelAssociations().getPrimaryJvmElement(executable.getDeclaringType()),
				name);
		final InferredPrototype inferredPrototype = getActionPrototypeProvider().createPrototypeFromSarlModel(
				context.getActionPrototypeContext(getActionPrototypeProvider()),
				actionName,
				Utils.isVarArg(executable.getParameters()), executable.getParameters());
		for (final Entry<ActionParameterTypes, List<InferredStandardParameter>> types : inferredPrototype.getInferredParameterTypes().entrySet()) {
			final List<InferredStandardParameter> argumentsToOriginal = types.getValue();
			it.append("def ").append(name); //$NON-NLS-1$
			it.append("(self"); //$NON-NLS-1$
			for (final InferredStandardParameter parameter : argumentsToOriginal) {
				if (!(parameter instanceof InferredValuedParameter)) {
					it.append(", "); //$NON-NLS-1$
					if (((XtendParameter) parameter.getParameter()).isVarArg()) {
						it.append("*"); //$NON-NLS-1$
					}
					it.append(parameter.getName()).append(" : ").append(parameter.getType().getType()); //$NON-NLS-1$
				}
			}
			it.append(")"); //$NON-NLS-1$
			if (actualReturnType != null) {
				it.append(" -> ").append(actualReturnType); //$NON-NLS-1$
			}
			it.append(":"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			if (actualReturnType != null) {
				it.append("return "); //$NON-NLS-1$
			}
			it.append("self.").append(name).append("("); //$NON-NLS-1$ //$NON-NLS-2$
			boolean first = true;
			for (final InferredStandardParameter parameter : argumentsToOriginal) {
				if (first) {
					first = false;
				} else {
					it.append(", "); //$NON-NLS-1$
				}
				if (parameter instanceof InferredValuedParameter) {
					final InferredValuedParameter valuedParameter = (InferredValuedParameter) parameter;
					generate(((SarlFormalParameter) valuedParameter.getParameter()).getDefaultValue(), null, it, context);
				} else {
					it.append(parameter.getName());
				}
			}
			it.append(")"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
		}
	}

	/** Generate the memorized guard evaluators.
	 *
	 * @param container the fully qualified name of the container of the guards.
	 * @param it the output.
	 * @param context the generation context.
	 */
	protected void generateGuardEvaluators(String container, PyAppendable it, IExtraLanguageGeneratorContext context) {
		final Map<String, Map<String, List<Pair<XExpression, String>>>> allGuardEvaluators = context.getMapData(EVENT_GUARDS_MEMENTO);
		final Map<String, List<Pair<XExpression, String>>> guardEvaluators = allGuardEvaluators.get(container);
		if (guardEvaluators == null) {
			return;
		}
		boolean first = true;
		for (final Entry<String, List<Pair<XExpression, String>>> entry : guardEvaluators.entrySet()) {
			if (first) {
				first = false;
			} else {
				it.newLine();
			}
			it.append("def __guard_"); //$NON-NLS-1$
			it.append(entry.getKey().replaceAll("[^a-zA-Z0-9_]+", "_")); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("__(self, occurrence):"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append("it = occurrence").newLine(); //$NON-NLS-1$
			final String eventHandleName = it.declareUniqueNameVariable(new Object(), "__event_handles"); //$NON-NLS-1$
			it.append(eventHandleName).append(" = list"); //$NON-NLS-1$
			for (final Pair<XExpression, String> guardDesc : entry.getValue()) {
				it.newLine();
				if (guardDesc.getKey() == null) {
					it.append(eventHandleName).append(".add(").append(guardDesc.getValue()).append(")"); //$NON-NLS-1$ //$NON-NLS-2$
				} else {
					it.append("if "); //$NON-NLS-1$
					generate(guardDesc.getKey(), null, it, context);
					it.append(":").increaseIndentation().newLine(); //$NON-NLS-1$
					it.append(eventHandleName).append(".add(").append(guardDesc.getValue()).append(")"); //$NON-NLS-1$ //$NON-NLS-2$
					it.decreaseIndentation();
				}
			}
			it.newLine().append("return ").append(eventHandleName); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
		}
	}

	//----------------------------------------
	// File Header
	//----------------------------------------

	@Override
	protected void generateFileHeader(QualifiedName qualifiedName, ExtraLanguageAppendable appendable,
			IExtraLanguageGeneratorContext context) {
		appendable.append(PYTHON_FILE_HEADER);
		appendable.newLine();
		appendable.append(getGenerationComment(context));
		appendable.newLine().newLine();
	}

	@Override
	protected void generateImportStatement(QualifiedName qualifiedName, ExtraLanguageAppendable appendable,
			IExtraLanguageGeneratorContext context) {
		final String typeName = qualifiedName.getLastSegment();
		final QualifiedName packageName = qualifiedName.skipLast(1);
		appendable.append("from "); //$NON-NLS-1$
		appendable.append(packageName.toString());
		appendable.append(" import "); //$NON-NLS-1$
		appendable.append(typeName);
		appendable.newLine();
	}

	//----------------------------------------
	// Types
	//----------------------------------------

	/** Generate the given object.
	 *
	 * @param clazz the class.
	 * @param context the context.
	 */
	protected void _generate(SarlClass clazz, IExtraLanguageGeneratorContext context) {
		final JvmDeclaredType jvmType = getJvmModelAssociations().getInferredType(clazz);
		final PyAppendable appendable = createAppendable(jvmType, context);
		if (generateTypeDeclaration(
				this.qualifiedNameProvider.getFullyQualifiedName(clazz).toString(),
				clazz.getName(), clazz.isAbstract(),
				getSuperTypes(clazz.getExtends(), clazz.getImplements()),
				getTypeBuilder().getDocumentation(clazz),
				true,
				clazz.getMembers(), appendable, context, null)) {
			final QualifiedName name = getQualifiedNameProvider().getFullyQualifiedName(clazz);
			writeFile(name, appendable, context);
		}
	}

	/** Generate the given object.
	 *
	 * @param interf the interface.
	 * @param context the context.
	 */
	protected void _generate(SarlInterface interf, IExtraLanguageGeneratorContext context) {
		final JvmDeclaredType jvmType = getJvmModelAssociations().getInferredType(interf);
		final PyAppendable appendable = createAppendable(jvmType, context);
		if (generateTypeDeclaration(
				this.qualifiedNameProvider.getFullyQualifiedName(interf).toString(),
				interf.getName(), true, interf.getExtends(),
				getTypeBuilder().getDocumentation(interf),
				true,
				interf.getMembers(), appendable, context, null)) {
			final QualifiedName name = getQualifiedNameProvider().getFullyQualifiedName(interf);
			writeFile(name, appendable, context);
		}
	}

	/** Generate the given object.
	 *
	 * @param enumeration the enumeration.
	 * @param context the context.
	 */
	protected void _generate(SarlEnumeration enumeration, IExtraLanguageGeneratorContext context) {
		final JvmDeclaredType jvmType = getJvmModelAssociations().getInferredType(enumeration);
		final PyAppendable appendable = createAppendable(jvmType, context);
		if (generateEnumerationDeclaration(enumeration, appendable, context)) {
			final QualifiedName name = getQualifiedNameProvider().getFullyQualifiedName(enumeration);
			writeFile(name, appendable, context);
		}
	}

	/** Generate the given object.
	 *
	 * @param annotation the annotation.
	 * @param context the context.
	 */
	protected void _generate(SarlAnnotationType annotation, IExtraLanguageGeneratorContext context) {
		final JvmDeclaredType jvmType = getJvmModelAssociations().getInferredType(annotation);
		final PyAppendable appendable = createAppendable(jvmType, context);
		if (generateTypeDeclaration(
				this.qualifiedNameProvider.getFullyQualifiedName(annotation).toString(),
				annotation.getName(), false, Collections.emptyList(),
				getTypeBuilder().getDocumentation(annotation),
				true,
				annotation.getMembers(), appendable, context, null)) {
			final QualifiedName name = getQualifiedNameProvider().getFullyQualifiedName(annotation);
			writeFile(name, appendable, context);
		}
	}

	/** Generate the given object.
	 *
	 * @param event the event.
	 * @param context the context.
	 */
	protected void _generate(SarlEvent event, IExtraLanguageGeneratorContext context) {
		final JvmDeclaredType jvmType = getJvmModelAssociations().getInferredType(event);
		final PyAppendable appendable = createAppendable(jvmType, context);
		final List<JvmTypeReference> superTypes;
		if (event.getExtends() != null) {
			superTypes = Collections.singletonList(event.getExtends());
		} else {
			superTypes = Collections.singletonList(getTypeReferences().getTypeForName(Event.class, event));
		}
		if (generateTypeDeclaration(
				this.qualifiedNameProvider.getFullyQualifiedName(event).toString(),
				event.getName(), event.isAbstract(), superTypes,
				getTypeBuilder().getDocumentation(event),
				true,
				event.getMembers(), appendable, context, null)) {
			final QualifiedName name = getQualifiedNameProvider().getFullyQualifiedName(event);
			writeFile(name, appendable, context);
		}
	}

	/** Generate the given object.
	 *
	 * @param agent the agent.
	 * @param context the context.
	 */
	protected void _generate(SarlAgent agent, IExtraLanguageGeneratorContext context) {
		final JvmDeclaredType jvmType = getJvmModelAssociations().getInferredType(agent);
		final PyAppendable appendable = createAppendable(jvmType, context);
		final List<JvmTypeReference> superTypes;
		if (agent.getExtends() != null) {
			superTypes = Collections.singletonList(agent.getExtends());
		} else {
			superTypes = Collections.singletonList(getTypeReferences().getTypeForName(Agent.class, agent));
		}
		final String qualifiedName = this.qualifiedNameProvider.getFullyQualifiedName(agent).toString();
		if (generateTypeDeclaration(
				qualifiedName,
				agent.getName(), agent.isAbstract(), superTypes,
				getTypeBuilder().getDocumentation(agent),
				true,
			agent.getMembers(), appendable, context, (it, context2) -> {
				generateGuardEvaluators(qualifiedName, it, context2);
			})) {
			final QualifiedName name = getQualifiedNameProvider().getFullyQualifiedName(agent);
			writeFile(name, appendable, context);
		}
	}

	/** Generate the given object.
	 *
	 * @param behavior the behavior.
	 * @param context the context.
	 */
	protected void _generate(SarlBehavior behavior, IExtraLanguageGeneratorContext context) {
		final JvmDeclaredType jvmType = getJvmModelAssociations().getInferredType(behavior);
		final PyAppendable appendable = createAppendable(jvmType, context);
		final List<JvmTypeReference> superTypes;
		if (behavior.getExtends() != null) {
			superTypes = Collections.singletonList(behavior.getExtends());
		} else {
			superTypes = Collections.singletonList(getTypeReferences().getTypeForName(Behavior.class, behavior));
		}
		final String qualifiedName = this.qualifiedNameProvider.getFullyQualifiedName(behavior).toString();
		if (generateTypeDeclaration(
				qualifiedName,
				behavior.getName(), behavior.isAbstract(), superTypes,
				getTypeBuilder().getDocumentation(behavior),
				true,
				behavior.getMembers(), appendable, context, (it, context2) -> {
				generateGuardEvaluators(qualifiedName, it, context2);
			})) {
			final QualifiedName name = getQualifiedNameProvider().getFullyQualifiedName(behavior);
			writeFile(name, appendable, context);
		}
	}

	/** Generate the given object.
	 *
	 * @param capacity the capacity.
	 * @param context the context.
	 */
	protected void _generate(SarlCapacity capacity, IExtraLanguageGeneratorContext context) {
		final JvmDeclaredType jvmType = getJvmModelAssociations().getInferredType(capacity);
		final PyAppendable appendable = createAppendable(jvmType, context);
		final List<? extends JvmTypeReference> superTypes;
		if (!capacity.getExtends().isEmpty()) {
			superTypes = capacity.getExtends();
		} else {
			superTypes = Collections.singletonList(getTypeReferences().getTypeForName(Capacity.class, capacity));
		}
		if (generateTypeDeclaration(
				this.qualifiedNameProvider.getFullyQualifiedName(capacity).toString(),
				capacity.getName(), true, superTypes,
				getTypeBuilder().getDocumentation(capacity),
				true,
				capacity.getMembers(), appendable, context, null)) {
			final QualifiedName name = getQualifiedNameProvider().getFullyQualifiedName(capacity);
			writeFile(name, appendable, context);
		}
	}

	/** Generate the given object.
	 *
	 * @param skill the skill.
	 * @param context the context.
	 */
	protected void _generate(SarlSkill skill, IExtraLanguageGeneratorContext context) {
		final JvmDeclaredType jvmType = getJvmModelAssociations().getInferredType(skill);
		final PyAppendable appendable = createAppendable(jvmType, context);

		List<JvmTypeReference> superTypes = getSuperTypes(skill.getExtends(), skill.getImplements());
		if (superTypes.isEmpty()) {
			superTypes = Collections.singletonList(getTypeReferences().getTypeForName(Skill.class, skill));
		}
		final String qualifiedName = this.qualifiedNameProvider.getFullyQualifiedName(skill).toString();
		if (generateTypeDeclaration(
				qualifiedName,
				skill.getName(), skill.isAbstract(), superTypes,
				getTypeBuilder().getDocumentation(skill),
				true,
				skill.getMembers(), appendable, context, (it, context2) -> {
				generateGuardEvaluators(qualifiedName, it, context2);
			})) {
			final QualifiedName name = getQualifiedNameProvider().getFullyQualifiedName(skill);
			writeFile(name, appendable, context);
		}
	}

	//----------------------------------------
	// Members
	//----------------------------------------

	/** Generate the given object.
	 *
	 * @param clazz the class.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	protected void _generate(SarlClass clazz, PyAppendable it, IExtraLanguageGeneratorContext context) {
		generateTypeDeclaration(
				this.qualifiedNameProvider.getFullyQualifiedName(clazz).toString(),
				clazz.getName(), clazz.isAbstract(),
				getSuperTypes(clazz.getExtends(), clazz.getImplements()),
				getTypeBuilder().getDocumentation(clazz),
				true,
				clazz.getMembers(), it, context, null);
	}

	/** Generate the given object.
	 *
	 * @param interf the interface.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	protected void _generate(SarlInterface interf, PyAppendable it, IExtraLanguageGeneratorContext context) {
		generateTypeDeclaration(
				this.qualifiedNameProvider.getFullyQualifiedName(interf).toString(),
				interf.getName(), true, interf.getExtends(),
				getTypeBuilder().getDocumentation(interf),
				true,
				interf.getMembers(), it, context, null);
	}

	/** Generate the given object.
	 *
	 * @param enumeration the enumeration.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	protected void _generate(SarlEnumeration enumeration, PyAppendable it, IExtraLanguageGeneratorContext context) {
		generateEnumerationDeclaration(enumeration, it, context);
	}

	/** Generate the given object.
	 *
	 * @param annotation the annotation.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	protected void _generate(SarlAnnotationType annotation, PyAppendable it, IExtraLanguageGeneratorContext context) {
		generateTypeDeclaration(
				this.qualifiedNameProvider.getFullyQualifiedName(annotation).toString(),
				annotation.getName(), false, Collections.emptyList(),
				getTypeBuilder().getDocumentation(annotation),
				true,
				annotation.getMembers(), it, context, null);
	}

	/** Generate the given object.
	 *
	 * @param field the fields.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	protected void _generate(SarlField field, PyAppendable it, IExtraLanguageGeneratorContext context) {
		if (field.isStatic()) {
			// Static field
			generatePythonField(field, it, context);
		} else {
			final String key = this.qualifiedNameProvider.getFullyQualifiedName(field.getDeclaringType()).toString();
			final List<SarlField> fields = context.getMultimapValues(INSTANCE_VARIABLES_MEMENTO, key);
			fields.add(field);
		}
	}

	/** Generate the given object.
	 *
	 * @param action the action.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	protected void _generate(SarlAction action, PyAppendable it, IExtraLanguageGeneratorContext context) {
		final String feature = getFeatureNameConverter(context).convertDeclarationName(action.getName(), action);
		generateExecutable(feature, action, !action.isStatic(), action.isAbstract(),
				action.getReturnType(),
				getTypeBuilder().getDocumentation(action),
				it, context);
	}

	/** Generate the given object.
	 *
	 * @param constructor the constructor.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	protected void _generate(SarlConstructor constructor, PyAppendable it, IExtraLanguageGeneratorContext context) {
		if (constructor.isStatic()) {
			generateExecutable("___static_init___", constructor, false, false, null, //$NON-NLS-1$
					getTypeBuilder().getDocumentation(constructor),
					it, context);
			it.newLine().append("___static_init___()"); //$NON-NLS-1$
		} else {
			generateExecutable("__init__", constructor, true, false, null, //$NON-NLS-1$
					getTypeBuilder().getDocumentation(constructor),
					it, context);
		}
	}

	/** Generate the given object.
	 *
	 * @param handler the behavior unit.
	 * @param it the target for the generated content.
	 * @param context the context.
	 */
	protected void _generate(SarlBehaviorUnit handler, PyAppendable it, IExtraLanguageGeneratorContext context) {
		final JvmTypeReference event = handler.getName();
		final String handleName = it.declareUniqueNameVariable(handler, "__on_" + event.getSimpleName() + "__"); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("def ").append(handleName).append("(self, occurrence):"); //$NON-NLS-1$ //$NON-NLS-2$
		it.increaseIndentation().newLine();
		generateDocString(getTypeBuilder().getDocumentation(handler), it);
		if (handler.getExpression() != null) {
			generate(handler.getExpression(), null, it, context);
		} else {
			it.append("pass"); //$NON-NLS-1$
		}
		it.decreaseIndentation().newLine();
		final String key = this.qualifiedNameProvider.getFullyQualifiedName(handler.getDeclaringType()).toString();
		final Map<String, Map<String, List<Pair<XExpression, String>>>> map = context.getMapData(EVENT_GUARDS_MEMENTO);
		Map<String, List<Pair<XExpression, String>>> submap = map.get(key);
		if (submap == null) {
			submap = new HashMap<>();
			map.put(key, submap);
		}
		final String eventId = event.getIdentifier();
		List<Pair<XExpression, String>> guards = submap.get(eventId);
		if (guards == null) {
			guards = new ArrayList<>();
			submap.put(eventId, guards);
		}
		guards.add(new Pair<>(handler.getGuard(), handleName));
	}

	/** Generate the given object.
	 *
	 * @param uses the capacity uses.
	 * @param it the target for the generated content.
	 * @param context the context.
	 * @see #computeCapacityFunctionMarkers(JvmDeclaredType)
	 * @see #markCapacityFunctions(PyAppendable)
	 * @see #_before(SarlCapacityUses, IExtraLanguageGeneratorContext)
	 * @see #createAppendable(JvmDeclaredType, IExtraLanguageGeneratorContext)
	 */
	protected void _generate(SarlCapacityUses uses, PyAppendable it, IExtraLanguageGeneratorContext context) {
		// Do nothing when reaching the statement.
	}

	/** Mark the functions of the used capacities in order to have a valid feature call within the code.
	 *
	 * @param uses the capacity uses.
	 * @param context the context.
	 */
	protected void _before(SarlCapacityUses uses, IExtraLanguageGeneratorContext context) {
		// Rename the function in order to produce the good features at the calls.
		for (final JvmTypeReference capacity : uses.getCapacities()) {
			final JvmType type = capacity.getType();
			if (type instanceof JvmDeclaredType) {
				computeCapacityFunctionMarkers((JvmDeclaredType) type);
			}
		}
	}

	private void computeCapacityFunctionMarkers(JvmDeclaredType leafType) {
		final Map<JvmOperation, String> mapping = new HashMap<>();
		final LinkedList<JvmDeclaredType> buffer = new LinkedList<>();
		final Set<String> processed = new TreeSet<>();
		buffer.addLast(leafType);
		while (!buffer.isEmpty()) {
			final JvmDeclaredType type = buffer.removeFirst();
			boolean markOne = false;
			for (final JvmOperation operation : type.getDeclaredOperations()) {
				if (!mapping.containsKey(operation)) {
					markOne = true;
					mapping.put(operation, "getSkill(" + type.getSimpleName() //$NON-NLS-1$
						+ ")." + operation.getSimpleName()); //$NON-NLS-1$
				}
			}
			if (markOne) {
				for (final JvmTypeReference superTypeReference : type.getExtendedInterfaces()) {
					if (processed.add(superTypeReference.getIdentifier())
						&& superTypeReference.getType() instanceof JvmDeclaredType) {
						buffer.addLast((JvmDeclaredType) superTypeReference.getType());
					}
				}
			}
		}
		this.useCapacityMapping = mapping;
	}

	private void markCapacityFunctions(PyAppendable it) {
		final Map<JvmOperation, String> mapping = this.useCapacityMapping;
		this.useCapacityMapping = new HashMap<>();
		final ImportManager imports = it.getImportManager();
		for (final Entry<JvmOperation, String> entry : mapping.entrySet()) {
			final JvmOperation operation = entry.getKey();
			final JvmDeclaredType type = operation.getDeclaringType();
			imports.addImportFor(type);
			it.declareVariable(operation, entry.getValue());
		}
	}

}

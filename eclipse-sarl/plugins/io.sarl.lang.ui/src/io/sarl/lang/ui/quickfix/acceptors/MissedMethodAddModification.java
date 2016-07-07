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

package io.sarl.lang.ui.quickfix.acceptors;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.inject.Inject;

import com.google.common.base.Strings;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmArrayType;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.eclipse.xtext.xtype.XImportSection;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.builders.IBlockExpressionBuilder;
import io.sarl.lang.codebuilder.builders.IExpressionBuilder;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;
import io.sarl.lang.util.Utils;

/**
 * Add missed methods.
 *
 * <p>FIXME: Use the CodeBuilder provided by Xtend.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class MissedMethodAddModification extends SARLSemanticModification {

	@Inject
	private CodeBuilderFactory codeBuilderFactory;

	private final String[] operationUris;

	private MissedMethodAddModification(String[] operationUris) {
		this.operationUris = operationUris;
	}

	/** Create the quick fix if needed.
	 *
	 * <p>User data contains the name of the expected type.
	 *
	 * @param provider - the quick fix provider.
	 * @param issue - the issue to fix.
	 * @param acceptor - the quick fix acceptor.
	 * @param label - the label of the quick fix.
	 * @param operationUris - the URIs of the missed operations.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor,
			String label, String[] operationUris) {
		if (operationUris.length > 0) {
			final MissedMethodAddModification modification = new MissedMethodAddModification(operationUris);
			provider.getInjector().injectMembers(modification);
			modification.setIssue(issue);
			modification.setTools(provider);
			acceptor.accept(
					issue,
					label,
					label,
					null,
					modification);
		}
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		final XtendTypeDeclaration clazz = (XtendTypeDeclaration) element;
		final SarlScript script = EcoreUtil2.getContainerOfType(element, SarlScript.class);
		final IXtextDocument document = context.getXtextDocument();
		final Set<JvmType> importableTypes = new HashSet<>();
		addMissedFunctions(script, clazz, document, importableTypes);
		addMissedImports(script, document, importableTypes);
	}

	@SuppressWarnings({"checkstyle:methodlength", "checkstyle:cyclomaticcomplexity",
			"checkstyle:npathcomplexity", "checkstyle:nestedifdepth"})
	private void addMissedFunctions(
			final SarlScript script,
			final XtendTypeDeclaration container, final IXtextDocument document,
			final Set<JvmType> importableTypes) throws Exception {
		final SARLQuickfixProvider tools = getTools();
		final int insertOffset = tools.getInsertOffset(container);
		final int length = tools.getSpaceSize(document, insertOffset);
		final JvmDeclaredType containerType = tools.getJvmAssociations().getInferredType(container);
		final String containerQualifiedName = containerType.getQualifiedName();
		final ReplacingAppendable appendable = tools.getAppendableFactory().create(document,
				(XtextResource) container.eResource(), insertOffset, length);
		final boolean initialIndent = container.getMembers().isEmpty();
		appendable.newLine();
		if (initialIndent) {
			appendable.increaseIndentation();
		}

		final ResourceSet resourceSet = script.eResource().getResourceSet();
		final IBlockExpressionBuilder blockBuilder = this.codeBuilderFactory.createXBlockExpression(resourceSet);
		final Resource fakeResource = blockBuilder.eResource();
		try {
			final IExpressionBuilder exprBuilder = this.codeBuilderFactory.createXExpression(fakeResource);
			final String autoGeneratedComment = "// " + blockBuilder.getAutoGeneratedActionString(); //$NON-NLS-1$

			for (final JvmOperation operation : tools.getJvmOperationsFromURIs(container, this.operationUris)) {
				appendable.newLine();
				// Annotations
				for (final JvmAnnotationReference annotation : operation.getAnnotations()) {
					final JvmAnnotationType annotationType = annotation.getAnnotation();
					if (!Utils.isSARLAnnotation(annotationType.getQualifiedName())) {
						appendable.append("@").append(annotationType.getSimpleName()).newLine(); //$NON-NLS-1$
						importableTypes.add(annotationType);
					}
				}
				// Modifiers
				final JvmVisibility visibility = container.getDeclaredVisibility();
				if (visibility != null) {
					switch (visibility) {
					case PRIVATE:
						appendable.append(
								getTools().getGrammarAccess().getXtendGrammarAccess()
								.getCommonModifierAccess().getPrivateKeyword_1().getValue())
								.append(" "); //$NON-NLS-1$
						break;
					case PROTECTED:
						appendable.append(
								getTools().getGrammarAccess().getXtendGrammarAccess()
								.getCommonModifierAccess().getProtectedKeyword_2().getValue())
								.append(" "); //$NON-NLS-1$
						break;
					case PUBLIC:
						appendable.append(
								getTools().getGrammarAccess().getXtendGrammarAccess()
								.getCommonModifierAccess().getPublicKeyword_0().getValue())
								.append(" "); //$NON-NLS-1$
						break;
					case DEFAULT:
					default:
						appendable.append(
								getTools().getGrammarAccess().getXtendGrammarAccess()
								.getCommonModifierAccess().getPackageKeyword_3().getValue())
								.append(" "); //$NON-NLS-1$
						break;
					}
				}
				if (operation.isStrictFloatingPoint()) {
					appendable.append(
							getTools().getGrammarAccess().getXtendGrammarAccess()
							.getCommonModifierAccess().getStrictfpKeyword_8().getValue())
							.append(" "); //$NON-NLS-1$
				}
				if (operation.isSynchronized()) {
					appendable.append(
							getTools().getGrammarAccess().getXtendGrammarAccess()
							.getCommonModifierAccess().getSynchronizedKeyword_11().getValue())
							.append(" "); //$NON-NLS-1$
				}
				// Type parameters
				if (!operation.getTypeParameters().isEmpty()) {
					appendable.append(tools.getGrammarAccess().getActionAccess()
							.getLessThanSignKeyword_5_0().getValue());
					boolean addComa = false;
					for (final JvmTypeParameter typeParameter : operation.getTypeParameters()) {
						if (addComa) {
							appendable.append(tools.getGrammarAccess().getActionAccess()
									.getCommaKeyword_5_2_0().getValue());
						} else {
							addComa = true;
						}
						appendable.append(typeParameter.getIdentifier());
						importableTypes.add(typeParameter);
					}
					appendable.append(tools.getGrammarAccess().getActionAccess()
							.getGreaterThanSignKeyword_5_3().getValue());
				}
				// Name
				appendable.append(tools.getGrammarAccess().getMethodModifierAccess()
						.getOverrideKeyword_1().getValue()).append(" "); //$NON-NLS-1$
				appendable.append(operation.getSimpleName());
				// Parameters
				if (!operation.getParameters().isEmpty()) {
					appendable.append(tools.getGrammarAccess().getActionAccess()
							.getLeftParenthesisKeyword_7_0().getValue());
					for (int i = 0; i < operation.getParameters().size(); ++i) {
						final JvmFormalParameter parameter = operation.getParameters().get(i);
						if (i > 0) {
							appendable.append(tools.getGrammarAccess().getActionAccess()
									.getCommaKeyword_5_2_0().getValue()).append(" "); //$NON-NLS-1$
						}
						// Parameter name
						appendable.append(parameter.getName()).append(" "); //$NON-NLS-1$
						// Parameter type
						appendable.append(tools.getGrammarAccess().getActionAccess()
								.getColonKeyword_8_0().getValue()).append(" "); //$NON-NLS-1$
						final JvmTypeReference parameterTypeReference = parameter.getParameterType();
						final JvmType parameterType = parameterTypeReference.getType();
						if (operation.isVarArgs() && i == operation.getParameters().size() - 1
								&& parameterType instanceof JvmArrayType) {
							final JvmType componentType = ((JvmArrayType) parameterType).getComponentType();
							appendable.append(componentType);
							appendable.append(tools.getGrammarAccess().getParameterAccess()
									.getVarArgAsteriskKeyword_6_0_0().getValue());
							importableTypes.add(componentType);
						} else {
							appendable.append(parameterType);
							importableTypes.add(parameterType);
						}

						if (Utils.hasAnnotation(parameter, DefaultValue.class)) {
							String defaultValue = null;
							final String key = Utils.annotationString(parameter, DefaultValue.class);
							final String argument = tools.getActionPrototypeProvider().toJavaArgument(
									containerQualifiedName,
									key);
							final int idx = argument.lastIndexOf('.');
							final JvmType type;
							final String fieldName;
							if (idx > 0) {
								final String typeName = argument.substring(0, idx);
								JvmType localtype = tools.getTypeServices().getTypeReferences().findDeclaredType(
										typeName, container);
								if (localtype == null) {
									final QualifiedName qn0 = tools.getQualifiedNameConverter().toQualifiedName(typeName);
									final Iterator<XtendTypeDeclaration> iterator = script.getXtendTypes().iterator();
									while (localtype == null && iterator.hasNext()) {
										final XtendTypeDeclaration declaredType = iterator.next();
										final QualifiedName qn = tools.getQualifiedNameProvider()
												.getFullyQualifiedName(declaredType);
										if (qn0.equals(qn)) {
											localtype = tools.getJvmAssociations().getInferredType(declaredType);
										}
									}
								}
								assert localtype != null : "Type not found: " + typeName; //$NON-NLS-1$
								type = localtype;
								fieldName = argument.substring(idx + 1);
							} else {
								type = containerType;
								fieldName = argument;
							}
							assert type instanceof JvmDeclaredType : "Type not found"; //$NON-NLS-1$
							if (type instanceof JvmDeclaredType) {
								final Iterator<JvmField> iterator = ((JvmDeclaredType) type).getDeclaredFields().iterator();
								while (defaultValue == null && iterator.hasNext()) {
									final JvmField field = iterator.next();
									if (fieldName.equals(field.getSimpleName())) {
										String value = Utils.annotationString(field, SarlSourceCode.class);
										if (!Strings.isNullOrEmpty(value)) {
											value = value.trim();
											if (!Strings.isNullOrEmpty(value)) {
												defaultValue = value;
											}
										}
									}
								}
							}
							if (!Strings.isNullOrEmpty(defaultValue)) {
								appendable.append(" ").append(tools.getGrammarAccess().getParameterAccess() //$NON-NLS-1$
										.getEqualsSignKeyword_6_1_0().getValue()).append(" "); //$NON-NLS-1$
								appendable.append(defaultValue);
							}
						}
					}
					appendable.append(tools.getGrammarAccess().getActionAccess()
							.getRightParenthesisKeyword_7_2().getValue());
				}

				// Return type
				final JvmTypeReference returnTypeReference = operation.getReturnType();
				final JvmType returnType = (returnTypeReference != null) ? returnTypeReference.getType() : null;
				final boolean hasReturnType = !Utils.isPrimitiveVoid(returnType);
				if (hasReturnType) {
					appendable.append(" ") //$NON-NLS-1$
						.append(tools.getGrammarAccess().getActionAccess().getColonKeyword_8_0().getValue())
						.append(" ") //$NON-NLS-1$
						.append(returnType);
				}

				// Exceptions
				if (!operation.getExceptions().isEmpty()) {
					appendable.append(" ").append(tools.getGrammarAccess().getActionAccess() //$NON-NLS-1$
							.getThrowsKeyword_9_0_0().getValue()).append(" "); //$NON-NLS-1$
					boolean addComa = false;
					for (final JvmTypeReference exceptionType : operation.getExceptions()) {
						if (addComa) {
							appendable.append(tools.getGrammarAccess().getActionAccess()
									.getCommaKeyword_9_0_2_0().getValue());
						} else {
							addComa = true;
						}
						final JvmType exType = exceptionType.getType();
						appendable.append(exType);
						importableTypes.add(exType);
					}
				}

				// Fired events
				if (Utils.hasAnnotation(operation, FiredEvent.class)) {
					appendable.append(" ").append(tools.getGrammarAccess().getActionAccess() //$NON-NLS-1$
							.getFiresKeyword_9_1_0().getValue()).append(" "); //$NON-NLS-1$
					boolean addComa = false;
					for (final JvmTypeReference eventType : Utils.annotationClasses(operation, FiredEvent.class)) {
						if (addComa) {
							appendable.append(tools.getGrammarAccess().getActionAccess()
									.getCommaKeyword_9_1_2_0().getValue());
						} else {
							addComa = true;
						}
						final JvmType evtType = eventType.getType();
						appendable.append(evtType);
						importableTypes.add(evtType);
					}
				}
				// Body
				appendable.append(" ").append(tools.getGrammarAccess().getXBlockExpressionAccess() //$NON-NLS-1$
						.getLeftCurlyBracketKeyword_1().getValue());
				appendable.increaseIndentation().newLine();
				appendable.append(autoGeneratedComment);

				if (hasReturnType) {
					assert returnType != null;
					appendable.newLine().append(exprBuilder.getDefaultValueForType(returnType.getIdentifier()));
				}

				appendable.decreaseIndentation().newLine();
				appendable.append(tools.getGrammarAccess().getXBlockExpressionAccess()
						.getRightCurlyBracketKeyword_3().getValue());
				appendable.newLine();
			}
			appendable.decreaseIndentation().newLine();
			appendable.commitChanges();
		} finally {
			fakeResource.unload();
		}
	}

	private void addMissedImports(SarlScript script,
			IXtextDocument document, Set<JvmType> importableTypes) throws Exception {
		final SARLQuickfixProvider tools = getTools();
		final int insertOffset = tools.getImportInsertOffset(script);
		final ReplacingAppendable appendable = tools.getAppendableFactory().create(document,
				(XtextResource) script.eResource(), insertOffset, 0);
		final ImportManager importManager = new ImportManager();
		final XImportSection importSection = script.getImportSection();
		if (importSection != null) {
			for (final XImportDeclaration declaration : importSection.getImportDeclarations()) {
				final JvmDeclaredType type = declaration.getImportedType();
				if (type != null) {
					importManager.addImportFor(type);
				}
			}
		}
		for (final JvmType importableType : importableTypes) {
			if (importManager.addImportFor(importableType)) {
				appendable.newLine();
				appendable.append(
						tools.getGrammarAccess()
						.getXImportDeclarationAccess().getImportKeyword_0().getValue());
				appendable.append(" "); //$NON-NLS-1$
				appendable.append(importableType.getQualifiedName());
			}
		}
		appendable.commitChanges();
	}

}

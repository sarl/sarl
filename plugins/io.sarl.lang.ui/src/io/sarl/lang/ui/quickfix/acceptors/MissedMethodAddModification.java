/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import com.google.common.base.Strings;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.tasks.TaskTags;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.eclipse.xtext.xtype.XImportSection;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.EarlyExit;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.annotation.Generated;
import io.sarl.lang.annotation.ImportedCapacityFeature;
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
			MissedMethodAddModification modification = new MissedMethodAddModification(operationUris);
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
		XtendTypeDeclaration clazz = (XtendTypeDeclaration) element;
		SarlScript script = EcoreUtil2.getContainerOfType(element, SarlScript.class);
		IXtextDocument document = context.getXtextDocument();
		Set<JvmType> importableTypes = new HashSet<>();
		addMissedFunctions(script, clazz, document, importableTypes);
		addMissedImports(script, document, importableTypes);
	}

	@SuppressWarnings({"checkstyle:methodlength", "checkstyle:cyclomaticcomplexity",
			"checkstyle:npathcomplexity", "checkstyle:nestedifdepth"})
	private void addMissedFunctions(
			SarlScript script,
			XtendTypeDeclaration container, IXtextDocument document,
			Set<JvmType> importableTypes) throws Exception {
		SARLQuickfixProvider tools = getTools();
		int insertOffset = tools.getInsertOffset(container);
		int length = tools.getSpaceSize(document, insertOffset);
		JvmDeclaredType containerType = tools.getJvmAssociations().getInferredType(container);
		String containerQualifiedName = containerType.getQualifiedName();
		ReplacingAppendable appendable = tools.getAppendableFactory().create(document,
				(XtextResource) container.eResource(), insertOffset, length);
		boolean initialIndent = (container.getMembers().isEmpty());
		appendable.newLine();
		if (initialIndent) {
			appendable.increaseIndentation();
		}
		Set<String> hiddenAnnotations = new TreeSet<>();
		for (Class<?> type : Arrays.asList(Generated.class, DefaultValueUse.class, DefaultValue.class,
				DefaultValueSource.class, FiredEvent.class, EarlyExit.class, ImportedCapacityFeature.class)) {
			hiddenAnnotations.add(type.getName());
		}
		for (String operationUriAsString : this.operationUris) {
			URI operationURI = URI.createURI(operationUriAsString);
			EObject overridden = container.eResource().getResourceSet().getEObject(operationURI, true);
			if (overridden instanceof JvmOperation) {
				JvmOperation operation = (JvmOperation) overridden;
				if (!Utils.hasAnnotation(operation, DefaultValueUse.class)) {
					appendable.newLine();
					// Annotations
					for (JvmAnnotationReference annotation : operation.getAnnotations()) {
						JvmAnnotationType annotationType = annotation.getAnnotation();
						if (!hiddenAnnotations.contains(annotationType.getQualifiedName())) {
							appendable.append("@").append(annotationType.getSimpleName()).newLine(); //$NON-NLS-1$
							importableTypes.add(annotationType);
						}
					}
					// Modifiers
					switch (operation.getVisibility()) {
					case PRIVATE:
						appendable.append("private "); //$NON-NLS-1$
						break;
					case PROTECTED:
						appendable.append("protected "); //$NON-NLS-1$
						break;
					case PUBLIC:
						// Default visibility
						//appendable.append("public "); //$NON-NLS-1$
						break;
					case DEFAULT:
					default:
						appendable.append("package "); //$NON-NLS-1$
						break;
					}
					if (operation.isStrictFloatingPoint()) {
						appendable.append("strictfp "); //$NON-NLS-1$
					}
					if (operation.isSynchronized()) {
						appendable.append("synchronized "); //$NON-NLS-1$
					}
					// Type parameters
					if (!operation.getTypeParameters().isEmpty()) {
						appendable.append(tools.getGrammarAccess().getActionAccess()
								.getLessThanSignKeyword_5_0().getValue());
						boolean addComa = false;
						for (JvmTypeParameter typeParameter : operation.getTypeParameters()) {
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
							JvmFormalParameter parameter = operation.getParameters().get(i);
							if (i > 0) {
								appendable.append(tools.getGrammarAccess().getActionAccess()
										.getCommaKeyword_5_2_0().getValue()).append(" "); //$NON-NLS-1$
							}
							// Parameter name
							appendable.append(parameter.getName()).append(" "); //$NON-NLS-1$
							// Parameter type
							appendable.append(tools.getGrammarAccess().getActionAccess()
									.getColonKeyword_8_0().getValue()).append(" "); //$NON-NLS-1$
							LightweightTypeReference paramType = Utils.toLightweightTypeReference(parameter.getParameterType(),
									tools.getTypeServices());
							if (operation.isVarArgs() && i == operation.getParameters().size() - 1) {
								paramType = paramType.getComponentType();
								appendable.append(paramType);
								appendable.append(tools.getGrammarAccess().getVarArgTokenAccess()
										.getAsteriskKeyword().getValue());
							} else {
								appendable.append(paramType);
							}
							importableTypes.add(paramType.getType());

							if (Utils.hasAnnotation(parameter, DefaultValue.class)) {
								String defaultValue = null;
								String key = Utils.annotationString(parameter, DefaultValue.class);
								String argument = tools.getActionPrototypeProvider().toJavaArgument(
										containerQualifiedName,
										key);
								int idx = argument.lastIndexOf('.');
								JvmType type = null;
								String fieldName;
								if (idx > 0) {
									String typeName = argument.substring(0, idx);
									type = tools.getTypeServices().getTypeReferences().findDeclaredType(
											typeName, container);
									if (type == null) {
										QualifiedName qn0 = tools.getQualifiedNameConverter().toQualifiedName(typeName);
										Iterator<XtendTypeDeclaration> iterator = script.getXtendTypes().iterator();
										while (type == null && iterator.hasNext()) {
											XtendTypeDeclaration declaredType = iterator.next();
											QualifiedName qn = tools.getQualifiedNameProvider()
													.getFullyQualifiedName(declaredType);
											if (qn0.equals(qn)) {
												type = tools.getJvmAssociations().getInferredType(declaredType);
											}
										}
									}
									assert (type != null) : "Type not found: " + typeName; //$NON-NLS-1$
									fieldName = argument.substring(idx + 1);
								} else {
									type = containerType;
									fieldName = argument;
								}
								assert (type instanceof JvmDeclaredType) : "Type not found"; //$NON-NLS-1$
								if (type instanceof JvmDeclaredType) {
									Iterator<JvmField> iterator = ((JvmDeclaredType) type).getDeclaredFields().iterator();
									while (defaultValue == null && iterator.hasNext()) {
										JvmField field = iterator.next();
										if (fieldName.equals(field.getSimpleName())) {
											String value = Utils.annotationString(field, Generated.class);
											if (value != null) {
												value = value.trim();
												if (!value.isEmpty()) {
													defaultValue = value;
												}
											}
										}
									}
								}
								if (!Strings.isNullOrEmpty(defaultValue)) {
									appendable.append(" ").append(tools.getGrammarAccess().getParameterAccess() //$NON-NLS-1$
											.getEqualsSignKeyword_5_1_0().getValue()).append(" "); //$NON-NLS-1$
									appendable.append(defaultValue);
								}
							}
						}
						appendable.append(tools.getGrammarAccess().getActionAccess()
								.getRightParenthesisKeyword_7_2().getValue());
					}
					// Return type
					JvmTypeReference returnTypeReference = operation.getReturnType();
					LightweightTypeReference returnType = null;
					if (returnTypeReference != null) {
						returnType = Utils.toLightweightTypeReference(returnTypeReference, tools.getTypeServices());
					}
					if (returnType != null && !returnType.isPrimitiveVoid()) {
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
						for (JvmTypeReference exceptionType : operation.getExceptions()) {
							if (addComa) {
								appendable.append(tools.getGrammarAccess().getActionAccess()
										.getCommaKeyword_9_0_2_0().getValue());
							} else {
								addComa = true;
							}
							LightweightTypeReference exType = Utils.toLightweightTypeReference(exceptionType,
									tools.getTypeServices());
							appendable.append(exType);
							importableTypes.add(exType.getType());
						}
					}
					// Fired events
					if (Utils.hasAnnotation(operation, FiredEvent.class)) {
						appendable.append(" ").append(tools.getGrammarAccess().getActionAccess() //$NON-NLS-1$
								.getFiresKeyword_9_1_0().getValue()).append(" "); //$NON-NLS-1$
						boolean addComa = false;
						for (JvmTypeReference eventType : Utils.annotationClasses(operation, FiredEvent.class)) {
							if (addComa) {
								appendable.append(tools.getGrammarAccess().getActionAccess()
										.getCommaKeyword_9_1_2_0().getValue());
							} else {
								addComa = true;
							}
							LightweightTypeReference evtType = Utils.toLightweightTypeReference(eventType,
									tools.getTypeServices());
							appendable.append(evtType);
							importableTypes.add(evtType.getType());
						}
					}
					// Body
					appendable.append(" ").append(tools.getGrammarAccess().getXBlockExpressionAccess() //$NON-NLS-1$
							.getLeftCurlyBracketKeyword_1().getValue());
					appendable.increaseIndentation().newLine();
					TaskTags tags = tools.getTaskTagProvider().getTaskTags(container.eResource());
					String taskTag;
					if (tags != null && tags.getTaskTags() != null && !tags.getTaskTags().isEmpty()) {
						taskTag = tags.getTaskTags().get(0).getName();
					} else {
						taskTag = "TODO"; //$NON-NLS-1$
					}
					appendable.append("// ") //$NON-NLS-1$
						.append(taskTag)
						.append(" ") //$NON-NLS-1$
						.append(io.sarl.lang.generator.helper.Messages.SARLCodeGenerator_0);

					if (returnType != null && !returnType.isPrimitiveVoid()) {
						appendable.newLine().append(Utils.getDefaultValueForType(returnType));
					}

					appendable.decreaseIndentation().newLine();
					appendable.append(tools.getGrammarAccess().getXBlockExpressionAccess()
							.getRightCurlyBracketKeyword_3().getValue());
					appendable.newLine();
				}
			}
		}
		appendable.decreaseIndentation().newLine();
		appendable.commitChanges();
	}

	private void addMissedImports(SarlScript script,
			IXtextDocument document, Set<JvmType> importableTypes) throws Exception {
		SARLQuickfixProvider tools = getTools();
		int insertOffset = tools.getImportInsertOffset(script);
		ReplacingAppendable appendable = tools.getAppendableFactory().create(document,
				(XtextResource) script.eResource(), insertOffset, 0);
		ImportManager importManager = new ImportManager();
		XImportSection importSection = script.getImportSection();
		if (importSection != null) {
			for (XImportDeclaration declaration : importSection.getImportDeclarations()) {
				JvmDeclaredType type = declaration.getImportedType();
				if (type != null) {
					importManager.addImportFor(type);
				}
			}
		}
		for (JvmType importableType : importableTypes) {
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

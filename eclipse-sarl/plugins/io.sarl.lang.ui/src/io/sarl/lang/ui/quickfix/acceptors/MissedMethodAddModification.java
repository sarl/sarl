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
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.annotation.Generated;
import javax.inject.Inject;

import com.google.common.base.Strings;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.text.correction.IProposalRelevance;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationReference;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmArrayType;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmLowerBound;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeConstraint;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.output.FakeTreeAppendable;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.eclipse.xtext.xtype.XImportSection;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.FiredEvent;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.appenders.SarlActionSourceAppender;
import io.sarl.lang.codebuilder.builders.IExpressionBuilder;
import io.sarl.lang.codebuilder.builders.IFormalParameterBuilder;
import io.sarl.lang.codebuilder.builders.ITypeParameterBuilder;
import io.sarl.lang.formatting2.FormatterFacade;
import io.sarl.lang.jvmmodel.IDefaultVisibilityProvider;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.typesystem.SARLAnnotationUtil;
import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;
import io.sarl.lang.util.Utils;

/**
 * Add missed methods.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public final class MissedMethodAddModification extends SARLSemanticModification {

	private static final String PREFIX = "class X{"; //$NON-NLS-1$

	private static final String POSTFIX = "}"; //$NON-NLS-1$

	private static final String PATTERN = "^\\s*class\\s+X\\s*\\{[^\n\r]*[\n\r]+(.*?)[^\n\r]*\\}\\s*$"; //$NON-NLS-1$

	@Inject
	private CodeBuilderFactory codeBuilderFactory;

	@Inject
	private AnnotationLookup annotationFinder;

	@Inject
	private SARLAnnotationUtil annotationUtils;

	@Inject
	private IDefaultVisibilityProvider defaultVisibilityProvider;

	@Inject
	private ReflectExtensions reflect;

	@Inject
	private FormatterFacade formatterFacade;

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
					JavaPluginImages.IMG_CORRECTION_ADD,
					modification,
					IProposalRelevance.ADD_UNIMPLEMENTED_METHODS);
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

	/** Replies if the given method is marked has automatically generated by the SARL compiler.
	 *
	 * @param method - the method to check.
	 * @return <code>true</code> if the method is annoted with SyntheticMember; <code>false</code>
	 *     otherwise.
	 */
	public static boolean isGeneratedOperation(JvmOperation method) {
		for (final JvmAnnotationReference annotation : method.getAnnotations()) {
			if (Objects.equals(SyntheticMember.class.getName(), annotation.getAnnotation().getIdentifier())
				|| Objects.equals(Generated.class.getName(), annotation.getAnnotation().getIdentifier())) {
				return true;
			}
		}
		return false;
	}

	private static boolean isCR(char character) {
		return character == '\n' || character == '\r';
	}

	private static String removeCR(String text) {
		if (text == null || text.isEmpty()) {
			return text;
		}
		int start = 0;
		int len = text.length();
		while (start < len && isCR(text.charAt(start))) {
			++start;
		}
		if (len > start) {
			while ((len - 1) >= start && isCR(text.charAt(len - 1))) {
				--len;
			}
		}
		return text.substring(start, len);
	}

	@SuppressWarnings({"checkstyle:methodlength", "checkstyle:cyclomaticcomplexity",
			"checkstyle:npathcomplexity", "checkstyle:nestedifdepth"})
	private void addMissedFunctions(
			final SarlScript script,
			final XtendTypeDeclaration container, final IXtextDocument document,
			final Set<JvmType> importableTypes) throws Exception {
		final SARLQuickfixProvider tools = getTools();
		final SARLGrammarKeywordAccess keywords = tools.getGrammarAccess();
		final int insertOffset = tools.getInsertOffset(container);
		final int length = tools.getSpaceSize(document, insertOffset);
		final JvmDeclaredType containerType = tools.getJvmAssociations().getInferredType(container);
		final String containerQualifiedName = containerType.getQualifiedName();
		final ReplacingAppendable appendable = tools.getAppendableFactory().create(document,
				(XtextResource) container.eResource(), insertOffset, length);
		final boolean initialIndent = container.getMembers().isEmpty();

		final String indent = removeCR(this.reflect.invoke(appendable, "getIndentationString").toString()); //$NON-NLS-1$
		final FakeTreeAppendable addedCode = new FakeTreeAppendable();
		addedCode.append(indent);
		final ResourceSet resourceSet = script.eResource().getResourceSet();
		for (final JvmOperation operation : tools.getJvmOperationsFromURIs(container, this.operationUris)) {
			if (!isGeneratedOperation(operation)) {
				final SarlActionSourceAppender actionBuilder = this.codeBuilderFactory.buildOverrideSarlAction(
						operation.getSimpleName(), resourceSet);

				// Annotations
				for (final JvmAnnotationReference annotation : operation.getAnnotations()) {
					final JvmAnnotationType annotationType = annotation.getAnnotation();
					if (!Utils.isSARLAnnotation(annotationType.getQualifiedName())) {
						actionBuilder.addAnnotation(annotationType.getQualifiedName());
						importableTypes.add(annotationType);
					}
				}
				// Modifiers
				final JvmVisibility visibility = operation.getVisibility();
				if (visibility != null
						&& !this.defaultVisibilityProvider.isDefaultVisibility(
								actionBuilder.getSarlAction(), visibility)) {
					switch (visibility) {
					case PRIVATE:
						actionBuilder.addModifier(keywords.getPrivateKeyword());
						break;
					case PROTECTED:
						actionBuilder.addModifier(keywords.getProtectedKeyword());
						break;
					case PUBLIC:
						actionBuilder.addModifier(keywords.getPublicKeyword());
						break;
					case DEFAULT:
					default:
						actionBuilder.addModifier(keywords.getPackageKeyword());
						break;
					}
				}
				if (operation.isStrictFloatingPoint()) {
					actionBuilder.addModifier(keywords.getStrictfpKeyword());
				}
				if (operation.isSynchronized()) {
					actionBuilder.addModifier(keywords.getSynchronizedKeyword());
				}
				// Type parameters
				for (final JvmTypeParameter typeParameter : operation.getTypeParameters()) {
					final ITypeParameterBuilder tpbuilder = actionBuilder.addTypeParameter(typeParameter.getName());
					for (final JvmTypeConstraint constraint : typeParameter.getConstraints()) {
						if (constraint instanceof JvmUpperBound) {
							tpbuilder.addUpperConstraint(constraint.getTypeReference().getIdentifier());
						} else if (constraint instanceof JvmLowerBound) {
							tpbuilder.addLowerConstraint(constraint.getTypeReference().getIdentifier());
						}
					}
				}
				// Formal parameters
				final List<JvmFormalParameter> parameters = operation.getParameters();
				final int len = parameters.size();
				for (int i = 0; i < len; ++i) {
					final JvmFormalParameter parameter = parameters.get(i);
					// Parameter name
					final IFormalParameterBuilder paramBuilder = actionBuilder.addParameter(parameter.getName());
					// Parameter type
					final JvmTypeReference parameterTypeReference = parameter.getParameterType();
					final JvmType parameterType = parameterTypeReference.getType();
					if (operation.isVarArgs() && i == operation.getParameters().size() - 1
							&& parameterType instanceof JvmArrayType) {
						final JvmType componentType = ((JvmArrayType) parameterType).getComponentType();
						paramBuilder.setParameterType(componentType.getIdentifier());
						importableTypes.add(componentType);
						paramBuilder.setVarArg(true);
					} else {
						paramBuilder.setParameterType(parameterType.getIdentifier());
						importableTypes.add(parameterType);
					}

					final JvmAnnotationReference annotationRef = this.annotationFinder.findAnnotation(
							parameter, DefaultValue.class);
					if (annotationRef != null) {
						String defaultValue = null;
						final String key = this.annotationUtils.findStringValue(annotationRef);
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
									String value = this.annotationUtils.findStringValue(field, SarlSourceCode.class);
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
							final IExpressionBuilder defaultValueExpr = paramBuilder.getDefaultValue();
							defaultValueExpr.setExpression(defaultValue);
						}
					}
				}
				// Return type
				final JvmTypeReference returnTypeReference = operation.getReturnType();
				final JvmType returnType = (returnTypeReference != null) ? returnTypeReference.getType() : null;
				final boolean hasReturnType = !Utils.isPrimitiveVoid(returnType);
				if (hasReturnType && returnType != null) {
					actionBuilder.setReturnType(returnType.getIdentifier());
					importableTypes.add(returnType);
				}
				// Exceptions
				for (final JvmTypeReference exceptionType : operation.getExceptions()) {
					final JvmType exType = exceptionType.getType();
					if (!Utils.isSARLAnnotation(exType.getIdentifier())) {
						actionBuilder.addException(exType.getIdentifier());
						importableTypes.add(exType);
					}
				}
				// Fired events
				final JvmAnnotationReference annotationRef = this.annotationFinder.findAnnotation(operation, FiredEvent.class);
				if (annotationRef != null) {
					for (final JvmTypeReference eventType : this.annotationUtils.findTypeValues(annotationRef)) {
						final JvmType evtType = eventType.getType();
						actionBuilder.addFiredEvent(evtType.getIdentifier());
						importableTypes.add(evtType);
					}
				}
				// Body
				actionBuilder.getExpression().setDefaultAutoGeneratedContent(
						hasReturnType && returnType != null ? returnType.getIdentifier() : null);


				// Add the action in the appender
				actionBuilder.build(addedCode);

				// Release the resources
				actionBuilder.dispose();
			}
		}

		String content = this.formatterFacade.format(PREFIX + addedCode.getContent() + POSTFIX);
		final Pattern pattern = Pattern.compile(PATTERN, Pattern.DOTALL);
		final Matcher matcher = pattern.matcher(content);
		if (matcher.matches()) {
			content = matcher.group(1);
		}

		appendable.newLine();
		if (initialIndent) {
			appendable.newLine();
		}
		appendable.append(content);
		appendable.newLine();
		appendable.commitChanges();
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
						tools.getGrammarAccess().getImportKeyword());
				appendable.append(" "); //$NON-NLS-1$
				appendable.append(importableType.getQualifiedName());
			}
		}
		appendable.commitChanges();
	}

}

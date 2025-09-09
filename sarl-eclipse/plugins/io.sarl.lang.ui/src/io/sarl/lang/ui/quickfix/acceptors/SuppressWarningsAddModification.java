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

package io.sarl.lang.ui.quickfix.acceptors;

import java.text.MessageFormat;
import java.util.Objects;
import java.util.regex.Pattern;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.text.correction.IProposalRelevance;
import org.eclipse.xtend.core.xtend.XtendAnnotationTarget;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;

import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

/**
 * Add a suppress-warning annotation.
 *
 * @author $Author: sgalland$
 * @version io.sarl.lang.ui 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 */
@SuppressWarnings("restriction")
public final class SuppressWarningsAddModification extends SARLSemanticModification {

	private final URI uri;

	private final String code;

	/** Constructor.
	 *
	 * @param uri the URI of the element to modify.
	 * @param code the code of the issue to ignore.
	 */
	private SuppressWarningsAddModification(URI uri, String code) {
		this.uri = uri;
		this.code = code;
	}

	/** Create the quick fix if needed.
	 *
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param acceptor the quick fix acceptor.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor) {
		final var file = provider.getProjectUtil().findFileStorage(issue.getUriToProblem(), false);
		final var resourceSet = provider.getResourceSetProvider().get(file.getProject());
		final EObject failingObject;
		try {
			failingObject = resourceSet.getEObject(issue.getUriToProblem(), true);
		} catch (Exception exception) {
			// Something is going really wrong. Must of the time the cause is a major syntax error.
			return;
		}
		var eObject = EcoreUtil2.getContainerOfType(failingObject, XtendAnnotationTarget.class);
		var first = true;
		var relevance = IProposalRelevance.ADD_SUPPRESSWARNINGS;
		while (eObject != null) {
			final var uri = EcoreUtil2.getNormalizedURI(eObject);
			final var modification = new SuppressWarningsAddModification(uri, issue.getCode());
			modification.setIssue(issue);
			modification.setTools(provider);
			final String elementName;
			final var name = provider.getQualifiedNameProvider().getFullyQualifiedName(eObject);
			if (name != null) {
				elementName = name.getLastSegment();
			} else if (first) {
				elementName = Messages.AddSuppressWarnings_0;
			} else {
				elementName = null;
			}
			if (elementName != null) {
				acceptor.accept(issue,
						MessageFormat.format(Messages.AddSuppressWarnings_1, elementName),
						MessageFormat.format(Messages.AddSuppressWarnings_2, elementName),
						JavaPluginImages.IMG_OBJS_ANNOTATION_ALT,
						modification,
						relevance);
				--relevance;
			}
			first = false;
			eObject = EcoreUtil2.getContainerOfType(eObject.eContainer(), XtendAnnotationTarget.class);
		}
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		final EObject currentElement;
		if (this.uri == null) {
			currentElement = element;
		} else {
			currentElement = element.eResource().getResourceSet().getEObject(this.uri, true);
		}
		if (currentElement instanceof XtendAnnotationTarget annotationTarget) {
			final var swtype = getTools().getTypeServices().getTypeReferences().findDeclaredType(
					SuppressWarnings.class, element);
			final var annotation = findAnnotation(annotationTarget, swtype);
			if (annotation == null) {
				addAnnotation(currentElement, context, swtype);
			} else {
				addAnnotation(currentElement, annotation, context);
			}
		}
	}

	private static XAnnotation findAnnotation(XtendAnnotationTarget target, JvmType suppressWarningsAnnotation) {
		for (final var annotation : target.getAnnotations()) {
			if (Objects.equals(annotation.getAnnotationType().getQualifiedName(),
					suppressWarningsAnnotation.getIdentifier())) {
				return annotation;
			}
		}
		return null;
	}

	/** Add SuppressWarnings annotation.
	 *
	 * @param element the element to receive the annotation.
	 * @param context the modification context.
	 * @param suppressWarningsAnnotation the type for the suppress warning annotation.
	 * @throws Exception if the document cannot be changed.
	 */
	protected void addAnnotation(EObject element, IModificationContext context,
			JvmType suppressWarningsAnnotation) throws Exception {
		final var node = NodeModelUtils.findActualNodeFor(element);
		if (node != null) {
			final var insertOffset = node.getOffset();
			final var document = context.getXtextDocument();
			final var length = getTools().getSpaceSize(document, insertOffset);
			final var appendable = getTools().getAppendableFactory().create(document,
					(XtextResource) element.eResource(), insertOffset, length);
			appendable.append(getTools().getGrammarAccess().getCommercialAtKeyword());
			appendable.append(suppressWarningsAnnotation);
			appendable.append("(\""); //$NON-NLS-1$
			appendable.append(extractId(this.code));
			appendable.append("\")"); //$NON-NLS-1$
			appendable.newLine();
			appendable.commitChanges();
		}
	}

	/** Add SuppressWarnings annotation.
	 *
	 * @param element the element to receive the annotation.
	 * @param annotation the suppress-warning annotation.
	 * @param context the modification context.
	 * @throws Exception if the document cannot be changed.
	 */
	protected void addAnnotation(EObject element, XAnnotation annotation,
			IModificationContext context) throws Exception {
		final var node = NodeModelUtils.findActualNodeFor(annotation);
		if (node != null) {
			final var document = context.getXtextDocument();
			final var startOffset = node.getOffset();
			final var parameterOffset = getTools().getOffsetForPattern(document, startOffset,
					"@\\s*" + toPattern(annotation.getAnnotationType().getQualifiedName()) //$NON-NLS-1$
					+ "\\s*\\(\\s*"); //$NON-NLS-1$
			final int insertOffset;
			if (document.getChar(parameterOffset) == '{') {
				insertOffset = parameterOffset + getTools().getSpaceSize(document, parameterOffset + 1) + 1;
			} else {
				insertOffset = parameterOffset;
			}
			final var appendable = getTools().getAppendableFactory().create(document,
					(XtextResource) element.eResource(), insertOffset, 0);
			appendable.append("\""); //$NON-NLS-1$
			appendable.append(extractId(this.code));
			appendable.append("\", "); //$NON-NLS-1$
			appendable.commitChanges();
		}
	}

	private static String toPattern(String qualifiedName) {
		final var buffer = new StringBuilder();
		final var parts = qualifiedName.split(Pattern.quote(".")); //$NON-NLS-1$
		final var len = parts.length - 1;
		for (var i = 0; i < len; ++i) {
			buffer.append(Pattern.quote(parts[i]));
			buffer.append(Pattern.quote(".")); //$NON-NLS-1$
			buffer.append(")?"); //$NON-NLS-1$
			buffer.insert(0, "(?:"); //$NON-NLS-1$
		}
		buffer.append(Pattern.quote(parts[len]));
		buffer.append(")?"); //$NON-NLS-1$
		buffer.insert(0, "(?:"); //$NON-NLS-1$
		return buffer.toString();
	}

	private static String extractId(String code) {
		final var index = code.lastIndexOf('.');
		if (index >= 0) {
			return code.substring(index + 1);
		}
		return code;
	}

}

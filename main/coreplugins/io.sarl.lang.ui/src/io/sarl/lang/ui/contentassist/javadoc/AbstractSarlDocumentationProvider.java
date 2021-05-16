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

package io.sarl.lang.ui.contentassist.javadoc;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.inject.Inject;

import com.google.common.base.Strings;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.text.DocumentRewriteSession;
import org.eclipse.jface.text.DocumentRewriteSessionType;
import org.eclipse.jface.text.IDocumentExtension4;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.DeprecationUtil;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.parser.IParseResult;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.util.PolymorphicDispatcher;
import org.eclipse.xtext.util.ReplaceRegion;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.ui.document.DocumentRewriter;
import org.eclipse.xtext.xbase.ui.document.DocumentRewriter.Section;
import org.eclipse.xtext.xbase.ui.imports.ReplaceConverter;
import org.eclipse.xtext.xbase.ui.refactoring.RefactoredResourceCopier;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.util.Utils;

/** Abstract tools for providing SARL documentation for JVM elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public abstract class AbstractSarlDocumentationProvider implements ISarlDocumentationProvider {

	@Inject
	private JvmTypesBuilder typeBuilder;

	@Inject
	private DocumentRewriter.Factory rewriterFactory;

	@Inject
	private RefactoredResourceCopier resourceCopier;

	@Inject
	private ReplaceConverter replaceConverter;

	@Inject
	private SarlJvmModelAssociations sarlAssociations;

	@Inject
	private CommonTypeComputationServices types;

	private final PolymorphicDispatcher<Object> generatorDispatcher;

	/** Constructor.
	 */
	public AbstractSarlDocumentationProvider() {
		this.generatorDispatcher = PolymorphicDispatcher.createForSingleTarget("_generate", 2, 2, this);
	}

	@Override
	public void generateDocumentationIfPossible(IXtextDocument document, ITextSelection selection) throws Exception {
		final XtextResource resource = reloadResource(document);

		final EObject semanticObject = findSemanticObject(resource, selection);

		if (isValidElement(semanticObject)) {
			final String documentation = this.typeBuilder.getDocumentation(semanticObject);
			if (Strings.isNullOrEmpty(documentation)) {
				generateDocumentation(document, selection, resource, semanticObject);
			}
		}
	}

	private EObject findSemanticObject(XtextResource resource, ITextSelection selection) {
		final IParseResult parseResult = resource.getParseResult();
		if (parseResult != null) {
			final ICompositeNode rootNode = parseResult.getRootNode();
			ILeafNode node = NodeModelUtils.findLeafNodeAtOffset(rootNode, selection.getOffset());
			if (node == null) {
				return null;
			}
			if (node.isHidden()) {
				if (selection.getLength() > node.getLength()) {
					node = NodeModelUtils.findLeafNodeAtOffset(rootNode, node.getEndOffset());
				} else {
					node = NodeModelUtils.findLeafNodeAtOffset(rootNode, selection.getOffset() - 1);
				}
			} else if (node.getOffset() == selection.getOffset()) {
				node = NodeModelUtils.findLeafNodeAtOffset(rootNode, selection.getOffset() - 1);
			}
			if (node != null) {
				EObject currentSemanticElement = NodeModelUtils.findActualSemanticObjectFor(node);
				if (currentSemanticElement != null) {
					EObject parent = currentSemanticElement.eContainer();
					while (parent != null && !isValidElement(parent)) {
						parent = parent.eContainer();
					}
					if (parent != null && isValidElement(parent)) {
						currentSemanticElement = parent;
					}
				}
				return currentSemanticElement;
			}
		}
		return null;
	}

	private XtextResource reloadResource(IXtextDocument document) {
		final IUnitOfWork<XtextResource, XtextResource> function = (XtextResource it) -> {
			return this.resourceCopier.loadIntoNewResourceSet(it);
		};
		return document.<XtextResource>priorityReadOnly(function);
	}

	/** Generate the documentation.
	 *
	 * @param document the document to change.
	 * @param selection the selected area.
	 * @param resource the Xtext resource.
	 * @param semanticObject the selected element.
	 * @throws Exception if something wrong append during the change application.
	 */
	protected void generateDocumentation(IXtextDocument document, ITextSelection selection, XtextResource resource,
			EObject semanticObject) throws Exception {
		final DocumentRewriter documentRewriter = this.rewriterFactory.create(document, resource);
		final DocumentRewriteSession rewriteSession;
		if (document instanceof IDocumentExtension4) {
			rewriteSession = ((IDocumentExtension4) document).startRewriteSession(DocumentRewriteSessionType.UNRESTRICTED);
		} else {
			rewriteSession = null;
		}

		computeChange(documentRewriter, semanticObject);

		final List<ReplaceRegion> changes = documentRewriter.getChanges();
		final TextEdit convertToTextEdit = this.replaceConverter.convertToTextEdit(changes);
		if (convertToTextEdit != null) {
			convertToTextEdit.apply(document);
		}
		if (rewriteSession != null) {
			((IDocumentExtension4) document).stopRewriteSession(rewriteSession);
		}
	}

	/** Compute the documentation addition.
	 *
	 * @param rewriter the source code rewriter.
	 * @param semanticObject the semantic object for which the documentation should be created.
	 */
	protected void computeChange(DocumentRewriter rewriter, EObject semanticObject) {
		final ICompositeNode elementNode = NodeModelUtils.findActualNodeFor(semanticObject);
		if (elementNode == null) {
			return;
		}
		final Section section = rewriter.newSection(elementNode.getOffset(), 0);
		generateDocumentationText(semanticObject, section);
	}

	/** Generate the text of the documentation.
	 *
	 * @param semanticObject the semantic object for which the documentation should be created.
	 * @param receiver the receiver of the documentation.
	 */
	protected void generateDocumentationText(EObject semanticObject, ISourceAppender receiver) {
		final Receiver it = new Receiver(receiver);
		this.generatorDispatcher.invoke(semanticObject, it);
		if (!it.isChanged()) {
			it.doEmptyDocument();
		}
		// Append into the original receiver
		receiver.append(getDocumentationEnd()).newLine(); //$NON-NLS-1$
	}

	/** Replies the text for starting a documentation.
	 *
	 * @return the text.
	 */
	protected abstract String getDocumentationStart();

	/** Replies the text for the prefix of a documentation line.
	 *
	 * @return the text.
	 */
	protected abstract String getDocumentationLinePrefix();

	/** Replies the text for ending a documentation.
	 *
	 * @return the text.
	 */
	protected abstract String getDocumentationEnd();

	/** Replies the type of the return value.
	 *
	 * @param function the function to read.
	 * @return the return type.
	 */
	protected LightweightTypeReference getReturnType(XtendFunction function) {
		JvmTypeReference type = function.getReturnType();
		if (type == null) {
			final JvmOperation operation = this.sarlAssociations.getDirectlyInferredOperation(function);
			if (operation != null) {
				type = operation.getReturnType();
			}
		}
		if (type != null) {
			return Utils.toLightweightTypeReference(type, this.types);
		}
		return null;
	}

	/** Replies if the given object is marked as deprecated.
	 *
	 * @param object the object to read.
	 * @return {@code true} if the object is deprecated.
	 */
	protected boolean isDeprecated(EObject object) {
		final EObject obj = this.sarlAssociations.getPrimaryJvmElement(object);
		if (obj instanceof JvmAnnotationTarget) {
			return DeprecationUtil.isTransitivelyDeprecated((JvmAnnotationTarget) obj);
		}
		return false;
	}

	/** Root function for the dispatch functions.
	 *
	 * @param object the element for which a documentation must be generated.
	 * @param it the receiver of the documentation..
	 */
	protected void _generate(EObject object, ISourceAppender it) {
		//
	}

	/**
	 * Wrapper for source appender.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	private class Receiver implements ISourceAppender {

		private final ISourceAppender delegate;

		private final AtomicBoolean hasStartPrefix = new AtomicBoolean(true);

		private final AtomicBoolean hasLinePrefix = new AtomicBoolean(false);

		private final AtomicBoolean changed = new AtomicBoolean(false);

		/** Constructor.
		 *
		 * @param delegate the delegate.
		 */
		Receiver(ISourceAppender delegate) {
			this.delegate = delegate;
		}

		/** Do an empty documentation.
		 */
		void doEmptyDocument() {
			this.delegate.append(getDocumentationStart()).newLine();
		}

		/** Indicates if the appender has changed.
		 *
		 * @return {@code true} if some text was appended.
		 */
		boolean isChanged() {
			return this.changed.get();
		}

		private void prepareChange() {
			this.changed.set(true);
			if (this.hasStartPrefix.getAndSet(false)) {
				this.delegate.append(getDocumentationStart()).newLine();
				this.delegate.append(getDocumentationLinePrefix()).newLine();
				this.delegate.append(getDocumentationLinePrefix());
			} else if (this.hasLinePrefix.getAndSet(false)) {
				this.delegate.append(getDocumentationLinePrefix());
			}
		}

		@Override
		public ISourceAppender append(CharSequence text) {
			prepareChange();
			this.delegate.append(text);
			return this;
		}

		@Override
		public ISourceAppender append(JvmType type) {
			prepareChange();
			this.delegate.append(type);
			return this;
		}

		@Override
		public ISourceAppender append(LightweightTypeReference typeRef) {
			prepareChange();
			this.delegate.append(typeRef);
			return this;
		}

		@Override
		public ISourceAppender newLine() {
			prepareChange();
			this.delegate.newLine();
			this.hasLinePrefix.set(true);
			return this;
		}

		@Override
		public ISourceAppender increaseIndentation() {
			return this;
		}

		@Override
		public ISourceAppender decreaseIndentation() {
			return this;
		}

		@Override
		public boolean isJava() {
			return this.delegate.isJava();
		}

		@Override
		public String toString() {
			return this.delegate.toString();
		}

	}

}

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

package io.sarl.lang.ui.quickfix;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Predicate;
import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.xtend.core.jvmmodel.IXtendJvmAssociations;
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend.ide.quickfix.XtendQuickfixProvider;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.common.types.util.jdt.IJavaElementFinder;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.Fix;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.ui.refactoring.impl.ProjectUtil;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.util.Arrays;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypeReferenceBuilder;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;

import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.ui.quickfix.acceptors.ActionAddModification;
import io.sarl.lang.ui.quickfix.acceptors.AnnotationRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.BehaviorUnitGuardRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.CapacityReferenceRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.CapacityUseAddModification;
import io.sarl.lang.ui.quickfix.acceptors.ExtendedTypeRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.FiredEventRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.ImplementedTypeRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.ImplementedTypeRemoveModification.RemovalType;
import io.sarl.lang.ui.quickfix.acceptors.MemberRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.MemberRenameModification;
import io.sarl.lang.ui.quickfix.acceptors.Messages;
import io.sarl.lang.ui.quickfix.acceptors.MissedMethodAddModification;
import io.sarl.lang.ui.quickfix.acceptors.ModifierRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.MultiModification;
import io.sarl.lang.ui.quickfix.acceptors.ProtectKeywordModification;
import io.sarl.lang.ui.quickfix.acceptors.ReturnTypeAddModification;
import io.sarl.lang.ui.quickfix.acceptors.ReturnTypeReplaceModification;
import io.sarl.lang.ui.quickfix.acceptors.SuperTypeRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.SuppressWarningsAddModification;
import io.sarl.lang.validation.SyntaxIssueCodes;

/**
 * Custom quickfixes.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#quick-fixes"
 */
@SuppressWarnings({"static-method", "checkstyle:methodcount", "checkstyle:classfanoutcomplexity"})
public class SARLQuickfixProvider extends XtendQuickfixProvider {

	@Inject
	private Injector injector;

	@Inject
	private ReplacingAppendable.Factory appendableFactory;

	@Inject
	private SARLGrammarKeywordAccess grammarAccess;

	@Inject
	private IXtendJvmAssociations associations;

	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private IActionPrototypeProvider prototypeProvider;

	@Inject
	private IQualifiedNameProvider qualifiedNameProvider;

	@Inject
	private ConfigurableIssueCodesProvider issueCodesProvider;

	@Inject
	private ProjectUtil projectUtil;

	@Inject
	private IResourceSetProvider resourceSetProvider;

	@Inject
	private AnnotationLookup annotationFinder;

	@Inject
	private JvmTypeReferenceBuilder typeParameterBuilder;

	@Inject
	private JvmTypesBuilder typeBuilder;

	@Inject
	private IJavaElementFinder javaElementFinder;

	/** Replies if the given code is for a ignorable warning.
	 *
	 * @param code the code of the warning.
	 * @return <code>true</code> if the warning could be ignored, <code>false</code> otherwise.
	 */
	public boolean isIgnorable(String code) {
		return this.issueCodesProvider.getConfigurableIssueCodes().containsKey(code);
	}

	@Override
	protected Predicate<Method> getFixMethodPredicate(final String issueCode) {
		return new Predicate<Method>() {
			@Override
			public boolean apply(Method input) {
				final Fix annotation = input.getAnnotation(Fix.class);
				final boolean result = annotation != null
						&& ("*".equals(annotation.value()) || issueCode.equals(annotation.value())) //$NON-NLS-1$
						&& input.getParameterTypes().length == 2 && Void.TYPE == input.getReturnType()
						&& input.getParameterTypes()[0].isAssignableFrom(Issue.class)
						&& input.getParameterTypes()[1].isAssignableFrom(IssueResolutionAcceptor.class);
				return result;
			}
		};
	}

	/** Add the fixes with suppress-warning annotations.
	 *
	 * @param issue the issue.
	 * @param acceptor the resolution acceptor.
	 */
	@Fix("*")
	public void fixSuppressWarnings(Issue issue, IssueResolutionAcceptor acceptor) {
		if (isIgnorable(issue.getCode())) {
			SuppressWarningsAddModification.accept(this, issue, acceptor);
		}
	}

	/** Replies the JVM operations that correspond to the given URIs.
	 *
	 * @param container the container of the operations.
	 * @param operationUris the URIs.
	 * @return the JVM operations.
	 */
	public List<JvmOperation> getJvmOperationsFromURIs(XtendTypeDeclaration container, String... operationUris) {
		// Collect the JvmOperation prior to any modification for ensuring that
		// URI are pointing the JvmOperations.
		final List<JvmOperation> operations = new ArrayList<>();
		final ResourceSet resourceSet = container.eResource().getResourceSet();
		for (final String operationUriAsString : operationUris) {
			final URI operationURI = URI.createURI(operationUriAsString);
			final EObject overridden = resourceSet.getEObject(operationURI, true);
			if (overridden instanceof JvmOperation) {
				final JvmOperation operation = (JvmOperation) overridden;
				if (this.annotationFinder.findAnnotation(operation, DefaultValueUse.class) == null) {
					operations.add(operation);
				}
			}
		}
		return operations;
	}

	/** Replies the injector used by this object.
	 *
	 * @return the injector.
	 */
	public Injector getInjector() {
		return this.injector;
	}

	/** Replies a provider of qualified name.
	 *
	 * @return the provider of qualified name.
	 */
	public IQualifiedNameProvider getQualifiedNameProvider() {
		return this.qualifiedNameProvider;
	}

	@Override
	public IQualifiedNameConverter getQualifiedNameConverter() {
		return super.getQualifiedNameConverter();
	}

	/** Replies the provider of action prototypes.
	 *
	 * @return the action prototype provider.
	 */
	public IActionPrototypeProvider getActionPrototypeProvider() {
		return this.prototypeProvider;
	}

	/** Replies the factory for appendable.
	 *
	 * @return the appendable factory.
	 */
	public ReplacingAppendable.Factory getAppendableFactory() {
		return this.appendableFactory;
	}

	/** Replies the Java element finder.
	 *
	 * @return the finder.
	 * @since 0.12
	 */
	public IJavaElementFinder getJavaElementFinder() {
		return this.javaElementFinder;
	}

	/** Replies the project utilities.
	 *
	 * @return the utilities.
	 */
	public ProjectUtil getProjectUtil() {
		return this.projectUtil;
	}

	/** Replies the resource set provider.
	 *
	 * @return the provider.
	 */
	public IResourceSetProvider getResourceSetProvider() {
		return this.resourceSetProvider;
	}

	/** Replies the type services.
	 *
	 * @return the type services.
	 */
	public CommonTypeComputationServices getTypeServices() {
		return this.services;
	}

	/** Replies the type parameter builder.
	 *
	 * @return the builder.
	 * @since 0.6
	 */
	public JvmTypeReferenceBuilder getJvmTypeParameterBuilder() {
		return this.typeParameterBuilder;
	}

	/** Replies the type builder.
	 *
	 * @return the builder.
	 * @since 0.6
	 */
	public JvmTypesBuilder getJvmTypeBuilder() {
		return this.typeBuilder;
	}

	/** Replies the JVM associations.
	 *
	 * @return the JVM associations.
	 */
	public IXtendJvmAssociations getJvmAssociations() {
		return this.associations;
	}

	/** Replies the SARL grammar accessor.
	 *
	 * @return the SARL grammar accessor.
	 */
	public SARLGrammarKeywordAccess getGrammarAccess() {
		return this.grammarAccess;
	}

	/** Remove the element related to the issue, and the whitespaces before the element until the given separator.
	 *
	 * @param issue the issue.
	 * @param document the document.
	 * @param separator the separator to consider.
	 * @return <code>true</code> if the separator was found, <code>false</code> if not.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	public boolean removeToPreviousSeparator(Issue issue, IXtextDocument document, String separator)
			throws BadLocationException {
		return removeToPreviousSeparator(issue.getOffset(), issue.getLength(), document, separator);
	}

	/** Remove the portion of text, and the whitespaces before the text until the given separator.
	 *
	 * @param offset the offset where to start to remove.
	 * @param length the length of the text to remove.
	 * @param document the document.
	 * @param separator the separator to consider.
	 * @return <code>true</code> if the separator was found, <code>false</code> if not.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	public boolean removeToPreviousSeparator(int offset, int length, IXtextDocument document, String separator)
			throws BadLocationException {
		// Skip spaces before the identifier until the separator
		int index = offset - 1;
		char c = document.getChar(index);
		while (Character.isWhitespace(c)) {
			index--;
			c = document.getChar(index);
		}

		// Test if it previous non-space character is the separator
		final boolean foundSeparator = document.getChar(index) == separator.charAt(0);
		if (foundSeparator) {
			index--;
			c = document.getChar(index);
			// Skip the previous spaces
			while (Character.isWhitespace(c)) {
				index--;
				c = document.getChar(index);
			}

			final int delta = offset - index - 1;
			document.replace(index + 1, length + delta, ""); //$NON-NLS-1$
		}

		return foundSeparator;
	}

	/** Replies the index where import declaration could be inserted into the given container.
	 *
	 * @param script the script to consider for the insertion
	 * @return the insertion index.
	 */
	public int getImportInsertOffset(SarlScript script) {
		final ICompositeNode node = NodeModelUtils.findActualNodeFor(script.getImportSection());
		if (node == null) {
			final List<INode> children = NodeModelUtils.findNodesForFeature(script,
					XtendPackage.eINSTANCE.getXtendFile_Package());
			if (children.isEmpty()) {
				return 0;
			}
			return children.get(0).getEndOffset();
		}
		return node.getEndOffset();
	}

	/** Remove the element related to the issue, and the whitespaces after the element until the given separator.
	 *
	 * @param issue the issue.
	 * @param document the document.
	 * @param separator the separator to consider.
	 * @return <code>true</code> if the separator was found, <code>false</code> if not.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	public boolean removeToNextSeparator(Issue issue, IXtextDocument document, String separator)
			throws BadLocationException {
		// Skip spaces after the identifier until the separator
		int index = issue.getOffset() + issue.getLength();
		char c = document.getChar(index);
		while (Character.isWhitespace(c)) {
			index++;
			c = document.getChar(index);
		}

		// Test if it next non-space character is the separator
		final boolean foundSeparator = document.getChar(index) == separator.charAt(0);
		if (foundSeparator) {
			index++;
			c = document.getChar(index);
			// Skip the previous spaces
			while (Character.isWhitespace(c)) {
				index++;
				c = document.getChar(index);
			}

			final int newLength = index - issue.getOffset();
			document.replace(issue.getOffset(), newLength, ""); //$NON-NLS-1$
		}

		return foundSeparator;
	}

	/** Remove the element related to the issue, and the whitespaces before the element until one of the given
	 * keywords is encountered.
	 *
	 * @param issue the issue.
	 * @param document the document.
	 * @param keyword1 the first keyword to consider.
	 * @param otherKeywords other keywords.
	 * @return <code>true</code> if one keyword was found, <code>false</code> if not.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	public boolean removeToPreviousKeyword(Issue issue, IXtextDocument document,
			String keyword1, String... otherKeywords) throws BadLocationException {
		// Skip spaces before the element
		int index = issue.getOffset() - 1;
		char c = document.getChar(index);
		while (Character.isWhitespace(c)) {
			index--;
			c = document.getChar(index);
		}

		// Skip non-spaces before the identifier
		final StringBuffer kw = new StringBuffer();
		while (!Character.isWhitespace(c)) {
			kw.insert(0, c);
			index--;
			c = document.getChar(index);
		}

		if (kw.toString().equals(keyword1) || Arrays.contains(otherKeywords, kw.toString())) {
			// Skip spaces before the previous keyword
			while (Character.isWhitespace(c)) {
				index--;
				c = document.getChar(index);
			}

			final int delta = issue.getOffset() - index - 1;
			document.replace(index + 1, issue.getLength() + delta, ""); //$NON-NLS-1$

			return true;
		}

		return false;
	}

	/** Remove the element related to the issue, and the whitespaces before the element until the begin separator,
	 * and the whitespaces after the element until the end separator.
	 *
	 * @param issue the issue.
	 * @param document the document.
	 * @param beginSeparator the separator before the element.
	 * @param endSeparator the separator after the element.
	 * @return <code>true</code> if the separator was found, <code>false</code> if not.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	public boolean removeBetweenSeparators(Issue issue, IXtextDocument document,
			String beginSeparator, String endSeparator) throws BadLocationException {
		int offset = issue.getOffset();
		int length = issue.getLength();

		// Skip spaces before the identifier until the separator
		int index = offset - 1;
		char c = document.getChar(index);
		while (Character.isWhitespace(c)) {
			index--;
			c = document.getChar(index);
		}

		// Test if it previous non-space character is the separator
		boolean foundSeparator = document.getChar(index) == beginSeparator.charAt(0);
		if (foundSeparator) {
			index--;
			c = document.getChar(index);
			// Skip the previous spaces
			while (Character.isWhitespace(c)) {
				index--;
				c = document.getChar(index);
			}

			length = length + (offset - index - 1);
			offset = index + 1;

			// Skip spaces after the identifier until the separator
			index = offset + length;
			c = document.getChar(index);
			while (Character.isWhitespace(c)) {
				index++;
				c = document.getChar(index);
			}

			// Test if it next non-space character is the separator
			foundSeparator = document.getChar(index) == endSeparator.charAt(0);
			if (foundSeparator) {
				index++;

				length = index - offset;
				document.replace(offset, length, ""); //$NON-NLS-1$
			}
		}

		return foundSeparator;
	}

	/** Replies the index where elements could be inserted into the given container.
	 *
	 * @param container the container to consider for the insertion
	 * @return the insertion index.
	 */
	public int getInsertOffset(XtendTypeDeclaration container) {
		if (container.getMembers().isEmpty()) {
			final ICompositeNode node = NodeModelUtils.findActualNodeFor(container);
			final ILeafNode openingBraceNode = IterableExtensions.findFirst(node.getLeafNodes(),
				lnode -> "{".equals(lnode.getText())); //$NON-NLS-1$
			if (openingBraceNode != null) {
				return openingBraceNode.getOffset() + 1;
			}
			return node.getEndOffset();
		}
		final EObject lastFeature = IterableExtensions.last(container.getMembers());
		final ICompositeNode node = NodeModelUtils.findActualNodeFor(lastFeature);
		return node.getEndOffset();
	}

	/** Replies the size of a sequence of whitespaces.
	 *
	 * @param document the document.
	 * @param offset the offset of the first character of the sequence.
	 * @return the number of whitespaces at the given offset.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	public int getSpaceSize(IXtextDocument document, int offset) throws BadLocationException {
		int size = 0;
		char c = document.getChar(offset + size);
		while (Character.isWhitespace(c)) {
			size++;
			c = document.getChar(offset + size);
		}
		return size;
	}

	/** Replies the offset that corresponds to the given regular expression pattern.
	 *
	 * @param document the document to parse.
	 * @param startOffset the offset in the text at which the pattern must be recognized.
	 * @param pattern the regular expression pattern.
	 * @return the offset (greater or equal to the startOffset), or <code>-1</code> if the pattern
	 *     cannot be recognized.
	 */
	public int getOffsetForPattern(IXtextDocument document, int startOffset, String pattern) {
		final Pattern compiledPattern = Pattern.compile(pattern);
		final Matcher matcher = compiledPattern.matcher(document.get());
		if (matcher.find(startOffset)) {
			final int end = matcher.end();
			return end;
		}
		return -1;
	}

	/** Replies the qualified name for the given name.
	 *
	 * @param name the name.
	 * @return the qualified name.
	 */
	public QualifiedName qualifiedName(String name) {
		if (!com.google.common.base.Strings.isNullOrEmpty(name)) {
			final List<String> segments = Strings.split(name, "."); //$NON-NLS-1$
			return QualifiedName.create(segments);
		}
		return QualifiedName.create();
	}

	/** Remove the exectuable feature.
	 *
	 * @param element the executable feature to remove.
	 * @param context the context of the change.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	public void removeExecutableFeature(EObject element, IModificationContext context) throws BadLocationException {
		final ICompositeNode node;
		final SarlAction action = EcoreUtil2.getContainerOfType(element, SarlAction.class);
		if (action == null) {
			final XtendMember feature = EcoreUtil2.getContainerOfType(element, XtendMember.class);
			node = NodeModelUtils.findActualNodeFor(feature);
		} else {
			node = NodeModelUtils.findActualNodeFor(action);
		}
		if (node != null) {
			remove(context.getXtextDocument(), node);
		}
	}

	@Override
	public <T extends EObject> void remove(EObject element, Class<T> type, IModificationContext context)
			throws BadLocationException {
		// Make the function visible
		super.remove(element, type, context);
	}

	/** Remove the given node and the whitespaces before/after.
	 *
	 * @param <T> the type of element to remove (must be for the given element or one of its container).
	 * @param element the source of the change.
	 * @param type the type of element to remove (must be for the given element or one of its container).
	 * @param context the modification context.
	 * @throws BadLocationException if the location cannot be computed properly.
	 */
	public <T extends EObject> void removeIncludingWhiteSpaces(EObject element, Class<T> type,
			IModificationContext context) throws BadLocationException {
		// Search the node
		final T container = EcoreUtil2.getContainerOfType(element, type);
		if (container == null) {
			return;
		}
		final ICompositeNode node = NodeModelUtils.findActualNodeFor(container);
		if (node == null) {
			return;
		}
		// Compute region for the node
		int offset = node.getOffset();
		int length = node.getLength();
		if (node.hasPreviousSibling()) {
			final INode previousSibling = node.getPreviousSibling();
			final int endOffset = previousSibling.getEndOffset();
			length = length + (offset - endOffset);
			offset = endOffset;
		}
		final IXtextDocument document = context.getXtextDocument();
		// Include spaces in the region
		final int docEndOffset = document.getLength();
		int endOffset = offset + length;
		while (offset > 0 && isBasicSpace(document.getChar(offset - 1))) {
			--offset;
			++length;
		}
		while (endOffset < docEndOffset && isBasicSpace(document.getChar(endOffset))) {
			++endOffset;
			++length;
		}
		document.replace(offset, length, ""); //$NON-NLS-1$
	}

	/** Replies if the gien character is a simple space character or a tabulation character.
	 * Line feeds and other white space characters that are supported by {@link Character#isWhitespace(char)}
	 * are not considered as basic space by the current function.
	 *
	 * @param character the character to test.
	 * @return {@code true} if the given character is a basic space or a tabulation character.
	 */
	public boolean isBasicSpace(char character) {
		return character == ' ' || character == '\t';
	}

	/** Quick fix for "Duplicate type".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_TYPE_NAME)
	public void fixDuplicateTopElements(Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Duplicate field".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_FIELD)
	public void fixDuplicateAttribute(Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Duplicate method".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_METHOD)
	public void fixDuplicateMethod(Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "disallowed variable name".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED)
	public void fixDisallowedFieldName(final Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRenameModification.accept(this, issue, acceptor);
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "disallowed variable name".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISCOURAGED)
	public void fixDiscouragedFieldName(final Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid member name".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_MEMBER_NAME)
	public void fixMemberName(final Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRenameModification.accept(this, issue, acceptor);
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Redundant interface implementation".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION)
	public void fixRedundantInterface(final Issue issue, IssueResolutionAcceptor acceptor) {
		ImplementedTypeRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Variable name shadowing".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING)
	public void fixVariableNameShadowing(final Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
		MemberRenameModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Override final operation".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(IssueCodes.OVERRIDDEN_FINAL)
	public void fixOverriddenFinal(Issue issue, IssueResolutionAcceptor acceptor) {
		final MultiModification modifications = new MultiModification(
				this, issue, acceptor,
				Messages.SARLQuickfixProvider_0,
				Messages.SARLQuickfixProvider_1);
		modifications.bind(XtendTypeDeclaration.class, SuperTypeRemoveModification.class);
		modifications.bind(XtendMember.class, MemberRemoveModification.class);
	}

	/** Quick fix for "Discouraged boolean expression".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION)
	public void fixDiscouragedBooleanExpression(final Issue issue, IssueResolutionAcceptor acceptor) {
		BehaviorUnitGuardRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Unreachable behavior unit".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.UNREACHABLE_BEHAVIOR_UNIT)
	public void fixUnreachableBehaviorUnit(Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid capacity type".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE)
	public void fixInvalidCapacityType(final Issue issue, IssueResolutionAcceptor acceptor) {
		CapacityReferenceRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid firing event type".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.INVALID_FIRING_EVENT_TYPE)
	public void fixInvalidFiringEventType(final Issue issue, IssueResolutionAcceptor acceptor) {
		FiredEventRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid implemented type".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.INVALID_IMPLEMENTED_TYPE)
	public void fixInvalidImplementedType(final Issue issue, IssueResolutionAcceptor acceptor) {
		ImplementedTypeRemoveModification.accept(this, issue, acceptor, RemovalType.OTHER);
	}

	/** Quick fix for "Invalid extended type".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.INVALID_EXTENDED_TYPE)
	public void fixInvalidExtendedType(final Issue issue, IssueResolutionAcceptor acceptor) {
		ExtendedTypeRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Cyclic hierarchy".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(IssueCodes.CYCLIC_INHERITANCE)
	public void fixCyclicInheritance(final Issue issue, IssueResolutionAcceptor acceptor) {
		ExtendedTypeRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Interface expected".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(IssueCodes.INTERFACE_EXPECTED)
	public void fixInteraceExpected(final Issue issue, IssueResolutionAcceptor acceptor) {
		ExtendedTypeRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Class expected".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(IssueCodes.CLASS_EXPECTED)
	public void fixClassExpected(final Issue issue, IssueResolutionAcceptor acceptor) {
		ExtendedTypeRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Discouraged capacity definition".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.DISCOURAGED_CAPACITY_DEFINITION)
	public void fixDiscouragedCapacityDefinition(Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor, SarlCapacity.class);
		ActionAddModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Incompatible return type".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE)
	public void fixIncompatibleReturnType(final Issue issue, IssueResolutionAcceptor acceptor) {
		ReturnTypeReplaceModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Return type is recommended".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED)
	public void fixReturnTypeRecommended(final Issue issue, IssueResolutionAcceptor acceptor) {
		ReturnTypeAddModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Unused agent capacity".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY)
	public void fixUnusedAgentCapacity(final Issue issue, IssueResolutionAcceptor acceptor) {
		CapacityReferenceRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Redundant capacity use".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE)
	public void fixRedundantAgentCapacityUse(final Issue issue, IssueResolutionAcceptor acceptor) {
		CapacityReferenceRemoveModification.accept(this, issue, acceptor);
	}

	@Override
	protected void doOverrideMethods(Issue issue,
			IssueResolutionAcceptor acceptor, String label,
			String[] operationUris) {
		MissedMethodAddModification.accept(this, issue, acceptor, label, operationUris);
	}

	@Override
	protected void internalDoAddAbstractKeyword(EObject element, IModificationContext context)
			throws BadLocationException {
		EObject container = element;
		if (element instanceof SarlAction) {
			container = element.eContainer();
		}
		XtendTypeDeclaration declaration = null;
		String keyword = null;
		if (container instanceof SarlAgent) {
			declaration = (XtendTypeDeclaration) container;
			keyword = getGrammarAccess().getAgentKeyword();
		} else if (container instanceof SarlBehavior) {
			declaration = (XtendTypeDeclaration) container;
			keyword = getGrammarAccess().getBehaviorKeyword();
		} else if (container instanceof SarlSkill) {
			declaration = (XtendTypeDeclaration) container;
			keyword = getGrammarAccess().getSkillKeyword();
		}
		if (declaration != null && keyword != null) {
			final IXtextDocument document = context.getXtextDocument();
			addAbstractKeyword(declaration, document, keyword);
		} else {
			super.internalDoAddAbstractKeyword(container, context);
		}
	}

	private void addAbstractKeyword(XtendTypeDeclaration typeDeclaration, IXtextDocument document,
			String declarationKeyword) throws BadLocationException {
		final ICompositeNode clazzNode = NodeModelUtils.findActualNodeFor(typeDeclaration);
		if (clazzNode == null) {
			throw new IllegalStateException("Cannot determine node for the type declaration" //$NON-NLS-1$
					+ typeDeclaration.getName());
		}
		int offset = -1;
		final Iterator<ILeafNode> nodes = clazzNode.getLeafNodes().iterator();
		while (offset == -1 && nodes.hasNext()) {
			final ILeafNode leafNode  = nodes.next();
			if (leafNode.getText().equals(declarationKeyword)) {
				offset = leafNode.getOffset();
			}
		}
		final ReplacingAppendable appendable = this.appendableFactory.create(document,
				(XtextResource) typeDeclaration.eResource(),
				offset, 0);
		appendable.append(getGrammarAccess()
				.getAbstractKeyword())
				.append(" "); //$NON-NLS-1$
		appendable.commitChanges();
	}

	/** Quick fix for the no viable alternative at an input that is a SARL keyword.
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(SyntaxIssueCodes.USED_RESERVED_KEYWORD)
	public void fixNoViableAlternativeAtKeyword(final Issue issue, IssueResolutionAcceptor acceptor) {
		ProtectKeywordModification.accept(this, issue, acceptor);
	}

	/** Quick fix for the discouraged annotation uses.
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.USED_RESERVED_SARL_ANNOTATION)
	public void fixDiscouragedAnnotationUse(final Issue issue, IssueResolutionAcceptor acceptor) {
		AnnotationRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for the manual definition of inline statements.
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.MANUAL_INLINE_DEFINITION)
	public void fixManualInlineDefinition(final Issue issue, IssueResolutionAcceptor acceptor) {
		AnnotationRemoveModification.accept(this, issue, acceptor);
	}

	@Override
	protected void createLinkingIssueQuickfixes(Issue issue, IssueResolutionAcceptor issueResolutionAcceptor,
			IXtextDocument xtextDocument, XtextResource resource, EObject referenceOwner, EReference unresolvedReference) throws Exception {
		CapacityUseAddModification.accept(this, issue, referenceOwner, issueResolutionAcceptor);
		super.createLinkingIssueQuickfixes(issue, issueResolutionAcceptor, xtextDocument, resource, referenceOwner, unresolvedReference);
	}

	/** Quick fix for "Potential problem of data sharing outside the control of the agent".
	 *
	 * @param issue the issue.
	 * @param acceptor the quick fix acceptor.
	 * @since 0.12
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL)
	public void fixPotentialMemorySharingOutsideAgentControl(final Issue issue, IssueResolutionAcceptor acceptor) {
		ModifierRemoveModification.accept(this, issue, acceptor, this.grammarAccess.getStaticStaticKeyword());
	}

}

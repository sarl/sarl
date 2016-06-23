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

package io.sarl.lang.ui.quickfix;

import java.util.Iterator;
import java.util.List;

import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.xtend.core.jvmmodel.IXtendJvmAssociations;
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend.ide.quickfix.XtendQuickfixProvider;
import org.eclipse.xtext.EcoreUtil2;
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
import org.eclipse.xtext.util.Arrays;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;

import io.sarl.lang.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.services.SARLGrammarAccess;
import io.sarl.lang.ui.quickfix.acceptors.ActionAddModification;
import io.sarl.lang.ui.quickfix.acceptors.BehaviorUnitGuardRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.CapacityReferenceRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.ExtendedTypeRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.FiredEventRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.ImplementedTypeRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.ImplementedTypeRemoveModification.RemovalType;
import io.sarl.lang.ui.quickfix.acceptors.MemberRemoveModification;
import io.sarl.lang.ui.quickfix.acceptors.MemberRenameModification;
import io.sarl.lang.ui.quickfix.acceptors.Messages;
import io.sarl.lang.ui.quickfix.acceptors.MissedMethodAddModification;
import io.sarl.lang.ui.quickfix.acceptors.MultiModification;
import io.sarl.lang.ui.quickfix.acceptors.ReturnTypeAddModification;
import io.sarl.lang.ui.quickfix.acceptors.ReturnTypeReplaceModification;
import io.sarl.lang.ui.quickfix.acceptors.SuperTypeRemoveModification;

/**
 * Custom quickfixes.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://www.eclipse.org/Xtext/documentation/304_ide_concepts.html#quick-fixes"
 */
@SuppressWarnings({"static-method", "checkstyle:methodcount"})
public class SARLQuickfixProvider extends XtendQuickfixProvider {

	@Inject
	private Injector injector;

	@Inject
	private ReplacingAppendable.Factory appendableFactory;

	@Inject
	private SARLGrammarAccess grammarAccess;

	@Inject
	private IXtendJvmAssociations associations;

	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private IActionPrototypeProvider prototypeProvider;

	@Inject
	private IQualifiedNameProvider qualifiedNameProvider;

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

	/** Replies the type services.
	 *
	 * @return the type serices.
	 */
	public CommonTypeComputationServices getTypeServices() {
		return this.services;
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
	public SARLGrammarAccess getGrammarAccess() {
		return this.grammarAccess;
	}

	/** Remove the element related to the issue, and the whitespaces before the element until the given separator.
	 *
	 * @param issue - the issue.
	 * @param document - the document.
	 * @param separator - the separator to consider.
	 * @return <code>true</code> if the separator was found, <code>false</code> if not.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	public boolean removeToPreviousSeparator(Issue issue, IXtextDocument document, String separator)
			throws BadLocationException {
		return removeToPreviousSeparator(issue.getOffset(), issue.getLength(), document, separator);
	}

	/** Remove the portion of text, and the whitespaces before the text until the given separator.
	 *
	 * @param offset - the offset where to start to remove.
	 * @param length - the length of the text to remove.
	 * @param document - the document.
	 * @param separator - the separator to consider.
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
		boolean foundSeparator = (document.getChar(index) == separator.charAt(0));
		if (foundSeparator) {
			index--;
			c = document.getChar(index);
			// Skip the previous spaces
			while (Character.isWhitespace(c)) {
				index--;
				c = document.getChar(index);
			}

			int delta = offset - index - 1;
			document.replace(index + 1, length + delta, ""); //$NON-NLS-1$
		}

		return foundSeparator;
	}

	/** Replies the index where import declaration could be inserted into the given container.
	 *
	 * @param script - the script to consider for the insertion
	 * @return the insertion index.
	 */
	public int getImportInsertOffset(SarlScript script) {
		ICompositeNode node = NodeModelUtils.findActualNodeFor(script.getImportSection());
		if (node == null) {
			List<INode> children = NodeModelUtils.findNodesForFeature(script, XtendPackage.eINSTANCE.getXtendFile_Package());
			if (children.isEmpty()) {
				return 0;
			}
			return children.get(0).getEndOffset();
		}
		return node.getEndOffset();
	}

	/** Remove the element related to the issue, and the whitespaces after the element until the given separator.
	 *
	 * @param issue - the issue.
	 * @param document - the document.
	 * @param separator - the separator to consider.
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
		boolean foundSeparator = (document.getChar(index) == separator.charAt(0));
		if (foundSeparator) {
			index++;
			c = document.getChar(index);
			// Skip the previous spaces
			while (Character.isWhitespace(c)) {
				index++;
				c = document.getChar(index);
			}

			int newLength = index - issue.getOffset();
			document.replace(issue.getOffset(), newLength, ""); //$NON-NLS-1$
		}

		return foundSeparator;
	}

	/** Remove the element related to the issue, and the whitespaces before the element until one of the given
	 * keywords is encountered.
	 *
	 * @param issue - the issue.
	 * @param document - the document.
	 * @param keyword1 - the first keyword to consider.
	 * @param otherKeywords - other keywords.
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
		StringBuffer kw = new StringBuffer();
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

			int delta = issue.getOffset() - index - 1;
			document.replace(index + 1, issue.getLength() + delta, ""); //$NON-NLS-1$

			return true;
		}

		return false;
	}

	/** Remove the element related to the issue, and the whitespaces before the element until the begin separator,
	 * and the whitespaces after the element until the end separator.
	 *
	 * @param issue - the issue.
	 * @param document - the document.
	 * @param beginSeparator - the separator before the element.
	 * @param endSeparator - the separator after the element.
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
		boolean foundSeparator = (document.getChar(index) == beginSeparator.charAt(0));
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
			foundSeparator = (document.getChar(index) == endSeparator.charAt(0));
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
	 * @param container - the container to consider for the insertion
	 * @return the insertion index.
	 */
	public int getInsertOffset(XtendTypeDeclaration container) {
		if (container.getMembers().isEmpty()) {
			ICompositeNode node = NodeModelUtils.findActualNodeFor(container);
			ILeafNode openingBraceNode = IterableExtensions.findFirst(node.getLeafNodes(),
					new Functions.Function1<ILeafNode, Boolean>() {
						@Override
						public Boolean apply(ILeafNode node) {
							return "{".equals(node.getText()); //$NON-NLS-1$
						}
					});
			if (openingBraceNode != null) {
				return openingBraceNode.getOffset() + 1;
			}
			return node.getEndOffset();
		}
		EObject lastFeature = IterableExtensions.last(container.getMembers());
		ICompositeNode node = NodeModelUtils.findActualNodeFor(lastFeature);
		return node.getEndOffset();
	}

	/** Replies the size of a sequence of whitespaces.
	 *
	 * @param document - the document.
	 * @param offset - the offset of the first character of the sequence.
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

	/** Replies the qualified name for the given name.
	 *
	 * @param name - the name.
	 * @return the qualified name.
	 */
	public QualifiedName qualifiedName(String name) {
		if (!com.google.common.base.Strings.isNullOrEmpty(name)) {
			List<String> segments = Strings.split(name, "."); //$NON-NLS-1$
			return QualifiedName.create(segments);
		}
		return QualifiedName.create();
	}

	/** Remove the exectuable feature.
	 *
	 * @param element - the executable feature to remove.
	 * @param context - the context of the change.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	public void removeExecutableFeature(EObject element, IModificationContext context) throws BadLocationException {
		ICompositeNode node = null;
		SarlAction action = EcoreUtil2.getContainerOfType(element, SarlAction.class);
		if (action == null) {
			XtendMember feature = EcoreUtil2.getContainerOfType(element, XtendMember.class);
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
		super.remove(element, type, context);
	}

	/** Quick fix for "Duplicate type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_TYPE_NAME)
	public void fixDuplicateTopElements(Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Duplicate field".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_FIELD)
	public void fixDuplicateAttribute(Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Duplicate method".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_METHOD)
	public void fixDuplicateMethod(Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "disallowed variable name".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISALLOWED)
	public void fixDisallowedFieldName(final Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRenameModification.accept(this, issue, acceptor);
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "disallowed variable name".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_DISCOURAGED)
	public void fixDiscouragedFieldName(final Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid member name".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_MEMBER_NAME)
	public void fixMemberName(final Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRenameModification.accept(this, issue, acceptor);
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Redundant interface implementation".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION)
	public void fixRedundantInterface(final Issue issue, IssueResolutionAcceptor acceptor) {
		ImplementedTypeRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Variable name shadowing".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING)
	public void fixVariableNameShadowing(final Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
		MemberRenameModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Override final operation".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.OVERRIDDEN_FINAL)
	public void fixOverriddenFinal(Issue issue, IssueResolutionAcceptor acceptor) {
		MultiModification modifications = new MultiModification(
				this, issue, acceptor,
				Messages.SARLQuickfixProvider_0,
				Messages.SARLQuickfixProvider_1);
		modifications.bind(XtendTypeDeclaration.class, SuperTypeRemoveModification.class);
		modifications.bind(XtendMember.class, MemberRemoveModification.class);
	}

	/** Quick fix for "Discouraged boolean expression".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION)
	public void fixDiscouragedBooleanExpression(final Issue issue, IssueResolutionAcceptor acceptor) {
		BehaviorUnitGuardRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Unreachable behavior unit".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.UNREACHABLE_BEHAVIOR_UNIT)
	public void fixUnreachableBehaviorUnit(Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid capacity type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.INVALID_CAPACITY_TYPE)
	public void fixInvalidCapacityType(final Issue issue, IssueResolutionAcceptor acceptor) {
		CapacityReferenceRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid firing event type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.INVALID_FIRING_EVENT_TYPE)
	public void fixInvalidFiringEventType(final Issue issue, IssueResolutionAcceptor acceptor) {
		FiredEventRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid implemented type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.INVALID_IMPLEMENTED_TYPE)
	public void fixInvalidImplementedType(final Issue issue, IssueResolutionAcceptor acceptor) {
		ImplementedTypeRemoveModification.accept(this, issue, acceptor, RemovalType.OTHER);
	}

	/** Quick fix for "Invalid extended type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.INVALID_EXTENDED_TYPE)
	public void fixInvalidExtendedType(final Issue issue, IssueResolutionAcceptor acceptor) {
		ExtendedTypeRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Cyclic hierarchy".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.CYCLIC_INHERITANCE)
	public void fixCyclicInheritance(final Issue issue, IssueResolutionAcceptor acceptor) {
		ExtendedTypeRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Interface expected".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INTERFACE_EXPECTED)
	public void fixInteraceExpected(final Issue issue, IssueResolutionAcceptor acceptor) {
		ExtendedTypeRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Class expected".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.CLASS_EXPECTED)
	public void fixClassExpected(final Issue issue, IssueResolutionAcceptor acceptor) {
		ExtendedTypeRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Discouraged capacity definition".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.DISCOURAGED_CAPACITY_DEFINITION)
	public void fixDiscouragedCapacityDefinition(Issue issue, IssueResolutionAcceptor acceptor) {
		MemberRemoveModification.accept(this, issue, acceptor, SarlCapacity.class);
		ActionAddModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Incompatible return type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE)
	public void fixIncompatibleReturnType(final Issue issue, IssueResolutionAcceptor acceptor) {
		ReturnTypeReplaceModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Return type is recommended".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED)
	public void fixReturnTypeRecommended(final Issue issue, IssueResolutionAcceptor acceptor) {
		ReturnTypeAddModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Unused agent capacity".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY)
	public void fixUnusedAgentCapacity(final Issue issue, IssueResolutionAcceptor acceptor) {
		CapacityReferenceRemoveModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Redundant capacity use".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE)
	public void fixRedundantAgentCapacityUse(final Issue issue, IssueResolutionAcceptor acceptor) {
		CapacityReferenceRemoveModification.accept(this, issue, acceptor);
	}

	@Override
	protected void doOverrideMethods(Issue issue,
			IssueResolutionAcceptor acceptor, String label,
			String[] operationUris) {
		// FIXME: Use the Xtend CodeBuilder API (see super function)
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
			keyword = getGrammarAccess().getAgentAccess().getAgentKeyword_3().getValue();
		} else if (container instanceof SarlBehavior) {
			declaration = (XtendTypeDeclaration) container;
			keyword = getGrammarAccess().getBehaviorAccess().getBehaviorKeyword_3().getValue();
		} else if (container instanceof SarlSkill) {
			declaration = (XtendTypeDeclaration) container;
			keyword = getGrammarAccess().getSkillAccess().getSkillKeyword_3().getValue();
		}
		if (declaration != null && keyword != null) {
			IXtextDocument document = context.getXtextDocument();
			addAbstractKeyword(declaration, document, keyword);
		} else {
			super.internalDoAddAbstractKeyword(container, context);
		}
	}

	private void addAbstractKeyword(XtendTypeDeclaration typeDeclaration, IXtextDocument document,
			String declarationKeyword) throws BadLocationException {
		ICompositeNode clazzNode = NodeModelUtils.findActualNodeFor(typeDeclaration);
		if (clazzNode == null) {
			throw new IllegalStateException("Cannot determine node for the type declaration" //$NON-NLS-1$
						+ typeDeclaration.getName());
		}
		int offset = -1;
		Iterator<ILeafNode> nodes = clazzNode.getLeafNodes().iterator();
		while (offset == -1 && nodes.hasNext()) {
			ILeafNode leafNode  = nodes.next();
			if (leafNode.getText().equals(declarationKeyword)) {
				offset = leafNode.getOffset();
			}
		}
		ReplacingAppendable appendable = this.appendableFactory.create(document,
				(XtextResource) typeDeclaration.eResource(),
				offset, 0);
		appendable.append(getGrammarAccess()
				.getCommonModifierAccess().getAbstractKeyword_4().getValue())
				.append(" "); //$NON-NLS-1$
		appendable.commitChanges();
	}

}

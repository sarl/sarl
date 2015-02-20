/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.Feature;
import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.services.SARLGrammarAccess;
import io.sarl.lang.ui.quickfix.semantic.AttributeRemovalMemberNameModification;
import io.sarl.lang.ui.quickfix.semantic.CapacityRemovalDiscouragedCapacityDefinitionModification;
import io.sarl.lang.ui.quickfix.semantic.DefaultActionDiscouragedCapacityDefinitionModification;
import io.sarl.lang.ui.quickfix.semantic.DiscouragedBooleanExpressionModification;
import io.sarl.lang.ui.quickfix.semantic.DuplicateActionModification;
import io.sarl.lang.ui.quickfix.semantic.DuplicateAttributeModification;
import io.sarl.lang.ui.quickfix.semantic.DuplicateTopElementModification;
import io.sarl.lang.ui.quickfix.semantic.ExecutableFeatureRemovalMemberNameModification;
import io.sarl.lang.ui.quickfix.semantic.IncompatibleReturnTypeModification;
import io.sarl.lang.ui.quickfix.semantic.IncompatibleTypeHierarchyModification;
import io.sarl.lang.ui.quickfix.semantic.InfixRedundantInterfaceModification;
import io.sarl.lang.ui.quickfix.semantic.InvalidCapacityTypeModification;
import io.sarl.lang.ui.quickfix.semantic.InvalidExtendedTypeModification;
import io.sarl.lang.ui.quickfix.semantic.InvalidFiringEventTypeModification;
import io.sarl.lang.ui.quickfix.semantic.InvalidImplementedTypeModification;
import io.sarl.lang.ui.quickfix.semantic.MissingMethodImplementationModification;
import io.sarl.lang.ui.quickfix.semantic.OverriddenFinalOperationModification;
import io.sarl.lang.ui.quickfix.semantic.OverriddenFinalTypeModification;
import io.sarl.lang.ui.quickfix.semantic.PostRedundantInterfaceModification;
import io.sarl.lang.ui.quickfix.semantic.PreRedundantInterfaceModification;
import io.sarl.lang.ui.quickfix.semantic.RedundantCapacityUseModification;
import io.sarl.lang.ui.quickfix.semantic.RemovalVariableNameShadowingModification;
import io.sarl.lang.ui.quickfix.semantic.RenamingMemberNameModification;
import io.sarl.lang.ui.quickfix.semantic.RenamingVariableNameShadowingModification;
import io.sarl.lang.ui.quickfix.semantic.UnreachableBehaviorUnitModification;
import io.sarl.lang.ui.quickfix.semantic.UnusedAgentCapacityModification;
import io.sarl.lang.ui.quickfix.semantic.ValueRemovalInvalidUseOfVarArgsModification;
import io.sarl.lang.ui.quickfix.semantic.VarArgRemovalInvalidUseOfVarArgsModification;
import io.sarl.lang.ui.quickfix.semantic.WrongPackageModification;
import io.sarl.lang.validation.IssueCodes;

import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.Fix;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.util.Arrays;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;
import org.eclipse.xtext.xbase.ui.quickfix.XbaseQuickfixProvider;

import com.google.inject.Inject;

/**
 * Custom quickfixes.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#quickfixes"
 */
public class SARLQuickfixProvider extends XbaseQuickfixProvider {

	@Inject
	private ReplacingAppendable.Factory appendableFactory;

	@Inject
	private SARLGrammarAccess grammarAccess;

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private TypesFactory typeFactory;

	/** Replies the factory for appendable.
	 *
	 * @return the appendable factory.
	 */
	public ReplacingAppendable.Factory getAppendableFactory() {
		return this.appendableFactory;
	}

	/** Replies the SARL grammar accessor.
	 *
	 * @return the SARL grammar accessor.
	 */
	public SARLGrammarAccess getGrammarAccess() {
		return this.grammarAccess;
	}

	/** Replies the finder of type references.
	 *
	 * @return the type reference finder.
	 */
	public TypeReferences getTypeReferences() {
		return this.typeReferences;
	}

	/** Replies type factory.
	 *
	 * @return the type factory.
	 */
	public TypesFactory getTypeFactory() {
		return this.typeFactory;
	}

	/** Create the reference to the type with the given name.
	 *
	 * @param typeName - the name of the type.
	 * @param context - the context of the reference.
	 * @return the type reference.
	 */
	public JvmParameterizedTypeReference newTypeRef(String typeName, EObject context) {
		JvmType type = this.typeReferences.findDeclaredType(typeName, context);
		if (type == null) {
			type = this.typeReferences.findDeclaredType("java.lang." + typeName, context); //$NON-NLS-1$
			if (type == null) {
				type = this.typeReferences.findDeclaredType("java.lang.Object", context); //$NON-NLS-1$
			}
		}
		if (type == null) {
			return null;
		}
		JvmParameterizedTypeReference reference = this.typeFactory.createJvmParameterizedTypeReference();
		reference.setType(type);
		return reference;
	}

	/** Remove the element related to the given issue, and the spaces before the element.
	 *
	 * @param issue - the issue.
	 * @param document - the document.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	@SuppressWarnings("static-method")
	public void removeElementWithPreviousSpaces(Issue issue, IXtextDocument document) throws BadLocationException {
		// Skip spaces before the identifier until the coma
		int index = issue.getOffset() - 1;
		char c = document.getChar(index);
		while (Character.isWhitespace(c)) {
			index--;
			c = document.getChar(index);
		}
		int delta = issue.getOffset() - index - 1;
		document.replace(index + 1, issue.getLength() + delta, ""); //$NON-NLS-1$
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
	@SuppressWarnings("static-method")
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

	/** Remove the element related to the issue, and the whitespaces after the element until the given separator.
	 *
	 * @param issue - the issue.
	 * @param document - the document.
	 * @param separator - the separator to consider.
	 * @return <code>true</code> if the separator was found, <code>false</code> if not.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	@SuppressWarnings("static-method")
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
	@SuppressWarnings("static-method")
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
	@SuppressWarnings("static-method")
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
	@SuppressWarnings("static-method")
	public int getInsertOffset(FeatureContainer container) {
		if (container.getFeatures().isEmpty()) {
			ICompositeNode node = NodeModelUtils.findActualNodeFor(container);
			ILeafNode openingBraceNode = IterableExtensions.findFirst(node.getLeafNodes(),
					new Functions.Function1<ILeafNode, Boolean>() {
				@Override
				public Boolean apply(ILeafNode p) {
					return "{".equals(p.getText()); //$NON-NLS-1$
				}
			});
			if (openingBraceNode != null) {
				return openingBraceNode.getOffset() + 1;
			}
			return node.getEndOffset();
		}
		EObject lastFeature = IterableExtensions.last(container.getFeatures());
		ICompositeNode node = NodeModelUtils.findActualNodeFor(lastFeature);
		return node.getEndOffset();
	}

	/** Replies the index where import declaration could be inserted into the given container.
	 *
	 * @param script - the script to consider for the insertion
	 * @return the insertion index.
	 */
	@SuppressWarnings("static-method")
	public int getImportInsertOffset(SarlScript script) {
		ICompositeNode node = NodeModelUtils.findActualNodeFor(script.getImportSection());
		if (node == null) {
			List<INode> children = NodeModelUtils.findNodesForFeature(script, SarlPackage.eINSTANCE.getSarlScript_Name());
			if (children.isEmpty()) {
				return 0;
			}
			return children.get(0).getEndOffset();
		}
		return node.getEndOffset();
	}

	/** Replies the size of a sequence of whitespaces.
	 *
	 * @param document - the document.
	 * @param offset - the offset of the first character of the sequence.
	 * @return the number of whitespaces at the given offset.
	 * @throws BadLocationException if there is a problem with the location of the element.
	 */
	@SuppressWarnings("static-method")
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
	@SuppressWarnings("static-method")
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
		Action action = EcoreUtil2.getContainerOfType(element, Action.class);
		if (action == null) {
			Feature feature = EcoreUtil2.getContainerOfType(element, Feature.class);
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

	/** Quick fix for "Wrong package".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.WRONG_PACKAGE)
	public void fixPackageName(Issue issue, IssueResolutionAcceptor acceptor) {
		WrongPackageModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Duplicate type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_TYPE_NAME)
	public void fixDuplicateTopElements(Issue issue, IssueResolutionAcceptor acceptor) {
		DuplicateTopElementModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Duplicate field".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_FIELD)
	public void fixDuplicateAttribute(Issue issue, IssueResolutionAcceptor acceptor) {
		DuplicateAttributeModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Duplicate method".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_METHOD)
	public void fixDuplicateMethod(Issue issue, IssueResolutionAcceptor acceptor) {
		DuplicateActionModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid member name".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_MEMBER_NAME)
	public void fixMemberName(final Issue issue, IssueResolutionAcceptor acceptor) {
		RenamingMemberNameModification.accept(this, issue, acceptor);
		AttributeRemovalMemberNameModification.accept(this, issue, acceptor);
		ExecutableFeatureRemovalMemberNameModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Redundant interface implementation".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION)
	public void fixRedundantInterface(final Issue issue, IssueResolutionAcceptor acceptor) {
		PreRedundantInterfaceModification.accept(this, issue, acceptor);
		PostRedundantInterfaceModification.accept(this, issue, acceptor);
		InfixRedundantInterfaceModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Variable name shadowing".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING)
	public void fixVariableNameShadowing(final Issue issue, IssueResolutionAcceptor acceptor) {
		RemovalVariableNameShadowingModification.accept(this, issue, acceptor);
		RenamingVariableNameShadowingModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Override final operation".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.OVERRIDDEN_FINAL_OPERATION)
	public void fixOverriddenFinalOperation(Issue issue, IssueResolutionAcceptor acceptor) {
		OverriddenFinalOperationModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Discouraged boolean expression".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION)
	public void fixDiscouragedBooleanExpression(final Issue issue, IssueResolutionAcceptor acceptor) {
		DiscouragedBooleanExpressionModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Unreachable behavior unit".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.UNREACHABLE_BEHAVIOR_UNIT)
	public void fixUnreachableBehaviorUnit(Issue issue, IssueResolutionAcceptor acceptor) {
		UnreachableBehaviorUnitModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid capacity type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_CAPACITY_TYPE)
	public void fixInvalidCapacityType(final Issue issue, IssueResolutionAcceptor acceptor) {
		InvalidCapacityTypeModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid firing event type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_FIRING_EVENT_TYPE)
	public void fixInvalidFiringEventType(final Issue issue, IssueResolutionAcceptor acceptor) {
		InvalidFiringEventTypeModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid implemented type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_IMPLEMENTED_TYPE)
	public void fixInvalidImplementedType(final Issue issue, IssueResolutionAcceptor acceptor) {
		InvalidImplementedTypeModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid extended type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_EXTENDED_TYPE)
	public void fixInvalidExtendedType(final Issue issue, IssueResolutionAcceptor acceptor) {
		InvalidExtendedTypeModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Inconsistent hierarchy".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INCONSISTENT_TYPE_HIERARCHY)
	public void fixInconsistentTypeHierarchy(final Issue issue, IssueResolutionAcceptor acceptor) {
		IncompatibleTypeHierarchyModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Override final type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.OVERRIDDEN_FINAL_TYPE)
	public void fixOverriddenFinalType(final Issue issue, IssueResolutionAcceptor acceptor) {
		OverriddenFinalTypeModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Discouraged capacity definition".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DISCOURAGED_CAPACITY_DEFINITION)
	public void fixDiscouragedCapacityDefinition(Issue issue, IssueResolutionAcceptor acceptor) {
		CapacityRemovalDiscouragedCapacityDefinitionModification.accept(this, issue, acceptor);
		DefaultActionDiscouragedCapacityDefinitionModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Missing method implementation".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.MISSING_METHOD_IMPLEMENTATION)
	public void fixMissingMethodImplementation(final Issue issue, IssueResolutionAcceptor acceptor) {
		MissingMethodImplementationModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Incompatible return type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE)
	public void fixIncompatibleReturnType(final Issue issue, IssueResolutionAcceptor acceptor) {
		IncompatibleReturnTypeModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Invalid default value for variadic parameter".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_USE_OF_VAR_ARG)
	public void fixNoDefaultValueForVariadicParameter(final Issue issue, IssueResolutionAcceptor acceptor) {
		VarArgRemovalInvalidUseOfVarArgsModification.accept(this, issue, acceptor);
		ValueRemovalInvalidUseOfVarArgsModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Unused agent capacity".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.UNUSED_AGENT_CAPACITY)
	public void fixUnusedAgentCapacity(final Issue issue, IssueResolutionAcceptor acceptor) {
		UnusedAgentCapacityModification.accept(this, issue, acceptor);
	}

	/** Quick fix for "Redundant capacity use".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.REDUNDANT_CAPACITY_USE)
	public void fixRedundantAgentCapacityUse(final Issue issue, IssueResolutionAcceptor acceptor) {
		RedundantCapacityUseModification.accept(this, issue, acceptor);
	}

}

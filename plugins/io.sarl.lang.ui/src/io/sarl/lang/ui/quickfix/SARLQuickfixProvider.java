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
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.Feature;
import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.FormalParameter;
import io.sarl.lang.sarl.ParameterizedFeature;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.TopElement;
import io.sarl.lang.services.SARLGrammarAccess;
import io.sarl.lang.validation.IssueCodes;

import java.text.MessageFormat;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModification;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.model.edit.ISemanticModification;
import org.eclipse.xtext.ui.editor.quickfix.Fix;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.util.Arrays;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
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

	/**
	 * Remove element with the spaces before.
	 *
	 * @param issue - the description of the element.
	 * @param document - the document to update.
	 * @throws BadLocationException if the location is bad.
	 */
	protected static void removeElementWithPreviousSpaces(Issue issue, IXtextDocument document) throws BadLocationException {
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

	/**
	 * Remove element and all the whitespaces before the element until the given separator.
	 * The given separator is also removed.
	 *
	 * @param issue - the description of the element.
	 * @param document - the document to update.
	 * @param separator - the separator to consider.
	 * @return <code>true</code> if the document was changed by this function;
	 * <code>false</code> if the document remains unchanged.
	 * @throws BadLocationException if the location is bad.
	 */
	protected static boolean removeToPreviousSeparator(Issue issue, IXtextDocument document, String separator)
			throws BadLocationException {
		return removeToPreviousSeparator(issue.getOffset(), issue.getLength(), document, separator);
	}

	/**
	 * Remove element and all the whitespaces before the element until the given separator.
	 * The given separator is also removed.
	 *
	 * @param offset - position of the element.
	 * @param length - length of the element.
	 * @param document - the document to update.
	 * @param separator - the separator to consider.
	 * @return <code>true</code> if the document was changed by this function;
	 * <code>false</code> if the document remains unchanged.
	 * @throws BadLocationException if the location is bad.
	 */
	protected static boolean removeToPreviousSeparator(int offset, int length, IXtextDocument document, String separator)
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

	/**
	 * Remove element and all the whitespaces after the element until the given separator.
	 * The given separator is also removed.
	 *
	 * @param issue - the description of the element.
	 * @param document - the document to update.
	 * @param separator - the separator to consider.
	 * @return <code>true</code> if the document was changed by this function;
	 * <code>false</code> if the document remains unchanged.
	 * @throws BadLocationException if the location is bad.
	 */
	protected static boolean removeToNextSeparator(Issue issue, IXtextDocument document, String separator)
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

	/**
	 * Remove element and all the whitespaces before the element until the given keyword.
	 * The given keyword is also removed.
	 *
	 * @param issue - the description of the element.
	 * @param document - the document to update.
	 * @param keyword1 - the first keyword to consider.
	 * @param otherKeywords - the other keywords to consider.
	 * @return <code>true</code> if the document was changed by this function;
	 * <code>false</code> if the document remains unchanged.
	 * @throws BadLocationException if the location is bad.
	 */
	protected static boolean removeToPreviousKeyword(Issue issue, IXtextDocument document,
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

	/**
	 * Remove element and all the whitespaces between the given separators and the element.
	 * The given separators are also removed.
	 *
	 * @param issue - the description of the element.
	 * @param document - the document to update.
	 * @param beginSeparator - the left separator to consider.
	 * @param endSeparator - the right separator to consider.
	 * @return <code>true</code> if the document was changed by this function;
	 * <code>false</code> if the document remains unchanged.
	 * @throws BadLocationException if the location is bad.
	 */
	protected static boolean removeBetweenSeparators(Issue issue, IXtextDocument document,
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

	/** Replies the position where to insert child elements into the given container.
	 *
	 * @param container - the container in which the insertion position must be determined.
	 * @return the insertion position.
	 */
	protected static int getInsertOffset(FeatureContainer container) {
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

	/** Replies the size of a sequence of whitespaces.
	 *
	 * @param document - the document to read.
	 * @param offset - the position of the first character in the sequence.
	 * @return the number of whitespaces starting at the given offset.
	 * @throws BadLocationException if the location is bad.
	 */
	protected static int getSpaceSize(IXtextDocument document, int offset) throws BadLocationException {
		int size = 0;
		char c = document.getChar(offset + size);
		while (Character.isWhitespace(c)) {
			size++;
			c = document.getChar(offset + size);
		}
		return size;
	}

	/** Replies the qualified name for the given string representation.
	 *
	 * @param name - the name to convert.
	 * @return the qualified name.
	 */
	protected static QualifiedName qualifiedName(String name) {
		if (!com.google.common.base.Strings.isNullOrEmpty(name)) {
			List<String> segments = Strings.split(name, "."); //$NON-NLS-1$
			return QualifiedName.create(segments);
		}
		return QualifiedName.create();
	}

	/** Quick fix for "Wrong package".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.WRONG_PACKAGE)
	@SuppressWarnings("static-method")
	public void fixPackageName(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 1) {
			final String expectedPackage = issue.getData()[0];
			String msg = MessageFormat.format(Messages.SARLQuickfixProvider_0, expectedPackage);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					String n;
					if (com.google.common.base.Strings.isNullOrEmpty(expectedPackage)) {
						n = null;
					} else {
						n = expectedPackage;
					}
					((SarlScript) element).setName(n);
				}
			});
		}
	}

	/** Quick fix for "Duplicate type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_TYPE_NAME)
	public void fixDuplicateTopElements(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 1) {
			QualifiedName duplicateName = qualifiedName(issue.getData()[0]);
			String msg = MessageFormat.format(Messages.SARLQuickfixProvider_1, duplicateName.getLastSegment());
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					remove(element, TopElement.class, context);
				}
			});
		}
	}

	/** Quick fix for "Duplicate field".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_FIELD)
	public void fixDuplicateAttribute(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 1) {
			QualifiedName duplicateName = qualifiedName(issue.getData()[0]);
			String msg = MessageFormat.format(Messages.SARLQuickfixProvider_2, duplicateName.getLastSegment());
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					remove(element, Attribute.class, context);
				}
			});
		}
	}

	/** Quick fix for "Duplicate method".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DUPLICATE_METHOD)
	public void fixDuplicateMethod(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 1) {
			String duplicateName = issue.getData()[0];
			String msg = MessageFormat.format(Messages.SARLQuickfixProvider_3, duplicateName);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					removeExecutableFeature(element, context);
				}
			});
		}
	}

	/** Remove the executable feature.
	 *
	 * @param element - the element
	 * @param context - the context.
	 * @throws BadLocationException if the location is bad.
	 */
	protected void removeExecutableFeature(EObject element, IModificationContext context) throws BadLocationException {
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

	/** Quick fix for "Invalid member name".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_MEMBER_NAME)
	public void fixMemberName(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length >= 3) {
			String type = issue.getData()[0];
			String invalidName = issue.getData()[1];
			for (int i = 2; i < issue.getData().length; i++) {
				final String validName = issue.getData()[i];
				String msg = MessageFormat.format(Messages.SARLQuickfixProvider_4,
						type, invalidName, validName);
				acceptor.accept(issue, msg, msg, null, new IModification() {
					@Override
					public void apply(IModificationContext context) throws Exception {
						context.getXtextDocument().replace(issue.getOffset(), issue.getLength(), validName);
					}
				});
			}
			String msg = MessageFormat.format(Messages.SARLQuickfixProvider_5, type, invalidName);
			if ("attribute".equals(type)) { //$NON-NLS-1$
				acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void apply(EObject element, IModificationContext context) throws Exception {
						remove(element, Attribute.class, context);
					}
				});
			} else {
				acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
					@Override
					public void apply(EObject element, IModificationContext context) throws Exception {
						removeExecutableFeature(element, context);
					}
				});
			}
		}
	}

	/** Quick fix for "Redundant interface implementation".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION)
	public void fixRedundantInterface(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 2) {
			String redundantName = issue.getData()[0];
			String mode = issue.getData()[1];
			String msg = MessageFormat.format(Messages.SARLQuickfixProvider_6, redundantName);
			ISemanticModification fct;
			switch (mode) {
			case "pre": //$NON-NLS-1$
				fct = new ISemanticModification() {
					@Override
					public void apply(EObject element, IModificationContext context) throws Exception {
						IXtextDocument document = context.getXtextDocument();
						removeToPreviousSeparator(issue, document, ","); //$NON-NLS-1$
					}
				};
				break;
			case "post": //$NON-NLS-1$
				fct = new ISemanticModification() {
					@Override
					public void apply(EObject element, IModificationContext context) throws Exception {
						IXtextDocument document = context.getXtextDocument();
						removeToNextSeparator(issue, document, ","); //$NON-NLS-1$
					}
				};
				break;
			default:
				fct = new ISemanticModification() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void apply(EObject element, IModificationContext context) throws Exception {
						IXtextDocument document = context.getXtextDocument();
						String sep = ","; //$NON-NLS-1$
						if (!removeToPreviousSeparator(issue, document, sep)) {
							if (!removeToNextSeparator(issue, document, sep)) {
								removeToPreviousKeyword(issue, document,
										SARLQuickfixProvider.this.grammarAccess.getSkillAccess()
											.getImplementsKeyword_3_1_0().getValue());
							}
						}
					}
				};
				break;
			}
			acceptor.accept(issue, msg, msg, null, fct);
		}
	}

	/** Quick fix for "Variable name shadowing".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING)
	public void fixVariableNameShadowing(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 2) {
			String redundantName = issue.getData()[0];
			final String newName = issue.getData()[1];
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_5,
					Messages.SARLQuickfixProvider_7, redundantName);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					remove(element, Attribute.class, context);
				}
			});
			msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_4,
					Messages.SARLQuickfixProvider_7, redundantName, newName);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					IXtextDocument document = context.getXtextDocument();
					document.replace(issue.getOffset(), issue.getLength(), newName);
				}
			});
		}
	}

	/** Quick fix for "Override final operation".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.OVERRIDDEN_FINAL_OPERATION)
	public void fixOverriddenFinal(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 1) {
			String signature = issue.getData()[0];
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_5,
					Messages.SARLQuickfixProvider_8, signature);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					removeExecutableFeature(element, context);
				}
			});
		}
	}

	/** Quick fix for "Discouraged boolean expression".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@SuppressWarnings("static-method")
	@Fix(IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION)
	public void fixDiscouragedBooleanExpression(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 0) {
			String msg = Messages.SARLQuickfixProvider_9;
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					IXtextDocument document = context.getXtextDocument();
					removeBetweenSeparators(issue, document, "[", "]"); //$NON-NLS-1$//$NON-NLS-2$
				}
			});
		}
	}

	/** Quick fix for "Unreachable behavior unit".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.UNREACHABLE_BEHAVIOR_UNIT)
	public void fixUnreachableBehaviorUnit(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 1) {
			String eventName = issue.getData()[0];
			String msg = MessageFormat.format(Messages.SARLQuickfixProvider_10, eventName);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					remove(element, BehaviorUnit.class, context);
				}
			});
		}
	}

	/** Quick fix for "Invalid capacity type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_CAPACITY_TYPE)
	public void fixInvalidCapacityType(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 1) {
			String typeName = issue.getData()[0];
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_5,
					Messages.SARLQuickfixProvider_11, typeName);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					IXtextDocument document = context.getXtextDocument();
					String sep = ","; //$NON-NLS-1$
					if (!removeToPreviousSeparator(issue, document, sep)) {
						if (!removeToNextSeparator(issue, document, sep)) {
							removeToPreviousKeyword(issue, document,
									SARLQuickfixProvider.this.grammarAccess.getRequiredCapacityAccess()
										.getRequiresKeyword_1().getValue(),
									SARLQuickfixProvider.this.grammarAccess.getCapacityUsesAccess()
										.getUsesKeyword_1().getValue());
						}
					}
				}
			});
		}
	}

	/** Quick fix for "Invalid firing event type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_FIRING_EVENT_TYPE)
	public void fixInvalidFiringEventType(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 1) {
			String typeName = issue.getData()[0];
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_5,
					Messages.SARLQuickfixProvider_11, typeName);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					IXtextDocument document = context.getXtextDocument();
					String sep = ","; //$NON-NLS-1$
					if (!removeToPreviousSeparator(issue, document, sep)) {
						if (!removeToNextSeparator(issue, document, sep)) {
							removeToPreviousKeyword(issue, document,
									SARLQuickfixProvider.this.grammarAccess.getActionSignatureAccess()
										.getFiresKeyword_5_0().getValue());
						}
					}
				}
			});
		}
	}

	/** Quick fix for "Invalid implemented type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_IMPLEMENTED_TYPE)
	public void fixInvalidImplementedType(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 1) {
			String typeName = issue.getData()[0];
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_5,
					Messages.SARLQuickfixProvider_11, typeName);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					IXtextDocument document = context.getXtextDocument();
					String sep = ","; //$NON-NLS-1$
					if (!removeToPreviousSeparator(issue, document, sep)) {
						if (!removeToNextSeparator(issue, document, sep)) {
							removeToPreviousKeyword(issue, document,
									SARLQuickfixProvider.this.grammarAccess.getSkillAccess()
										.getImplementsKeyword_3_1_0().getValue());
						}
					}
				}
			});
		}
	}

	/** Quick fix for "Invalid extended type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INVALID_EXTENDED_TYPE)
	public void fixInvalidExtendedType(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 2) {
			QualifiedName superTypeName = qualifiedName(issue.getData()[1]);
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_5,
					Messages.SARLQuickfixProvider_11, superTypeName.getLastSegment());
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					IXtextDocument document = context.getXtextDocument();
					String sep = ","; //$NON-NLS-1$
					if (!removeToPreviousSeparator(issue, document, sep)) {
						if (!removeToNextSeparator(issue, document, sep)) {
							removeToPreviousKeyword(issue, document,
									SARLQuickfixProvider.this.grammarAccess.getAgentAccess().getExtendsKeyword_3_0().getValue());
						}
					}
				}
			});
		}
	}

	/** Quick fix for "Inconsistent hierarchy".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.INCONSISTENT_TYPE_HIERARCHY)
	public void fixInconsistentTypeHierarchy(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 2) {
			QualifiedName superTypeName = qualifiedName(issue.getData()[1]);
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_5,
					Messages.SARLQuickfixProvider_11, superTypeName.getLastSegment());
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					IXtextDocument document = context.getXtextDocument();
					String sep = ","; //$NON-NLS-1$
					if (!removeToPreviousSeparator(issue, document, sep)) {
						if (!removeToNextSeparator(issue, document, sep)) {
							removeToPreviousKeyword(issue, document,
									SARLQuickfixProvider.this.grammarAccess.getSkillAccess()
										.getImplementsKeyword_3_1_0().getValue(),
									SARLQuickfixProvider.this.grammarAccess.getAgentAccess()
										.getExtendsKeyword_3_0().getValue());
						}
					}
				}
			});
		}
	}

	/** Quick fix for "Override final type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.OVERRIDDEN_FINAL_TYPE)
	public void fixOverriddenFinalType(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 1) {
			String typeName = issue.getData()[0];
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_5,
					Messages.SARLQuickfixProvider_11, typeName);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					IXtextDocument document = context.getXtextDocument();
					String sep = ","; //$NON-NLS-1$
					if (!removeToPreviousSeparator(issue, document, sep)) {
						if (!removeToNextSeparator(issue, document, sep)) {
							removeToPreviousKeyword(issue, document,
									SARLQuickfixProvider.this.grammarAccess.getSkillAccess()
										.getImplementsKeyword_3_1_0().getValue(),
									SARLQuickfixProvider.this.grammarAccess.getAgentAccess()
										.getExtendsKeyword_3_0().getValue());
						}
					}
				}
			});
		}
	}

	/** Quick fix for "Discouraged capacity definition".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.DISCOURAGED_CAPACITY_DEFINITION)
	public void fixDiscouragedCapacityDefinition(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 2) {
			String capacityName = issue.getData()[0];
			final String defaultActionName = issue.getData()[1];
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_5,
					Messages.SARLQuickfixProvider_12, capacityName);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					remove(element, Capacity.class, context);
				}
			});
			msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_13,
					Messages.SARLQuickfixProvider_8, defaultActionName);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					FeatureContainer container = EcoreUtil2.getContainerOfType(element, FeatureContainer.class);
					if (container != null) {
						int insertOffset = getInsertOffset(container);
						IXtextDocument document = context.getXtextDocument();
						int length = getSpaceSize(document, insertOffset);
						ReplacingAppendable appendable = SARLQuickfixProvider.this.appendableFactory.create(document,
								(XtextResource) element.eResource(), insertOffset, length);
						boolean changeIndentation = container.getFeatures().isEmpty();
						if (changeIndentation) {
							appendable.increaseIndentation();
						}
						appendable.newLine().append("def ").append(defaultActionName); //$NON-NLS-1$
						if (changeIndentation) {
							appendable.decreaseIndentation();
						}
						appendable.newLine();
						appendable.commitChanges();
					}
				}
			});
		}
	}

	/** Quick fix for "Missing method implementation".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@SuppressWarnings("unchecked")
	@Fix(IssueCodes.MISSING_METHOD_IMPLEMENTATION)
	public void fixMissingMethodImplementation(Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length >= 1) {
			StringBuffer lines = new StringBuffer();
			final Map<String, String> methods = CollectionLiterals.newTreeMap(null);
			for (int i = 0; i < issue.getData().length; i += 2) {
				String meth = issue.getData()[i];
				lines.append(MessageFormat.format(Messages.SARLQuickfixProvider_14, meth));
				methods.put(meth, issue.getData()[i + 1]);
			}
			String description = MessageFormat.format(Messages.SARLQuickfixProvider_15, lines);
			acceptor.accept(issue, Messages.SARLQuickfixProvider_16, description, null, new ISemanticModification() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					FeatureContainer container = EcoreUtil2.getContainerOfType(element, FeatureContainer.class);
					if (container != null) {
						int insertOffset = getInsertOffset(container);
						IXtextDocument document = context.getXtextDocument();
						int length = getSpaceSize(document, insertOffset);
						ReplacingAppendable appendable = SARLQuickfixProvider.this.appendableFactory.create(document,
								(XtextResource) element.eResource(), insertOffset, length);
						boolean initialIndent = (container.getFeatures().isEmpty());
						if (initialIndent) {
							appendable.increaseIndentation();
						}
						for (Entry<String, String> meth : methods.entrySet()) {
							appendable.newLine().append(meth.getKey()).append(" {"); //$NON-NLS-1$
							appendable.increaseIndentation();
							appendable.newLine().append("// TODO ").append(//$NON-NLS-1$
									io.sarl.lang.genmodel.Messages.SARLCodeGenerator_0);
							String value = meth.getValue();
							if (!com.google.common.base.Strings.isNullOrEmpty(value)) {
								appendable.newLine().append(value);
							}
							appendable.decreaseIndentation();
							appendable.newLine().append("}"); //$NON-NLS-1$
						}
						appendable.decreaseIndentation();
						appendable.newLine();
						appendable.commitChanges();
					}
				}
			});
		}
	}

	/** Quick fix for "Incompatible return type".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@SuppressWarnings("static-method")
	@Fix(org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE)
	public void fixIncompatibleReturnType(final Issue issue, IssueResolutionAcceptor acceptor) {
		if (issue.getData() != null && issue.getData().length == 2) {
			final String expectedType = issue.getData()[1];
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_17,
					Messages.SARLQuickfixProvider_11,
					expectedType);
			acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
				@Override
				public void apply(EObject element, IModificationContext context) throws Exception {
					IXtextDocument document = context.getXtextDocument();
					document.replace(issue.getOffset(), issue.getLength(), expectedType);
				}
			});
		}
	}

	/** Quick fix for "Invalid default value for variadic parameter".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@SuppressWarnings("static-method")
	@Fix(IssueCodes.INVALID_USE_OF_VAR_ARG)
	public void fixNoDefaultValueForVariadicParameter(final Issue issue, IssueResolutionAcceptor acceptor) {
		String msg = Messages.SARLQuickfixProvider_18;
		acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
			@Override
			public void apply(EObject element, IModificationContext context) throws Exception {
				IXtextDocument document = context.getXtextDocument();
				removeElementWithPreviousSpaces(issue, document);
			}
		});
		msg = Messages.SARLQuickfixProvider_19;
		acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
			@Override
			public void apply(EObject element, IModificationContext context) throws Exception {
				ParameterizedFeature container = EcoreUtil2.getContainerOfType(element, ParameterizedFeature.class);
				if (container != null && !container.getParams().isEmpty()) {
					FormalParameter formalParameter = IterableExtensions.last(container.getParams());
					if (formalParameter.getDefaultValue() != null) {
						ICompositeNode node = NodeModelUtils.findActualNodeFor(formalParameter.getDefaultValue());
						if (node != null) {
							IXtextDocument document = context.getXtextDocument();
							removeToPreviousSeparator(node.getOffset(), node.getLength(), document, "="); //$NON-NLS-1$
						}
					}
				}
			}
		});
	}

	/** Quick fix for "Unused agent capacity".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.UNUSED_AGENT_CAPACITY)
	public void fixUnusedAgentCapacity(final Issue issue, IssueResolutionAcceptor acceptor) {
		String typeName = issue.getData()[0];
		String msg = MessageFormat.format(
				Messages.SARLQuickfixProvider_5,
				Messages.SARLQuickfixProvider_12, typeName);
		acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void apply(EObject element, IModificationContext context) throws Exception {
				IXtextDocument document = context.getXtextDocument();
				String sep = ","; //$NON-NLS-1$
				if (!removeToPreviousSeparator(issue, document, sep)) {
					if (!removeToNextSeparator(issue, document, sep)) {
						removeToPreviousKeyword(issue, document,
								SARLQuickfixProvider.this.grammarAccess.getCapacityUsesAccess().getUsesKeyword_1().getValue(),
								SARLQuickfixProvider.this.grammarAccess.getRequiredCapacityAccess()
									.getRequiresKeyword_1().getValue());
					}
				}
			}
		});
	}

	/** Quick fix for "Redundant capacity use".
	 *
	 * @param issue - the issue.
	 * @param acceptor - the quick fix acceptor.
	 */
	@Fix(IssueCodes.REDUNDANT_CAPACITY_USE)
	public void fixRedundantAgentCapacityUse(final Issue issue, IssueResolutionAcceptor acceptor) {
		String typeName = issue.getData()[0];
		String msg = MessageFormat.format(
				Messages.SARLQuickfixProvider_5,
				Messages.SARLQuickfixProvider_12, typeName);
		acceptor.accept(issue, msg, msg, null, new ISemanticModification() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void apply(EObject element, IModificationContext context) throws Exception {
				IXtextDocument document = context.getXtextDocument();
				String sep = ","; //$NON-NLS-1$
				if (!removeToPreviousSeparator(issue, document, sep)) {
					if (!removeToNextSeparator(issue, document, sep)) {
						removeToPreviousKeyword(issue, document,
								SARLQuickfixProvider.this.grammarAccess.getCapacityUsesAccess().getUsesKeyword_1().getValue(),
								SARLQuickfixProvider.this.grammarAccess.getRequiredCapacityAccess()
									.getRequiresKeyword_1().getValue());
					}
				}
			}
		});
	}

}

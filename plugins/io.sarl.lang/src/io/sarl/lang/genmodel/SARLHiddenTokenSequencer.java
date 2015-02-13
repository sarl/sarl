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
package io.sarl.lang.genmodel;

import io.sarl.lang.services.SARLGrammarAccess;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.AbstractElement;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Action;
import org.eclipse.xtext.Group;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.TerminalRule;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.serializer.sequencer.HiddenTokenSequencer;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.compiler.DocumentationAdapter;

import com.google.common.base.Strings;
import com.google.inject.Inject;
import com.google.inject.Singleton;

/** Sequencer for hidden tokens related to the SARL language.
 *
 * This implementation enables the output of the comments associated to the SARL Ecore elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class SARLHiddenTokenSequencer extends HiddenTokenSequencer {

	private AbstractRule mlRule;
	private AbstractRule slRule;
	private String mlPrefix;
	private String slPrefix;

	private String bufferedComment;

	/**
	 */
	public SARLHiddenTokenSequencer() {
		//
	}

	/** Change the accessor to the SARL grammar.
	 *
	 * @param grammarAccess - the grammar accessor.
	 */
	@Inject
	public void setGrammarAccess(SARLGrammarAccess grammarAccess) {
		this.mlRule = grammarAccess.getML_COMMENTRule();
		this.slRule = grammarAccess.getSL_COMMENTRule();
		this.mlPrefix = findKeyword(grammarAccess.getML_COMMENTRule());
		this.slPrefix = findKeyword(grammarAccess.getSL_COMMENTRule());
	}

	/** Replies the prefix comment associated to the given Ecore element.
	 *
	 * @param object - the Ecore element.
	 * @return the comment or <code>null</code> if none.
	 */
	@SuppressWarnings("static-method")
	protected String getAssociatedPrefixComment(EObject object) {
		if (object != null) {
			for (Adapter adapter : object.eAdapters()) {
				if (adapter instanceof DocumentationAdapter) {
					String comment = ((DocumentationAdapter) adapter).getDocumentation();
					if (!Strings.isNullOrEmpty(comment)) {
						return comment;
					}
				}
			}
		}
		return null;
	}

	/** Replies the inner block comment associated to the given Ecore block.
	 *
	 * @param object - the Ecore block.
	 * @return the comment or <code>null</code> if none.
	 */
	@SuppressWarnings("static-method")
	protected String getAssociatedInnerBlockComment(XBlockExpression object) {
		for (Adapter adapter : object.eAdapters()) {
			if (adapter instanceof BlockInnerDocumentationAdapter) {
				String comment = ((BlockInnerDocumentationAdapter) adapter).getDocumentation();
				if (!Strings.isNullOrEmpty(comment)) {
					return comment;
				}
			}
		}
		return null;
	}

	/** Replies the postfix comment associated to the given Ecore element.
	 *
	 * @param object - the Ecore element.
	 * @return the comment or <code>null</code> if none.
	 */
	@SuppressWarnings("static-method")
	protected String getAssociatedPostfixComment(EObject object) {
		if (object != null) {
			for (Adapter adapter : object.eAdapters()) {
				if (adapter instanceof PostDocumentationAdapter) {
					String comment = ((PostDocumentationAdapter) adapter).getDocumentation();
					if (!Strings.isNullOrEmpty(comment)) {
						return comment;
					}
				}
			}
		}
		return null;
	}

	private static String findKeyword(TerminalRule rule) {
		for (AbstractElement element : ((Group) rule.getAlternatives()).getElements()) {
			if (element instanceof Keyword) {
				Keyword kw = (Keyword) element;
				return Strings.nullToEmpty(kw.getValue());
			}
		}
		return null;
	}

	/** Emit the comment for the given Ecore element.
	 *
	 * @param commentText - the comment.
	 */
	protected void emitComments(String commentText) {
		String comment = commentText;
		// Remove the spaces
		if (comment != null) {
			comment = comment.replaceFirst("^[ \n\r\t\f]+", ""); //$NON-NLS-1$//$NON-NLS-2$
			comment = comment.replaceFirst("[ \n\r\t\f]+$", ""); //$NON-NLS-1$//$NON-NLS-2$
		}
		if (!Strings.isNullOrEmpty(comment)) {
			assert (comment != null);
			AbstractRule rule;
			if (comment.startsWith(this.mlPrefix)) {
				rule = this.mlRule;
				comment = "\n" + comment; //$NON-NLS-1$
			} else if (comment.startsWith(this.slPrefix)) {
				rule = this.slRule;
			} else if (comment.contains("\n")) { //$NON-NLS-1$
				rule = this.mlRule;
				comment = "\n/* " + comment + " */"; //$NON-NLS-1$//$NON-NLS-2$
			} else {
				rule = this.slRule;
				comment = "// " + comment + "\n"; //$NON-NLS-1$//$NON-NLS-2$
			}
			this.delegate.acceptComment(rule, comment, null);
		}
	}

	@Override
	public boolean enterAssignedAction(Action action, EObject semanticChild,
			ICompositeNode node) {
		emitComments(getAssociatedPrefixComment(semanticChild));
		return super.enterAssignedAction(action, semanticChild, node);
	}

	@Override
	public boolean enterAssignedParserRuleCall(RuleCall rc,
			EObject semanticChild, ICompositeNode node) {
		if (!super.enterAssignedParserRuleCall(rc, semanticChild, node)) {
			return false;
		}
		emitComments(getAssociatedPrefixComment(semanticChild));
		if (semanticChild instanceof XBlockExpression) {
			String comment = getAssociatedInnerBlockComment((XBlockExpression) semanticChild);
			if (!Strings.isNullOrEmpty(comment)) {
				this.bufferedComment = comment;
			}
		}
		return true;
	}

	@Override
	public void leaveAssignedAction(Action action, EObject semanticChild) {
		emitComments(getAssociatedPostfixComment(semanticChild));
		super.leaveAssignedAction(action, semanticChild);
	}

	@Override
	public void leaveAssignedParserRuleCall(RuleCall rc, EObject semanticChild) {
		emitComments(getAssociatedPostfixComment(semanticChild));
		this.delegate.leaveAssignedParserRuleCall(rc, semanticChild);
	}

	@Override
	public void acceptUnassignedKeyword(Keyword keyword, String token, ILeafNode node) {
		super.acceptUnassignedKeyword(keyword, token, node);
		if (this.bufferedComment != null) {
			emitComments(this.bufferedComment);
			this.bufferedComment = null;
		}
	}

}

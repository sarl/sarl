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

package io.sarl.lang.bugfixes.unpublished;

import java.util.List;
import javax.inject.Singleton;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.formatting2.AbstractFormatter2;
import org.eclipse.xtext.formatting2.IFormattableDocument;
import org.eclipse.xtext.formatting2.ITextReplacer;
import org.eclipse.xtext.formatting2.ITextReplacerContext;
import org.eclipse.xtext.formatting2.regionaccess.IComment;
import org.eclipse.xtext.formatting2.regionaccess.IHiddenRegion;
import org.eclipse.xtext.formatting2.regionaccess.ISemanticRegion;
import org.eclipse.xtext.formatting2.regionaccess.ITextRegionAccess;
import org.eclipse.xtext.formatting2.regionaccess.ITextReplacement;
import org.eclipse.xtext.formatting2.regionaccess.ITextSegment;
import org.eclipse.xtext.util.Strings;

/** FIXME: Xtext upgrade, Fixing a bug in Xtext Formatter2 API that avoid to have a good
 * indentation for the first comment in a block.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class BugMultilineCommentIndentation {

	/** Fixing the bug.
	 *
	 * @param context the replacement context.
	 * @param comment the comment for which the fix must be applied.
	 * @return the new context.
	 */
	@SuppressWarnings("static-method")
	public ITextReplacerContext fix(final ITextReplacerContext context, IComment comment) {
		final IHiddenRegion hiddenRegion = comment.getHiddenRegion();
		if (detectBugSituation(hiddenRegion) && fixBug(hiddenRegion)) {
			// Indentation of the first comment line
			final ITextRegionAccess access = comment.getTextRegionAccess();
			final ITextSegment target = access.regionForOffset(comment.getOffset(), 0);
			context.addReplacement(target.replaceWith(context.getIndentationString(1)));
			// Indentation of the comment's lines
			return new FixedReplacementContext(context);
		}
		return context;
	}

	private static boolean detectBugSituation(IHiddenRegion hiddenRegion) {
		if (hiddenRegion != null) {
			final ISemanticRegion semanticRegion = hiddenRegion.getPreviousSemanticRegion();
			if (semanticRegion != null) {
				final EObject element = semanticRegion.getGrammarElement();
				if (element instanceof Keyword
						&& Strings.equal(((Keyword) element).getValue(), "{")) { //$NON-NLS-1$
					return true;
				}
			}
		}
		return false;
	}

	private static boolean fixBug(IHiddenRegion hiddenRegion) {
		boolean needBugFix = true;
		final ISemanticRegion semanticRegion = hiddenRegion.getNextSemanticRegion();
		if (semanticRegion != null) {
			final EObject element = semanticRegion.getGrammarElement();
			if (element instanceof Keyword
					&& Strings.equal(((Keyword) element).getValue(), "}")) { //$NON-NLS-1$
				needBugFix = false;
			}
		}
		return needBugFix;
	}

	/** Comment fixer.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class FixedReplacementContext implements ITextReplacerContext {

		private final ITextReplacerContext context;

		/** Constructor.
		 *
		 * @param context the replacement context to fix.
		 */
		FixedReplacementContext(ITextReplacerContext context) {
			this.context = context;
		}

		@Override
		public ITextReplacerContext withReplacer(ITextReplacer replacer) {
			return this.context.withReplacer(replacer);
		}

		@Override
		public ITextReplacerContext withIndentation(int indentation) {
			return this.context.withIndentation(indentation);
		}

		@Override
		public ITextReplacerContext withDocument(IFormattableDocument document) {
			return this.context.withDocument(document);
		}

		@Override
		public void setNextReplacerIsChild() {
			this.context.setNextReplacerIsChild();
		}

		@Override
		public void setCanAutowrap(Integer value) {
			this.context.setCanAutowrap(value);
		}

		@Override
		public void setAutowrap(boolean value) {
			this.context.setAutowrap(value);
		}

		@Override
		public boolean isInsideFormattedRegion() {
			return this.context.isInsideFormattedRegion();
		}

		@Override
		public boolean isAutowrap() {
			return this.context.isAutowrap();
		}

		@Override
		public ITextReplacer getReplacer() {
			return this.context.getReplacer();
		}

		@Override
		public List<ITextReplacement> getReplacementsUntil(ITextReplacerContext first) {
			return this.context.getReplacementsUntil(first);
		}

		@Override
		public ITextReplacerContext getPreviousContext() {
			return this.context.getPreviousContext();
		}

		@Override
		public String getNewLinesString(int count) {
			return this.context.getNewLinesString(count);
		}

		@Override
		public Iterable<ITextReplacement> getLocalReplacements() {
			return this.context.getLocalReplacements();
		}

		@Override
		public Iterable<ITextReplacement> getLocalReplacementsReverse() {
			return this.context.getLocalReplacementsReverse();
		}

		@Override
		public int getLeadingCharsInLineCount() {
			return this.context.getLeadingCharsInLineCount();
		}

		@Override
		public String getIndentationString(int indentationLevel) {
			return this.context.getIndentationString(indentationLevel);
		}

		@Override
		public String getIndentationString() {
			return getIndentationString(getIndentation());
		}

		@Override
		public int getIndentation() {
			return this.context.getIndentation() + 1;
		}

		@Override
		public AbstractFormatter2 getFormatter() {
			return this.context.getFormatter();
		}

		@Override
		public IFormattableDocument getDocument() {
			return this.context.getDocument();
		}

		@Override
		public Integer canAutowrap() {
			return this.context.canAutowrap();
		}

		@Override
		public void addReplacement(ITextReplacement replacement) {
			this.context.addReplacement(replacement);
		}

		@Override
		public boolean isWrapInRegion() {
			return this.context.isWrapInRegion();
		}

		@Override
		public boolean isWrapSincePrevious() {
			return this.context.isWrapSincePrevious();
		}

	}

}

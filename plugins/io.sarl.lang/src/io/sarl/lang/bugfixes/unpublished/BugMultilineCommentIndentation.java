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

/** FIXME: Fixing a bug in Xtext Formatter2 API that avoid to have a good
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
		IHiddenRegion hiddenRegion = comment.getHiddenRegion();
		boolean needBugFix = false;
		if (hiddenRegion != null) {
			ISemanticRegion semanticRegion = hiddenRegion.getPreviousSemanticRegion();
			if (semanticRegion != null) {
				EObject element = semanticRegion.getGrammarElement();
				if (element instanceof Keyword
						&& Strings.equal(((Keyword) element).getValue(), "{")) { //$NON-NLS-1$
					needBugFix = true;
					semanticRegion = hiddenRegion.getNextSemanticRegion();
					if (semanticRegion != null) {
						element = semanticRegion.getGrammarElement();
						if (element instanceof Keyword
								&& Strings.equal(((Keyword) element).getValue(), "}")) { //$NON-NLS-1$
							needBugFix = false;
						}
					}
				}
			}
		}
		if (needBugFix) {			
			// Indentation of the first comment line
			ITextRegionAccess access = comment.getTextRegionAccess();
			ITextSegment target = access.regionForOffset(comment.getOffset(), 0);
			context.addReplacement(target.replaceWith(context.getIndentationString(1)));
			// Indentation of the comment's lines
			return new ITextReplacerContext() {
				@Override
				public ITextReplacerContext withReplacer(ITextReplacer replacer) {
					return context.withReplacer(replacer);
				}
				@Override
				public ITextReplacerContext withIndentation(int indentation) {
					return context.withIndentation(indentation);
				}
				@Override
				public ITextReplacerContext withDocument(IFormattableDocument document) {
					return context.withDocument(document);
				}
				@Override
				public void setNextReplacerIsChild() {
					context.setNextReplacerIsChild();
				}
				@Override
				public void setCanAutowrap(Integer value) {
					context.setCanAutowrap(value);
				}
				@Override
				public void setAutowrap(boolean value) {
					context.setAutowrap(value);
				}
				@Override
				public boolean isInsideFormattedRegion() {
					return context.isInsideFormattedRegion();
				}
				@Override
				public boolean isAutowrap() {
					return context.isAutowrap();
				}
				@Override
				public ITextReplacer getReplacer() {
					return context.getReplacer();
				}
				@Override
				public List<ITextReplacement> getReplacementsUntil(ITextReplacerContext first) {
					return context.getReplacementsUntil(first);
				}
				@Override
				public ITextReplacerContext getPreviousContext() {
					return context.getPreviousContext();
				}
				@Override
				public String getNewLinesString(int count) {
					return context.getNewLinesString(count);
				}
				@Override
				public Iterable<ITextReplacement> getLocalReplacements() {
					return context.getLocalReplacements();
				}
				@Override
				public int getLeadingCharsInLineCount() {
					return context.getLeadingCharsInLineCount();
				}
				@Override
				public String getIndentationString(int indentationLevel) {
					return context.getIndentationString(indentationLevel);
				}
				@Override
				public String getIndentationString() {
					return getIndentationString(getIndentation());
				}
				@Override
				public int getIndentation() {
					return context.getIndentation() + 1;
				}
				@Override
				public AbstractFormatter2 getFormatter() {
					return context.getFormatter();
				}
				@Override
				public IFormattableDocument getDocument() {
					return context.getDocument();
				}
				@Override
				public Integer canAutowrap() {
					return context.canAutowrap();
				}
				@Override
				public void addReplacement(ITextReplacement replacement) {
					context.addReplacement(replacement);
				}
			};
		}
		return context;
	}

}

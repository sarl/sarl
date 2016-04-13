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

import javax.inject.Singleton;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.formatting2.ITextReplacerContext;
import org.eclipse.xtext.formatting2.regionaccess.IComment;
import org.eclipse.xtext.formatting2.regionaccess.IHiddenRegion;
import org.eclipse.xtext.formatting2.regionaccess.ISemanticRegion;
import org.eclipse.xtext.formatting2.regionaccess.ITextRegionAccess;
import org.eclipse.xtext.formatting2.regionaccess.ITextSegment;
import org.eclipse.xtext.util.Strings;

/** FIXME: Fixing a bug in Xtext Formatter2 API that avoid to have a good
 * indentation for the last comment in a block.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class BugSinglelineCommentIndentation {

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
			ISemanticRegion semanticRegion = hiddenRegion.getNextSemanticRegion();
			if (semanticRegion != null) {
				EObject element = semanticRegion.getGrammarElement();
				if (element instanceof Keyword
						&& Strings.equal(((Keyword) element).getValue(), "}")) { //$NON-NLS-1$
					needBugFix = true;
					semanticRegion = hiddenRegion.getPreviousSemanticRegion();
					if (semanticRegion != null) {
						element = semanticRegion.getGrammarElement();
						if (element instanceof Keyword
								&& Strings.equal(((Keyword) element).getValue(), "{")) { //$NON-NLS-1$
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
		}
		return context;
	}

}

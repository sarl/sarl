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

package io.sarl.lang.bugfixes.unpublished;

import com.google.inject.Singleton;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.formatting2.ITextReplacerContext;
import org.eclipse.xtext.formatting2.regionaccess.IComment;
import org.eclipse.xtext.formatting2.regionaccess.IHiddenRegion;
import org.eclipse.xtext.util.Strings;

/** FIXME: Xtext upgrade, Fixing a bug in Xtext Formatter2 API that avoid to have a good
 * indentation for the last comment in a block.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
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
		final var hiddenRegion = comment.getHiddenRegion();
		if (detectBugSituation(hiddenRegion)
				&& fixBug(hiddenRegion)) {
			// Indentation of the first comment line
			final var access = comment.getTextRegionAccess();
			final var target = access.regionForOffset(comment.getOffset(), 0);
			context.addReplacement(target.replaceWith(context.getIndentationString(1)));
		}
		return context;
	}

	private static boolean detectBugSituation(IHiddenRegion hiddenRegion) {
		if (hiddenRegion != null) {
			final var semanticRegion = hiddenRegion.getNextSemanticRegion();
			if (semanticRegion != null) {
				final var element = semanticRegion.getGrammarElement();
				if (element instanceof Keyword kw
						&& Strings.equal(kw.getValue(), "}")) { //$NON-NLS-1$
					return true;
				}
			}
		}
		return false;
	}

	private static boolean fixBug(IHiddenRegion hiddenRegion) {
		var needBugFix = true;
		final var semanticRegion = hiddenRegion.getPreviousSemanticRegion();
		if (semanticRegion != null) {
			final var element = semanticRegion.getGrammarElement();
			if (element instanceof Keyword kw
					&& Strings.equal(kw.getValue(), "{")) { //$NON-NLS-1$
				needBugFix = false;
			}
		}
		return needBugFix;
	}

}

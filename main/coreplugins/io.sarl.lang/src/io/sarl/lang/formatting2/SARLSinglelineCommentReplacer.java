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

package io.sarl.lang.formatting2;

import com.google.inject.Inject;
import org.eclipse.xtext.formatting2.ITextReplacerContext;
import org.eclipse.xtext.formatting2.internal.CommentReplacer;
import org.eclipse.xtext.formatting2.internal.WhitespaceReplacer;
import org.eclipse.xtext.formatting2.regionaccess.IComment;

import io.sarl.lang.bugfixes.unpublished.BugSinglelineCommentIndentation;
import io.sarl.lang.documentation.IDocumentationFormatter;

/**
 * Format the single-line block comment according to the SARL standards.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLSinglelineCommentReplacer extends CommentReplacer {

	@Inject
	private IDocumentationFormatter formatter;

	@Inject
	private BugSinglelineCommentIndentation bugfix;

	/** Construct the replacer.
	 *
	 * @param comment the comment to format.
	 */
	public SARLSinglelineCommentReplacer(IComment comment) {
		super(comment);
	}

	@Override
	public void configureWhitespace(WhitespaceReplacer leading, WhitespaceReplacer trailing) {
		// Do not configure the whitespaces before and after the single line comment in order
		// to let the developer to write them as s/he want.

		// The whitespace replacers are lready configured for the current context.
		// It means that line lines may be added around the single line comment.
	}

	@Override
	public ITextReplacerContext createReplacements(ITextReplacerContext context) {
		final IComment comment = getComment();
		if (context != null && comment != null) {
			this.formatter.formatSinglelineComment(this.bugfix.fix(context, comment), comment);
		}
		return context;
	}

}

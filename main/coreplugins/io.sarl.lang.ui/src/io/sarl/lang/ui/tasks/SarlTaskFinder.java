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

package io.sarl.lang.ui.tasks;

import com.google.common.base.Objects;
import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.tasks.DefaultTaskFinder;
import org.eclipse.xtext.xbase.lib.Extension;

import io.sarl.lang.services.SARLGrammarAccess;

/** Finder for task tags.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class SarlTaskFinder extends DefaultTaskFinder {

	@Inject
	@Extension
	private SARLGrammarAccess grammarAccess;

	@Override
	protected boolean canContainTaskTags(final ILeafNode node) {
		final boolean result = super.canContainTaskTags(node);
		if (!result) {
			return isRichComment(node);
		}
		return result;
	}

	@Override
	protected String stripText(final ILeafNode node, final String text) {
		final boolean isRichComment = isRichComment(node);
		if (isRichComment) {
			final char newLine = '\n';
			final int index = text.indexOf(newLine);
			if (index != -1) {
				return text.substring(0, index);
			}
			return text;
		}
		return super.stripText(node, text);
	}

	private boolean isRichComment(final ILeafNode node) {
		final EObject grammarElement = node.getGrammarElement();
		if (grammarElement instanceof RuleCall) {
			return Objects.equal(((RuleCall) node.getGrammarElement()).getRule(), this.grammarAccess.getCOMMENT_RICH_TEXT_ENDRule())
					|| Objects.equal(((RuleCall) node.getGrammarElement()).getRule(), this.grammarAccess.getCOMMENT_RICH_TEXT_INBETWEENRule());
		}
		return false;
	}

}

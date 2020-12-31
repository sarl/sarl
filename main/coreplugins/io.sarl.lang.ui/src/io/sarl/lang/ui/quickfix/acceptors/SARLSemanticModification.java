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

package io.sarl.lang.ui.quickfix.acceptors;

import java.lang.ref.WeakReference;

import org.eclipse.xtext.ui.editor.model.edit.ISemanticModification;
import org.eclipse.xtext.validation.Issue;

import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

/**
 * Custom quickfixes.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#quickfixes"
 */
public abstract class SARLSemanticModification implements ISemanticModification {

	private WeakReference<Issue> issue;

	private WeakReference<SARLQuickfixProvider> tools;

	/** Set the issue fixed by this quick fix modification.
	 *
	 * @param issue the fixed issue.
	 */
	protected void setIssue(Issue issue) {
		this.issue = new WeakReference<>(issue);
	}

	/** Set the tools related to the quick fixes.
	 *
	 * @param tools the tools.
	 */
	protected void setTools(SARLQuickfixProvider tools) {
		this.tools = new WeakReference<>(tools);
	}

	/** Replies the issue fixed by this quick fix modification.
	 *
	 * @return the fixed issue.
	 */
	protected Issue getIssue() {
		return this.issue.get();
	}

	/** Replies the tools related to the quick fixes.
	 *
	 * @return the tools.
	 */
	protected SARLQuickfixProvider getTools() {
		return this.tools.get();
	}

}

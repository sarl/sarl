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

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;

import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

/**
 * A Semantic modificaton that contains multiple modifications that are selecting
 * according to the type of the source element.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class MultiModification extends SARLSemanticModification {

	private final Map<Class<?>, Class<? extends SARLSemanticModification>> modificationTypes = new HashMap<>();

	/** Constructor.
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param acceptor the quick fix acceptor.
	 * @param label the label of the modification.
	 * @param description the description of the modification.
	 */
	public MultiModification(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor,
			String label, String description) {
		setTools(provider);
		setIssue(issue);
		acceptor.accept(
				issue,
				label,
				description,
				JavaPluginImages.IMG_CORRECTION_MULTI_FIX,
				this);
	}

	/** Add a semantic modification related to the given element type.
	 *
	 * @param type the type of the element.
	 * @param modification the modification
	 */
	public void bind(Class<?> type, Class<? extends SARLSemanticModification> modification) {
		this.modificationTypes.put(type, modification);
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		Class<? extends SARLSemanticModification> selected = null;
		Class<?> deeperType = null;

		for (final Entry<Class<?>, Class<? extends SARLSemanticModification>> entry : this.modificationTypes.entrySet()) {
			if (entry.getKey().isInstance(element)) {
				if (deeperType == null
						|| deeperType.isAssignableFrom(entry.getKey())) {
					deeperType = entry.getKey();
					selected = entry.getValue();
				}
			}
		}

		if (selected != null) {
			final SARLSemanticModification modification = selected.getDeclaredConstructor().newInstance();
			modification.setIssue(getIssue());
			modification.setTools(getTools());
			modification.apply(element, context);
		}
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.typehierarchy;

import com.google.inject.Inject;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.internal.ui.util.OpenTypeHierarchyUtil;
import org.eclipse.jdt.ui.actions.OpenTypeHierarchyAction;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.util.jdt.IJavaElementFinder;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;

/**
 * Handler for opening the type hierarchy from a SARL script.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.7
 */
@SuppressWarnings("restriction")
public class OpenTypeHierarchyHandler extends AbstractHandler {

	@Inject
	private SarlScriptTypeSelector typeSelector;

	@Inject
	private IJavaElementFinder javaElementFinder;

	@Inject
	private SarlJvmModelAssociations sarlAssociations;

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		final var activeSelection = HandlerUtil.getCurrentStructuredSelection(event);
		if (activeSelection != null && activeSelection != StructuredSelection.EMPTY) {
			final var selectedElement = this.typeSelector.searchAndSelect(false, activeSelection.toArray());
			if (selectedElement != null) {
				IJavaElement realJavaElement = null;
				if (selectedElement.element instanceof IJavaElement cvalue) {
					realJavaElement = cvalue;
				} else if (selectedElement.element instanceof XtendTypeDeclaration cvalue) {
					final var jvmElement = this.sarlAssociations.getPrimaryJvmElement(cvalue);
					if (jvmElement instanceof JvmIdentifiableElement cvalue0) {
						realJavaElement = this.javaElementFinder.findElementFor(cvalue0);
					}
				}
				if (realJavaElement != null) {
					OpenTypeHierarchyUtil.open(realJavaElement, PlatformUI.getWorkbench().getActiveWorkbenchWindow());
				}
			} else {
				final var jdtTool = new OpenTypeHierarchyAction(HandlerUtil.getActiveSite(event));
				jdtTool.run(activeSelection);
			}
		}
		return null;
	}

}

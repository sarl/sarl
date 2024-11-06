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

package io.sarl.eclipse.refactoring;

import java.text.MessageFormat;

import com.google.inject.Inject;
import com.google.inject.Provider;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.xtend.core.xtend.AnonymousClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.utils.EditorUtils;
import org.eclipse.xtext.ui.refactoring.ui.SyncUtil;
import org.eclipse.xtext.util.PolymorphicDispatcher;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.xbase.ui.refactoring.RefactoredResourceCopier;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlArtifact;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;

/**
 * Move the selected SARL element.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.12
 */
@SuppressWarnings("restriction")
public class SarlMoveElementHandler extends AbstractHandler {

	@Inject
	private Provider<SyncUtil> syncUtil;

	@Inject
	private RefactoredResourceCopier resourceCopier;

	private final PolymorphicDispatcher<Object> moveDispatcher;

	/** Constructor.
	 */
	public SarlMoveElementHandler() {
		this.moveDispatcher = PolymorphicDispatcher.createForSingleTarget("_move", 2, 2, this); //$NON-NLS-1$
	}

	private static String getTypeName(XtendMember element) {
		if (element instanceof SarlBehaviorUnit) {
			return "a SARL behavior unit"; //$NON-NLS-1$
		}
		if (element instanceof XtendConstructor) {
			return "a SARL class constructor"; //$NON-NLS-1$
		}
		if (element instanceof XtendFunction) {
			return "a SARL action"; //$NON-NLS-1$
		}
		if (element instanceof XtendField) {
			return "a SARL field"; //$NON-NLS-1$
		}
		if (element instanceof SarlAgent) {
			return "a SARL agent"; //$NON-NLS-1$
		}
		if (element instanceof SarlArtifact) {
			return "a SARL artifact"; //$NON-NLS-1$
		}
		if (element instanceof SarlBehavior) {
			return "a SARL behavior"; //$NON-NLS-1$
		}
		if (element instanceof SarlCapacity) {
			return "a SARL capacity"; //$NON-NLS-1$
		}
		if (element instanceof SarlEvent) {
			return "a SARL event"; //$NON-NLS-1$
		}
		if (element instanceof SarlSkill) {
			return "a SARL skill"; //$NON-NLS-1$
		}
		if (element instanceof SarlSpace) {
			return "a SARL space"; //$NON-NLS-1$
		}
		if (element instanceof SarlAnnotationType) {
			if (element.getDeclaringType() != null) {
				return "an inner SARL annotation"; //$NON-NLS-1$
			}
			return "a SARL annotation"; //$NON-NLS-1$
		}
		if (element instanceof SarlClass) {
			if (element.getDeclaringType() != null) {
				return "an inner SARL class"; //$NON-NLS-1$
			}
			return "a SARL class"; //$NON-NLS-1$
		}
		if (element instanceof SarlEnumeration) {
			if (element.getDeclaringType() != null) {
				return "an inner SARL enumeration"; //$NON-NLS-1$
			}
			return "a SARL enumeration"; //$NON-NLS-1$
		}
		if (element instanceof SarlInterface) {
			if (element.getDeclaringType() != null) {
				return "an inner SARL interface"; //$NON-NLS-1$
			}
			return "a SARL interface"; //$NON-NLS-1$
		}
		if (element instanceof AnonymousClass) {
			return "a SARL anonymous class"; //$NON-NLS-1$
		}
		return MessageFormat.format("a SARL element of type {0}", element.eClass().getName()); //$NON-NLS-1$
	}

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		try {
			// Save all editors
			this.syncUtil.get().totalSync(true);
			final var editor = EditorUtils.getActiveXtextEditor(event);
			if (editor != null) {
				final var selection = (ITextSelection) editor.getSelectionProvider().getSelection();
				final var document = editor.getDocument();
				final var resource = reloadResource(document);
				final var semanticObject = findSemanticObject(resource, selection);
				if (semanticObject == null) {
					SARLEclipsePlugin.getDefault().openError(editor.getShell(),
							"Move Element", //$NON-NLS-1$
							"Unable to find a SARL element into the current selection.", //$NON-NLS-1$
							"The current selection is not inside a SARL element", null); //$NON-NLS-1$
				}
				this.moveDispatcher.invoke(semanticObject, editor.getShell());
			}
			return null;
		} catch (Exception exception) {
			throw new ExecutionException(exception.getLocalizedMessage(), exception);
		}
	}

	private XtextResource reloadResource(IXtextDocument document) {
		final IUnitOfWork<XtextResource, XtextResource> function = (XtextResource it) -> {
			return this.resourceCopier.loadIntoNewResourceSet(it);
		};
		return document.<XtextResource>priorityReadOnly(function);
	}

	private XtendMember findSemanticObject(XtextResource resource, ITextSelection selection) {
		final var parseResult = resource.getParseResult();
		if (parseResult != null) {
			final var rootNode = parseResult.getRootNode();
			var node = NodeModelUtils.findLeafNodeAtOffset(rootNode, selection.getOffset());
			if (node == null) {
				return null;
			}
			if (node.isHidden()) {
				if (selection.getLength() > node.getLength()) {
					node = NodeModelUtils.findLeafNodeAtOffset(rootNode, node.getEndOffset());
				} else {
					node = NodeModelUtils.findLeafNodeAtOffset(rootNode, selection.getOffset() - 1);
				}
			} else if (node.getOffset() == selection.getOffset()) {
				node = NodeModelUtils.findLeafNodeAtOffset(rootNode, selection.getOffset() - 1);
			}
			if (node != null) {
				final var currentSemanticElement = NodeModelUtils.findActualSemanticObjectFor(node);
				if (currentSemanticElement != null) {
					if (currentSemanticElement instanceof XtendMember cvalue) {
						return cvalue;
					}
					return EcoreUtil2.getContainerOfType(currentSemanticElement.eContainer(), XtendMember.class);
				}
			}
		}
		return null;
	}

	/** Show the error message for an unsupported element.
	 *
	 * @param element the element to move.
	 * @param shell the UI shell.
	 */
	protected void showUnsupportedElementError(XtendMember element, Shell shell) {
		final var typeName = getTypeName(element);
		SARLEclipsePlugin.getDefault().openError(shell,
				"Move Element", //$NON-NLS-1$
				MessageFormat.format("Unable to move {0}.", typeName), //$NON-NLS-1$
				MessageFormat.format("The refactoring feature does not support ", typeName), null); //$NON-NLS-1$
	}

	/** Generic handler for moving a member.
	 *
	 * @param element the element to move.
	 * @param shell the UI shell.
	 */
	protected void _move(XtendMember element, Shell shell) {
		showUnsupportedElementError(element, shell);
	}

	/** Move a type declaration. The anonymous classes cannot be moved.
	 *
	 * @param element the element to move.
	 * @param shell the UI shell.
	 */
	protected void _move(XtendTypeDeclaration element, Shell shell) {
		if (element instanceof AnonymousClass) {
			showUnsupportedElementError(element, shell);
		} else if (element.getDeclaringType() != null) {
			showUnsupportedElementError(element, shell);
		} else {
			showUnsupportedElementError(element, shell);
		}
	}

}

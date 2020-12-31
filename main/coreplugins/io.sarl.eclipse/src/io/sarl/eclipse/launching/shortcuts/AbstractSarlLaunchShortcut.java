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

package io.sarl.eclipse.launching.shortcuts;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.debug.ui.ILaunchShortcut2;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.parser.IParseResult;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.utils.EditorUtils;
import org.eclipse.xtext.util.Pair;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.AbstractSarlScriptInteractiveSelector;
import io.sarl.lang.sarl.SarlScript;

/** Abstract implementation of a shortcut for launching a SARL agent or SARL applications.
 *
 * @param <ET> the type of the valid objects that are supported by this shortcut.
 * @param <JT> the type of the valid objects that are supported by this shortcut.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public abstract class AbstractSarlLaunchShortcut<ET extends EObject, JT>
		extends AbstractSarlScriptInteractiveSelector<ET, JT> implements ILaunchShortcut2 {

	@Override
	public ILaunchConfiguration[] getLaunchConfigurations(ISelection selection) {
		// let the framework resolve configurations based on resource mapping
		return null;
	}

	@Override
	public ILaunchConfiguration[] getLaunchConfigurations(IEditorPart editorpart) {
		// let the framework resolve configurations based on resource mapping
		return null;
	}

	@Override
	public IResource getLaunchableResource(ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			final IStructuredSelection sel = (IStructuredSelection) selection;
			return findResource(sel.toArray());
		}
		return null;
	}

	@Override
	public IResource getLaunchableResource(IEditorPart editorpart) {
		final XtextEditor xtextEditor = EditorUtils.getXtextEditor(editorpart);
		if (xtextEditor != null) {
			return xtextEditor.getResource();
		}
		return null;
	}

	/** Replies the qualified name of the element to launch, from the given configuration.
	 *
	 * @param configuration the configuration to read.
	 * @return the qualified name of the element.
	 */
	protected abstract String getElementQualifiedName(ILaunchConfiguration configuration);

	/** Replies the name of the configuration type supported by this short cut.
	 *
	 * @return the configuration type name.
	 */
	protected abstract String getConfigurationType();

	/** Replies the URI of the type of the valid types.
	 *
	 * @return the type.
	 * @since 0.7
	 */
	protected abstract Class<JT> getValidJavaType();

	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:nestedifdepth"})
	private IResource findResource(Object[] elements) {
		try {
			for (final Object element : elements) {
				final URI fileURI = getResourceURIForValidEObject(element);
				if (fileURI != null) {
					for (final Pair<IStorage, IProject> storage: this.storage2UriMapper.getStorages(fileURI)) {
						final Object obj = storage.getFirst();
						if (obj instanceof IResource) {
							final IResource res = (IResource) obj;
							if (isValidResource(res)) {
								return res;
							}
						}
					}
				} else {
					final LinkedList<Object> stack = new LinkedList<>();
					stack.add(element);
					final Class<ET> evalidType = getValidEObjectType();
					final Class<JT> jvalidType = getValidJavaType();
					while (!stack.isEmpty()) {
						final Object current = stack.removeFirst();
						if (current instanceof IFile) {
							final IFile file = (IFile) current;
							if (isValidResource(file)) {
								final ResourceSet resourceSet = this.resourceSetProvider.get(file.getProject());
								final URI resourceURI = URI.createPlatformResourceURI(file.getFullPath().toString(), true);
								final Resource resource = resourceSet.getResource(resourceURI, true);
								if (resource != null) {
									for (final EObject content : resource.getContents()) {
										if (content instanceof SarlScript
											&& !EcoreUtil2.getAllContentsOfType(content, evalidType).isEmpty()) {
											return file;
										}
									}
								}
							}
						} else if (current instanceof IFolder) {
							final IFolder folder = (IFolder) current;
							if (isValidResource(folder)) {
								try {
									stack.addAll(Arrays.asList(folder.members(0)));
								} catch (CoreException exception) {
									// Ignore the failing resources
								}
							}
						} else if (current instanceof IType) {
							final IType type = (IType) current;
							final String qn = type.getFullyQualifiedName();
							final IJavaProject project = type.getJavaProject();
							if (this.jdt.isSubClassOf(this.jdt.toTypeFinder(project), qn, jvalidType.getName())) {
								return type.getResource();
							}
						} else if (current instanceof IPackageFragment) {
							final IPackageFragment fragment = (IPackageFragment) current;
							stack.addAll(Arrays.asList(fragment.getNonJavaResources()));
							for (final Object child : fragment.getChildren()) {
								stack.add(child);
							}
						} else if (current instanceof IPackageFragmentRoot) {
							final IPackageFragmentRoot fragment = (IPackageFragmentRoot) current;
							stack.addAll(Arrays.asList(fragment.getNonJavaResources()));
							for (final Object child : fragment.getChildren()) {
								stack.add(child);
							}
						} else if (current instanceof IJavaProject) {
							stack.addAll(Arrays.asList(((IJavaProject) current).getNonJavaResources()));
						}
					}
				}
			}
		} catch (JavaModelException exception) {
			//
		}
		return null;
	}

	/**
	 * Resolves an element type that can be launched from the given scope and launches in the
	 * specified mode.
	 *
	 * @param mode launch mode.
	 * @param scope the elements to consider for an element type that can be launched.
	 */
	private void searchAndLaunch(String mode, Object... scope) {
		final ElementDescription element = searchAndSelect(true, scope);
		if (element != null) {
			try {
				launch(element.projectName, element.elementName, mode);
			} catch (CoreException e) {
				SARLEclipsePlugin.getDefault().openError(getShell(),
						io.sarl.eclipse.util.Messages.AbstractSarlScriptInteractiveSelector_1,
						e.getStatus().getMessage(), null, e);
			}
		}
	}

	@Override
	public void launch(ISelection selection, String mode) {
		if (selection instanceof IStructuredSelection) {
			searchAndLaunch(mode, ((IStructuredSelection) selection).toArray());
		}
	}

	@Override
	public void launch(IEditorPart editor, String mode) {
		final XtextEditor xtextEditor = EditorUtils.getXtextEditor(editor);
		final ISelection selection = xtextEditor.getSelectionProvider().getSelection();
		if (selection instanceof ITextSelection) {
			final ITextSelection sel = (ITextSelection) selection;
			final EObject obj = xtextEditor.getDocument().readOnly(resource -> {
				final IParseResult parseRes = resource.getParseResult();
				if (parseRes == null) {
					return null;
				}
				final ICompositeNode rootNode = parseRes.getRootNode();
				final ILeafNode node = NodeModelUtils.findLeafNodeAtOffset(rootNode, sel.getOffset());
				return NodeModelUtils.findActualSemanticObjectFor(node);
			});
			if (obj != null) {
				final EObject elt = EcoreUtil2.getContainerOfType(obj, getValidEObjectType());
				if (elt != null) {
					searchAndLaunch(mode, elt);
					return;
				}
			}
		} else if (selection != null) {
			launch(selection, mode);
			return;
		}
		// Default launching
		searchAndLaunch(mode, xtextEditor.getResource());
	}

	/**
	 * Launches the given element type in the specified mode.
	 *
	 * @param projectName the name of the project.
	 * @param fullyQualifiedName the element name.
	 * @param mode launch mode
	 * @throws CoreException if something is going wrong.
	 */
	protected void launch(String projectName, String fullyQualifiedName, String mode)
			throws CoreException {
		final List<ILaunchConfiguration> configs = getCandidates(projectName, fullyQualifiedName);
		ILaunchConfiguration config = null;
		final int count = configs.size();
		if (count == 1) {
			config = configs.get(0);
		} else if (count > 1) {
			config = chooseConfiguration(configs);
			if (config == null) {
				return;
			}
		}
		if (config == null) {
			config = createConfiguration(projectName, fullyQualifiedName);
		}
		if (config != null) {
			DebugUITools.launch(config, mode);
		}
	}

	/** Collect the listing of {@link ILaunchConfiguration}s that apply to the given element.
	 *
	 * @param projectName the name of the project.
	 * @param fullyQualifiedName the element.
	 * @return the list of {@link ILaunchConfiguration}s or an empty list, never {@code null}
	 * @throws CoreException if something is going wrong.
	 */
	protected List<ILaunchConfiguration> getCandidates(String projectName,
			String fullyQualifiedName) throws CoreException {
		final ILaunchConfigurationType ctype = getLaunchManager().getLaunchConfigurationType(getConfigurationType());
		List<ILaunchConfiguration> candidateConfigs = Collections.emptyList();
		final ILaunchConfiguration[] configs = getLaunchManager().getLaunchConfigurations(ctype);
		candidateConfigs = new ArrayList<>(configs.length);
		for (int i = 0; i < configs.length; i++) {
			final ILaunchConfiguration config = configs[i];
			if (Objects.equals(
					getElementQualifiedName(config),
					fullyQualifiedName)
					&& Objects.equals(
							config.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""), //$NON-NLS-1$
							projectName)) {
				candidateConfigs.add(config);
			}
		}
		return candidateConfigs;
	}

	/**
	 * Returns a configuration from the given collection of configurations that should be launched,
	 * or {@code null} to cancel. Default implementation opens a selection dialog that allows
	 * the user to choose one of the specified launch configurations.  Returns the chosen configuration,
	 * or {@code null} if the user cancels.
	 *
	 * @param configList list of configurations to choose from.
	 * @return configuration to launch or {@code null} to cancel.
	 */
	@SuppressWarnings("static-method")
	protected ILaunchConfiguration chooseConfiguration(List<ILaunchConfiguration> configList) {
		final IDebugModelPresentation labelProvider = DebugUITools.newDebugModelPresentation();
		final ElementListSelectionDialog dialog = new ElementListSelectionDialog(getShell(), labelProvider);
		dialog.setElements(configList.toArray());
		dialog.setTitle(Messages.AbstractSarlLaunchShortcut_0);
		dialog.setMessage(Messages.AbstractSarlLaunchShortcut_1);
		dialog.setMultipleSelection(false);
		final int result = dialog.open();
		labelProvider.dispose();
		if (result == Window.OK) {
			return (ILaunchConfiguration) dialog.getFirstResult();
		}
		return null;
	}

	/**
	 * Creates and returns a new configuration based on the specified type.
	 *
	 * @param projectName the name of the project.
	 * @param fullyQualifiedName the element.
	 * @return launch configuration configured to launch the specified type
	 */
	protected abstract ILaunchConfiguration createConfiguration(String projectName, String fullyQualifiedName);

}

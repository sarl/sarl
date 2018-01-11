/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

import javax.inject.Inject;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchManager;
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
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.progress.IProgressService;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.parser.IParseResult;
import org.eclipse.xtext.ui.editor.XtextEditor;
import org.eclipse.xtext.ui.editor.utils.EditorUtils;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.util.Pair;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;
import io.sarl.eclipse.util.Jdt2Ecore;
import io.sarl.lang.core.Agent;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.labeling.SARLImages;

/** Shortcut for launching a SARL agent.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public class LaunchAgentShortcut implements ILaunchShortcut2 {

	@Inject
	private IQualifiedNameProvider nameProvider;

	@Inject
	private IStorage2UriMapper storage2UriMapper;

	@Inject
	private IResourceSetProvider resourceSetProvider;

	@Inject
	private Jdt2Ecore jdt;

	@Inject
	private ILabelProvider labelProvider;

	@Inject
	private SARLImages images;

	@Inject
	private ILaunchConfigurationConfigurator configurator;

	@Inject
	private ILaunchConfigurationAccessor accessor;

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
				final SarlAgent agent = EcoreUtil2.getContainerOfType(obj, SarlAgent.class);
				if (agent != null) {
					searchAndLaunch(mode, agent);
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
	 * Launches the given agent type in the specified mode.
	 *
	 * @param projectName the name of the project.
	 * @param fullyQualifiedNameOfAgent the agent.
	 * @param mode launch mode
	 * @throws CoreException if something is going wrong.
	 */
	protected void launch(String projectName, String fullyQualifiedNameOfAgent, String mode)
			throws CoreException {
		final List<ILaunchConfiguration> configs = getCandidates(projectName, fullyQualifiedNameOfAgent);
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
			config = createConfiguration(projectName, fullyQualifiedNameOfAgent);
		}
		if (config != null) {
			DebugUITools.launch(config, mode);
		}
	}

	/**
	 * Creates and returns a new configuration based on the specified type.
	 *
	 * @param projectName the name of the project.
	 * @param fullyQualifiedNameOfAgent the agent.
	 * @return launch configuration configured to launch the specified type
	 */
	protected ILaunchConfiguration createConfiguration(String projectName, String fullyQualifiedNameOfAgent) {
		try {
			return this.configurator.newConfiguration(projectName, fullyQualifiedNameOfAgent);
		} catch (CoreException exception) {
			SARLEclipsePlugin.getDefault().openError(getShell(),
					Messages.SARLLaunchShortcut_0, exception.getStatus().getMessage(), exception);
			return null;
		}
	}

	/**
	 * Resolves an agent type that can be launched from the given scope and launches in the
	 * specified mode.
	 *
	 * @param mode launch mode.
	 * @param scope the elements to consider for an agent type that can be launched.
	 */
	private void searchAndLaunch(String mode, Object... scope) {
		try {
			final List<AgentDescription> agents = findAgents(scope, PlatformUI.getWorkbench().getProgressService());
			AgentDescription agent = null;
			if (agents.isEmpty()) {
				SARLEclipsePlugin.getDefault().openError(getShell(), Messages.SARLLaunchShortcut_0, Messages.SARLLaunchShortcut_2,
						null);
			} else if (agents.size() > 1) {
				agent = chooseType(agents);
			}  else {
				agent = agents.get(0);
			}
			if (agent != null) {
				try {
					launch(agent.projectName, agent.agentName, mode);
				} catch (CoreException e) {
					SARLEclipsePlugin.getDefault().openError(getShell(), Messages.SARLLaunchShortcut_0,
							e.getStatus().getMessage(), e);
				}
			}
		} catch (InterruptedException exception) {
			//
		} catch (InvocationTargetException exception) {
			SARLEclipsePlugin.getDefault().openError(getShell(), Messages.SARLLaunchShortcut_0, null,
					exception);
		}
	}

	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:nestedifdepth"})
	private List<AgentDescription> findAgents(Object[] elements, IProgressService progress)
			throws InvocationTargetException, InterruptedException {
		final List<AgentDescription> descs = new ArrayList<>();
		progress.busyCursorWhile(monitor -> {
			try {
				monitor.beginTask(Messages.SARLLaunchShortcut_5, elements.length);
				for (final Object element : elements) {
					if (element instanceof SarlAgent) {
						final SarlAgent agent = (SarlAgent) element;
						final URI fileURI = agent.eResource().getURI();
						for (final Pair<IStorage, IProject> storage: this.storage2UriMapper.getStorages(fileURI)) {
							descs.add(new AgentDescription(
									storage.getSecond().getName(),
									this.nameProvider.getFullyQualifiedName(agent).toString()));
							break;
						}
					} else {
						final LinkedList<Object> stack = new LinkedList<>();
						stack.add(element);
						while (!stack.isEmpty()) {
							final Object current = stack.removeFirst();
							if (current instanceof IFile) {
								final IFile file = (IFile) current;
								if (isValidResource(file)) {
									final ResourceSet resourceSet = this.resourceSetProvider.get(file.getProject());
									final URI resourceURI = URI.createPlatformResourceURI(file.getFullPath().toString(), true);
									if (resourceURI != null) {
										try {
											final Resource resource = resourceSet.getResource(resourceURI, true);
											if (resource != null) {
												final String projectName = file.getProject().getName();
												for (final EObject content : resource.getContents()) {
													if (content instanceof SarlScript) {
														for (final SarlAgent agent : EcoreUtil2.getAllContentsOfType(content, SarlAgent.class)) {
															descs.add(new AgentDescription(
																	projectName,
																	this.nameProvider.getFullyQualifiedName(agent).toString()));
														}
													}
												}
											}
										} catch (Throwable exception) {
											// The exception is ignore because it is assumed it is caused by a
											// file from which a Xtext resource cannot be extracted.
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
								if (this.jdt.isSubClassOf(this.jdt.toTypeFinder(project), qn, Agent.class.getName())) {
									descs.add(new AgentDescription(project.getElementName(), qn));
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
					monitor.worked(1);
				}
			} catch (JavaModelException exception) {
				throw new InvocationTargetException(exception);
			}
		});
		return descs;
	}

	/** Replies if the given resource could be considered for discovering an agent to be launched.
	 *
	 * @param resource the resource.
	 * @return {@code true} if the resource could be explored.
	 */
	@SuppressWarnings("static-method")
	protected boolean isValidResource(IResource resource) {
		return resource.isAccessible() && !resource.isHidden() && !resource.isPhantom() && !resource.isDerived();
	}

	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:nestedifdepth"})
	private IResource findResource(Object[] elements) {
		try {
			for (final Object element : elements) {
				if (element instanceof SarlAgent) {
					final SarlAgent agent = (SarlAgent) element;
					final URI fileURI = agent.eResource().getURI();
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
											&& !EcoreUtil2.getAllContentsOfType(content, SarlAgent.class).isEmpty()) {
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
							if (this.jdt.isSubClassOf(this.jdt.toTypeFinder(project), qn, Agent.class.getName())) {
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

	/** Collect the listing of {@link ILaunchConfiguration}s that apply to the given agent.
	 *
	 * @param projectName the name of the project.
	 * @param fullyQualifiedNameOfAgent the agent.
	 * @return the list of {@link ILaunchConfiguration}s or an empty list, never <code>null</code>
	 * @throws CoreException if something is going wrong.
	 */
	private List<ILaunchConfiguration> getCandidates(String projectName,
			String fullyQualifiedNameOfAgent) throws CoreException {
		final ILaunchConfigurationType ctype = getLaunchManager().getLaunchConfigurationType(
				this.accessor.getLaunchConfigurationType());
		List<ILaunchConfiguration> candidateConfigs = Collections.emptyList();
		final ILaunchConfiguration[] configs = getLaunchManager().getLaunchConfigurations(ctype);
		candidateConfigs = new ArrayList<>(configs.length);
		for (int i = 0; i < configs.length; i++) {
			final ILaunchConfiguration config = configs[i];
			if (Objects.equals(
					this.accessor.getAgent(config),
					fullyQualifiedNameOfAgent)
					&& Objects.equals(
							config.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, ""), //$NON-NLS-1$
							projectName)) {
				candidateConfigs.add(config);
			}
		}
		return candidateConfigs;
	}

	/**
	 * Prompts the user to select an agent from the given agent types.
	 *
	 * @param agents the agent types to choose from.
	 * @return the selected agent or <code>null</code> if none.
	 */
	protected AgentDescription chooseType(List<AgentDescription> agents) {
		final ElementListSelectionDialog dialog = new ElementListSelectionDialog(getShell(),
				new LabelProvider());
		dialog.setElements(agents.toArray());
		dialog.setTitle(Messages.SARLLaunchShortcut_6);
		dialog.setMessage(Messages.SARLLaunchShortcut_7);
		dialog.setMultipleSelection(false);
		final int result = dialog.open();
		if (result == Window.OK) {
			return (AgentDescription) dialog.getFirstResult();
		}
		return null;
	}

	/**
	 * Returns a configuration from the given collection of configurations that should be launched,
	 * or <code>null</code> to cancel. Default implementation opens a selection dialog that allows
	 * the user to choose one of the specified launch configurations.  Returns the chosen configuration,
	 * or <code>null</code> if the user cancels.
	 *
	 * @param configList list of configurations to choose from.
	 * @return configuration to launch or <code>null</code> to cancel.
	 */
	@SuppressWarnings("static-method")
	protected ILaunchConfiguration chooseConfiguration(List<ILaunchConfiguration> configList) {
		final IDebugModelPresentation labelProvider = DebugUITools.newDebugModelPresentation();
		final ElementListSelectionDialog dialog = new ElementListSelectionDialog(getShell(), labelProvider);
		dialog.setElements(configList.toArray());
		dialog.setTitle(Messages.SARLLaunchShortcut_8);
		dialog.setMessage(Messages.SARLLaunchShortcut_9);
		dialog.setMultipleSelection(false);
		final int result = dialog.open();
		labelProvider.dispose();
		if (result == Window.OK) {
			return (ILaunchConfiguration) dialog.getFirstResult();
		}
		return null;
	}

	/**
	 * Convenience method to return the active workbench window shell.
	 *
	 * @return active workbench window shell
	 */
	protected static Shell getShell() {
		return JDIDebugUIPlugin.getActiveWorkbenchShell();
	}

	/**
	 * Returns the singleton launch manager.
	 *
	 * @return launch manager
	 */
	protected static ILaunchManager getLaunchManager() {
		return DebugPlugin.getDefault().getLaunchManager();
	}

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

	/** Description of an agent to launch.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class AgentDescription {

		/** Project name.
		 */
		@SuppressWarnings("checkstyle:visibilitymodifier")
		public final String projectName;

		/** Agent fully qualified name.
		 */
		@SuppressWarnings("checkstyle:visibilitymodifier")
		public final String agentName;

		AgentDescription(String project, String agent) {
			this.projectName = project;
			this.agentName = agent;
		}

		@Override
		public String toString() {
			return this.agentName;
		}

	}

	/** Label provider.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SuppressWarnings("synthetic-access")
	private class LabelProvider implements ILabelProvider {

		/** Constructor.
		 */
		LabelProvider() {
			//
		}

		@Override
		public void addListener(ILabelProviderListener listener) {
			LaunchAgentShortcut.this.labelProvider.addListener(listener);
		}

		@Override
		public void dispose() {
			LaunchAgentShortcut.this.labelProvider.dispose();
		}

		@Override
		public boolean isLabelProperty(Object element, String property) {
			return LaunchAgentShortcut.this.labelProvider.isLabelProperty(element, property);
		}

		@Override
		public void removeListener(ILabelProviderListener listener) {
			LaunchAgentShortcut.this.labelProvider.removeListener(listener);
		}

		@Override
		public Image getImage(Object element) {
			if (element instanceof AgentDescription) {
				return LaunchAgentShortcut.this.images.forAgent(JvmVisibility.PRIVATE, 0).createImage();
			}
			return LaunchAgentShortcut.this.labelProvider.getImage(element);
		}

		@Override
		public String getText(Object element) {
			if (element instanceof AgentDescription) {
				return ((AgentDescription) element).agentName;
			}
			return LaunchAgentShortcut.this.labelProvider.getText(element);
		}

	}

}

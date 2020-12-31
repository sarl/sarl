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

package io.sarl.eclipse.util;

import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import javax.inject.Inject;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchManager;
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
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.progress.IProgressService;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.util.Pair;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;
import io.sarl.lang.core.Agent;
import io.sarl.lang.sarl.SarlScript;

/** Abstract implementation of a utility class that enables to find the elements within a
 * SARL script and interactively select it.
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
public abstract class AbstractSarlScriptInteractiveSelector<ET extends EObject, JT> {

	/** Mapping from storage resource to URI.
	 */
	@Inject
	protected IStorage2UriMapper storage2UriMapper;

	/** Configurator of launch config.
	 */
	@Inject
	protected ILaunchConfigurationConfigurator configurator;

	/** Provider of resource set.
	 */
	@Inject
	protected IResourceSetProvider resourceSetProvider;

	/** Converter from JDT objects to Ecore objects.
	 */
	@Inject
	protected Jdt2Ecore jdt;

	/** Provider of labels and icons.
	 */
	@Inject
	protected ILabelProvider labelProvider;

	/** Replies if the given resource could be considered for discovering an agent to be launched.
	 *
	 * @param resource the resource.
	 * @return {@code true} if the resource could be explored.
	 */
	@SuppressWarnings("static-method")
	protected boolean isValidResource(IResource resource) {
		return resource.isAccessible() && !resource.isHidden() && !resource.isPhantom() && !resource.isDerived();
	}

	/** Replies if the given element could be considered as selectable.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the element could be selected.
	 */
	protected abstract boolean isSelectableElement(ET element);

	/** Replies the URI of the resource in which the given object is located if it is an eligible object.
	 *
	 * @param object the object.
	 * @return the URI.
	 * @since 0.7
	 */
	protected abstract URI getResourceURIForValidEObject(Object object);

	/** Replies the URI of the type of the valid types.
	 *
	 * @return the type.
	 * @since 0.7
	 */
	protected abstract Class<ET> getValidEObjectType();

	/**
	 * Returns the singleton launch manager.
	 *
	 * @return launch manager
	 */
	protected static ILaunchManager getLaunchManager() {
		return DebugPlugin.getDefault().getLaunchManager();
	}

	/**
	 * Convenient method to return the active workbench window shell.
	 *
	 * @return active workbench window shell
	 */
	protected static Shell getShell() {
		return JDIDebugUIPlugin.getActiveWorkbenchShell();
	}

	/** Replies the qualified name of the given element.
	 *
	 * @param element the element.
	 * @return the qualified name.
	 */
	protected abstract String getQualifiedNameFor(ET element);

	/** Replies the supported elements that are into the given selection.
	 *
	 * @param selection the selected elements.
	 * @param progress the progress monitor.
	 * @return the supported elements within the selection
	 * @throws InvocationTargetException on failure when retrieving an element.
	 * @throws InterruptedException if the search is cancelled.
	 */
	@SuppressWarnings({"unchecked", "checkstyle:cyclomaticcomplexity", "checkstyle:nestedifdepth"})
	private List<ElementDescription> findElements(Object[] selection, IProgressService progress)
			throws InvocationTargetException, InterruptedException {
		final List<ElementDescription> descs = new ArrayList<>();
		progress.busyCursorWhile(monitor -> {
			try {
				monitor.beginTask(
						MessageFormat.format(Messages.AbstractSarlScriptInteractiveSelector_0, getElementsLabel()),
						selection.length);
				for (final Object element : selection) {
					final URI fileURI = getResourceURIForValidEObject(element);
					if (fileURI != null) {
						for (final Pair<IStorage, IProject> storage: this.storage2UriMapper.getStorages(fileURI)) {
							descs.add(new ElementDescription(
									storage.getSecond().getName(),
									getQualifiedNameFor((ET) element),
									element));
							break;
						}
					} else {
						final LinkedList<Object> stack = new LinkedList<>();
						stack.add(element);
						final Class<ET> validElementType = getValidEObjectType();
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
														final List<ET> types = EcoreUtil2.getAllContentsOfType(content, validElementType);
														for (final ET elt : types) {
															if (isSelectableElement(elt)) {
																descs.add(new ElementDescription(
																		projectName,
																		getQualifiedNameFor(elt),
																		elt));
															}
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
									descs.add(new ElementDescription(project.getElementName(), qn, type));
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

	/** Search the elements based on the given scope, and select one.
	 * If more than one element was found, the user selects interactively one.
	 *
	 * @param showEmptySelectionError indicates if this function shows an error when the selection is empty.
	 * @param scope the elements to consider for an element type that can be launched.
	 * @return the selected element; or {@code null} if there is no selection.
	 */
	public ElementDescription searchAndSelect(boolean showEmptySelectionError, Object... scope) {
		try {
			final List<ElementDescription> elements = findElements(scope, PlatformUI.getWorkbench().getProgressService());
			ElementDescription element = null;
			if (elements == null || elements.isEmpty()) {
				if (showEmptySelectionError) {
					SARLEclipsePlugin.getDefault().openError(getShell(),
							Messages.AbstractSarlScriptInteractiveSelector_1,
							MessageFormat.format(Messages.AbstractSarlScriptInteractiveSelector_2, getElementLabel()),
							null, null);
				}
			} else if (elements.size() > 1) {
				element = chooseElement(elements);
			}  else {
				element = elements.get(0);
			}
			return element;
		} catch (InterruptedException exception) {
			//
		} catch (Exception exception) {
			SARLEclipsePlugin.getDefault().openError(getShell(), Messages.AbstractSarlScriptInteractiveSelector_1, null,
					null, exception);
		}
		return null;
	}

	/** Replies the text that describes a single element to select.
	 *
	 * @return the text describing a single element to select.
	 */
	protected abstract String getElementLabel();

	/** Replies the long text that describes a single element to select.
	 *
	 * @return the long text describing a single element to select.
	 */
	protected abstract String getElementLongLabel();

	/** Replies the text that describes multiple element to select.
	 *
	 * @return the text describing multiple element to select.
	 */
	protected abstract String getElementsLabel();

	/**
	 * Prompts the user to select an element from the given element types.
	 *
	 * @param elements the element types to choose from.
	 * @return the selected element or {@code null} if none.
	 */
	private ElementDescription chooseElement(List<ElementDescription> elements) {
		final ElementListSelectionDialog dialog = new ElementListSelectionDialog(getShell(),
				new LabelProvider());
		dialog.setElements(elements.toArray());
		dialog.setTitle(MessageFormat.format(Messages.AbstractSarlScriptInteractiveSelector_3, getElementLabel()));
		dialog.setMessage(MessageFormat.format(Messages.AbstractSarlScriptInteractiveSelector_3,
				getElementLongLabel()));
		dialog.setMultipleSelection(false);
		final int result = dialog.open();
		if (result == Window.OK) {
			return (ElementDescription) dialog.getFirstResult();
		}
		return null;
	}

	/** Replies the icon associated to the elements.
	 *
	 * @param element the element for which the icon should be replied, or {@code null} if it is unknown.
	 * @return the icon.
	 */
	protected abstract Image getElementImage(Object element);

	/** Description of an element to launch.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	public static class ElementDescription {

		/** Project name.
		 */
		@SuppressWarnings("checkstyle:visibilitymodifier")
		public final String projectName;

		/** Element fully qualified name.
		 */
		@SuppressWarnings("checkstyle:visibilitymodifier")
		public final String elementName;

		/** Element fully qualified name.
		 */
		@SuppressWarnings("checkstyle:visibilitymodifier")
		public final Object element;

		/** Constructor.
		 *
		 * @param project the name of the project.
		 * @param name the name of the element to launch.
		 * @param element the object.
		 */
		protected ElementDescription(String project, String name, Object element) {
			this.projectName = project;
			this.elementName = name;
			this.element = element;
		}

		@Override
		public String toString() {
			return this.elementName;
		}

	}

	/** Label provider.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class LabelProvider implements ILabelProvider {

		/** Constructor.
		 */
		LabelProvider() {
			//
		}

		@Override
		public void addListener(ILabelProviderListener listener) {
			AbstractSarlScriptInteractiveSelector.this.labelProvider.addListener(listener);
		}

		@Override
		public void dispose() {
			AbstractSarlScriptInteractiveSelector.this.labelProvider.dispose();
		}

		@Override
		public boolean isLabelProperty(Object element, String property) {
			return AbstractSarlScriptInteractiveSelector.this.labelProvider.isLabelProperty(element, property);
		}

		@Override
		public void removeListener(ILabelProviderListener listener) {
			AbstractSarlScriptInteractiveSelector.this.labelProvider.removeListener(listener);
		}

		@Override
		public Image getImage(Object element) {
			if (element instanceof ElementDescription) {
				return getElementImage(((ElementDescription) element).element);
			}
			return AbstractSarlScriptInteractiveSelector.this.labelProvider.getImage(element);
		}

		@Override
		public String getText(Object element) {
			if (element instanceof ElementDescription) {
				return ((ElementDescription) element).elementName;
			}
			return AbstractSarlScriptInteractiveSelector.this.labelProvider.getText(element);
		}

	}

}

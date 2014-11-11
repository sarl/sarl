/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.launching;

import io.sarl.eclipse.SARLConfig;
import io.sarl.eclipse.SARLEclipsePlugin;

import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.debug.ui.actions.ControlAccessibleListener;
import org.eclipse.jdt.internal.debug.ui.launcher.AbstractJavaMainTab;
import org.eclipse.jdt.internal.debug.ui.launcher.DebugTypeSelectionDialog;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

import com.google.common.base.Strings;

/**
 * Class for the main launch configuration tab.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class MainLaunchConfigurationTab extends AbstractJavaMainTab {

	private volatile String lastAgentNameError;
	private Text agentNameTextField;
	private Button agentNameSearchButton;
	private Image image;

	/**
	 */
	public MainLaunchConfigurationTab() {
		//
	}

	@Override
	public Image getImage() {
		if (this.image == null) {
			this.image = SARLEclipsePlugin.getImage(SARLConfig.SARL_LOGO_IMAGE);
		}
		return this.image;
	}

	@Override
	public String getId() {
		return "io.sarl.eclipse.debug.ui.sarlMainTab"; //$NON-NLS-1$
	}

	@Override
	public String getName() {
		return Messages.MainLaunchConfigurationTab_7;
	}

	@Override
	public void createControl(Composite parent) {
		Composite comp = SWTFactory.createComposite(parent, parent.getFont(), 1, 1, GridData.FILL_BOTH);
		((GridLayout) comp.getLayout()).verticalSpacing = 0;
		createProjectEditor(comp);
		createVerticalSpacer(comp, 1);
		createAgentNameEditor(comp, Messages.MainLaunchConfigurationTab_0);
		setControl(comp);
	}

	/**
	 * Creates the widgets for specifying a agent name.
	 *
	 * @param parent - the parent composite.
	 * @param text - the label of the group.
	 */
	protected void createAgentNameEditor(Composite parent, String text) {
		Group group = SWTFactory.createGroup(parent, text, 2, 1, GridData.FILL_HORIZONTAL);
		this.agentNameTextField = SWTFactory.createSingleText(group, 1);
		this.agentNameTextField.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void modifyText(ModifyEvent e) {
				MainLaunchConfigurationTab.this.lastAgentNameError = null;
				updateLaunchConfigurationDialog();
			}
		});
		ControlAccessibleListener.addListener(this.agentNameTextField, group.getText());
		this.agentNameSearchButton = createPushButton(group, Messages.MainLaunchConfigurationTab_1, null);
		this.agentNameSearchButton.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				//
			}
			@Override
			public void widgetSelected(SelectionEvent e) {
				handleAgentNameSearchButtonSelected();
			}
		});
	}

	@Override
	public void initializeFrom(ILaunchConfiguration config) {
		super.initializeFrom(config);
		updateAgentNameFromConfig(config);
	}

	/**
	 * Loads the agent name from the launch configuration's preference store.
	 *
	 * @param config - the config to load the agent name from
	 */
	protected void updateAgentNameFromConfig(ILaunchConfiguration config) {
		String agentName = SARLEclipsePlugin.EMPTY_STRING;
		try {
			agentName = config.getAttribute(
					SARLConfig.ATTR_AGENT_NAME,
					SARLEclipsePlugin.EMPTY_STRING);
		} catch (CoreException ce) {
			SARLEclipsePlugin.log(ce);
		}
		this.agentNameTextField.setText(agentName);
	}

	@Override
	public boolean isValid(ILaunchConfiguration config) {
		setErrorMessage(null);
		setMessage(null);
		return isValidProjectName() && isValidAgentName();
	}

	/** Replies if the agent name is valid.
	 *
	 * @return the validity state.
	 */
	protected boolean isValidAgentName() {
		if (this.lastAgentNameError != null) {
			boolean isValid = Strings.isNullOrEmpty(this.lastAgentNameError);
			if (!isValid) {
				setErrorMessage(this.lastAgentNameError);
			}
			return isValid;
		}
		String name = this.agentNameTextField.getText();
		if (Strings.isNullOrEmpty(name)) {
			this.lastAgentNameError = Messages.MainLaunchConfigurationTab_2;
			setErrorMessage(this.lastAgentNameError);
			return false;
		}
		if (!isAgentNameDefined(name)) {
			this.lastAgentNameError = MessageFormat.format(Messages.MainLaunchConfigurationTab_8, name);
			setErrorMessage(this.lastAgentNameError);
			return false;
		}
		this.lastAgentNameError = SARLEclipsePlugin.EMPTY_STRING;
		return true;
	}

	/** Replies if the project name is valid.
	 * <p>
	 * Copied from JDT.
	 *
	 * @return the validity state.
	 */
	protected boolean isValidProjectName() {
		String name = this.fProjText.getText();
		if (Strings.isNullOrEmpty(name)) {
			setErrorMessage(Messages.MainLaunchConfigurationTab_3);
			return false;
		}
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IStatus status = workspace.validateName(name, IResource.PROJECT);
		if (status.isOK()) {
			IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(name);
			if (!project.exists()) {
				setErrorMessage(MessageFormat.format(
						Messages.MainLaunchConfigurationTab_4, name));
				return false;
			}
			if (!project.isOpen()) {
				setErrorMessage(MessageFormat.format(
						Messages.MainLaunchConfigurationTab_5, name));
				return false;
			}
		} else {
			setErrorMessage(MessageFormat.format(
					Messages.MainLaunchConfigurationTab_6, status.getMessage()));
			return false;
		}
		return true;
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(
				IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
				this.fProjText.getText().trim());
		config.setAttribute(
				SARLConfig.ATTR_AGENT_NAME,
				this.agentNameTextField.getText().trim());
		mapResources(config);
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		IJavaElement javaElement = getContext();
		if (javaElement != null) {
			initializeJavaProject(javaElement, config);
		} else {
			config.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
					SARLEclipsePlugin.EMPTY_STRING);
		}
		initializeAgentName(javaElement, config);
	}

	private String extractNameFromJavaElement(final IJavaElement javaElement) {
		final String[] name = new String[1];
		try {
			getLaunchConfigurationDialog().run(true, true, new IRunnableWithProgress() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void run(IProgressMonitor pm) throws InvocationTargetException {
					try {
						IJavaProject javaProject = javaElement.getJavaProject();
						IType agentType = javaProject.findType("io.sarl.lang.core.Agent"); //$NON-NLS-1$
						IType[] types = agentType.newTypeHierarchy(pm).getAllSubtypes(agentType);
						if (types != null && types.length > 0) {
							name[0] = types[0].getFullyQualifiedName();
						}
					} catch (JavaModelException e) {
						setErrorMessage(e.getLocalizedMessage());
					}
				}
			});
		} catch (Exception e) {
			setErrorMessage(e.getLocalizedMessage());
		}

		return Strings.nullToEmpty(name[0]);
	}

	/**
	 * Initialize the given configuration with the agent name attributes associated
	 * to the given element.
	 *
	 * @param javaElement - the element from which information may be retrieved.
	 * @param config - the config to set with the agent name.
	 */
	protected void initializeAgentName(IJavaElement javaElement, ILaunchConfigurationWorkingCopy config) {
		String name = extractNameFromJavaElement(javaElement);

		// Set the attribute
		config.setAttribute(SARLConfig.ATTR_AGENT_NAME, name);

		// Rename the launch configuration
		if (name.length() > 0) {
			int index = name.lastIndexOf('.');
			if (index > 0) {
				name = name.substring(index + 1);
			}
			name = getLaunchConfigurationDialog().generateName(name);
			config.rename(name);
		}
	}

	private IType[] searchAgentNames() {
		final IType[][] res = new IType[1][];
		res[0] = new IType[0];
		final String projectName = this.fProjText.getText();
		IStatus status = ResourcesPlugin.getWorkspace().validateName(projectName, IResource.PROJECT);
		if (status.isOK()) {
			try {
				getLaunchConfigurationDialog().run(true, true, new IRunnableWithProgress() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void run(IProgressMonitor pm) throws InvocationTargetException {
						try {
							IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
							IJavaProject javaProject = JavaCore.create(project);
							IType agentType = javaProject.findType("io.sarl.lang.core.Agent"); //$NON-NLS-1$
							res[0] = agentType.newTypeHierarchy(pm).getAllSubtypes(agentType);
						} catch (JavaModelException e) {
							setErrorMessage(e.getLocalizedMessage());
						}
					}
				});
			} catch (Exception e) {
				setErrorMessage(e.getLocalizedMessage());
			}
		}
		return res[0];
	}

	private boolean isAgentNameDefined(final String agentName) {
		final String projectName = this.fProjText.getText();
		IStatus status = ResourcesPlugin.getWorkspace().validateName(projectName, IResource.PROJECT);
		if (status.isOK()) {
			try {
				final boolean[] res = new boolean[1];
				getLaunchConfigurationDialog().run(true, true, new IRunnableWithProgress() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void run(IProgressMonitor pm) throws InvocationTargetException {
						try {
							IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
							IJavaProject javaProject = JavaCore.create(project);
							IType agentType = javaProject.findType("io.sarl.lang.core.Agent"); //$NON-NLS-1$
							if (agentType != null) {
								IType type = javaProject.findType(agentName);
								if (type != null) {
									res[0] = type.newSupertypeHierarchy(pm).contains(agentType);
								}
							}
						} catch (JavaModelException e) {
							setErrorMessage(e.getLocalizedMessage());
						}
					}
				});
				return res[0];
			} catch (Exception _) {
				//
			}
		}
		return false;
	}

	/** Invoked when the search button for the agent agent was clocked.
	 */
	protected void handleAgentNameSearchButtonSelected() {
		IType[] types = searchAgentNames();
		// Ask to the user
		DebugTypeSelectionDialog mmsd = new DebugTypeSelectionDialog(getShell(),
				types, ""); //$NON-NLS-1$
		if (mmsd.open() == Window.CANCEL) {
			return;
		}
		IType type = (IType) mmsd.getFirstResult();
		if (type != null) {
			this.agentNameTextField.setText(type.getFullyQualifiedName());
			this.fProjText.setText(type.getJavaProject().getElementName());
		}
	}

}

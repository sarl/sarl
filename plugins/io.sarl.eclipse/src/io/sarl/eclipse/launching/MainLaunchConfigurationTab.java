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
import io.sarl.eclipse.util.Utilities;

import java.lang.ref.SoftReference;
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

	private volatile SoftReference<Image> image;
	private volatile String lastAgentNameError;
	private Text agentNameTextField;
	private Button agentNameSearchButton;
	private Button showLogoOptionButton;
	private Button showLogInfoButton;
	private Button offlineButton;
	private Button defaultContextIdentifierButton;
	private Button randomContextIdentifierButton;
	private Button bootContextIdentifierButton;

	private final WidgetListener defaultListener = new WidgetListener();

	/**
	 */
	public MainLaunchConfigurationTab() {
		//
	}

	@Override
	public Image getImage() {
		Image img = (this.image == null) ? null : this.image.get();
		if (img == null) {
			img = SARLEclipsePlugin.getDefault().getImage(SARLConfig.SARL_LOGO_IMAGE);
			this.image = new SoftReference<>(img);
		}
		return img;
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
		createVerticalSpacer(comp, 1);
		createContextIdentifierTypeEditor(comp, Messages.MainLaunchConfigurationTab_9);
		createVerticalSpacer(comp, 1);
		createLaunchOptionEditor(comp, Messages.MainLaunchConfigurationTab_10);
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

	/**
	 * Creates the widgets for configuring the context identifier.
	 *
	 * @param parent - the parent composite.
	 * @param text - the label of the group.
	 */
	protected void createContextIdentifierTypeEditor(Composite parent, String text) {
		Group group = SWTFactory.createGroup(parent, text, 1, 1, GridData.FILL_HORIZONTAL);
		this.defaultContextIdentifierButton = createRadioButton(group, Messages.MainLaunchConfigurationTab_11);
		this.defaultContextIdentifierButton.addSelectionListener(this.defaultListener);
		this.randomContextIdentifierButton = createRadioButton(group, Messages.MainLaunchConfigurationTab_12);
		this.randomContextIdentifierButton.addSelectionListener(this.defaultListener);
		this.bootContextIdentifierButton = createRadioButton(group, Messages.MainLaunchConfigurationTab_13);
		this.bootContextIdentifierButton.addSelectionListener(this.defaultListener);
	}

	/** Replies the type of context identifier selected by the user.
	 *
	 * @return the type of context identifier.
	 */
	protected RootContextIdentifierType getSelectedContextIdentifierType() {
		if (this.randomContextIdentifierButton.getSelection()) {
			return RootContextIdentifierType.RANDOM_CONTEXT_ID;
		}
		if (this.bootContextIdentifierButton.getSelection()) {
			return RootContextIdentifierType.BOOT_AGENT_CONTEXT_ID;
		}
		return RootContextIdentifierType.DEFAULT_CONTEXT_ID;
	}

	/**
	 * Creates the widgets for configuring the launch options.
	 *
	 * @param parent - the parent composite.
	 * @param text - the label of the group.
	 */
	protected void createLaunchOptionEditor(Composite parent, String text) {
		Group group = SWTFactory.createGroup(parent, text, 1, 1, GridData.FILL_HORIZONTAL);
		this.showLogoOptionButton = createCheckButton(group, Messages.MainLaunchConfigurationTab_14);
		this.showLogoOptionButton.addSelectionListener(this.defaultListener);
		this.showLogInfoButton = createCheckButton(group, Messages.MainLaunchConfigurationTab_15);
		this.showLogInfoButton.addSelectionListener(this.defaultListener);
		this.offlineButton = createCheckButton(group, Messages.MainLaunchConfigurationTab_16);
		this.offlineButton.addSelectionListener(this.defaultListener);
	}

	@Override
	public void initializeFrom(ILaunchConfiguration config) {
		super.initializeFrom(config);
		updateAgentNameFromConfig(config);
		updateContextIdentifierTypeFromConfig(config);
		updateLaunchOptionsFromConfig(config);
	}

	/**
	 * Loads the context identifier type from the launch configuration's preference store.
	 *
	 * @param config - the config to load the agent name from
	 */
	protected void updateContextIdentifierTypeFromConfig(ILaunchConfiguration config) {
		RootContextIdentifierType type = RootContextIdentifierType.DEFAULT_CONTEXT_ID;
		try {
			String typeName = config.getAttribute(SARLConfig.ATTR_ROOT_CONTEXT_ID_TYPE, (String) null);
			if (!Strings.isNullOrEmpty(typeName)) {
				type = RootContextIdentifierType.valueOf(typeName);
			}
		} catch (Exception ce) {
			SARLEclipsePlugin.getDefault().log(ce);
		}
		assert (type != null);
		switch (type) {
		case RANDOM_CONTEXT_ID:
			this.randomContextIdentifierButton.setSelection(true);
			break;
		case BOOT_AGENT_CONTEXT_ID:
			this.bootContextIdentifierButton.setSelection(true);
			break;
		case DEFAULT_CONTEXT_ID:
		default:
			this.defaultContextIdentifierButton.setSelection(true);
			break;
		}
	}

	/**
	 * Loads the launch options from the launch configuration's preference store.
	 *
	 * @param config - the config to load the agent name from
	 */
	protected void updateLaunchOptionsFromConfig(ILaunchConfiguration config) {
		boolean showLogo = false;
		try {
			showLogo = config.getAttribute(SARLConfig.ATTR_SHOW_LOGO_OPTION, false);
		} catch (CoreException ce) {
			SARLEclipsePlugin.getDefault().log(ce);
		}
		boolean showLogInfo = true;
		try {
			showLogInfo = config.getAttribute(SARLConfig.ATTR_SHOW_LOG_INFO, false);
		} catch (CoreException ce) {
			SARLEclipsePlugin.getDefault().log(ce);
		}
		boolean offline = true;
		try {
			offline = config.getAttribute(SARLConfig.ATTR_SRE_OFFLINE, true);
		} catch (CoreException ce) {
			SARLEclipsePlugin.getDefault().log(ce);
		}
		this.showLogoOptionButton.setSelection(showLogo);
		this.showLogInfoButton.setSelection(showLogInfo);
		this.offlineButton.setSelection(offline);
	}

	/**
	 * Loads the agent name from the launch configuration's preference store.
	 *
	 * @param config - the config to load the agent name from
	 */
	protected void updateAgentNameFromConfig(ILaunchConfiguration config) {
		String agentName = null;
		try {
			agentName = config.getAttribute(
					SARLConfig.ATTR_AGENT_NAME,
					(String) null);
		} catch (CoreException ce) {
			SARLEclipsePlugin.getDefault().log(ce);
		}
		this.agentNameTextField.setText(Strings.nullToEmpty(agentName));
	}

	@Override
	public boolean isValid(ILaunchConfiguration config) {
		setErrorMessage(null);
		setMessage(null);
		return isValidProjectName() && isValidAgentName()
				&& isValidContextIdentifierType() && isValidLaunchOptions();
	}

	/** Replies if the context identfiier type is valid.
	 *
	 * @return the validity state.
	 */
	@SuppressWarnings("static-method")
	protected boolean isValidContextIdentifierType() {
		return true;
	}

	/** Replies if the launch options are valid.
	 *
	 * @return the validity state.
	 */
	@SuppressWarnings("static-method")
	protected boolean isValidLaunchOptions() {
		return true;
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
		this.lastAgentNameError = Utilities.EMPTY_STRING;
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
				Strings.emptyToNull(this.fProjText.getText().trim()));
		config.setAttribute(
				SARLConfig.ATTR_AGENT_NAME,
				Strings.emptyToNull(this.agentNameTextField.getText().trim()));
		config.setAttribute(
				SARLConfig.ATTR_ROOT_CONTEXT_ID_TYPE,
				getSelectedContextIdentifierType().name());
		config.setAttribute(
				SARLConfig.ATTR_SHOW_LOGO_OPTION,
				this.showLogoOptionButton.getSelection());
		config.setAttribute(
				SARLConfig.ATTR_SHOW_LOG_INFO,
				this.showLogInfoButton.getSelection());
		config.setAttribute(
				SARLConfig.ATTR_SRE_OFFLINE,
				this.offlineButton.getSelection());
		mapResources(config);
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		IJavaElement javaElement = getContext();
		if (javaElement != null) {
			initializeJavaProject(javaElement, config);
		} else {
			config.removeAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME);
		}
		initializeAgentName(javaElement, config);
		initializeContextIdentifierType(config);
		initializeLaunchOptions(config);
	}

	/**
	 * Reset the given configuration with the context identifier type.
	 *
	 * @param config - the config to set with the context identifier type.
	 */
	@SuppressWarnings("static-method")
	protected void initializeContextIdentifierType(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(SARLConfig.ATTR_ROOT_CONTEXT_ID_TYPE, RootContextIdentifierType.DEFAULT_CONTEXT_ID.name());
	}

	/**
	 * Reset the given configuration with the launch options.
	 *
	 * @param config - the config to set with the launch options.
	 */
	@SuppressWarnings("static-method")
	protected void initializeLaunchOptions(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(SARLConfig.ATTR_SHOW_LOGO_OPTION, false);
		config.setAttribute(SARLConfig.ATTR_SHOW_LOG_INFO, true);
		config.setAttribute(SARLConfig.ATTR_SRE_OFFLINE, true);
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
	 * Reset the given configuration with the agent name attributes associated
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

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class WidgetListener implements SelectionListener {

		/**
		 */
		public WidgetListener() {
			//
		}

		@Override
		public void widgetDefaultSelected(SelectionEvent e) {
			//
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void widgetSelected(SelectionEvent e) {
			updateLaunchConfigurationDialog();
		}

	}

}

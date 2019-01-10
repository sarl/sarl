/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.eclipse.launching.dialog;

import java.lang.ref.SoftReference;
import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;

import javax.inject.Inject;

import com.google.common.base.Strings;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
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
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
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

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;
import io.sarl.eclipse.launching.config.RootContextIdentifierType;
import io.sarl.eclipse.util.Utilities;

/**
 * The main launch configuration tab.
 *
 * <p>This configuration tab enables to enter the name of the agent to launch,
 * the launching parameters.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLAgentMainLaunchConfigurationTab extends AbstractJavaMainTab {

	private volatile SoftReference<Image> image;

	private volatile String lastAgentNameError;

	private Text agentNameTextField;

	private Button agentNameSearchButton;

	private Button showLogoOptionButton;

	private Button showLogInfoButton;

	private Button offlineButton;

	private Button runInEclipseButton;

	private Button enableAssertionsInDebugModeButton;

	private Button enableAssertionsInRunModeButton;

	private Button defaultContextIdentifierButton;

	private Button randomContextIdentifierButton;

	private Button bootContextIdentifierButton;

	private final WidgetListener defaultListener = new WidgetListener();

	@Inject
	private ILaunchConfigurationConfigurator configurator;

	@Inject
	private ILaunchConfigurationAccessor accessor;

	/** Construct a main configuration tab for SARL agent.
	 */
	public SARLAgentMainLaunchConfigurationTab() {
		//
	}

	@Override
	public Image getImage() {
		Image img = (this.image == null) ? null : this.image.get();
		if (img == null) {
			img = SARLEclipsePlugin.getDefault().getImage(SARLEclipseConfig.SARL_LOGO_IMAGE);
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
		final Composite comp = SWTFactory.createComposite(parent, parent.getFont(), 1, 1, GridData.FILL_BOTH);
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
	 * @param parent the parent composite.
	 * @param text the label of the group.
	 */
	protected void createAgentNameEditor(Composite parent, String text) {
		final Group group = SWTFactory.createGroup(parent, text, 2, 1, GridData.FILL_HORIZONTAL);
		this.agentNameTextField = SWTFactory.createSingleText(group, 1);
		this.agentNameTextField.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void modifyText(ModifyEvent event) {
				SARLAgentMainLaunchConfigurationTab.this.lastAgentNameError = null;
				updateLaunchConfigurationDialog();
			}
		});
		ControlAccessibleListener.addListener(this.agentNameTextField, group.getText());
		this.agentNameSearchButton = createPushButton(group, Messages.MainLaunchConfigurationTab_1, null);
		this.agentNameSearchButton.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(SelectionEvent event) {
				//
			}

			@Override
			public void widgetSelected(SelectionEvent event) {
				handleAgentNameSearchButtonSelected();
			}
		});
	}

	/**
	 * Creates the widgets for configuring the context identifier.
	 *
	 * @param parent the parent composite.
	 * @param text the label of the group.
	 */
	protected void createContextIdentifierTypeEditor(Composite parent, String text) {
		final Group group = SWTFactory.createGroup(parent, text, 1, 1, GridData.FILL_HORIZONTAL);
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
	 * @param parent the parent composite.
	 * @param text the label of the group.
	 */
	protected void createLaunchOptionEditor(Composite parent, String text) {
		final Group group = SWTFactory.createGroup(parent, text, 1, 1, GridData.FILL_HORIZONTAL);
		this.showLogoOptionButton = createCheckButton(group, Messages.MainLaunchConfigurationTab_14);
		this.showLogoOptionButton.addSelectionListener(this.defaultListener);
		this.showLogInfoButton = createCheckButton(group, Messages.MainLaunchConfigurationTab_15);
		this.showLogInfoButton.addSelectionListener(this.defaultListener);
		this.offlineButton = createCheckButton(group, Messages.MainLaunchConfigurationTab_16);
		this.offlineButton.addSelectionListener(this.defaultListener);
		this.enableAssertionsInRunModeButton = createCheckButton(group, Messages.SARLMainLaunchConfigurationTab_2);
		this.enableAssertionsInRunModeButton.addSelectionListener(this.defaultListener);
		this.enableAssertionsInDebugModeButton = createCheckButton(group, Messages.SARLMainLaunchConfigurationTab_1);
		this.enableAssertionsInDebugModeButton.addSelectionListener(this.defaultListener);
		this.runInEclipseButton = createCheckButton(group, Messages.SARLMainLaunchConfigurationTab_0);
		this.runInEclipseButton.addSelectionListener(this.defaultListener);
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
	 * @param config the config to load the agent name from
	 */
	protected void updateContextIdentifierTypeFromConfig(ILaunchConfiguration config) {
		final RootContextIdentifierType type = this.accessor.getDefaultContextIdentifier(config);
		assert type != null;
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
	 * @param config the config to load the agent name from
	 */
	protected void updateLaunchOptionsFromConfig(ILaunchConfiguration config) {
		final boolean showLogo = this.accessor.getShowLogoFlag(config);
		final boolean showLogInfo = this.accessor.getShowLogInfoFlag(config);
		final boolean offline = this.accessor.getOfflineFlag(config);
		final boolean runInEclipse = this.accessor.isEmbeddedSRE(config);
		final boolean enableAssertionsRun = this.accessor.isAssertionEnabledInRunMode(config);
		final boolean enableAssertionsDebug = this.accessor.isAssertionEnabledInDebugMode(config);
		this.showLogoOptionButton.setSelection(showLogo);
		this.showLogInfoButton.setSelection(showLogInfo);
		this.offlineButton.setSelection(offline);
		this.enableAssertionsInRunModeButton.setSelection(enableAssertionsRun);
		this.enableAssertionsInDebugModeButton.setSelection(enableAssertionsDebug);
		this.runInEclipseButton.setSelection(runInEclipse);
	}

	/**
	 * Loads the agent name from the launch configuration's preference store.
	 *
	 * @param config the config to load the agent name from
	 */
	protected void updateAgentNameFromConfig(ILaunchConfiguration config) {
		final String agentName = this.accessor.getAgent(config);
		this.agentNameTextField.setText(Strings.nullToEmpty(agentName));
	}

	@Override
	public boolean isValid(ILaunchConfiguration config) {
		setErrorMessage(null);
		setMessage(null);
		return isValidProjectName() && isValidAgentName()
				&& isValidContextIdentifierType() && isValidLaunchOptions();
	}

	/** Replies if the context identifier type is valid.
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
			final boolean isValid = Strings.isNullOrEmpty(this.lastAgentNameError);
			if (!isValid) {
				setErrorMessage(this.lastAgentNameError);
			}
			return isValid;
		}
		final String name = this.agentNameTextField.getText();
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
	 *
	 * <p>Copied from JDT.
	 *
	 * @return the validity state.
	 */
	protected boolean isValidProjectName() {
		final String name = this.fProjText.getText();
		if (Strings.isNullOrEmpty(name)) {
			setErrorMessage(Messages.MainLaunchConfigurationTab_3);
			return false;
		}
		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		final IStatus status = workspace.validateName(name, IResource.PROJECT);
		if (status.isOK()) {
			final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(name);
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
		this.configurator.setProjectName(config, Strings.emptyToNull(this.fProjText.getText().trim()));
		this.configurator.setAgent(config, Strings.emptyToNull(this.agentNameTextField.getText().trim()));
		this.configurator.setDefaultContextIdentifier(config, getSelectedContextIdentifierType());
		this.configurator.setLaunchingFlags(config,
				this.showLogoOptionButton.getSelection(),
				this.showLogInfoButton.getSelection(),
				this.offlineButton.getSelection());
		this.configurator.setAssertionEnabledInRunMode(config, this.enableAssertionsInRunModeButton.getSelection());
		this.configurator.setAssertionEnabledInDebugMode(config, this.enableAssertionsInDebugModeButton.getSelection());
		this.configurator.setEmbeddedSRE(config, this.runInEclipseButton.getSelection());
		mapResources(config);
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		final IJavaElement javaElement = getContext();
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
	 * @param config the config to set with the context identifier type.
	 */
	protected void initializeContextIdentifierType(ILaunchConfigurationWorkingCopy config) {
		this.configurator.setDefaultContextIdentifier(config, null);
	}

	/**
	 * Reset the given configuration with the launch options.
	 *
	 * @param config the config to set with the launch options.
	 */
	protected void initializeLaunchOptions(ILaunchConfigurationWorkingCopy config) {
		this.configurator.setLaunchingFlags(config, null, null, null);
	}

	private String extractNameFromJavaElement(final IJavaElement javaElement) {
		String name = null;
		if (javaElement != null) {
			final String[] nameRef = new String[1];
			try {
				getLaunchConfigurationDialog().run(true, true, new IRunnableWithProgress() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void run(IProgressMonitor pm) throws InvocationTargetException {
						try {
							final IJavaProject javaProject = javaElement.getJavaProject();
							final IType agentType = javaProject.findType("io.sarl.lang.core.Agent"); //$NON-NLS-1$
							final IType[] types = agentType.newTypeHierarchy(pm).getAllSubtypes(agentType);
							if (types != null && types.length > 0) {
								nameRef[0] = types[0].getFullyQualifiedName();
							}
						} catch (JavaModelException e) {
							setErrorMessage(e.getLocalizedMessage());
							JDIDebugUIPlugin.log(e);
						}
					}
				});
			} catch (Exception e) {
				setErrorMessage(e.getLocalizedMessage());
				JDIDebugUIPlugin.log(e);
			}
			name = nameRef[0];
		}
		return Strings.nullToEmpty(name);
	}

	/**
	 * Reset the given configuration with the agent name attributes associated
	 * to the given element.
	 *
	 * @param javaElement the element from which information may be retrieved.
	 * @param config the config to set with the agent name.
	 */
	protected void initializeAgentName(IJavaElement javaElement, ILaunchConfigurationWorkingCopy config) {
		String name = extractNameFromJavaElement(javaElement);

		// Set the attribute
		this.configurator.setAgent(config, name);

		// Rename the launch configuration
		if (name.length() > 0) {
			final int index = name.lastIndexOf('.');
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
		final IStatus status = ResourcesPlugin.getWorkspace().validateName(projectName, IResource.PROJECT);
		if (status.isOK()) {
			try {
				getLaunchConfigurationDialog().run(true, true, new IRunnableWithProgress() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void run(IProgressMonitor pm) throws InvocationTargetException {
						try {
							final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
							final IJavaProject javaProject = JavaCore.create(project);
							final IType agentType = javaProject.findType("io.sarl.lang.core.Agent"); //$NON-NLS-1$
							res[0] = agentType.newTypeHierarchy(pm).getAllSubtypes(agentType);
						} catch (JavaModelException e) {
							setErrorMessage(e.getLocalizedMessage());
							JDIDebugUIPlugin.log(e);
						}
					}
				});
			} catch (Exception e) {
				setErrorMessage(e.getLocalizedMessage());
				JDIDebugUIPlugin.log(e);
			}
		}
		return res[0];
	}

	private boolean isAgentNameDefined(final String agentName) {
		final String projectName = this.fProjText.getText();
		final IStatus status = ResourcesPlugin.getWorkspace().validateName(projectName, IResource.PROJECT);
		if (status.isOK()) {
			try {
				final boolean[] res = new boolean[1];
				getLaunchConfigurationDialog().run(true, true, new IRunnableWithProgress() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void run(IProgressMonitor pm) throws InvocationTargetException {
						try {
							final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
							final IJavaProject javaProject = JavaCore.create(project);
							final IType agentType = javaProject.findType("io.sarl.lang.core.Agent"); //$NON-NLS-1$
							if (agentType != null) {
								final IType type = javaProject.findType(agentName);
								if (type != null) {
									res[0] = type.newSupertypeHierarchy(pm).contains(agentType);
								}
							}
						} catch (JavaModelException e) {
							setErrorMessage(e.getLocalizedMessage());
							JDIDebugUIPlugin.log(e);
						}
					}
				});
				return res[0];
			} catch (Exception exception) {
				//
			}
		}
		return false;
	}

	/** Invoked when the search button for the agent agent was clocked.
	 */
	protected void handleAgentNameSearchButtonSelected() {
		final IType[] types = searchAgentNames();
		// Ask to the user
		final DebugTypeSelectionDialog mmsd = new DebugTypeSelectionDialog(getShell(),
				types, ""); //$NON-NLS-1$
		if (mmsd.open() == Window.CANCEL) {
			return;
		}
		final IType type = (IType) mmsd.getFirstResult();
		if (type != null) {
			this.agentNameTextField.setText(type.getFullyQualifiedName());
			this.fProjText.setText(type.getJavaProject().getElementName());
		}
	}

	/** Listener of events in internal components for refreshing the tab.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class WidgetListener implements SelectionListener {

		WidgetListener() {
			//
		}

		@Override
		public void widgetDefaultSelected(SelectionEvent event) {
			//
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void widgetSelected(SelectionEvent event) {
			updateLaunchConfigurationDialog();
		}

	}

}

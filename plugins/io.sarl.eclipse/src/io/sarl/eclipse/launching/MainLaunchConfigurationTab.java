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

import io.sarl.eclipse.util.PluginUtil;

import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.core.IClassFile;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaModel;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.internal.debug.ui.actions.ControlAccessibleListener;
import org.eclipse.jdt.internal.debug.ui.launcher.AbstractJavaMainTab;
import org.eclipse.jdt.internal.debug.ui.launcher.DebugTypeSelectionDialog;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
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
			this.image = PluginUtil.getImage(PluginUtil.SARL_LOGO_IMAGE);
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
		String agentName = PluginUtil.EMPTY_STRING;
		try {
			agentName = config.getAttribute(
					LaunchConfigurationConstants.ATTR_AGENT_NAME,
					PluginUtil.EMPTY_STRING);
		} catch (CoreException ce) {
			PluginUtil.log(ce);
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
		this.lastAgentNameError = PluginUtil.EMPTY_STRING;
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
				LaunchConfigurationConstants.ATTR_AGENT_NAME,
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
					PluginUtil.EMPTY_STRING);
		}
		initializeAgentName(javaElement, config);
	}

	private String extractNameFromJavaElement(IJavaElement javaElement) {
		String name = null;
		IJavaElement jElement = javaElement;

		if (jElement instanceof IMember) {
			IMember member = (IMember) jElement;
			if (member.isBinary()) {
				jElement = member.getClassFile();
			} else {
				jElement = member.getCompilationUnit();
			}
		}

		// Search for the agent name
		if (jElement instanceof ICompilationUnit || jElement instanceof IClassFile) {
			try {
				IJavaSearchScope scope = SearchEngine.createJavaSearchScope(
						new IJavaElement[] {jElement},
						false);
				AgentTypeSearchEngine engine = new AgentTypeSearchEngine();
				IType[] types = engine.searchAgentTypes(
						getLaunchConfigurationDialog(),
						scope);
				if (types != null && (types.length > 0)) {
					// Simply grab the first main type found in the searched element
					name = types[0].getFullyQualifiedName();
				}
			} catch (InterruptedException ie) {
				PluginUtil.log(ie);
			} catch (InvocationTargetException ite) {
				PluginUtil.log(ite);
			}
		}

		return Strings.nullToEmpty(name);
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
		config.setAttribute(LaunchConfigurationConstants.ATTR_AGENT_NAME, name);

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

	private IJavaElement[] extractElementsFromProject() {
		IJavaProject project = getJavaProject();
		IJavaElement[] elements = null;
		if ((project == null) || !project.exists()) {
			IJavaModel model = JavaCore.create(ResourcesPlugin.getWorkspace().getRoot());
			if (model != null) {
				try {
					elements = model.getJavaProjects();
				} catch (JavaModelException e) {
					PluginUtil.log(e);
				}
			}
		} else {
			elements = new IJavaElement[] {project};
		}
		if (elements == null) {
			elements = new IJavaElement[]{};
		}
		return elements;
	}

	private IType[] searchAgentNames() {
		IJavaElement[] elements = extractElementsFromProject();
		int constraints = IJavaSearchScope.SOURCES;
		constraints |= IJavaSearchScope.APPLICATION_LIBRARIES;
		IJavaSearchScope searchScope = SearchEngine.createJavaSearchScope(elements, constraints);
		AgentTypeSearchEngine engine = new AgentTypeSearchEngine();
		IType[] types;
		try {
			types = engine.searchAgentTypes(
					getLaunchConfigurationDialog(),
					searchScope);
			if (types == null) {
				types = new IType[0];
			}
			return types;
		} catch (InvocationTargetException e) {
			setErrorMessage(e.getMessage());
			return null;
		} catch (InterruptedException e) {
			setErrorMessage(e.getMessage());
			return null;
		}
	}

	private boolean isAgentNameDefined(String agentName) {
		IJavaElement[] elements = extractElementsFromProject();
		int constraints = IJavaSearchScope.SOURCES;
		constraints |= IJavaSearchScope.APPLICATION_LIBRARIES;
		IJavaSearchScope searchScope = SearchEngine.createJavaSearchScope(elements, constraints);
		AgentTypeSearchEngine engine = new AgentTypeSearchEngine();
		IType type;
		try {
			type = engine.searchAgentType(
					getLaunchConfigurationDialog(),
					searchScope,
					agentName);
			return type != null;
		} catch (InvocationTargetException e) {
			setErrorMessage(e.getMessage());
			return false;
		} catch (InterruptedException e) {
			setErrorMessage(e.getMessage());
			return false;
		}
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

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
package io.sarl.eclipse.launch;

import io.sarl.eclipse.util.PluginUtil;

import java.lang.reflect.InvocationTargetException;

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
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.internal.debug.ui.actions.ControlAccessibleListener;
import org.eclipse.jdt.internal.debug.ui.launcher.AbstractJavaMainTab;
import org.eclipse.jdt.internal.debug.ui.launcher.DebugTypeSelectionDialog;
import org.eclipse.jdt.internal.debug.ui.launcher.LauncherMessages;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jface.window.Window;
import org.eclipse.osgi.util.NLS;
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

/**
 * Class for the main launch configuration tab.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class MainLaunchConfigurationTab extends AbstractJavaMainTab {

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
		return LauncherMessages.JavaMainTab__Main_19;
	}

	@Override
	public void createControl(Composite parent) {
		Composite comp = SWTFactory.createComposite(parent, parent.getFont(), 1, 1, GridData.FILL_BOTH);
		((GridLayout) comp.getLayout()).verticalSpacing = 0;
		createProjectEditor(comp);
		createVerticalSpacer(comp, 1);
		// TODO: Use NLS
		createAgentNameEditor(comp, "Agent qualified name:"); //$NON-NLS-1$
		setControl(comp);
		// TODO: Add help context
		//PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(),
		//IJavaDebugHelpContextIds.LAUNCH_CONFIGURATION_DIALOG_MAIN_TAB);
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
				updateLaunchConfigurationDialog();
			}
		});
		ControlAccessibleListener.addListener(this.agentNameTextField, group.getText());
		// TODO: Use NLS.
		this.agentNameSearchButton = createPushButton(group, "Search...", null); //$NON-NLS-1$
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
		String agentName = EMPTY_STRING;
		try {
			agentName = config.getAttribute(
					LaunchConfigurationConstants.ATTR_AGENT_NAME,
					EMPTY_STRING);
		} catch (CoreException ce) {
			JDIDebugUIPlugin.log(ce);
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
		String name = this.agentNameTextField.getText().trim();
		if (name.isEmpty()) {
			// TODO: Use NLS.
			setErrorMessage("You must specify the name of the agent to launch");  //$NON-NLS-1$
			return false;
		}
		return true;
	}

	/** Replies if the project name is valid.
	 * <p>
	 * Copied from JDT.
	 *
	 * @return the validity state.
	 */
	protected boolean isValidProjectName() {
		String name = this.fProjText.getText().trim();
		if (name.isEmpty()) {
			//TODO: Use NLS. See below for example.
			setErrorMessage("You must specify the project to launch.");  //$NON-NLS-1$
			return false;
		}
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IStatus status = workspace.validateName(name, IResource.PROJECT);
		if (status.isOK()) {
			IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(name);
			if (!project.exists()) {
				setErrorMessage(NLS.bind(LauncherMessages.JavaMainTab_20, new String[] {name}));
				return false;
			}
			if (!project.isOpen()) {
				setErrorMessage(NLS.bind(LauncherMessages.JavaMainTab_21, new String[] {name}));
				return false;
			}
		} else {
			setErrorMessage(NLS.bind(LauncherMessages.JavaMainTab_19,
					new String[]{status.getMessage()}));
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
					EMPTY_STRING);
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
				JDIDebugUIPlugin.log(ie);
			} catch (InvocationTargetException ite) {
				JDIDebugUIPlugin.log(ite);
			}
		}

		if (name == null) {
			name = EMPTY_STRING;
		}

		return name;
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
					JDIDebugUIPlugin.log(e);
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

	/** Invoked when the search button for the agent agent was clocked.
	 */
	protected void handleAgentNameSearchButtonSelected() {
		IType[] types = searchAgentNames();
		// Ask to the user
		DebugTypeSelectionDialog mmsd = new DebugTypeSelectionDialog(getShell(),
				types, LauncherMessages.JavaMainTab_Choose_Main_Type_11);
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

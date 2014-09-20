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

import java.io.File;
import java.io.IOException;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaJRETab;
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.internal.debug.ui.actions.ControlAccessibleListener;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.PlatformUI;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

/**
 * Class for the configuration tab for the JRE and the SARL runtime environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class RuntimeEnvironmentTab extends JavaJRETab {

	/** Empty string.
	 */
	protected static final String EMPTY_STRING = ""; //$NON-NLS-1$

	private String runtimeEnvironmentPath;
	private Label runtimeEnvironmentLabel;
	private Button runtimeEnvironmentSearchButton;

	/**
	 */
	public RuntimeEnvironmentTab() {
		//
	}

	@Override
	public String getName() {
		// TODO: Use NLS.
		return "Runtime Environment";  //$NON-NLS-1$
	}

	@Override
	public String getId() {
		return "io.sarl.eclipse.debug.ui.sarlRuntimeEnvironmentTab"; //$NON-NLS-1$
	}

	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);
		Composite oldComp = (Composite) getControl();
		Control[] children = oldComp.getChildren();
		Composite topComp = SWTFactory.createComposite(
				parent, parent.getFont(), 1, 1, GridData.FILL_HORIZONTAL);
		// TODO: Use NLS.
		createSARLRuntimeEnvironmentEditor(
				topComp,
				"SARL runtime environment:"); //$NON-NLS-1$
		for (Control ctl : children) {
			ctl.setParent(topComp);
		}
		setControl(topComp);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), getHelpContextId());
	}

	/**
	 * Creates the widgets for specifying a SARL runtime environment.
	 *
	 * @param parent - the parent composite.
	 * @param text - the label of the group.
	 */
	protected void createSARLRuntimeEnvironmentEditor(Composite parent, String text) {
		Group group = SWTFactory.createGroup(parent, text, 2, 1, GridData.FILL_HORIZONTAL);
		this.runtimeEnvironmentLabel = SWTFactory.createWrapLabel(group, EMPTY_STRING, 1);
		ControlAccessibleListener.addListener(this.runtimeEnvironmentLabel, group.getText());
		// TODO: Use NLS.
		this.runtimeEnvironmentSearchButton = createPushButton(group, "Browse...", null); //$NON-NLS-1$
		this.runtimeEnvironmentSearchButton.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				//
			}
			@Override
			public void widgetSelected(SelectionEvent e) {
				handleSARLRuntimeEnvironmentSearchButtonSelected();
			}
		});
	}

	/** Change the path to the SARL runtime environment.
	 *
	 * @param path - the path to the runtime environment
	 */
	protected void setSARLRuntimeEnvironment(String path) {
		String p = path;
		if (p == null) {
			p = EMPTY_STRING;
		}
		if (!p.equals(this.runtimeEnvironmentPath)) {
			this.runtimeEnvironmentPath = p;
			IPath thePath = new Path(p);
			thePath = thePath.removeFileExtension();
			thePath = thePath.removeFirstSegments(thePath.segmentCount() - 1);
			this.runtimeEnvironmentLabel.setText(thePath.toString());
			this.runtimeEnvironmentLabel.setToolTipText(p);
			updateLaunchConfigurationDialog();
		}
	}

	/** Change the path to the SARL runtime environment.
	 *
	 * @return the path of the SARL runtime environment, or {@link #EMPTY_STRING}.
	 */
	protected String getSARLRuntimeEnvironment() {
		return this.runtimeEnvironmentPath == null ? EMPTY_STRING : this.runtimeEnvironmentPath;
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		super.initializeFrom(configuration);
		updateSARLRuntimeEnvironmentFromConfig(configuration);
	}

	/**
	 * Loads the SARL runtime environment from the launch configuration's preference store.
	 *
	 * @param config - the config to load the runtime environment from
	 */
	protected void updateSARLRuntimeEnvironmentFromConfig(ILaunchConfiguration config) {
		String rteName = EMPTY_STRING;
		try {
			rteName = config.getAttribute(
					LaunchConfigurationConstants.ATTR_SARL_RUNTIME_ENVIRONMENT,
					EMPTY_STRING);
		} catch (CoreException ce) {
			JDIDebugUIPlugin.log(ce);
		}
		setSARLRuntimeEnvironment(rteName);
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		super.setDefaults(config);
		initializeSARLRuntimeEnvironment(getContext(), config);
	}

	/**
	 * Initialize the given configuration with the SARL runtime environment
	 * attributes associated to the given element.
	 *
	 * @param javaElement - the element from which information may be retrieved.
	 * @param config - the config to set with the SARL runtime environment.
	 */
	@SuppressWarnings("static-method")
	protected void initializeSARLRuntimeEnvironment(IJavaElement javaElement, ILaunchConfigurationWorkingCopy config) {
		// FIXME: Get the runtime from the general preferences.
		String jarFile = EMPTY_STRING;
		config.setAttribute(LaunchConfigurationConstants.ATTR_SARL_RUNTIME_ENVIRONMENT, jarFile);

		String mainType = EMPTY_STRING;
		config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME, mainType);
	}

	@Override
	public boolean isValid(ILaunchConfiguration config) {
		return super.isValid(config) && isValidSARLRuntimeEnvironment(config);
	}

	private boolean validateMainClass(Manifest manifest) {
		String mainClass = manifest.getMainAttributes().getValue("Main-Class"); //$NON-NLS-1$
		if (mainClass == null || mainClass.isEmpty()) {
			// TODO: Use NLS.
			setErrorMessage("The Jar file of the SARL runtime environment is not runnable.");  //$NON-NLS-1$
			return false;
		}
		return true;
	}

	private boolean validateSARLSpecificationVersion(Manifest manifest) {
		String sarlSpec = manifest.getMainAttributes().getValue("SARL-Spec-Version"); //$NON-NLS-1$
		if (sarlSpec == null || sarlSpec.isEmpty()) {
			// TODO: Use NLS.
			setErrorMessage("The Jar file is not a SARL runtime environment");  //$NON-NLS-1$
			return false;
		}
		Bundle bundle = Platform.getBundle("io.sarl.lang"); //$NON-NLS-1$
		if (bundle != null) {
			Version sarlVersion = bundle.getVersion();
			Version specVersion = new Version(sarlSpec);
			if (specVersion.getMajor() != sarlVersion.getMajor()
					|| (specVersion.getMajor() == sarlVersion.getMajor()
					&& specVersion.getMinor() != sarlVersion.getMinor())) {
				setErrorMessage(String.format(
						"Incompatible SARL specification version of the runtime environment: {0} != {1}.", //$NON-NLS-1$
						specVersion.toString(),
						sarlVersion.toString()));
				return false;
			}
		}
		return true;
	}

	/** Replies if the SARL runtime environment is valid.
	 *
	 * @param config - the current configuration.
	 * @return the validity state.
	 */
	protected boolean isValidSARLRuntimeEnvironment(ILaunchConfiguration config) {
		File file = getSARLRuntimeEnvironmentFile();
		if (file == null) {
			// TODO: Use NLS.
			setErrorMessage("You must specify the SARL runtime environment.");  //$NON-NLS-1$
			return false;
		}
		if (file.canRead()) {
			try (JarFile jFile = new JarFile(file)) {
				Manifest manifest = jFile.getManifest();
				return validateMainClass(manifest) && validateSARLSpecificationVersion(manifest);
			} catch (IOException e) {
				JDIDebugUIPlugin.log(e);
			}
		}
		// TODO: Use NLS.
		setErrorMessage("Cannot read the Jar file of the SARL runtime environment.");  //$NON-NLS-1$
		return false;
	}

	/** Replies the file that contains the SARL runtime environment.
	 *
	 * @return the file or <code>null</code> if none.
	 */
	protected File getSARLRuntimeEnvironmentFile() {
		String name = getSARLRuntimeEnvironment();
		if (name.isEmpty()) {
			return null;
		}
		IPath path = Path.fromPortableString(name);
		if (!path.isAbsolute()) {
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
			IPath workspaceLocation = workspace.getRoot().getLocation();
			path = workspaceLocation.append(path);
		}
		File file = path.toFile();
		return (file.canRead()) ? file : null;
	}

	/** Replies the main class in a jar file.
	 *
	 * @param jarFile - the jar file.
	 * @return the main class or {@link #EMPTY_STRING}
	 */
	protected static String getMainClass(File jarFile) {
		if (jarFile != null) {
			try (JarFile jFile = new JarFile(jarFile)) {
				Manifest manifest = jFile.getManifest();
				String mainClass = manifest.getMainAttributes().getValue("Main-Class"); //$NON-NLS-1$
				if (mainClass != null && !mainClass.isEmpty()) {
					return mainClass;
				}
			} catch (IOException e) {
				//
			}
		}
		return EMPTY_STRING;
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		super.performApply(configuration);
		File file = getSARLRuntimeEnvironmentFile();
		configuration.setAttribute(
				LaunchConfigurationConstants.ATTR_SARL_RUNTIME_ENVIRONMENT,
				(file == null) ? EMPTY_STRING : file.getAbsolutePath());
		configuration.setAttribute(
				IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
				getMainClass(file));
	}

	/** Invoked when the user want to search for a SARL runtime environment.
	 */
	protected void handleSARLRuntimeEnvironmentSearchButtonSelected() {
		// XXX: JARFileSelectionDialog may be used for selecting a jar file in the workspace.
		FileDialog dialog = new FileDialog(getShell(), SWT.OPEN);
		// TODO: Use NLS.
		dialog.setText("Select a SARL runtime environment"); //$NON-NLS-1$
		dialog.setFilterExtensions(new String[] {"*.jar"}); //$NON-NLS-1$
		File file = getSARLRuntimeEnvironmentFile();
		if (file != null) {
			dialog.setFileName(file.getAbsolutePath());
		}
		String selectedFile = dialog.open();
		if (selectedFile != null) {
			IPath path = Path.fromOSString(selectedFile);
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
			IPath workspaceLocation = workspace.getRoot().getLocation();
			if (workspaceLocation.isPrefixOf(path)) {
				path = workspaceLocation.makeRelativeTo(workspaceLocation);
			}
			setSARLRuntimeEnvironment(path.toPortableString());
		}
	}

}

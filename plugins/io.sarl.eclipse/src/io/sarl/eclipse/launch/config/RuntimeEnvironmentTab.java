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
package io.sarl.eclipse.launch.config;

import io.sarl.eclipse.launch.sre.ISREInstall;
import io.sarl.eclipse.launch.sre.ISREInstallChangedListener;
import io.sarl.eclipse.launch.sre.SARLRuntime;
import io.sarl.eclipse.launch.sre.SREInstallChangedAdapter;
import io.sarl.eclipse.preferences.SREsPreferencePage;
import io.sarl.eclipse.util.PluginUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaJRETab;
import org.eclipse.jdt.internal.debug.ui.actions.ControlAccessibleListener;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMInstall2;
import org.eclipse.jdt.launching.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
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

	private ISREInstallChangedListener sreListener;

	private final List<ISREInstall> runtimeEnvironments = new ArrayList<>();
	private Combo runtimeEnvironmentCombo;
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
		this.runtimeEnvironmentCombo = SWTFactory.createCombo(
				group,
				SWT.DROP_DOWN | SWT.READ_ONLY,
				1,
				new String[0]);
		this.runtimeEnvironmentCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectSRE(getSelectedSRE());
			}
		});
		ControlAccessibleListener.addListener(this.runtimeEnvironmentCombo, group.getText());
		// TODO: Use NLS.
		this.runtimeEnvironmentSearchButton = createPushButton(group, "Installed SREs...", null); //$NON-NLS-1$
		this.runtimeEnvironmentSearchButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				handleInstalledSREsButtonSelected();
			}
		});
	}

	/** Change the selected SRE.
	 *
	 * @param sre - the sre, if <code>null</code> reset to default.
	 */
	protected void selectSRE(ISREInstall sre) {
		ISREInstall theSRE = sre;
		if (theSRE == null) {
			theSRE = SARLRuntime.getDefaultSREInstall();
		}
		if (theSRE != null) {
			ISREInstall oldSRE = getSelectedSRE();
			if (oldSRE != theSRE) {
				int index = indexOf(theSRE);
				if (index >= 0) {
					this.runtimeEnvironmentCombo.select(index);
				}
			}
			if (!this.runtimeEnvironments.isEmpty()) {
				int selection = this.runtimeEnvironmentCombo.getSelectionIndex();
				if (selection < 0 || selection >= this.runtimeEnvironments.size()) {
					// Ensure that there is a selected element
					this.runtimeEnvironmentCombo.select(this.indexOf(theSRE));
				}
			}
		}
		updateLaunchConfigurationDialog();
	}

	private int indexOf(ISREInstall sre) {
		Iterator<ISREInstall> iterator = this.runtimeEnvironments.iterator();
		for (int i = 0; iterator.hasNext(); ++i) {
			ISREInstall s = iterator.next();
			if  (s.getId().equals(sre.getId())) {
				return i;
			}
		}
		return -1;
	}

	private String[] getSRELabels() {
		String[] labels = new String[this.runtimeEnvironments.size()];
		for (int i = 0; i < this.runtimeEnvironments.size(); ++i) {
			labels[i] = this.runtimeEnvironments.get(i).getName();
		}
		return labels;
	}


	/** Replies the selected SARL runtime environment.
	 *
	 * @return the SARL runtime environment or <code>null</code> if
	 * there is no selected SRE.
	 */
	protected ISREInstall getSelectedSRE() {
		int index = this.runtimeEnvironmentCombo.getSelectionIndex();
		if (index >= 0 && index < this.runtimeEnvironments.size()) {
			return this.runtimeEnvironments.get(index);
		}
		return null;
	}

	@Override
	protected void updateLaunchConfigurationDialog() {
		super.updateLaunchConfigurationDialog();
		this.runtimeEnvironmentCombo.setEnabled(!this.runtimeEnvironments.isEmpty());
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		super.initializeFrom(configuration);
		// Initialize the SRE list
		this.runtimeEnvironments.clear();
		ISREInstall[] sres = SARLRuntime.getSREInstalls();
		Arrays.sort(sres, new Comparator<ISREInstall>() {
			@Override
			public int compare(ISREInstall o1, ISREInstall o2) {
				return o1.getName().compareTo(o2.getName());
			}
		});
		List<String> labels = new ArrayList<>(sres.length);
		for (int i = 0; i < sres.length; ++i) {
			if (isValid(sres[i], false)) {
				this.runtimeEnvironments.add(sres[i]);
				labels.add(sres[i].getName());
			}
		}
		this.runtimeEnvironmentCombo.setItems(labels.toArray(new String[labels.size()]));
		// Wait for SRE list updates.
		this.sreListener = new InstallChange();
		SARLRuntime.addSREInstallChangedListener(this.sreListener);
		// Refresh the SRE selection
		selectSREFromConfig(configuration);
	}

	@Override
	public void dispose() {
		super.dispose();
		if (this.sreListener != null) {
			SARLRuntime.removeSREInstallChangedListener(this.sreListener);
			this.sreListener = null;
		}
	}

	@Override
	public void activated(ILaunchConfigurationWorkingCopy workingCopy) {
		super.activated(workingCopy);
		selectSREFromConfig(workingCopy);
	}

	/**
	 * Loads the SARL runtime environment from the launch configuration's preference store.
	 *
	 * @param config - the config to load the runtime environment from
	 */
	protected void selectSREFromConfig(ILaunchConfiguration config) {
		String sreId = EMPTY_STRING;
		try {
			sreId = config.getAttribute(
					LaunchConfigurationConstants.ATTR_SARL_RUNTIME_ENVIRONMENT,
					EMPTY_STRING);
		} catch (CoreException ce) {
			PluginUtil.log(ce);
		}
		ISREInstall sre = SARLRuntime.getSREFromId(sreId);
		selectSRE(sre);
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		super.setDefaults(config);
		resetSREConfiguration(getContext(), config);
	}

	/**
	 * Initialize the given configuration with the SARL runtime environment
	 * attributes associated to the given element.
	 *
	 * @param javaElement - the element from which information may be retrieved.
	 * @param config - the config to set with the SARL runtime environment.
	 */
	@SuppressWarnings("static-method")
	protected void resetSREConfiguration(IJavaElement javaElement, ILaunchConfigurationWorkingCopy config) {
		ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
		if (defaultSRE != null) {
			config.setAttribute(LaunchConfigurationConstants.ATTR_SARL_RUNTIME_ENVIRONMENT,
					defaultSRE.getId());
			config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
					defaultSRE.getMainClass());
		} else {
			config.setAttribute(LaunchConfigurationConstants.ATTR_SARL_RUNTIME_ENVIRONMENT,
					EMPTY_STRING);
			config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
					EMPTY_STRING);
		}
	}

	@Override
	public boolean isValid(ILaunchConfiguration config) {
		return isValidSARLRuntimeEnvironment(config) && super.isValid(config)
				&& isValidJREVersion(config);
	}

	/** Replies if the selected configuration has a valid version for
	 * a SARL application.
	 *
	 * @param config - the configuration.
	 * @return <code>true</code> if the JRE is compatible with SARL.
	 */
	protected boolean isValidJREVersion(ILaunchConfiguration config) {
		IVMInstall install = this.fJREBlock.getJRE();
		if (install instanceof IVMInstall2) {
			String version = ((IVMInstall2) install).getJavaVersion();
			if (version == null) {
				// TODO: Use NLS.
				setErrorMessage("Unknown version of the JRE " + install.getName()); //$NON-NLS-1$
				return false;
			}
			Version jreVersion = Version.parseVersion(version);
			Version minVersion = Version.parseVersion(LaunchConfigurationConstants.MINIMAL_JRE_VERSION);
			if (jreVersion.compareTo(minVersion) < 0) {
				// TODO: Use NLS.
				setErrorMessage("Invalid JRE version for " + install.getName() //$NON-NLS-1$
						+ ": " + version //$NON-NLS-1$
						+ "; Required version: " + LaunchConfigurationConstants.MINIMAL_JRE_VERSION); //$NON-NLS-1$
				return false;
			}
		}
		return true;
	}

	private boolean isValid(ISREInstall sre, boolean errorMessages) {
		if (!sre.isValidInstallation()) {
			if (errorMessages) {
				// TODO: Use NLS.
				setErrorMessage("Invalid installation of the SRE \"" + sre.getName() + "\""); //$NON-NLS-1$ //$NON-NLS-2$
			}
			return false;
		}
		// Check the SARL version.
		Bundle bundle = Platform.getBundle("io.sarl.lang"); //$NON-NLS-1$
		if (bundle != null) {
			Version sarlVersion = bundle.getVersion();
			Version minVersion = PluginUtil.parseVersion(sre.getMinimalSARLVersion());
			Version maxVersion = PluginUtil.parseVersion(sre.getMaximalSARLVersion());
			int cmp = PluginUtil.compareVersionToRange(sarlVersion, minVersion, maxVersion);
			if (cmp < 0) {
				if (errorMessages) {
					// TODO: Use NLS.
					setErrorMessage(
							"Incompatible SRE with SARL " + sarlVersion.toString() //$NON-NLS-1$
							+ ". Supported min version by SRE: " + minVersion.toString()); //$NON-NLS-1$
				}
				return false;
			} else if (cmp > 0) {
				if (errorMessages) {
					// TODO: Use NLS.
					setErrorMessage(
							"Incompatible SRE with SARL " + sarlVersion.toString() //$NON-NLS-1$
							+ ". Supported max version by SRE: " + maxVersion.toString()); //$NON-NLS-1$
				}
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
		try {
			if (this.runtimeEnvironments.isEmpty()) {
				// TODO: Use NLS.
				setErrorMessage("Cannot find installed SRE. Please install one."); //$NON-NLS-1$
				return false;
			}
			String id = config.getAttribute(
					LaunchConfigurationConstants.ATTR_SARL_RUNTIME_ENVIRONMENT,
					EMPTY_STRING);
			ISREInstall sre = SARLRuntime.getSREFromId(id);
			if (sre == null) {
				// TODO: Use NLS.
				setErrorMessage("Cannot find the SRE with id: " + id); //$NON-NLS-1$
				return false;
			}
			return isValid(sre, true);
		} catch (CoreException e) {
			PluginUtil.log(e);
			return false;
		}
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		super.performApply(configuration);
		ISREInstall sre = getSelectedSRE();
		if (sre != null) {
			configuration.setAttribute(
					LaunchConfigurationConstants.ATTR_SARL_RUNTIME_ENVIRONMENT,
					sre.getId());
			configuration.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
					sre.getMainClass());
		} else {
			configuration.setAttribute(
					LaunchConfigurationConstants.ATTR_SARL_RUNTIME_ENVIRONMENT,
					EMPTY_STRING);
			configuration.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
					EMPTY_STRING);
		}
	}

	/** Invoked when the user want to search for a SARL runtime environment.
	 */
	protected void handleInstalledSREsButtonSelected() {
		PreferencesUtil.createPreferenceDialogOn(
				getShell(),
				SREsPreferencePage.ID,
				new String[] {SREsPreferencePage.ID},
				null).open();
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class InstallChange extends SREInstallChangedAdapter {

		/**
		 */
		public InstallChange() {
			//
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void sreRemoved(ISREInstall sre) {
			int index = indexOf(sre);
			if (index >= 0) {
				ISREInstall selection = getSelectedSRE();
				RuntimeEnvironmentTab.this.runtimeEnvironments.remove(index);
				RuntimeEnvironmentTab.this.runtimeEnvironmentCombo.setItems(getSRELabels());
				selectSRE(selection);
			}
		}

		@Override
		public void sreChanged(PropertyChangeEvent event) {
			updateLaunchConfigurationDialog();
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void sreAdded(ISREInstall sre) {
			if (isValid(sre, false)) {
				ISREInstall selection = getSelectedSRE();
				RuntimeEnvironmentTab.this.runtimeEnvironments.add(sre);
				RuntimeEnvironmentTab.this.runtimeEnvironmentCombo.setItems(getSRELabels());
				if (selection == null) {
					selectSRE(sre);
				} else {
					selectSRE(selection);
				}
			}
		}

	}

}

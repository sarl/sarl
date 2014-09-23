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
import io.sarl.eclipse.util.PluginUtil;

import java.text.MessageFormat;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaJRETab;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMInstall2;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.PlatformUI;
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

	private SREConfigurationBlock sreBlock;
	private IPropertyChangeListener listener;

	/**
	 */
	public RuntimeEnvironmentTab() {
		//
	}

	@Override
	public String getName() {
		return Messages.RuntimeEnvironmentTab_0;
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
		this.sreBlock = new SREConfigurationBlock();
		this.sreBlock.createControl(parent);
		for (Control ctl : children) {
			ctl.setParent(this.sreBlock.getControl());
		}
		setControl(this.sreBlock.getControl());
		this.listener = new IPropertyChangeListener() {
			@Override
			public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent event) {
				updateLaunchConfigurationDialog();
			}
		};
		this.sreBlock.addPropertyChangeListener(this.listener);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), getHelpContextId());
	}

	@Override
	protected void updateLaunchConfigurationDialog() {
		super.updateLaunchConfigurationDialog();
		this.sreBlock.updateEnableState();
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		super.initializeFrom(configuration);
		this.sreBlock.initializeFrom(configuration);
	}

	@Override
	public void dispose() {
		super.dispose();
		if (this.listener != null) {
			this.sreBlock.removePropertyChangeListener(this.listener);
			this.listener = null;
		}
		this.sreBlock.dispose();
	}

	@Override
	public void activated(ILaunchConfigurationWorkingCopy workingCopy) {
		super.activated(workingCopy);
		this.sreBlock.selectSREFromConfig(workingCopy);
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		super.setDefaults(config);
		this.sreBlock.resetSREConfiguration(getContext(), config);
	}

	@Override
	public boolean isValid(ILaunchConfiguration config) {
		IStatus status = this.sreBlock.validate(config);
		if (status.isOK()) {
			return super.isValid(config) && isValidJREVersion(config);
		}
		setErrorMessage(status.getMessage());
		return false;
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
				setErrorMessage(MessageFormat.format(
						Messages.RuntimeEnvironmentTab_3, install.getName()));
				return false;
			}
			Version jreVersion = Version.parseVersion(version);
			Version minVersion = Version.parseVersion(LaunchConfigurationConstants.MINIMAL_JRE_VERSION);
			if (jreVersion.compareTo(minVersion) < 0) {
				setErrorMessage(MessageFormat.format(
						Messages.RuntimeEnvironmentTab_4,
						install.getName(),
						version,
						LaunchConfigurationConstants.MINIMAL_JRE_VERSION));
				return false;
			}
		}
		return true;
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		super.performApply(configuration);
		ISREInstall sre = this.sreBlock.getSelectedSRE();
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
					PluginUtil.EMPTY_STRING);
			configuration.setAttribute(
					IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
					PluginUtil.EMPTY_STRING);
		}
	}

}

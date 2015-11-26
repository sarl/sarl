/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import com.google.common.base.Strings;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaJRETab;
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMInstall2;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.PlatformUI;
import org.osgi.framework.Version;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.launching.sreproviding.StandardProjectSREProviderFactory;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.ProjectProvider;
import io.sarl.eclipse.runtime.ProjectSREProviderFactory;
import io.sarl.eclipse.runtime.SARLRuntime;
import io.sarl.eclipse.runtime.SREConfigurationBlock;

/**
 * Configuration tab for the JRE and the SARL runtime environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLRuntimeEnvironmentTab extends JavaJRETab {

	private SREConfigurationBlock sreBlock;

	private IPropertyChangeListener listener;

	/** Construct the tab for configuration the SRE.
	 */
	public SARLRuntimeEnvironmentTab() {
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

	private static List<ProjectSREProviderFactory> getProviderFromExtension() {
		IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(
				SARLEclipsePlugin.PLUGIN_ID,
				SARLEclipseConfig.EXTENSION_POINT_PROJECT_SRE_PROVIDER_FACTORY);
		if (extensionPoint != null) {
			List<ProjectSREProviderFactory> providers = new ArrayList<>();
			for (IConfigurationElement element : extensionPoint.getConfigurationElements()) {
				try {
					Object obj = element.createExecutableExtension("class"); //$NON-NLS-1$
					if (obj instanceof ProjectSREProviderFactory) {
						providers.add((ProjectSREProviderFactory) obj);
					} else {
						SARLEclipsePlugin.getDefault().logErrorMessage(
								"Cannot instance extension point: " + element.getName()); //$NON-NLS-1$
					}
				} catch (CoreException e) {
					SARLEclipsePlugin.getDefault().log(e);
				}
			}
			return providers;
		}
		return null;
	}

	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);

		ProjectProvider projectProvider = new ProjectAdapter();
		List<ProjectSREProviderFactory> sreProviderFactories = getProviderFromExtension();
		sreProviderFactories.add(new StandardProjectSREProviderFactory());

		this.sreBlock = new SREConfigurationBlock(true, projectProvider, sreProviderFactories);
		this.sreBlock.createControl(parent);
		Composite oldComp = (Composite) getControl();
		Control[] children = oldComp.getChildren();
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
		this.sreBlock.initialize();
		selectSREFromConfig(configuration);
	}

	/**
	 * Loads the SARL runtime environment from the launch configuration's preference store.
	 *
	 * @param config - the config to load the runtime environment from
	 */
	protected void selectSREFromConfig(ILaunchConfiguration config) {
		String sreId = null;
		try {
			sreId = config.getAttribute(
					SARLEclipseConfig.ATTR_SARL_RUNTIME_ENVIRONMENT,
					(String) null);
		} catch (CoreException ce) {
			SARLEclipsePlugin.getDefault().log(ce);
		}
		ISREInstall sre = SARLRuntime.getSREFromId(Strings.nullToEmpty(sreId));
		boolean notify = this.sreBlock.getNotify();
		boolean changed;
		try {
			this.sreBlock.setNotify(false);
			changed = this.sreBlock.selectSpecificSRE(sre);

			try {
				String useWideConfig = config.getAttribute(
						SARLEclipseConfig.ATTR_USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT,
						Boolean.TRUE.toString());
				if (Boolean.parseBoolean(useWideConfig)) {
					changed = this.sreBlock.selectSystemWideSRE();
				}
			} catch (CoreException ce) {
				SARLEclipsePlugin.getDefault().log(ce);
			}

			try {
				String useProjectConfig = config.getAttribute(
						SARLEclipseConfig.ATTR_USE_PROJECT_SARL_RUNTIME_ENVIRONMENT,
						Boolean.TRUE.toString());
				if (Boolean.parseBoolean(useProjectConfig)) {
					changed = this.sreBlock.selectProjectSRE();
				}
			} catch (CoreException ce) {
				SARLEclipsePlugin.getDefault().log(ce);
			}
		} finally {
			this.sreBlock.setNotify(notify);
		}
		if (changed) {
			updateLaunchConfigurationDialog();
		}
	}

	@Override
	public void dispose() {
		super.dispose();
		if (this.listener != null) {
			if (this.sreBlock != null) {
				this.sreBlock.removePropertyChangeListener(this.listener);
			}
			this.listener = null;
		}
		if (this.sreBlock != null) {
			this.sreBlock.dispose();
		}
	}

	@Override
	public void activated(ILaunchConfigurationWorkingCopy workingCopy) {
		super.activated(workingCopy);
		this.sreBlock.updateExternalSREButtonLabels();
		selectSREFromConfig(workingCopy);
	}

	@Override
	public boolean isValid(ILaunchConfiguration config) {
		IStatus status;
		try {
			String id = config.getAttribute(
					SARLEclipseConfig.ATTR_SARL_RUNTIME_ENVIRONMENT,
					(String) null);
			ISREInstall sre = SARLRuntime.getSREFromId(id);
			if (sre == null) {
				sre = this.sreBlock.getSelectedSRE();
			}
			if (sre == null) {
				if (Strings.isNullOrEmpty(id)) {
					status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, Messages.RuntimeEnvironmentTab_7);
				} else {
					status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, MessageFormat.format(
							Messages.RuntimeEnvironmentTab_6, Strings.nullToEmpty(id)));
				}
			} else {
				status = this.sreBlock.validate(sre);
			}
		} catch (CoreException e) {
			status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, e);
		}
		if (status.isOK()) {
			return super.isValid(config) && isValidJREVersion(config);
		}
		setErrorMessage(status.getMessage());
		Throwable throwable = status.getException();
		if (throwable != null) {
			JDIDebugUIPlugin.log(throwable);
		}
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
			Version minVersion = Version.parseVersion(SARLEclipseConfig.MINIMAL_JRE_VERSION);
			if (jreVersion.compareTo(minVersion) < 0) {
				setErrorMessage(MessageFormat.format(
						Messages.RuntimeEnvironmentTab_4,
						install.getName(),
						version,
						SARLEclipseConfig.MINIMAL_JRE_VERSION));
				return false;
			}
		}
		return true;
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		super.performApply(configuration);
		// Save the SRE specific parameters
		ISREInstall sre = this.sreBlock.getSelectedSRE();
		if (sre != null) {
			configuration.setAttribute(
					SARLEclipseConfig.ATTR_SARL_RUNTIME_ENVIRONMENT,
					sre.getId());
			String mainClass = sre.getMainClass();
			if (Strings.isNullOrEmpty(mainClass)) {
				configuration.removeAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME);
			} else {
				configuration.setAttribute(
						IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
						mainClass);
			}
		} else {
			configuration.removeAttribute(SARLEclipseConfig.ATTR_SARL_RUNTIME_ENVIRONMENT);
			configuration.removeAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME);
		}
		// Save the boolean configuration flags
		configuration.setAttribute(
				SARLEclipseConfig.ATTR_USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT,
				Boolean.toString(this.sreBlock.isSystemWideDefaultSRE()));
		configuration.setAttribute(
				SARLEclipseConfig.ATTR_USE_PROJECT_SARL_RUNTIME_ENVIRONMENT,
				Boolean.toString(this.sreBlock.isProjectSRE()));
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		super.setDefaults(config);
		ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
		if (defaultSRE != null) {
			config.setAttribute(SARLEclipseConfig.ATTR_SARL_RUNTIME_ENVIRONMENT,
					defaultSRE.getId());
			String mainClass = defaultSRE.getMainClass();
			if (Strings.isNullOrEmpty(mainClass)) {
				config.removeAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME);
			} else {
				config.setAttribute(
						IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
						mainClass);
			}
		} else {
			config.removeAttribute(SARLEclipseConfig.ATTR_SARL_RUNTIME_ENVIRONMENT);
			config.removeAttribute(IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME);
		}
		config.setAttribute(SARLEclipseConfig.ATTR_USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT,
				Boolean.TRUE.toString());
		config.setAttribute(SARLEclipseConfig.ATTR_USE_PROJECT_SARL_RUNTIME_ENVIRONMENT,
				Boolean.FALSE.toString());
	}

	/** Adapter that permits to provide a reference on a SARL project to the configuration
	 * tab that is using it.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class ProjectAdapter implements ProjectProvider {

		ProjectAdapter() {
			//
		}

		@Override
		@SuppressWarnings("synthetic-access")
		public IProject getProject() {
			ILaunchConfiguration config = getLaunchConfigurationWorkingCopy();
			if (config == null) {
				config = getLaunchConfiguration();
			}
			if (config != null) {
				try {
					String name = config.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, (String) null);
					if (name != null && name.length() > 0) {
						IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(name);
						if (project.exists()) {
							return project;
						}
					}
				} catch (CoreException e) {
					SARLEclipsePlugin.getDefault().log(e);
				}
			}
			return null;
		}

	}

}

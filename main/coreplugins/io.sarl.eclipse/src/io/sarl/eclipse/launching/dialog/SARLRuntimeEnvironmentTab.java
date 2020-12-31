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

package io.sarl.eclipse.launching.dialog;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import javax.inject.Inject;

import com.google.common.base.Strings;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
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

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;
import io.sarl.eclipse.launching.sreproviding.EclipseIDEProjectSREProviderFactory;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.ProjectProvider;
import io.sarl.eclipse.runtime.ProjectSREProviderFactory;
import io.sarl.eclipse.runtime.SARLRuntime;
import io.sarl.eclipse.runtime.SREConfigurationBlock;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.util.Utils;

/**
 * Configuration tab for the JRE and the SARL runtime environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see JavaJRETab
 */
public class SARLRuntimeEnvironmentTab extends JavaJRETab implements ISarlRuntimeEnvironmentTab {

	private final boolean resetJvaMainClass;

	private SREConfigurationBlock sreBlock;

	private IPropertyChangeListener listener;

	@Inject
	private ILaunchConfigurationConfigurator configurator;

	@Inject
	private ILaunchConfigurationAccessor accessor;

	private final ListenerList<ISreChangeListener> sreChangeListeners = new ListenerList<>();

	/** Construct the tab for configuration the SRE.
	 *
	 * @param resetJavaMainClass indicates if this tab should reset the Java main class within the configuration.
	 */
	public SARLRuntimeEnvironmentTab(boolean resetJavaMainClass) {
		this.resetJvaMainClass = resetJavaMainClass;
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
		final IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(
				SARLEclipsePlugin.PLUGIN_ID,
				SARLEclipseConfig.EXTENSION_POINT_PROJECT_SRE_PROVIDER_FACTORY);
		if (extensionPoint != null) {
			final List<ProjectSREProviderFactory> providers = new ArrayList<>();
			for (final IConfigurationElement element : extensionPoint.getConfigurationElements()) {
				try {
					final Object obj = element.createExecutableExtension("class"); //$NON-NLS-1$
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

		final ProjectProvider projectProvider = new ProjectAdapter();
		final List<ProjectSREProviderFactory> sreProviderFactories = getProviderFromExtension();
		sreProviderFactories.add(new EclipseIDEProjectSREProviderFactory());

		this.sreBlock = new SREConfigurationBlock(true, projectProvider, sreProviderFactories);

		final SREConfigurationBlock blk = getSREConfigurationBlock();
		blk.createControl(parent);
		final Composite oldComp = (Composite) getControl();
		final Control[] children = oldComp.getChildren();
		for (final Control ctl : children) {
			ctl.setParent(blk.getControl());
		}
		setControl(blk.getControl());
		this.listener = new IPropertyChangeListener() {
			@Override
			public void propertyChange(org.eclipse.jface.util.PropertyChangeEvent event) {
				updateLaunchConfigurationDialog();
				fireSreChange((ISREInstall) event.getNewValue());
			}
		};
		blk.addPropertyChangeListener(this.listener);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), getHelpContextId());
	}

	/** Invoke the listeners about the change of SRE.
	 *
	 * @param sre the new SRE.
	 * @since 0.11
	 */
	public void fireSreChange(ISREInstall sre) {
		final Object[] listeners = this.sreChangeListeners.getListeners();
		for (int i = 0; i < listeners.length; i++) {
			final ISreChangeListener listener = (ISreChangeListener) listeners[i];
			listener.sreChanged(sre);
		}
	}

	@Override
	public void addSreChangeListener(ISreChangeListener listener) {
		this.sreChangeListeners.add(listener);
	}

	@Override
	public void removeSreChangeListener(ISreChangeListener listener) {
		this.sreChangeListeners.remove(listener);
	}

	/** Replies the block that permits to configure the SRE.
	 *
	 * @return the SRE configuration clock.
	 */
	public SREConfigurationBlock getSREConfigurationBlock() {
		return this.sreBlock;
	}

	@Override
	protected void updateLaunchConfigurationDialog() {
		super.updateLaunchConfigurationDialog();
		getSREConfigurationBlock().updateEnableState();
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		super.initializeFrom(configuration);
		getSREConfigurationBlock().initialize();
		selectSREFromConfig(configuration);
	}

	/**
	 * Loads the SARL runtime environment from the launch configuration's preference store.
	 *
	 * @param config the config to load the runtime environment from
	 */
	protected void selectSREFromConfig(ILaunchConfiguration config) {
		final SREConfigurationBlock blk = getSREConfigurationBlock();
		final boolean notify = blk.getNotify();
		final boolean changed;
		try {
			blk.setNotify(false);
			if (this.accessor.getUseSystemSREFlag(config)) {
				changed = blk.selectSystemWideSRE();
			} else if (this.accessor.getUseProjectSREFlag(config)) {
				changed = blk.selectProjectSRE();
			} else {
				final String sreId = this.accessor.getSREId(config);
				final ISREInstall sre = SARLRuntime.getSREFromId(Strings.nullToEmpty(sreId));
				changed = blk.selectSpecificSRE(sre);
			}
		} finally {
			blk.setNotify(notify);
		}
		if (changed) {
			updateLaunchConfigurationDialog();
		}
	}

	@Override
	public void dispose() {
		super.dispose();
		final SREConfigurationBlock blk = getSREConfigurationBlock();
		if (this.listener != null) {
			if (blk != null) {
				blk.removePropertyChangeListener(this.listener);
			}
			this.listener = null;
		}
		if (blk != null) {
			blk.dispose();
		}
		this.sreChangeListeners.clear();
	}

	@Override
	public void activated(ILaunchConfigurationWorkingCopy workingCopy) {
		super.activated(workingCopy);
		getSREConfigurationBlock().updateExternalSREButtonLabels();
		selectSREFromConfig(workingCopy);
	}

	@Override
	public boolean isValid(ILaunchConfiguration config) {
		final String id = this.accessor.getSREId(config);
		final SREConfigurationBlock blk = getSREConfigurationBlock();
		ISREInstall sre = SARLRuntime.getSREFromId(id);
		if (sre == null) {
			sre = blk.getSelectedSRE();
		}
		final IStatus status;
		if (sre == null) {
			if (Strings.isNullOrEmpty(id)) {
				status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, Messages.RuntimeEnvironmentTab_7);
			} else {
				status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, MessageFormat.format(
						Messages.RuntimeEnvironmentTab_6, Strings.nullToEmpty(id)));
			}
		} else {
			status = blk.validate(sre);
		}
		if (status.isOK()) {
			return super.isValid(config) && isValidJREVersion(config);
		}
		setErrorMessage(status.getMessage());
		final Throwable throwable = status.getException();
		if (throwable != null) {
			JDIDebugUIPlugin.log(throwable);
		}
		return false;
	}

	/** Replies if the selected configuration has a valid version for
	 * a SARL application.
	 *
	 * @param config the configuration.
	 * @return <code>true</code> if the JRE is compatible with SARL.
	 */
	protected boolean isValidJREVersion(ILaunchConfiguration config) {
		final IVMInstall install = this.fJREBlock.getJRE();
		if (install instanceof IVMInstall2) {
			final String version = ((IVMInstall2) install).getJavaVersion();
			if (version == null) {
				setErrorMessage(MessageFormat.format(
						Messages.RuntimeEnvironmentTab_3, install.getName()));
				return false;
			}
			if (!Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath(version)) {
				setErrorMessage(MessageFormat.format(
						Messages.RuntimeEnvironmentTab_4,
						install.getName(),
						version,
						SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH,
						SARLVersion.INCOMPATIBLE_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH));
				return false;
			}
		}
		return true;
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		super.performApply(configuration);
		// Save the SRE specific parameters
		final SREConfigurationBlock blk = getSREConfigurationBlock();
		final ISREInstall sre = blk.getSelectedSRE();
		this.configurator.setRuntimeConfiguration(configuration, sre,
				blk.isSystemWideDefaultSRE(),
				blk.isProjectSRE(),
				this.resetJvaMainClass);
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		super.setDefaults(config);
		this.configurator.setRuntimeConfiguration(config, SARLRuntime.getDefaultSREInstall(), null, null, this.resetJvaMainClass);
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
					final String name = config.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, (String) null);
					if (name != null && name.length() > 0) {
						final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(name);
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

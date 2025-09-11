/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import com.google.common.base.Strings;
import com.google.inject.Inject;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaJRETab;
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMInstall2;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;

import io.sarl.apputils.eclipseextensions.launching.ISarlRuntimeEnvironmentTab;
import io.sarl.apputils.eclipseextensions.launching.ISreChangeListener;
import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstall;
import io.sarl.apputils.eclipseextensions.sreprovider.ProjectSREProviderFactories;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;
import io.sarl.eclipse.launching.sreproviding.EclipseIDEProjectSREProviderFactory;
import io.sarl.eclipse.runtime.ProjectProvider;
import io.sarl.eclipse.runtime.SARLRuntime;
import io.sarl.eclipse.runtime.SREConfigurationBlock;
import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.util.Utils;

/**
 * Configuration tab for the JRE and the SARL runtime environment.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see JavaJRETab
 */
@SuppressWarnings("restriction")
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
		return SARLEclipsePlugin.PLUGIN_ID + ".debug.ui.sarlRuntimeEnvironmentTab"; //$NON-NLS-1$
	}

	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);

		final var projectProvider = new ProjectAdapter();
		final var sreProviderFactories = ProjectSREProviderFactories.getSREProviderFactoriesFromExtension();
		sreProviderFactories.add(new EclipseIDEProjectSREProviderFactory());

		this.sreBlock = new SREConfigurationBlock(true, projectProvider, sreProviderFactories);

		final var blk = getSREConfigurationBlock();
		blk.createControl(parent);
		final var oldComp = (Composite) getControl();
		final var children = oldComp.getChildren();
		for (final var ctl : children) {
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
		final var listeners = this.sreChangeListeners.getListeners();
		for (var i = 0; i < listeners.length; i++) {
			final var listener = (ISreChangeListener) listeners[i];
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
		final var blk = getSREConfigurationBlock();
		final var notify = blk.getNotify();
		final boolean changed;
		try {
			blk.setNotify(false);
			if (this.accessor.getUseSystemSREFlag(config)) {
				changed = blk.selectSystemWideSRE();
			} else if (this.accessor.getUseProjectSREFlag(config)) {
				changed = blk.selectProjectSRE();
			} else {
				final var sreId = this.accessor.getSREId(config);
				final var sre = SARLRuntime.getSREFromId(Strings.nullToEmpty(sreId));
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
		final var blk = getSREConfigurationBlock();
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
		final var id = this.accessor.getSREId(config);
		final var blk = getSREConfigurationBlock();
		var sre = SARLRuntime.getSREFromId(id);
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
		final var throwable = status.getException();
		if (throwable != null) {
			JDIDebugUIPlugin.log(throwable);
		}
		return false;
	}

	/** Replies if the selected configuration has a valid version for
	 * a SARL application.
	 *
	 * @param config the configuration.
	 * @return {@code true} if the JRE is compatible with SARL.
	 */
	protected boolean isValidJREVersion(ILaunchConfiguration config) {
		final var install = this.fJREBlock.getJRE();
		if (install instanceof IVMInstall2 cvalue) {
			final var version = cvalue.getJavaVersion();
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
		final var blk = getSREConfigurationBlock();
		final var sre = blk.getSelectedSRE();
		this.configurator.setRuntimeConfiguration(configuration, sre,
				Boolean.valueOf(blk.isSystemWideDefaultSRE()),
				Boolean.valueOf(blk.isProjectSRE()),
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
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class ProjectAdapter implements ProjectProvider {

		ProjectAdapter() {
			//
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public IProject getProject() {
			ILaunchConfiguration config = getLaunchConfigurationWorkingCopy();
			if (config == null) {
				config = getLaunchConfiguration();
			}
			if (config != null) {
				try {
					final var name = config.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, (String) null);
					if (name != null && name.length() > 0) {
						final var project = ResourcesPlugin.getWorkspace().getRoot().getProject(name);
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

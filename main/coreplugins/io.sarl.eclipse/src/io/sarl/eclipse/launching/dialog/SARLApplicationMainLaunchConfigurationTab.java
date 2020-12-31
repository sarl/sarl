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

import java.lang.ref.SoftReference;
import java.text.MessageFormat;
import javax.inject.Inject;

import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaMainTab;
import org.eclipse.jdt.internal.debug.ui.launcher.LauncherMessages;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;
import io.sarl.eclipse.runtime.ISREInstall;

/**
 * The main launch configuration tab for SARL applications.
 *
 * <p>This configuration tab enables to enter the name of the main class to launch,
 * the launching parameters.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SARLApplicationMainLaunchConfigurationTab extends JavaMainTab implements ISreChangeListener {

	private volatile SoftReference<Image> image;

	private Button runInEclipseButton;

	private Button logShowStartInformationButton;

	private Button enableAssertionsInDebugModeButton;

	private Button enableAssertionsInRunModeButton;

	private final WidgetListener defaultListener = new WidgetListener();

	@Inject
	private ILaunchConfigurationConfigurator configurator;

	@Inject
	private ILaunchConfigurationAccessor accessor;

	/** Construct a main configuration tab for SARL application.
	 */
	public SARLApplicationMainLaunchConfigurationTab() {
		//
	}

	@Override
	public void sreChanged(ISREInstall sre) {
		//
	}

	@Override
	public Image getImage() {
		Image img = (this.image == null) ? null : this.image.get();
		if (img == null) {
			img = SARLEclipsePlugin.getDefault().getImage(SARLEclipseConfig.SARL_APPLICATION_IMAGE);
			this.image = new SoftReference<>(img);
		}
		return img;
	}

	@Override
	public String getId() {
		return "io.sarl.eclipse.debug.ui.sarlApplicationMainTab"; //$NON-NLS-1$
	}

	@Override
	public String getName() {
		return Messages.MainLaunchConfigurationTab_7;
	}

	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);
		final Composite comp = SWTFactory.createComposite(parent, parent.getFont(), 1, 1, GridData.FILL_BOTH);
		((GridLayout) comp.getLayout()).verticalSpacing = 0;
		createProjectEditor(comp);
		createVerticalSpacer(comp, 1);
		createMainTypeEditor(comp, LauncherMessages.JavaMainTab_Main_cla_ss__4);
		createVerticalSpacer(comp, 1);
		createLaunchOptionEditor(comp, Messages.MainLaunchConfigurationTab_10);
		setControl(comp);
	}

	/**
	 * Creates the widgets for configuring the launch options.
	 *
	 * @param parent the parent composite.
	 * @param text the label of the group.
	 */
	protected void createLaunchOptionEditor(Composite parent, String text) {
		final Group group = SWTFactory.createGroup(parent, text, 1, 1, GridData.FILL_HORIZONTAL);
		this.enableAssertionsInRunModeButton = createCheckButton(group, Messages.SARLMainLaunchConfigurationTab_2);
		this.enableAssertionsInRunModeButton.addSelectionListener(this.defaultListener);
		this.enableAssertionsInDebugModeButton = createCheckButton(group, Messages.SARLMainLaunchConfigurationTab_1);
		this.enableAssertionsInDebugModeButton.addSelectionListener(this.defaultListener);

		createVerticalSpacer(group, 1);

		this.runInEclipseButton = createCheckButton(group,
				MessageFormat.format(Messages.SARLMainLaunchConfigurationTab_0, Messages.SARLMainLaunchConfigurationTab_4));
		this.runInEclipseButton.addSelectionListener(this.defaultListener);
		this.logShowStartInformationButton = SWTFactory.createCheckButton(group, Messages.SARLMainLaunchConfigurationTab_3, null, false, 2);
		this.logShowStartInformationButton.addSelectionListener(this.defaultListener);
	}

	/**
	 * Loads the launch options from the launch configuration's preference store.
	 *
	 * @param config the config to load the agent name from
	 */
	protected void updateLaunchOptionsFromConfig(ILaunchConfiguration config) {
		final boolean runInEclipse = this.accessor.isEmbeddedSRE(config);
		final boolean showLaunchingParameters = this.accessor.isLaunhcingParametersPrintedOut(config);
		final boolean enableAssertionsRun = this.accessor.isAssertionEnabledInRunMode(config);
		final boolean enableAssertionsDebug = this.accessor.isAssertionEnabledInDebugMode(config);
		this.logShowStartInformationButton.setSelection(showLaunchingParameters);
		this.enableAssertionsInRunModeButton.setSelection(enableAssertionsRun);
		this.enableAssertionsInDebugModeButton.setSelection(enableAssertionsDebug);
		this.runInEclipseButton.setSelection(runInEclipse);
	}

	@Override
	public void initializeFrom(ILaunchConfiguration config) {
		super.initializeFrom(config);
		updateLaunchOptionsFromConfig(config);
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy config) {
		this.configurator.setLaunhcingParametersPrintedOut(config, this.logShowStartInformationButton.getSelection());
		this.configurator.setAssertionEnabledInRunMode(config, this.enableAssertionsInRunModeButton.getSelection());
		this.configurator.setAssertionEnabledInDebugMode(config, this.enableAssertionsInDebugModeButton.getSelection());
		this.configurator.setEmbeddedSRE(config, this.runInEclipseButton.getSelection());
		super.performApply(config);
	}

	/** Listener of events in internal components for refreshing the tab.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
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

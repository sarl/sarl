/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
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

package io.sarl.sre.eclipse.network;

import javax.inject.Inject;

import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaLaunchTab;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;
import io.sarl.eclipse.launching.config.LaunchConfigurationUtils;
import io.sarl.eclipse.launching.config.LaunchConfigurationUtils.InputExtraJreArguments;
import io.sarl.eclipse.launching.config.LaunchConfigurationUtils.OutputExtraJreArguments;
import io.sarl.sre.eclipse.JanusEclipsePlugin;
import io.sarl.sre.network.boot.configs.SreNetworkConfig;

/**
 * Configuration tab for the JRE and the SARL runtime environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class JanusLaunchNetworkTab extends JavaLaunchTab {

	/** Identifier of the contributor for the launch configuration.
	 */
	public static final String CONTRIBUTOR_ID = "io.sarl.sre.network"; //$NON-NLS-1$

	private Button enableNetworkButton;

	private Label clusterNameLabel;

	private Text clusterNameText;

	private Label minClusterSizeLabel;

	private Spinner minClusterSizeSpinner;

	private Button portAutoIncrementButton;

	private final WidgetListener defaultListener = new WidgetListener();

	@Inject
	private ILaunchConfigurationConfigurator configurator;

	@Inject
	private ILaunchConfigurationAccessor accessor;

	/** Construct the tab for configuration of the SRE networking feature.
	 */
	public JanusLaunchNetworkTab() {
		//
	}

	@Override
	public String getName() {
		return Messages.JanusLaunchNetworkTab_0;
	}

	@Override
	public Image getImage() {
		return JanusEclipsePlugin.getDefault().getImage("/icons/lan-network-icon.png");
	}

	@Override
	public String getId() {
		return "io.sarl.sre.eclipse.network.janusLaunchNetworkTab"; //$NON-NLS-1$
	}

	private static Spinner createSpinner(Composite parent, int hspan, int min, int max, int increment, int pageIncrement) {
		Spinner spinner = new Spinner(parent, SWT.SINGLE | SWT.BORDER);
		spinner.setFont(parent.getFont());
		spinner.setMinimum(min);
		spinner.setMaximum(max);
		spinner.setIncrement(increment);
		spinner.setPageIncrement(pageIncrement);
		spinner.setDigits(0);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalSpan = hspan;
		spinner.setLayoutData(gd);
		return spinner;
	}

	@Override
	public void createControl(Composite parent) {
		final Composite topComp = SWTFactory.createComposite(parent, parent.getFont(), 2, 1, GridData.FILL_HORIZONTAL, 5, 5);

		this.enableNetworkButton = SWTFactory.createCheckButton(topComp, Messages.JanusLaunchNetworkTab_1, null, false, 2);
		this.enableNetworkButton.addSelectionListener(this.defaultListener);

		createSeparator(parent, 2);
		
		this.clusterNameLabel = SWTFactory.createLabel(topComp, Messages.JanusLaunchNetworkTab_2, 1);
		this.clusterNameText = SWTFactory.createSingleText(topComp, 1);
		this.clusterNameText.addModifyListener(this.defaultListener);

		this.minClusterSizeLabel = SWTFactory.createLabel(topComp, Messages.JanusLaunchNetworkTab_3, 1);
		this.minClusterSizeSpinner = createSpinner(topComp, 1, 1, 1000, 1, 1);
		this.minClusterSizeSpinner.addModifyListener(this.defaultListener);

		this.portAutoIncrementButton = SWTFactory.createCheckButton(topComp, Messages.JanusLaunchNetworkTab_4, null, false, 2);
		this.portAutoIncrementButton.addSelectionListener(this.defaultListener);
		
		setControl(topComp);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), getHelpContextId());
		updateComponentStates();
	}

	/** Update the states of the components (enabling).
	 */
	protected void updateComponentStates() {
		final boolean enable = this.enableNetworkButton.getSelection();
		this.clusterNameLabel.setEnabled(enable);
		this.clusterNameText.setEnabled(enable);
		this.minClusterSizeLabel.setEnabled(enable);
		this.minClusterSizeSpinner.setEnabled(enable);
		this.portAutoIncrementButton.setEnabled(enable);
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		super.initializeFrom(configuration);
		final InputExtraJreArguments arguments = LaunchConfigurationUtils.createInputExtraJreArguments(CONTRIBUTOR_ID);
		arguments.read(configuration, this.accessor);
		this.enableNetworkButton.setSelection(arguments.arg(SreNetworkConfig.ENABLE_NAME, SreNetworkConfig.DEFAULT_ENABLE_VALUE));
		this.clusterNameText.setText(arguments.arg(SreNetworkConfig.CLUSTER_NAME_NAME, SreNetworkConfig.DEFAULT_CLUSTER_NAME_VALUE));
		this.minClusterSizeSpinner.setSelection(arguments.arg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME, SreNetworkConfig.DEFAULT_MIN_CLUSTER_SIZE_VALUE));
		this.portAutoIncrementButton.setSelection(arguments.arg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME, SreNetworkConfig.DEFAULT_PORT_AUTO_INCREMENT_VALUE));
		updateComponentStates();
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		final OutputExtraJreArguments arguments = LaunchConfigurationUtils.createOutputExtraJreArguments(CONTRIBUTOR_ID);
		arguments.arg(SreNetworkConfig.ENABLE_NAME, SreNetworkConfig.DEFAULT_ENABLE_VALUE);
		arguments.arg(SreNetworkConfig.CLUSTER_NAME_NAME, SreNetworkConfig.DEFAULT_CLUSTER_NAME_VALUE);
		arguments.arg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME, SreNetworkConfig.DEFAULT_MIN_CLUSTER_SIZE_VALUE);
		arguments.arg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME, SreNetworkConfig.DEFAULT_PORT_AUTO_INCREMENT_VALUE);
		arguments.apply(config, this.configurator);
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		final OutputExtraJreArguments arguments = LaunchConfigurationUtils.createOutputExtraJreArguments(CONTRIBUTOR_ID);
		arguments.arg(SreNetworkConfig.ENABLE_NAME, this.enableNetworkButton.getSelection());
		arguments.arg(SreNetworkConfig.CLUSTER_NAME_NAME, this.clusterNameText.getText());
		arguments.arg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME, this.minClusterSizeSpinner.getSelection());
		arguments.arg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME, this.portAutoIncrementButton.getSelection());
		arguments.apply(configuration, this.configurator);
	}
	
	/** Listener of events in internal components for refreshing the tab.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class WidgetListener implements SelectionListener, ModifyListener {

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
			if (event.getSource() == JanusLaunchNetworkTab.this.enableNetworkButton) {
				updateComponentStates();
			}
			updateLaunchConfigurationDialog();
		}

		@Override
		public void modifyText(ModifyEvent e) {
			updateLaunchConfigurationDialog();
		}

	}

}

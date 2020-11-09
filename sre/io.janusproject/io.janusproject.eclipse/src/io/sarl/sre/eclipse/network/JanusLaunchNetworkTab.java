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

import static io.sarl.eclipse.launching.dialog.SarlSwtFactory.createSpinner;
import static io.sarl.eclipse.launching.dialog.SarlSwtFactory.getCommandLineDefinition;
import static io.sarl.eclipse.launching.dialog.SarlSwtFactory.getCommandLineOption;
import static org.eclipse.debug.internal.ui.SWTFactory.createComposite;
import static org.eclipse.debug.internal.ui.SWTFactory.createLabel;
import static org.eclipse.debug.internal.ui.SWTFactory.createSingleText;
import static org.eclipse.debug.internal.ui.SWTFactory.createWrapLabel;

import java.text.MessageFormat;

import javax.inject.Inject;

import org.arakhne.afc.bootique.variables.VariableNames;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaLaunchTab;
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
import io.sarl.sre.network.boot.configs.SreNetworkConfigModule;

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

	private static final int MIN_CLUSTER_SIZE = 1;

	private static final int MAX_CLUSTER_SIZE = 100;

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

	@Override
	public void createControl(Composite parent) {
		final Composite topComp = createComposite(parent, parent.getFont(), 2, 1, GridData.FILL_HORIZONTAL, 5, 5);

		createWrapLabel(topComp, Messages.JanusLaunchNetworkTab_5, 2);

		createVerticalSpacer(topComp, 2);
		createSeparator(parent, 2);
		createVerticalSpacer(topComp, 2);

		this.enableNetworkButton = SWTFactory.createCheckButton(topComp, 
				MessageFormat.format(Messages.JanusLaunchNetworkTab_1,
						getCommandLineOption(SreNetworkConfigModule.NETWORK_LONG_OPTION, true),
						getCommandLineDefinition(VariableNames.toPropertyName(SreNetworkConfig.ENABLE_NAME), true)),
				null, false, 2);
		this.enableNetworkButton.addSelectionListener(this.defaultListener);

		createVerticalSpacer(topComp, 2);
		createSeparator(parent, 2);
		createVerticalSpacer(topComp, 2);

		this.clusterNameLabel = createLabel(topComp,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_2,
						getCommandLineDefinition(VariableNames.toPropertyName(SreNetworkConfig.CLUSTER_NAME_NAME), Messages.JanusLaunchNetworkTab_8)),
				1);
		this.clusterNameText = createSingleText(topComp, 1);
		this.clusterNameText.setMessage(MessageFormat.format(Messages.JanusLaunchNetworkTab_7,
				SreNetworkConfig.createStandardClusterName(Messages.JanusLaunchNetworkTab_6)));
		this.clusterNameText.addModifyListener(this.defaultListener);

		this.minClusterSizeLabel = createLabel(topComp,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_3,
						getCommandLineDefinition(VariableNames.toPropertyName(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME), Messages.JanusLaunchNetworkTab_9)),
				1);
		this.minClusterSizeSpinner = createSpinner(topComp, 1, MIN_CLUSTER_SIZE, MAX_CLUSTER_SIZE);
		this.minClusterSizeSpinner.addModifyListener(this.defaultListener);

		this.portAutoIncrementButton = SWTFactory.createCheckButton(topComp,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_4,
						getCommandLineDefinition(VariableNames.toPropertyName(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME), true)),
				null, false, 2);
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
	public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
		final OutputExtraJreArguments arguments = LaunchConfigurationUtils.createOutputExtraJreArguments(CONTRIBUTOR_ID);
		final boolean enable = SreNetworkConfig.DEFAULT_ENABLE_VALUE;
		if (enable) {
			arguments.arg(SreNetworkConfig.ENABLE_NAME, enable);
			arguments.arg(SreNetworkConfig.CLUSTER_NAME_NAME, SreNetworkConfig.DEFAULT_CLUSTER_NAME_VALUE);
			arguments.arg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME, SreNetworkConfig.DEFAULT_MIN_CLUSTER_SIZE_VALUE);
			arguments.arg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME, SreNetworkConfig.DEFAULT_PORT_AUTO_INCREMENT_VALUE);
			this.configurator.setExtraClasspathProvider(configuration, CONTRIBUTOR_ID,
					JanusNetworkClasspathProvider.class.getName());
		} else {
			arguments.clearArg(SreNetworkConfig.ENABLE_NAME);
			arguments.clearArg(SreNetworkConfig.CLUSTER_NAME_NAME);
			arguments.clearArg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME);
			arguments.clearArg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME);
			this.configurator.setExtraClasspathProvider(configuration, CONTRIBUTOR_ID, null);
		}
		arguments.apply(configuration, this.configurator);
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		final OutputExtraJreArguments arguments = LaunchConfigurationUtils.createOutputExtraJreArguments(CONTRIBUTOR_ID);
		final boolean enable = this.enableNetworkButton.getSelection();
		if (enable) {
			arguments.arg(SreNetworkConfig.ENABLE_NAME, enable);
			arguments.arg(SreNetworkConfig.CLUSTER_NAME_NAME, this.clusterNameText.getText());
			arguments.arg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME, this.minClusterSizeSpinner.getSelection());
			arguments.arg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME, this.portAutoIncrementButton.getSelection());
			this.configurator.setExtraClasspathProvider(configuration, CONTRIBUTOR_ID,
					JanusNetworkClasspathProvider.class.getName());
		} else {
			arguments.clearArg(SreNetworkConfig.ENABLE_NAME);
			arguments.clearArg(SreNetworkConfig.CLUSTER_NAME_NAME);
			arguments.clearArg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME);
			arguments.clearArg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME);
			this.configurator.setExtraClasspathProvider(configuration, CONTRIBUTOR_ID, null);
		}
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
		public void modifyText(ModifyEvent event) {
			updateLaunchConfigurationDialog();
		}

	}

}

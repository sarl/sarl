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

package io.sarl.sre.eclipse.network;

import static io.sarl.eclipse.launching.dialog.SarlSwtFactory.createInfoBubble;
import static io.sarl.eclipse.launching.dialog.SarlSwtFactory.createInfoDecorator;
import static io.sarl.eclipse.launching.dialog.SarlSwtFactory.createSpinner;
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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;
import io.sarl.eclipse.launching.config.LaunchConfigurationUtils;
import io.sarl.eclipse.launching.config.LaunchConfigurationUtils.InputExtraJreArguments;
import io.sarl.eclipse.launching.config.LaunchConfigurationUtils.OutputExtraJreArguments;
import io.sarl.lang.util.CliUtilities;
import io.sarl.sre.eclipse.JanusEclipsePlugin;
import io.sarl.sre.network.boot.configs.JoinMethod;
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

	/**
	 * Identifier of the contributor for the launch configuration.
	 */
	public static final String CONTRIBUTOR_ID = "io.sarl.sre.network"; //$NON-NLS-1$

	private static final int MIN_CLUSTER_SIZE = 1;

	private static final int MAX_CLUSTER_SIZE = 100;

	private static final int INDENT = 25;

	private Button enableNetworkButton;

	private Group globalGroup;

	private Label clusterNameLabel;

	private Text clusterNameText;

	private Label minClusterSizeLabel;

	private Spinner minClusterSizeSpinner;

	private Button portAutoIncrementButton;

	private Group hazelcastMulticastGroup;

	private Button tcpIPClusterRadioButton;

	private Label tcpIPClusterRadioButtonBubble;

	private Button multicastClusterRadioButton;

	private Label multicastClusterRadioButtonBubble;

	private Text hazelcastIPMembersTextField;

	private final WidgetListener defaultListener = new WidgetListener();

	@Inject
	private ILaunchConfigurationConfigurator configurator;

	@Inject
	private ILaunchConfigurationAccessor accessor;

	/**
	 * Construct the tab for configuration of the SRE networking feature.
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

		this.enableNetworkButton = SWTFactory
				.createCheckButton(topComp,
						MessageFormat.format(Messages.JanusLaunchNetworkTab_1,
								CliUtilities.getCommandLineOption(SreNetworkConfigModule.NETWORK_LONG_OPTION, true),
								CliUtilities.getCommandLineDefinition(
										VariableNames.toPropertyName(SreNetworkConfig.ENABLE_NAME), true)),
						null, false, 2);
		this.enableNetworkButton.addSelectionListener(this.defaultListener);

		this.globalGroup = SWTFactory.createGroup(topComp, Messages.JanusLaunchNetworkTab_11, 2, 2,
				GridData.FILL_HORIZONTAL);

		final String defaultClusterName = SreNetworkConfig.createStandardClusterName(Messages.JanusLaunchNetworkTab_6);
		this.clusterNameLabel = createLabel(this.globalGroup,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_2,
						CliUtilities.getCommandLineDefinition(
								VariableNames.toPropertyName(SreNetworkConfig.CLUSTER_NAME_NAME),
								Messages.JanusLaunchNetworkTab_8)),
				1);
		this.clusterNameText = createSingleText(this.globalGroup, 1);
		this.clusterNameText.setMessage(defaultClusterName);
		this.clusterNameText.addModifyListener(this.defaultListener);
		createInfoDecorator(this.clusterNameText, MessageFormat.format(Messages.JanusLaunchNetworkTab_7,
				defaultClusterName, Messages.JanusLaunchNetworkTab_6));

		this.minClusterSizeLabel = createLabel(this.globalGroup,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_3,
						CliUtilities.getCommandLineDefinition(
								VariableNames.toPropertyName(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME),
								Messages.JanusLaunchNetworkTab_9)),
				1);
		this.minClusterSizeSpinner = createSpinner(this.globalGroup, 1, MIN_CLUSTER_SIZE, MAX_CLUSTER_SIZE);
		this.minClusterSizeSpinner.addModifyListener(this.defaultListener);
		createInfoDecorator(this.minClusterSizeSpinner, MessageFormat.format(Messages.JanusLaunchNetworkTab_10,
				MIN_CLUSTER_SIZE, SreNetworkConfig.DEFAULT_MIN_CLUSTER_SIZE_VALUE));

		this.portAutoIncrementButton = SWTFactory.createCheckButton(this.globalGroup,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_4,
						CliUtilities.getCommandLineDefinition(
								VariableNames.toPropertyName(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME), true)),
				null, false, 2);
		this.portAutoIncrementButton.addSelectionListener(this.defaultListener);

		createVerticalSpacer(topComp, 2);
		createSeparator(parent, 2);
		createVerticalSpacer(topComp, 2);

		this.hazelcastMulticastGroup = SWTFactory.createGroup(this.globalGroup,
				MessageFormat.format("Node discovery ({0})",
						CliUtilities.getCommandLineDefinition(
								VariableNames.toPropertyName(SreNetworkConfig.JOIN_METHOD_NAME),
								Messages.JanusLaunchNetworkTab_15)),
				2, 2, GridData.FILL_HORIZONTAL);

		this.multicastClusterRadioButton = createRadioButton(this.hazelcastMulticastGroup,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_13, Messages.JanusLaunchNetworkTab_15,
						JoinMethod.MULTICAST.toJsonString()));
		this.multicastClusterRadioButton.addSelectionListener(this.defaultListener);
		this.multicastClusterRadioButtonBubble = createInfoBubble(this.hazelcastMulticastGroup,
				Messages.JanusLaunchNetworkTab_17);

		this.tcpIPClusterRadioButton = createRadioButton(this.hazelcastMulticastGroup,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_12, Messages.JanusLaunchNetworkTab_15,
						JoinMethod.TCP_IP.toJsonString()));
		this.tcpIPClusterRadioButton.addSelectionListener(this.defaultListener);
		this.tcpIPClusterRadioButtonBubble = createInfoBubble(this.hazelcastMulticastGroup,
				Messages.JanusLaunchNetworkTab_16);

		createVerticalSpacer(topComp, 1);

		this.hazelcastIPMembersTextField = createSingleText(this.hazelcastMulticastGroup, 2);
		((GridData) this.hazelcastIPMembersTextField.getLayoutData()).horizontalIndent = INDENT;
		this.hazelcastIPMembersTextField.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent event) {
				scheduleUpdateJob();
			}
		});

		setControl(topComp);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), getHelpContextId());
		updateComponentStates();
	}

	/**
	 * Update the states of the components (enabling).
	 */
	protected void updateComponentStates() {
		final boolean enable = this.enableNetworkButton.getSelection();
		final boolean enableIpList = enable && this.tcpIPClusterRadioButton.getSelection();
		this.globalGroup.setEnabled(enable);
		this.clusterNameLabel.setEnabled(enable);
		this.clusterNameText.setEnabled(enable);
		this.minClusterSizeLabel.setEnabled(enable);
		this.minClusterSizeSpinner.setEnabled(enable);
		this.portAutoIncrementButton.setEnabled(enable);
		this.hazelcastMulticastGroup.setEnabled(enable);
		this.multicastClusterRadioButton.setEnabled(enable);
		this.multicastClusterRadioButtonBubble.setEnabled(enable);
		this.tcpIPClusterRadioButton.setEnabled(enable);
		this.tcpIPClusterRadioButtonBubble.setEnabled(enable);
		this.hazelcastIPMembersTextField.setEnabled(enableIpList);
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		super.initializeFrom(configuration);
		final InputExtraJreArguments arguments = LaunchConfigurationUtils.createInputExtraJreArguments(CONTRIBUTOR_ID);
		arguments.read(configuration, this.accessor);
		final boolean enable = arguments.arg(SreNetworkConfig.ENABLE_NAME, SreNetworkConfig.DEFAULT_ENABLE_VALUE);
		this.enableNetworkButton.setSelection(enable);
		this.clusterNameText.setText(
				arguments.arg(SreNetworkConfig.CLUSTER_NAME_NAME, SreNetworkConfig.DEFAULT_CLUSTER_NAME_VALUE));
		this.minClusterSizeSpinner.setSelection(
				arguments.arg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME, SreNetworkConfig.DEFAULT_MIN_CLUSTER_SIZE_VALUE));
		this.portAutoIncrementButton.setSelection(arguments.arg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME,
				SreNetworkConfig.DEFAULT_PORT_AUTO_INCREMENT_VALUE));

		final JoinMethod join = JoinMethod.valueOfCaseInsensitive(
				arguments.arg(SreNetworkConfig.JOIN_METHOD_NAME, JoinMethod.getDefault().toJsonString()));
		switch (join) {
		case MULTICAST:
			this.multicastClusterRadioButton.setSelection(true);
			break;
		case TCP_IP:
			this.tcpIPClusterRadioButton.setSelection(true);
			break;
		default:
			break;
		}
		this.hazelcastIPMembersTextField
				.setText(arguments.arg(SreNetworkConfig.IP_LIST_CLUSTER, SreNetworkConfig.DEFAULT_IP_LIST_CLUSTER));

		updateComponentStates();
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
		final OutputExtraJreArguments arguments = LaunchConfigurationUtils
				.createOutputExtraJreArguments(CONTRIBUTOR_ID);
		arguments.arg(SreNetworkConfig.ENABLE_NAME, SreNetworkConfig.DEFAULT_ENABLE_VALUE,
				SreNetworkConfig.DEFAULT_ENABLE_VALUE);
		arguments.resetArg(SreNetworkConfig.CLUSTER_NAME_NAME);
		arguments.resetArg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME);
		arguments.resetArg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME);
		if (SreNetworkConfig.DEFAULT_ENABLE_VALUE) {
			this.configurator.setExtraClasspathProvider(configuration, CONTRIBUTOR_ID,
					JanusNetworkClasspathProvider.class.getName());
		} else {
			this.configurator.setExtraClasspathProvider(configuration, CONTRIBUTOR_ID, null);
		}
		arguments.resetArg(SreNetworkConfig.JOIN_METHOD_NAME);
		arguments.resetArg(SreNetworkConfig.IP_LIST_CLUSTER);
		arguments.apply(configuration, this.configurator);
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		final OutputExtraJreArguments arguments = LaunchConfigurationUtils
				.createOutputExtraJreArguments(CONTRIBUTOR_ID);
		final boolean enable = this.enableNetworkButton.getSelection();
		arguments.arg(SreNetworkConfig.ENABLE_NAME, enable, SreNetworkConfig.DEFAULT_ENABLE_VALUE);
		arguments.arg(SreNetworkConfig.CLUSTER_NAME_NAME, this.clusterNameText.getText(),
				SreNetworkConfig.DEFAULT_CLUSTER_NAME_VALUE);
		arguments.arg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME, this.minClusterSizeSpinner.getSelection(),
				SreNetworkConfig.DEFAULT_MIN_CLUSTER_SIZE_VALUE);
		arguments.arg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME, this.portAutoIncrementButton.getSelection(),
				SreNetworkConfig.DEFAULT_PORT_AUTO_INCREMENT_VALUE);
		if (enable) {
			this.configurator.setExtraClasspathProvider(configuration, CONTRIBUTOR_ID,
					JanusNetworkClasspathProvider.class.getName());
		} else {
			this.configurator.setExtraClasspathProvider(configuration, CONTRIBUTOR_ID, null);
		}

		if (this.tcpIPClusterRadioButton.getSelection()) {
			arguments.arg(SreNetworkConfig.JOIN_METHOD_NAME, JoinMethod.TCP_IP.toJsonString(),
					JoinMethod.getDefault().toJsonString());
			arguments.arg(SreNetworkConfig.IP_LIST_CLUSTER, this.hazelcastIPMembersTextField.getText(),
					SreNetworkConfig.DEFAULT_IP_LIST_CLUSTER);
		}

		if (this.multicastClusterRadioButton.getSelection()) {
			arguments.arg(SreNetworkConfig.JOIN_METHOD_NAME, JoinMethod.MULTICAST.toJsonString(),
					JoinMethod.getDefault().toJsonString());
			arguments.resetArg(SreNetworkConfig.IP_LIST_CLUSTER);
		}

		arguments.apply(configuration, this.configurator);
	}

	@Override
	public boolean isValid(ILaunchConfiguration launchConfig) {
		setMessage(null);
		setErrorMessage(null);

		if (!SreNetworkConfig.validateClusterMemberIPsArray(this.hazelcastIPMembersTextField.getText())) {
			setErrorMessage(Messages.JanusLaunchNetworkTab_14);
			return false;
		}

		return true;
	}

	/**
	 * Listener of events in internal components for refreshing the tab.
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
			final Object clickedObject = event.getSource();
			if (clickedObject == JanusLaunchNetworkTab.this.enableNetworkButton
					|| clickedObject == JanusLaunchNetworkTab.this.tcpIPClusterRadioButton
					|| clickedObject == JanusLaunchNetworkTab.this.multicastClusterRadioButton) {
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

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

	/**
	 * Regexp of a quarter of an IP adress.
	 */
	private static final String ZEROTO255 = "([01]?[0-9]{1,2}|2[0-4][0-9]|25[0-5])"; //$NON-NLS-1$

	/**
	 * Regexp of a full IP address.
	 */
	private static final String IP_REGEXP = ZEROTO255 + "\\." + ZEROTO255 + "\\." + ZEROTO255 + "\\." + ZEROTO255; //$NON-NLS-1$

	private static final int MIN_CLUSTER_SIZE = 1;

	private static final int MAX_CLUSTER_SIZE = 100;

	private Button enableNetworkButton;

	private Label clusterNameLabel;

	private Text clusterNameText;

	private Label minClusterSizeLabel;

	private Spinner minClusterSizeSpinner;

	private Button portAutoIncrementButton;

	private Group hazelcastMulticastGroup;

	private Button tcpIPClusterRadioButton;

	private Button multicastClusterRadioButton;

	private Group hazelcastIPMembersGroup;

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

		createVerticalSpacer(topComp, 2);
		createSeparator(parent, 2);
		createVerticalSpacer(topComp, 2);

		final String defaultClusterName = SreNetworkConfig.createStandardClusterName(Messages.JanusLaunchNetworkTab_6);
		this.clusterNameLabel = createLabel(topComp,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_2,
						CliUtilities.getCommandLineDefinition(
								VariableNames.toPropertyName(SreNetworkConfig.CLUSTER_NAME_NAME),
								Messages.JanusLaunchNetworkTab_8)),
				1);
		this.clusterNameText = createSingleText(topComp, 1);
		this.clusterNameText.setMessage(defaultClusterName);
		this.clusterNameText.addModifyListener(this.defaultListener);
		createInfoDecorator(this.clusterNameText, MessageFormat.format(Messages.JanusLaunchNetworkTab_7,
				defaultClusterName, Messages.JanusLaunchNetworkTab_6));

		this.minClusterSizeLabel = createLabel(topComp,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_3,
						CliUtilities.getCommandLineDefinition(
								VariableNames.toPropertyName(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME),
								Messages.JanusLaunchNetworkTab_9)),
				1);
		this.minClusterSizeSpinner = createSpinner(topComp, 1, MIN_CLUSTER_SIZE, MAX_CLUSTER_SIZE);
		this.minClusterSizeSpinner.addModifyListener(this.defaultListener);
		createInfoDecorator(this.minClusterSizeSpinner, MessageFormat.format(Messages.JanusLaunchNetworkTab_10,
				MIN_CLUSTER_SIZE, SreNetworkConfig.DEFAULT_MIN_CLUSTER_SIZE_VALUE));

		this.portAutoIncrementButton = SWTFactory.createCheckButton(topComp,
				MessageFormat.format(Messages.JanusLaunchNetworkTab_4,
						CliUtilities.getCommandLineDefinition(
								VariableNames.toPropertyName(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME), true)),
				null, false, 2);
		this.portAutoIncrementButton.addSelectionListener(this.defaultListener);

		this.hazelcastMulticastGroup = SWTFactory.createGroup(topComp, Messages.JanusLaunchNetworkTab_11, 1, 2,
				GridData.FILL_HORIZONTAL);
		this.tcpIPClusterRadioButton = createRadioButton(hazelcastMulticastGroup, Messages.JanusLaunchNetworkTab_12);
		this.tcpIPClusterRadioButton.addSelectionListener(this.defaultListener);
		this.multicastClusterRadioButton = createRadioButton(hazelcastMulticastGroup,
				Messages.JanusLaunchNetworkTab_13);
		this.multicastClusterRadioButton.addSelectionListener(this.defaultListener);

		this.hazelcastIPMembersGroup = SWTFactory.createGroup(topComp, Messages.JanusLaunchNetworkTab_14, 2, 1,
				GridData.FILL_HORIZONTAL);
		this.hazelcastIPMembersTextField = createSingleText(hazelcastIPMembersGroup, 1);
		this.hazelcastIPMembersTextField.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent event) {
				final String iplist = ((Text) event.widget).getText();
				final String[] ipArray = iplist.split(",");
				Boolean validIps = true;
				for (final String s : ipArray) {
					if (!s.matches(IP_REGEXP)) {
						validIps = false;
					}
				}

				if (!validIps) {
					setErrorMessage(Messages.JanusLaunchNetworkTab_15);
				}
			}
		});

		switch (JoinMethod.getDefault()) {
		case MULTICAST:
			this.tcpIPClusterRadioButton.setSelection(false);
			this.multicastClusterRadioButton.setSelection(true);
			this.hazelcastIPMembersTextField.setEnabled(false);
			this.hazelcastIPMembersTextField.setEditable(false);
			this.hazelcastMulticastGroup.setEnabled(false);
			break;
		case TCP_IP:
			this.tcpIPClusterRadioButton.setSelection(true);
			this.multicastClusterRadioButton.setSelection(false);
			this.hazelcastIPMembersTextField.setEnabled(true);
			this.hazelcastIPMembersTextField.setEditable(true);
			this.hazelcastIPMembersTextField.setSelection(1, this.hazelcastIPMembersTextField.getText().length() + 1);
			this.hazelcastMulticastGroup.setEnabled(true);
			this.hazelcastIPMembersTextField.setText(SreNetworkConfig.DEFAULT_IP_LIST_CLUSTER);
			this.hazelcastIPMembersGroup.layout();
			break;
		default:
			break;
		}

		this.tcpIPClusterRadioButton.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetSelected(SelectionEvent event) {

				JanusLaunchNetworkTab.this.hazelcastIPMembersTextField.setEnabled(true);
				JanusLaunchNetworkTab.this.hazelcastIPMembersTextField.setEditable(true);
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent event) {
				//
			}
		});
		this.multicastClusterRadioButton.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetSelected(SelectionEvent event) {
				JanusLaunchNetworkTab.this.hazelcastIPMembersTextField.setEnabled(false);
				JanusLaunchNetworkTab.this.hazelcastIPMembersTextField.setEditable(false);
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent event) {
				//
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
		this.clusterNameLabel.setEnabled(enable);
		this.clusterNameText.setEnabled(enable);
		this.minClusterSizeLabel.setEnabled(enable);
		this.minClusterSizeSpinner.setEnabled(enable);
		this.portAutoIncrementButton.setEnabled(enable);
		this.hazelcastMulticastGroup.setEnabled(enable);

		/*
		 * final boolean enableTCPIP = enable &&
		 * this.tcpIPClusterRadioButton.getSelection();
		 * System.out.println("netenable: "+enable+" enableTCPIP: "+enableTCPIP);
		 * this.hazelcastIPMembersTextField.setEnabled(enableTCPIP);
		 * this.hazelcastIPMembersTextField.setEditable(enableTCPIP);
		 */
		// this.hazelcastMulticastGroup.setEnabled(enableTCPIP);
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

		switch (JoinMethod.valueOfCaseInsensitive(
				arguments.arg(SreNetworkConfig.JOIN_METHOD_NAME, JoinMethod.getDefault().toJsonString()))) {
		case MULTICAST:
			this.tcpIPClusterRadioButton.setSelection(false);
			this.multicastClusterRadioButton.setSelection(true);
			this.hazelcastIPMembersTextField.setEnabled(false);
			this.hazelcastIPMembersTextField.setEditable(false);
			this.hazelcastMulticastGroup.setEnabled(false);
			break;
		case TCP_IP:
			this.tcpIPClusterRadioButton.setSelection(true);
			this.multicastClusterRadioButton.setSelection(false);
			this.hazelcastIPMembersTextField.setEnabled(true);
			this.hazelcastIPMembersTextField.setEditable(true);
			this.hazelcastIPMembersTextField.setSelection(1, this.hazelcastIPMembersTextField.getText().length() + 1);
			this.hazelcastMulticastGroup.setEnabled(true);
			this.hazelcastIPMembersTextField
					.setText(arguments.arg(SreNetworkConfig.IP_LIST_CLUSTER, SreNetworkConfig.DEFAULT_IP_LIST_CLUSTER));
			this.hazelcastIPMembersGroup.layout();
			break;
		default:
			break;
		}

		updateComponentStates();
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy configuration) {
		final OutputExtraJreArguments arguments = LaunchConfigurationUtils
				.createOutputExtraJreArguments(CONTRIBUTOR_ID);
		arguments.arg(SreNetworkConfig.ENABLE_NAME, SreNetworkConfig.DEFAULT_ENABLE_IN_ECLIPSE_VALUE,
				SreNetworkConfig.DEFAULT_ENABLE_VALUE);
		arguments.resetArg(SreNetworkConfig.CLUSTER_NAME_NAME);
		arguments.resetArg(SreNetworkConfig.MIN_CLUSTER_SIZE_NAME);
		arguments.resetArg(SreNetworkConfig.PORT_AUTO_INCREMENT_NAME);
		if (SreNetworkConfig.DEFAULT_ENABLE_IN_ECLIPSE_VALUE) {
			this.configurator.setExtraClasspathProvider(configuration, CONTRIBUTOR_ID,
					JanusNetworkClasspathProvider.class.getName());
		} else {
			this.configurator.setExtraClasspathProvider(configuration, CONTRIBUTOR_ID, null);
		}

		arguments.resetArg(SreNetworkConfig.JOIN_METHOD_NAME);
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

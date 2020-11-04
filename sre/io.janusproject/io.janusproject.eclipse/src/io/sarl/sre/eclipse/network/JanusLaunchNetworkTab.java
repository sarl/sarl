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

import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaLaunchTab;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;

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

	private Button enableNetworkButton;

	private final WidgetListener defaultListener = new WidgetListener();

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
	public String getId() {
		return "io.sarl.sre.eclipse.network.janusLaunchNetworkTab"; //$NON-NLS-1$
	}

	@Override
	public void createControl(Composite parent) {
		final Font font = parent.getFont();
		final Composite topComp = SWTFactory.createComposite(parent, font, 1, 1, GridData.FILL_HORIZONTAL, 0, 0);

		this.enableNetworkButton = createCheckButton(topComp, Messages.JanusLaunchNetworkTab_1);
		this.enableNetworkButton.addSelectionListener(this.defaultListener);

		setControl(topComp);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), getHelpContextId());
	}

	@Override
	protected void updateLaunchConfigurationDialog() {
		super.updateLaunchConfigurationDialog();
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		super.initializeFrom(configuration);
	}

	@Override
	public void dispose() {
		super.dispose();
	}

	@Override
	public void activated(ILaunchConfigurationWorkingCopy workingCopy) {
		super.activated(workingCopy);
	}

	@Override
	public boolean isValid(ILaunchConfiguration config) {
		return true;
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		//
	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		//
	}

	/** Listener of events in internal components for refreshing the tab.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
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

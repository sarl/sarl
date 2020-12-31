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

import javax.inject.Inject;

import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.debug.ui.StringVariableSelectionDialog;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaArgumentsTab;
import org.eclipse.jdt.internal.debug.ui.actions.ControlAccessibleListener;
import org.eclipse.jdt.internal.debug.ui.launcher.VMArgumentsBlock;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;

/**
 * Class for the configuration tab for the SARL arguments.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLArgumentsTab extends JavaArgumentsTab {

	private static final int HEIGHT_HINT = 40;

	private static final int WIDTH_HINT = 100;

	/** Arguments for the SRE.
	 */
	protected Text sreArgumentsText;

	@Inject
	private ILaunchConfigurationConfigurator configurator;

	@Inject
	private ILaunchConfigurationAccessor accessor;

	/** Construct a configuration tab for the SARL arguments.
	 */
	public SARLArgumentsTab() {
		//
	}

	@Override
	public String getId() {
		return "io.sarl.eclipse.debug.ui.sarlArgumentsTab"; //$NON-NLS-1$
	}

	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);

		// Change the label for the program arguments
		final Group group = (Group) this.fPrgmArgumentsText.getParent();
		group.setText(Messages.SARLArgumentsTab_0);
		ControlAccessibleListener.addListener(this.fPrgmArgumentsText, group.getText());

		createSREArgsBlock(group.getParent(), parent.getFont());
	}

	/** Create the block for the SRE arguments.
	 *
	 * @param parent the parent composite.
	 * @param font the font for the block.
	 */
	protected void createSREArgsBlock(Composite parent, Font font) {
		// Create the block for the SRE
		final Group group = new Group(parent, SWT.NONE);
		group.setFont(font);
		final GridLayout layout = new GridLayout();
		group.setLayout(layout);
		group.setLayoutData(new GridData(GridData.FILL_BOTH));
		// Move the SRE argument block before the JVM argument block
		group.moveAbove(this.fVMArgumentsBlock.getControl());

		group.setText(Messages.SARLArgumentsTab_1);

		createSREArgsText(group, font);
		createSREArgsVariableButton(group);
	}

	private void createSREArgsText(Group group, Font font) {
		this.sreArgumentsText = new Text(group, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.V_SCROLL);
		this.sreArgumentsText.addTraverseListener(new TraverseListener() {
			@Override
			public void keyTraversed(TraverseEvent event) {
				switch (event.detail) {
				case SWT.TRAVERSE_ESCAPE:
				case SWT.TRAVERSE_PAGE_NEXT:
				case SWT.TRAVERSE_PAGE_PREVIOUS:
					event.doit = true;
					break;
				case SWT.TRAVERSE_RETURN:
				case SWT.TRAVERSE_TAB_NEXT:
				case SWT.TRAVERSE_TAB_PREVIOUS:
					if ((SARLArgumentsTab.this.sreArgumentsText.getStyle() & SWT.SINGLE) != 0) {
						event.doit = true;
					} else {
						if (!SARLArgumentsTab.this.sreArgumentsText.isEnabled() || (event.stateMask & SWT.MODIFIER_MASK) != 0) {
							event.doit = true;
						}
					}
					break;
				default:
				}
			}
		});
		final GridData gd = new GridData(GridData.FILL_BOTH);
		gd.heightHint = HEIGHT_HINT;
		gd.widthHint = WIDTH_HINT;
		this.sreArgumentsText.setLayoutData(gd);
		this.sreArgumentsText.setFont(font);
		this.sreArgumentsText.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void modifyText(ModifyEvent evt) {
				scheduleUpdateJob();
			}
		});
		ControlAccessibleListener.addListener(this.sreArgumentsText, group.getText());
	}

	private void createSREArgsVariableButton(Group group) {
		final Button sreArgVariableButton = createPushButton(group, "", null); //$NON-NLS-1$
		sreArgVariableButton.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));
		sreArgVariableButton.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(SelectionEvent event) {
				final StringVariableSelectionDialog dialog = new StringVariableSelectionDialog(getShell());
				dialog.open();
				final String variable = dialog.getVariableExpression();
				if (variable != null) {
					SARLArgumentsTab.this.sreArgumentsText.insert(variable);
				}
			}
		});
	}

	@Override
	protected VMArgumentsBlock createVMArgsBlock() {
		return new JVMArgsBlock();
	}

	/** This class redefines the label of the JVM argument block.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class JVMArgsBlock extends VMArgumentsBlock {

		JVMArgsBlock() {
			//
		}

		@Override
		public String getName() {
			return Messages.SARLArgumentsTab_2;
		}

		@Override
		public void createControl(Composite parent) {
			super.createControl(parent);

			// Change the label for the program arguments
			final Group group = (Group) this.fVMArgumentsText.getParent();
			group.setText(Messages.SARLArgumentsTab_3);
			ControlAccessibleListener.addListener(this.fVMArgumentsText, group.getText());

			// Add the message for the max memory
			SWTFactory.createLabel(group, Messages.SARLArgumentsTab_5, 0);
		}

	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		super.setDefaults(config);
		this.configurator.setSRELaunchingArguments(config, null);
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		super.initializeFrom(configuration);
		this.sreArgumentsText.setText(this.accessor.getSRELaunchingArguments(configuration));
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		this.configurator.setSRELaunchingArguments(configuration, getAttributeValueFrom(this.sreArgumentsText));
		super.performApply(configuration);
	}

}

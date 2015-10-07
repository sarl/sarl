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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.StringVariableSelectionDialog;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaArgumentsTab;
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.internal.debug.ui.actions.ControlAccessibleListener;
import org.eclipse.jdt.internal.debug.ui.launcher.LauncherMessages;
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
import org.eclipse.xtext.util.Strings;

import io.sarl.eclipse.SARLEclipseConfig;

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
		Group group = (Group) this.fPrgmArgumentsText.getParent();
		String newLabel = io.sarl.eclipse.launching.dialog.LauncherMessages.SARLArgumentsTab_0;
		group.setText(newLabel);
		ControlAccessibleListener.addListener(this.fPrgmArgumentsText, group.getText());

		Font font = parent.getFont();
		Composite comp = group.getParent();

		createSREArgsBlock(comp, font);
	}

	/** Create the block for the SRE arguments.
	 *
	 * @param parent - the parent composite.
	 * @param font - the font for the block.
	 */
	protected void createSREArgsBlock(Composite parent, Font font) {
		// Create the block for the SRE
		Group group = new Group(parent, SWT.NONE);
		group.setFont(font);
		GridLayout layout = new GridLayout();
		group.setLayout(layout);
		group.setLayoutData(new GridData(GridData.FILL_BOTH));
		// Move the SRE argument block before the JVM argument block
		group.moveAbove(this.fVMArgumentsBlock.getControl());

		String controlName = io.sarl.eclipse.launching.dialog.LauncherMessages.SARLArgumentsTab_1;
		group.setText(controlName);

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
		GridData gd = new GridData(GridData.FILL_BOTH);
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
		String buttonLabel = LauncherMessages.JavaArgumentsTab_5;
		Button sreArgVariableButton = createPushButton(group, buttonLabel, null);
		sreArgVariableButton.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_END));
		sreArgVariableButton.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(SelectionEvent event) {
				StringVariableSelectionDialog dialog = new StringVariableSelectionDialog(getShell());
				dialog.open();
				String variable = dialog.getVariableExpression();
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
			return io.sarl.eclipse.launching.dialog.LauncherMessages.SARLArgumentsTab_2;
		}

		@Override
		public void createControl(Composite parent) {
			super.createControl(parent);

			Group group;
			String newLabel;

			// Change the label for the program arguments
			group = (Group) this.fVMArgumentsText.getParent();
			newLabel = io.sarl.eclipse.launching.dialog.LauncherMessages.SARLArgumentsTab_3;
			group.setText(newLabel);
			ControlAccessibleListener.addListener(this.fVMArgumentsText, group.getText());
		}

	}

	@Override
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		super.setDefaults(config);
		config.setAttribute(SARLEclipseConfig.ATTR_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS, (String) null);
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		super.initializeFrom(configuration);
		try {
			this.sreArgumentsText.setText(configuration.getAttribute(
					SARLEclipseConfig.ATTR_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS, Strings.emptyIfNull(null)));
		} catch (CoreException e) {
			setErrorMessage(MessageFormat.format(
					io.sarl.eclipse.launching.dialog.LauncherMessages.SARLArgumentsTab_4,
					e.getStatus().getMessage()));
			JDIDebugUIPlugin.log(e);
		}
	}

	@Override
	public void performApply(ILaunchConfigurationWorkingCopy configuration) {
		configuration.setAttribute(SARLEclipseConfig.ATTR_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS,
				getAttributeValueFrom(this.sreArgumentsText));
		super.performApply(configuration);
	}

}

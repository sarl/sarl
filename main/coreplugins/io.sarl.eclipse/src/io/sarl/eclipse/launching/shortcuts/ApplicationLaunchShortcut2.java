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

package io.sarl.eclipse.launching.shortcuts;

import javax.inject.Inject;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaApplicationLaunchShortcut;
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.internal.debug.ui.launcher.LauncherMessages;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.xtext.xbase.ui.launching.LaunchShortcutUtil;

import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.config.ILaunchConfigurationConfigurator;

/** Shortcut for launching a SARL application.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ApplicationLaunchShortcut2 extends JavaApplicationLaunchShortcut {

	@Inject
	private ILaunchConfigurationAccessor accessor;

	@Inject
	private ILaunchConfigurationConfigurator configurator;

	@Override
	protected ILaunchConfigurationType getConfigurationType() {
		return getLaunchManager().getLaunchConfigurationType(
				this.accessor.getApplicationLaunchConfigurationType());
	}

	@Override
	protected ILaunchConfiguration createConfiguration(IType type) {
		try {
			return this.configurator.newApplicationLaunchConfiguration(
				type.getJavaProject().getElementName(),
				type.getTypeQualifiedName('.'),
				SarlStandardClasspathProvider.class);
		} catch (CoreException exception) {
			MessageDialog.openError(JDIDebugUIPlugin.getActiveWorkbenchShell(),
					LauncherMessages.JavaLaunchShortcut_3, exception.getStatus().getMessage());
		}
		return null;
	}

	/** Replies the launch manager.
	 *
	 * @return the launch manager.
	 */
	@SuppressWarnings("static-method")
	protected ILaunchManager getLaunchManager() {
		return DebugPlugin.getDefault().getLaunchManager();
	}

	@Override
	public void launch(ISelection selection, String mode) {
		if (selection instanceof IStructuredSelection) {
			final IStructuredSelection newSelection = LaunchShortcutUtil
					.replaceWithJavaElementDelegates((IStructuredSelection) selection, SarlJavaElementDelegateMainLaunch.class);
			super.launch(newSelection, mode);
		}
	}

	@Override
	public void launch(IEditorPart editor, String mode) {
		final SarlJavaElementDelegateMainLaunch javaElementDelegate = editor.getAdapter(SarlJavaElementDelegateMainLaunch.class);
		if (javaElementDelegate != null) {
			super.launch(new StructuredSelection(javaElementDelegate), mode);
		} else {
			super.launch(editor, mode);
		}
	}

}

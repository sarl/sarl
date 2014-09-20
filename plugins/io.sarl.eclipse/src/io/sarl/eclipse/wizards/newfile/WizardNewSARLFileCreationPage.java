/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.wizards.newfile;

import io.sarl.eclipse.util.PluginUtil;

import java.io.InputStream;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.xtext.util.StringInputStream;

/**
 * First page of the SARL new file wizard.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class WizardNewSARLFileCreationPage extends WizardNewFileCreationPage {

	private static final String WIZARD_NAME = "SARL File Wizard"; //$NON-NLS-1$
	private static final String PAGE_TITLE = "SARL File Wizard"; //$NON-NLS-1$
	private static final String PAGE_DESCRIPTION = "Create a SARL File"; //$NON-NLS-1$
	private static final String SARL_FILE_EXTENSION = "sarl"; //$NON-NLS-1$

	/**
	 * @param selection - selection in the IDE.
	 */
	public WizardNewSARLFileCreationPage(IStructuredSelection selection) {
		super(WIZARD_NAME, selection);

		setTitle(PAGE_TITLE);
		setDescription(PAGE_DESCRIPTION);
		setFileExtension(SARL_FILE_EXTENSION);
		setImageDescriptor(PluginUtil.getImageDescriptor(
				PluginUtil.NEW_FILE_WIZARD_DIALOG_IMAGE));
	}

	private static IPath determinePackageName(IPath path) {
		if (path != null) {
			IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(path.segment(0));
			try {
				if (project != null && project.hasNature(JavaCore.NATURE_ID)) {
					IJavaProject javaProject = JavaCore.create(project);
					for (IClasspathEntry entry : javaProject.getRawClasspath()) {
						if (entry.getPath().isPrefixOf(path)) {
							return path.removeFirstSegments(entry.getPath().segmentCount());
						}
					}
				}
			} catch (Exception e) {
				// Ignore the exceptions since they are not useful (hopefully)
			}
		}
		return null;
	}

	@Override
	protected InputStream getInitialContents() {
		StringBuilder content = new StringBuilder();

		IPath folderInWorkspace = getContainerFullPath();

		IPath packagePath = determinePackageName(folderInWorkspace);
		if (packagePath != null && packagePath.segmentCount() > 0) {
			content.append("package "); //$NON-NLS-1$
			content.append(packagePath.segment(0));
			for (int i = 1; i < packagePath.segmentCount(); ++i) {
				content.append('.');
				content.append(packagePath.segment(i));
			}
			content.append("\n"); //$NON-NLS-1$
		}

		return new StringInputStream(content.toString());
	}

}

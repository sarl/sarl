/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.bspl.eclipse.wizards.newfile;

import java.io.InputStream;

import com.google.inject.Inject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.xtext.util.StringInputStream;
import org.eclipse.xtext.xbase.conversion.XbaseValueConverterService;

import io.sarl.bspl.eclipse.BSPLEclipseConfig;
import io.sarl.bspl.eclipse.BSPLEclipsePlugin;
import io.sarl.bspl.lang.services.BSPLGrammarKeywordAccess;
/**
 * First page of the BSPL new file wizard.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("restriction")
public class NewBsplFileWizardPage extends WizardNewFileCreationPage {

	@Inject
	private XbaseValueConverterService converter;

	@Inject
	private BSPLGrammarKeywordAccess grammarAccess;

	/** Constructor.
	 * @param selection selection in the IDE.
	 * @param fileExtension the extension of the file to create.
	 */
	public NewBsplFileWizardPage(IStructuredSelection selection, String fileExtension) {
		super(Messages.NewBsplFileWizardPage_0, selection);
		setTitle(Messages.NewBsplFileWizardPage_0);
		setDescription(Messages.NewBsplFileWizardPage_1);
		setFileExtension(fileExtension);
		setImageDescriptor(BSPLEclipsePlugin.getDefault().getImageDescriptor(BSPLEclipseConfig.NEW_FILE_WIZARD_DIALOG_IMAGE));
	}

	private static IPath determinePackageName(IPath path) {
		if (path != null) {
			final var project = ResourcesPlugin.getWorkspace().getRoot().getProject(path.segment(0));
			try {
				if (project != null && project.hasNature(JavaCore.NATURE_ID)) {
					final var javaProject = JavaCore.create(project);
					for (final var entry : javaProject.getRawClasspath()) {
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
		final var content = new StringBuilder();

		final var folderInWorkspace = getContainerFullPath();

		final var packagePath = determinePackageName(folderInWorkspace);

		if (packagePath != null && packagePath.segmentCount() > 0) {
			content.append(this.grammarAccess.getPackageKeyword() + " "); //$NON-NLS-1$
			final var packageName = new StringBuilder();
			packageName.append(packagePath.segment(0));
			for (var i = 1; i < packagePath.segmentCount(); ++i) {
				packageName.append(this.grammarAccess.getFullStopKeyword());
				packageName.append(packagePath.segment(i));
			}
			content.append(this.converter.getQualifiedNameValueConverter().toString(packageName.toString()));
			content.append("\n"); //$NON-NLS-1$
		}

		return new StringInputStream(content.toString());
	}

}

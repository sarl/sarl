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

package io.sarl.eclipse.perspectives;

import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.progress.IProgressConstants;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.explorer.SARLPackageExplorerPart;

/**
 * Factory for the SARL Eclipse perspective.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @see SARLEclipseConfig#ID_SARL_PERSPECTIVE
 */
public class SARLPerspectiveFactory implements IPerspectiveFactory {

	private static final float LEFT_PANEL_RATIO = 0.25f;

	private static final float BOTTOM_PANEL_RATIO = 0.75f;

	private static final float RIGHT_PANEL_RATIO = 0.75f;

	@Override
	public void createInitialLayout(IPageLayout layout) {
		final var editorArea = layout.getEditorArea();

		final var projectManagementFolder = layout.createFolder("left", IPageLayout.LEFT, //$NON-NLS-1$
				LEFT_PANEL_RATIO, editorArea);
		projectManagementFolder.addView(SARLPackageExplorerPart.ID_PACKAGES);
		projectManagementFolder.addPlaceholder(JavaUI.ID_TYPE_HIERARCHY);
		projectManagementFolder.addPlaceholder(IPageLayout.ID_PROJECT_EXPLORER);

		final var outputfolder = layout.createFolder("bottom", IPageLayout.BOTTOM, //$NON-NLS-1$
				BOTTOM_PANEL_RATIO, editorArea);
		outputfolder.addView(IPageLayout.ID_PROBLEM_VIEW);
		outputfolder.addView(IConsoleConstants.ID_CONSOLE_VIEW);
		outputfolder.addView(IPageLayout.ID_TASK_LIST);
		outputfolder.addPlaceholder(JavaUI.ID_JAVADOC_VIEW);
		outputfolder.addPlaceholder(JavaUI.ID_SOURCE_VIEW);
		outputfolder.addPlaceholder(NewSearchUI.SEARCH_VIEW_ID);
		outputfolder.addPlaceholder(IPageLayout.ID_BOOKMARKS);
		outputfolder.addPlaceholder(IProgressConstants.PROGRESS_VIEW_ID);
		outputfolder.addPlaceholder(JavaUI.ID_PACKAGES);

		final var outlineFolder = layout.createFolder("right", IPageLayout.RIGHT, //$NON-NLS-1$
				RIGHT_PANEL_RATIO, editorArea);
		outlineFolder.addView(IPageLayout.ID_OUTLINE);

		layout.addActionSet(IDebugUIConstants.LAUNCH_ACTION_SET);
		layout.addActionSet(JavaUI.ID_ACTION_SET);
		layout.addActionSet(JavaUI.ID_ELEMENT_CREATION_ACTION_SET);
		layout.addActionSet(IPageLayout.ID_NAVIGATE_ACTION_SET);

		// views - sarl
		layout.addShowViewShortcut(SARLPackageExplorerPart.ID_PACKAGES);
		layout.addShowViewShortcut(JavaUI.ID_TYPE_HIERARCHY);
		layout.addShowViewShortcut(JavaUI.ID_SOURCE_VIEW);
		layout.addShowViewShortcut(JavaUI.ID_JAVADOC_VIEW);

		// views - search
		layout.addShowViewShortcut(NewSearchUI.SEARCH_VIEW_ID);

		// views - debugging
		layout.addShowViewShortcut(IConsoleConstants.ID_CONSOLE_VIEW);

		// views - standard workbench
		layout.addShowViewShortcut(IPageLayout.ID_OUTLINE);
		layout.addShowViewShortcut(IPageLayout.ID_PROBLEM_VIEW);
		layout.addShowViewShortcut(IPageLayout.ID_TASK_LIST);
		layout.addShowViewShortcut(IProgressConstants.PROGRESS_VIEW_ID);
		layout.addShowViewShortcut(IPageLayout.ID_PROJECT_EXPLORER);
		layout.addShowViewShortcut("org.eclipse.pde.runtime.LogView"); //$NON-NLS-1$

		// new actions - Java project creation wizard
		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlProject"); //$NON-NLS-1$
		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlScript"); //$NON-NLS-1$
		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlAgent"); //$NON-NLS-1$
		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlBehavior"); //$NON-NLS-1$
		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlCapacity"); //$NON-NLS-1$
		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlEvent"); //$NON-NLS-1$
		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlSkill"); //$NON-NLS-1$

		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlClass"); //$NON-NLS-1$
		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlInterface"); //$NON-NLS-1$
		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlEnumeration"); //$NON-NLS-1$
		layout.addNewWizardShortcut(SARLEclipsePlugin.PLUGIN_ID + ".wizard.newSarlAnnotation"); //$NON-NLS-1$

		//layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewEnumCreationWizard"); //$NON-NLS-1$

		layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewPackageCreationWizard"); //$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.jdt.ui.wizards.NewSourceFolderCreationWizard");	 //$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder"); //$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file"); //$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.ui.editors.wizards.UntitledTextFileWizard"); //$NON-NLS-1$

		// 'Window' > 'Open Perspective' contributions
		//--- Add the SARL debug perspective
		layout.addPerspectiveShortcut(SARLEclipseConfig.ID_SARL_DEBUG_PERSPECTIVE);
		//--- Add the Java perspectives
		layout.addPerspectiveShortcut(JavaUI.ID_PERSPECTIVE);
		layout.addPerspectiveShortcut(JavaUI.ID_BROWSING_PERSPECTIVE);
		//--- Add the Debug perspectives
		layout.addPerspectiveShortcut(IDebugUIConstants.ID_DEBUG_PERSPECTIVE);
	}

}

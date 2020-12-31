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

package io.sarl.eclipse.perspectives;

import org.eclipse.debug.internal.ui.IInternalDebugUIConstants;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.console.IConsoleConstants;

import io.sarl.eclipse.SARLEclipseConfig;

/**
 * Factory for the SARL Debug Eclipse perspective.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see SARLEclipseConfig#ID_SARL_DEBUG_PERSPECTIVE
 */
public class SARLDebugPerspectiveFactory implements IPerspectiveFactory {

	private static final float NAVIGATION_PANEL_RATIO = 0.45f;

	private static final float BOTTOM_PANEL_RATIO = 0.75f;

	private static final float TOOL_PANEL_RATIO = 0.5f;

	private static final float OUTLINE_PANEL_RATIO = 0.75f;

	@Override
	public void createInitialLayout(IPageLayout layout) {
		final IFolderLayout consoleFolder = layout.createFolder(IInternalDebugUIConstants.ID_CONSOLE_FOLDER_VIEW,
				IPageLayout.BOTTOM, BOTTOM_PANEL_RATIO, layout.getEditorArea());
		consoleFolder.addView(IConsoleConstants.ID_CONSOLE_VIEW);
		consoleFolder.addView(IPageLayout.ID_TASK_LIST);
		consoleFolder.addPlaceholder(IPageLayout.ID_BOOKMARKS);
		consoleFolder.addPlaceholder(IPageLayout.ID_PROP_SHEET);

		final IFolderLayout navFolder = layout.createFolder(IInternalDebugUIConstants.ID_NAVIGATOR_FOLDER_VIEW,
				IPageLayout.TOP, NAVIGATION_PANEL_RATIO, layout.getEditorArea());
		navFolder.addView(IDebugUIConstants.ID_DEBUG_VIEW);
		navFolder.addPlaceholder(IPageLayout.ID_PROJECT_EXPLORER);

		final IFolderLayout toolsFolder = layout.createFolder(IInternalDebugUIConstants.ID_TOOLS_FOLDER_VIEW,
				IPageLayout.RIGHT, TOOL_PANEL_RATIO, IInternalDebugUIConstants.ID_NAVIGATOR_FOLDER_VIEW);
		toolsFolder.addView(IDebugUIConstants.ID_VARIABLE_VIEW);
		toolsFolder.addView(IDebugUIConstants.ID_BREAKPOINT_VIEW);
		toolsFolder.addPlaceholder(IDebugUIConstants.ID_EXPRESSION_VIEW);
		toolsFolder.addPlaceholder(IDebugUIConstants.ID_REGISTER_VIEW);

		final IFolderLayout outlineFolder = layout.createFolder(IInternalDebugUIConstants.ID_OUTLINE_FOLDER_VIEW,
				IPageLayout.RIGHT, OUTLINE_PANEL_RATIO, layout.getEditorArea());
		outlineFolder.addView(IPageLayout.ID_OUTLINE);

		layout.addActionSet(IDebugUIConstants.LAUNCH_ACTION_SET);
		layout.addActionSet(IDebugUIConstants.DEBUG_ACTION_SET);

		// Set the view shortcuts
		layout.addShowViewShortcut(IDebugUIConstants.ID_DEBUG_VIEW);
		layout.addShowViewShortcut(IDebugUIConstants.ID_VARIABLE_VIEW);
		layout.addShowViewShortcut(IDebugUIConstants.ID_BREAKPOINT_VIEW);
		layout.addShowViewShortcut(IDebugUIConstants.ID_EXPRESSION_VIEW);
		layout.addShowViewShortcut(IPageLayout.ID_OUTLINE);
		layout.addShowViewShortcut(IConsoleConstants.ID_CONSOLE_VIEW);
		layout.addShowViewShortcut(IPageLayout.ID_TASK_LIST);

		// 'Window' > 'Open Perspective' contributions
		//--- Add the SARL debug perspective
		layout.addPerspectiveShortcut(SARLEclipseConfig.ID_SARL_PERSPECTIVE);
	}

}

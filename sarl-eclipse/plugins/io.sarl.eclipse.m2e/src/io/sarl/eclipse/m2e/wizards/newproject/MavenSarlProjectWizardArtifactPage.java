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

package io.sarl.eclipse.m2e.wizards.newproject;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.m2e.core.project.ProjectImportConfiguration;
import org.eclipse.m2e.core.ui.internal.wizards.MavenProjectWizardArtifactPage;

import io.sarl.lang.SARLConfig;

/**
 * Wizard page for artifacts that is used when creating a Maven-based SARL project.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.m2e 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.m2e
 * @since 0.13
 */
@SuppressWarnings("restriction")
public final class MavenSarlProjectWizardArtifactPage extends MavenProjectWizardArtifactPage {

	  /** Constructor.
	   *
	   * @param projectImportConfiguration the configuration for importing.
	   */
	  public MavenSarlProjectWizardArtifactPage(ProjectImportConfiguration projectImportConfiguration) {
	    super(projectImportConfiguration);
	  }

	  @Override
	public List<String> getFolders() {
		final var folders = super.getFolders();
		final var newFolders = new ArrayList<>(folders);
		newFolders.add(SARLConfig.FOLDER_SOURCE_SARL);
		newFolders.add(SARLConfig.FOLDER_SOURCE_GENERATED);
		return newFolders;
	}
	  
}

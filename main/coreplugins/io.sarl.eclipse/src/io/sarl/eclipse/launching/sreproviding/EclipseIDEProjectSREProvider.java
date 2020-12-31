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

package io.sarl.eclipse.launching.sreproviding;

import com.google.common.base.MoreObjects;
import com.google.common.base.Strings;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.properties.RuntimeEnvironmentPropertyPage;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.ProjectSREProvider;
import io.sarl.eclipse.runtime.SARLRuntime;

/** Default implementation of a project SRE provider.
 * This provider is reading the Eclipse IDE properties associated to the project and
 * determine the corresponding SRE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see EclipseIDEProjectSREProviderFactory
 */
public class EclipseIDEProjectSREProvider implements ProjectSREProvider {

	private final boolean projectSpecificConfiguration;

	private final boolean projectUseSystemSRE;

	private final String projectSRE;

	/** Constructor.
	 * @param project the project.
	 */
	public EclipseIDEProjectSREProvider(IProject project) {
		boolean tmpSpecific = false;
		boolean tmpUseSystem = false;
		String tmpSRE = null;
		if (project != null) {
			try {
				QualifiedName propertyName = RuntimeEnvironmentPropertyPage.qualify(
						RuntimeEnvironmentPropertyPage.PROPERTY_NAME_HAS_PROJECT_SPECIFIC);
				tmpSpecific = Boolean.parseBoolean(MoreObjects.firstNonNull(
						project.getPersistentProperty(propertyName),
						Boolean.FALSE.toString()));
				if (tmpSpecific) {
					propertyName = RuntimeEnvironmentPropertyPage.qualify(
							RuntimeEnvironmentPropertyPage.PROPERTY_NAME_USE_SYSTEM_WIDE_SRE);
					tmpUseSystem = Boolean.parseBoolean(MoreObjects.firstNonNull(
							project.getPersistentProperty(propertyName), Boolean.FALSE.toString()));
					if (!tmpUseSystem) {
						propertyName = RuntimeEnvironmentPropertyPage.qualify(
								RuntimeEnvironmentPropertyPage.PROPERTY_NAME_SRE_INSTALL_ID);
						tmpSRE = Strings.nullToEmpty(project.getPersistentProperty(propertyName));
					}
				}
			} catch (CoreException e) {
				SARLEclipsePlugin.getDefault().log(e);
			}
		}
		this.projectSpecificConfiguration = tmpSpecific;
		this.projectUseSystemSRE = tmpUseSystem;
		this.projectSRE = tmpSRE;
	}

	@Override
	public boolean hasProjectSpecificSREConfiguration() {
		return this.projectSpecificConfiguration;
	}

	@Override
	public boolean isSystemSREUsed() {
		return this.projectUseSystemSRE;
	}

	@Override
	public String getSREInstallIdentifier() {
		return this.projectSRE;
	}

	@Override
	public ISREInstall getProjectSREInstall() {
		ISREInstall sre = null;
		if (this.projectSpecificConfiguration
				&& !this.projectUseSystemSRE) {
			if (!Strings.isNullOrEmpty(this.projectSRE)) {
				sre = SARLRuntime.getSREFromId(this.projectSRE);
			}
		}
		if (sre == null) {
			sre = SARLRuntime.getDefaultSREInstall();
		}
		return sre;
	}

}

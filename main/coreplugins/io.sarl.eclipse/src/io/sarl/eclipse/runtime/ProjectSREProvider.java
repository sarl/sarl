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

package io.sarl.eclipse.runtime;

/** The objects that are implementing this interface are able to
 * provide a project-level SRE to the {@link ProjectSREProvider}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ProjectSREProvider {

	/** Replies if the project has a specific project configuration.
	 *
	 * @return <code>true</code> if the project has a specific configuration;
	 *     otherwise <code>false</code>.
	 */
	boolean hasProjectSpecificSREConfiguration();

	/** Replies if the project has a specific project configuration that is
	 * using the system SRE.
	 *
	 * @return <code>true</code> if the project has a specific configuration AND
	 *     this configuration specifies that the system SRE is used; <code>false</code>
	 *     in the other cases.
	 */
	boolean isSystemSREUsed();

	/** Replies if identifier of the SRE used by the project in its specific
	 * configuration.
	 *
	 * @return the identifier of the SRE used by the project when it has a specific
	 *     configuration AND not using the system SRE; otherwise {@code null}.
	 */
	String getSREInstallIdentifier();

	/** Replies the SRE that is used by the project.
	 *
	 * @return the SRE install for the project.
	 */
	ISREInstall getProjectSREInstall();

}

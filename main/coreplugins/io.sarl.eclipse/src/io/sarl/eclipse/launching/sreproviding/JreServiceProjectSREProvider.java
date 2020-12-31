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

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import io.sarl.bootstrap.SRE;
import io.sarl.eclipse.runtime.AbstractSREInstall;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.ProjectSREProvider;

/** Implementation of a project SRE provider based on the Java service definitions.
 * This provider is reading the JRE service definitions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see ServiceLoader
 * @see JreServiceProjectSREProviderFactory
 */
public class JreServiceProjectSREProvider implements ProjectSREProvider {

	private final String id;

	private final String name;

	private final String location;

	private final String bootstrap;

	private final List<IRuntimeClasspathEntry> classpath;

	/** Construct a SRE installation provider.
	 *
	 * @param id the identifier of this SRE installation.
	 * @param name the human readable name of this SRE installation.
	 * @param location is the location of this SRE installation.
	 * @param bootstrap the bootstrap of the SRE installation.
	 * @param classpath the classpath associated to the provided SRE.
	 */
	public JreServiceProjectSREProvider(String id, String name, String location, String bootstrap,
			List<IRuntimeClasspathEntry> classpath) {
		this.id = id;
		this.name = name;
		this.location = location;
		this.bootstrap = bootstrap;
		this.classpath = classpath;
	}

	@Override
	public boolean hasProjectSpecificSREConfiguration() {
		return true;
	}

	@Override
	public boolean isSystemSREUsed() {
		return false;
	}

	@Override
	public String getSREInstallIdentifier() {
		return this.id;
	}

	@Override
	public ISREInstall getProjectSREInstall() {
		return new BootstrappedSREInstall(this.id, this.name, this.location, this.bootstrap, this.classpath);
	}

	/** Implementation of an SREInstall that is related to a bootstrapped SRE.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	private static class BootstrappedSREInstall extends AbstractSREInstall {

		private final String location;

		/** Construct a SRE installation.
		 *
		 * @param id the identifier of this SRE installation.
		 * @param name the human readable name of this SRE installation.
		 * @param location is the location of this SRE installation.
		 * @param bootstrap the bootstrap of the SRE installation.
		 * @param classpath the libraries on the classpath.
		 */
		BootstrappedSREInstall(String id, String name, String location, String bootstrap, List<IRuntimeClasspathEntry> classpath) {
			super(id);
			setName(name);
			this.location = location;
			setBootstrap(bootstrap);
			setMainClass(SRE.class.getName());
			setClassPathEntries(classpath);
		}

		@Override
		public String getName() {
			final String name = getNameNoDefault();
			if (Strings.isNullOrEmpty(name)) {
				return getLocation();
			}
			return name;
		}

		@Override
		public String getLocation() {
			return this.location;
		}

		@Override
		public IPath getPreferredClassPathContainerPath() {
			return null;
		}

		@Override
		public Map<String, String> getAvailableCommandLineOptions() {
			return Collections.emptyMap();
		}

		@Override
		public String getSREArguments() {
			return ""; //$NON-NLS-1$
		}

		@Override
		public String getJVMArguments() {
			return ""; //$NON-NLS-1$
		}

		@Override
		public void getAsXML(Document document, Element element) throws IOException {
			// There is no need to store the preferences and configuration into the SRE preferences.
		}

		@Override
		public void setFromXML(Element element) throws IOException {
			// There is no need to store the preferences and configuration into the SRE preferences.
		}

		@Override
		protected void resolveDirtyFields(boolean forceSettings) {
			// Assuming that all the fields have a valid value.
		}

	}
}

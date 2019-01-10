/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.janusproject.eclipse.sre;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import com.google.common.base.Strings;
import com.google.common.collect.Maps;
import org.eclipse.core.runtime.IPath;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import io.janusproject.Boot;
import io.janusproject.Bootstrap;
import io.janusproject.JanusConfig;
import io.janusproject.eclipse.buildpath.JanusClasspathContainer;
import io.janusproject.eclipse.buildpath.JanusClasspathContainerInitializer;

import io.sarl.eclipse.runtime.AbstractSREInstall;
import io.sarl.eclipse.runtime.SREConstants;
import io.sarl.eclipse.util.BundleUtil.IBundleDependencies;

/**
 * Provide Janus as a SRE install.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JanusSREInstall extends AbstractSREInstall {

	/**
	 * The unique identifier of this SRE.
	 */
	public static final String JANUS_SRE_ID = "io.janusproject.plugin.sre"; //$NON-NLS-1$

	/**
	 * The path where this SRE plugin jar is effectively installed.
	 */
	private final IPath janusSREInstallPath;

	/**
	 * The path of this installation of the Janus plugin.
	 */
	private final String location;

	/**
	 * Creates the a JANUS SRE install.
	 */
	public JanusSREInstall() {
		super(JANUS_SRE_ID);
		final IBundleDependencies dependencies = JanusClasspathContainer.getJanusPlatformClasspath();
		//
		this.janusSREInstallPath = dependencies.getBundleBinaryPath();
		assert this.janusSREInstallPath != null;
		this.location = this.janusSREInstallPath.toPortableString();
		setName(JanusConfig.JANUS_DEFAULT_PLATFORM_NAME);
		setMainClass(Boot.class.getName());
		setBootstrap(Bootstrap.class.getName());
		//
		setClassPathEntries(dependencies.getTransitiveRuntimeClasspathEntries(true));
	}

	@Override
	public String getName() {
		final String name = getNameNoDefault();
		if (Strings.isNullOrEmpty(name)) {
			return Messages.JanusSREInstall_0;
		}
		return name;
	}

	@Override
	public String getLocation() {
		return this.location;
	}

	@Override
	public IPath getPreferredClassPathContainerPath() {
		return JanusClasspathContainerInitializer.CONTAINER_ID;
	}

	@Override
	public Map<String, String> getAvailableCommandLineOptions() {
		final Map<String, String> options = Maps.newHashMap();
		options.put(SREConstants.MANIFEST_CLI_SHOW_LOGO, ""); //$NON-NLS-1$
		options.put(SREConstants.MANIFEST_CLI_HIDE_LOGO, formatCommandLineOption(Boot.CLI_OPTION_NOLOGO_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_SHOW_INFO, formatCommandLineOption(Boot.CLI_OPTION_LOG_LONG, "info")); //$NON-NLS-1$
		options.put(SREConstants.MANIFEST_CLI_HIDE_INFO, formatCommandLineOption(Boot.CLI_OPTION_LOG_LONG, "warning")); //$NON-NLS-1$
		options.put(SREConstants.MANIFEST_CLI_DEFAULT_CONTEXT_ID, formatCommandLineOption(Boot.CLI_OPTION_WORLDID_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_RANDOM_CONTEXT_ID, formatCommandLineOption(Boot.CLI_OPTION_RANDOMID_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_BOOT_AGENT_CONTEXT_ID, formatCommandLineOption(Boot.CLI_OPTION_BOOTID_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_SRE_OFFLINE, formatCommandLineOption(Boot.CLI_OPTION_OFFLINE_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_EMBEDDED, formatCommandLineOption(Boot.CLI_OPTION_EMBEDDED_LONG, null));
		options.put(SREConstants.MANIFEST_CLI_NO_MORE_OPTION, formatCommandLineOption(null, null));
		return Collections.unmodifiableMap(options);
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
		// Ignore this function since the Janus SRE is embedded in the product.
		// There is no need to store the preferences and configuration into the SRE preferences.
	}

	@Override
	public void setFromXML(Element element) throws IOException {
		// Ignore this function since the Janus SRE is embedded in the product.
		// There is no need to read the preferences and configuration into the SRE preferences.
	}

	@Override
	protected void resolveDirtyFields(boolean forceSettings) {
		// Assuming that all the fields have a valid value.
	}

}

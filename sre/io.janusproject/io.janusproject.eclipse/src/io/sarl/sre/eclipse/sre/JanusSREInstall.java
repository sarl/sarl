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

package io.sarl.sre.eclipse.sre;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import com.google.common.base.Strings;
import com.google.common.collect.Maps;
import org.eclipse.core.runtime.IPath;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import io.sarl.api.bootiquebase.config.LogConfig;
import io.sarl.api.bootiquebase.config.LogConfigModule;
import io.sarl.api.bootiquebase.config.LogLevel;
import io.sarl.bootstrap.SREBootstrap;
import io.sarl.eclipse.runtime.AbstractSREInstall;
import io.sarl.eclipse.runtime.SRECommandLineOptions;
import io.sarl.eclipse.util.BundleUtil.IBundleDependencies;
import io.sarl.lang.util.CliUtilities;
import io.sarl.sre.boot.Boot;
import io.sarl.sre.boot.configs.subconfigs.BootConfigModule;
import io.sarl.sre.boot.configs.subconfigs.RootContextType;
import io.sarl.sre.eclipse.buildpath.JanusClasspathContainer;
import io.sarl.sre.eclipse.buildpath.JanusClasspathContainerInitializer;

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
		setMainClass(Boot.class.getName());
		setBootstrap(SREBootstrap.class.getName());
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
		// Logging
		options.put(SRECommandLineOptions.CLI_LOG, LogConfigModule.LOG_LONG_OPTION);
		options.put(SRECommandLineOptions.CLI_LOG_VALUES, LogLevel.getAllJsonLabels(false));
		options.put(SRECommandLineOptions.CLI_LOG_DEFAULT_VALUE, LogLevel.toJsonString(LogConfig.DEFAULT_LEVEL));
		// Embedded
		//options.put(SRECommandLineOptions.CLI_EMBEDDED, formatCommandLineOption(null, null));
		// Root context configuration
		options.put(SRECommandLineOptions.CLI_DEFAULT_CONTEXT_ID,
				CliUtilities.getUnixCommandLineOption(BootConfigModule.BOOT_TYPE_OPTION, RootContextType.DEFAULT.toJsonString()));
		options.put(SRECommandLineOptions.CLI_RANDOM_CONTEXT_ID,
				CliUtilities.getUnixCommandLineOption(BootConfigModule.BOOT_TYPE_OPTION, RootContextType.RANDOM.toJsonString()));
		options.put(SRECommandLineOptions.CLI_BOOT_AGENT_CONTEXT_ID,
				CliUtilities.getUnixCommandLineOption(BootConfigModule.BOOT_TYPE_OPTION, RootContextType.BOOT_AGENT_NAME.toJsonString()));
		// Option for disabling the command-line options.
		options.put(SRECommandLineOptions.CLI_NO_MORE_OPTION, CliUtilities.getUnixCommandLineLastOptionPrefix());
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

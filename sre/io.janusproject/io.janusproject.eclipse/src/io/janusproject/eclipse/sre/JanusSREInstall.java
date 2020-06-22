/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
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
import java.util.logging.Level;

import com.google.common.base.Strings;
import com.google.common.collect.Maps;
import org.eclipse.core.runtime.IPath;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import io.janusproject.eclipse.buildpath.JanusClasspathContainer;
import io.janusproject.eclipse.buildpath.JanusClasspathContainerInitializer;
import io.sarl.api.bootiquebase.config.LogLevel;
import io.sarl.bootstrap.SREBootstrap;
import io.sarl.eclipse.runtime.AbstractSREInstall;
import io.sarl.eclipse.runtime.SRECommandLineOptions;
import io.sarl.eclipse.util.BundleUtil.IBundleDependencies;
import io.sarl.sre.boot.Boot;
import io.sarl.sre.boot.configs.SreConfig;
import io.sarl.sre.boot.configs.subconfigs.BootConfigModule;
import io.sarl.sre.boot.configs.subconfigs.RootContextType;

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

	// TODO: the Log4jIntegrationModule does not provide a public field with the name of the option.
	private static final String LOG_CLI_OPTION = "log"; //$NON-NLS-1$

	private static final String OPTION_TERMINATOR = "--"; //$NON-NLS-1$

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
		//TODO Verify that PREFIX is the correct replacement of the previous config that was .JANUS_DEFAULT_PLATFORM_NAME
		setName(SreConfig.PREFIX);
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
		options.put(SRECommandLineOptions.CLI_SHOW_INFO, formatCommandLineOption(LOG_CLI_OPTION, new LogLevel(Level.INFO).toJsonString()));
		options.put(SRECommandLineOptions.CLI_HIDE_INFO, formatCommandLineOption(LOG_CLI_OPTION, new LogLevel(Level.WARNING).toJsonString()));
		// Networking
		//options.put(SRECommandLineOptions.CLI_SRE_OFFLINE, formatCommandLineOption(null, null));
		//options.put(SRECommandLineOptions.CLI_SRE_ONLINE, formatCommandLineOption(null, null));
		// Embedded
		//options.put(SRECommandLineOptions.CLI_EMBEDDED, formatCommandLineOption(null, null));
		// Root context configuration
		options.put(SRECommandLineOptions.CLI_DEFAULT_CONTEXT_ID,
				formatCommandLineOption(BootConfigModule.BOOT_TYPE_OPTION, RootContextType.DEFAULT.toJsonString()));
		options.put(SRECommandLineOptions.CLI_RANDOM_CONTEXT_ID,
				formatCommandLineOption(BootConfigModule.BOOT_TYPE_OPTION, RootContextType.RANDOM.toJsonString()));
		options.put(SRECommandLineOptions.CLI_BOOT_AGENT_CONTEXT_ID,
				formatCommandLineOption(BootConfigModule.BOOT_TYPE_OPTION, RootContextType.BOOT_AGENT_NAME.toJsonString()));
		// Option for disabling the command-line options.
		options.put(SRECommandLineOptions.CLI_NO_MORE_OPTION, OPTION_TERMINATOR);
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

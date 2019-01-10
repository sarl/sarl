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

package io.sarl.maven.sre;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Map;
import java.util.Objects;
import java.util.jar.Attributes;
import java.util.jar.Manifest;

import com.google.common.base.Strings;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;

import io.sarl.eclipse.runtime.SREConstants;
import io.sarl.lang.SARLVersion;

/** Abstract mojo for compiling SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSREMojo extends AbstractMojo {

	/**
	 * The current Maven session.
	 */
	@Parameter(defaultValue = "${session}", required = true, readonly = true)
	private MavenSession session;

	/**
	 * The current Maven project.
	 */
	@Parameter(defaultValue = "${project}", readonly = true, required = true)
	private MavenProject project;

	/** The boot class of the SRE.
	 */
	@Parameter(required = true)
	private String mainClass;

	/** The bootstrap of the SRE.
	 *
	 * @since 0.6
	 */
	@Parameter(required = false)
	private String bootstrap;

	/**
	 * The version of the SARL specification supported by the SRE.
	 */
	@Parameter(defaultValue = SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING, required = true)
	private String sarlSpecificationVersion;

	/**
	 * Name of the SRE.
	 */
	@Parameter(required = true)
	private String sreName;

	/**
	 * Arguments for the Java virtual machine that will be used for running the SRE.
	 */
	@Parameter
	private String vmArguments;

	/**
	 * Arguments for the application that will be ran by the SRE.
	 */
	@Parameter
	private String applicationArguments;

	/**
	 * Command line options.
	 */
	@Parameter
	private CommandLineOptions commandLineOptions;

	/**
	 * Indicates if the SRE is standalone. A standalone SRE is a SRE that is given in a single archive with
	 * all its dependencies inside.
	 *
	 * <p>If this parameter is <code>true</code>, the SRE does not need other Maven artifact to be used.
	 * If this parameter is <code>false</code>, the SRE needs another Maven artifact to be used.
	 *
	 * <p>By default, a SRE is assued to be standalone.
	 * @deprecated since 0.7, no replacement
	 */
	@Parameter(defaultValue = "true")
	@Deprecated
	private boolean standaloneSRE;

	private ManifestUpdater manifestUpdater;

	/** Replies the manifest updater.
	 *
	 * @return the updater.
	 */
	public ManifestUpdater getManifestUpdater() {
		return this.manifestUpdater;
	}

	/** Change the manifest updater.
	 *
	 * @param updater the updater.
	 */
	public void setManifestUpdater(ManifestUpdater updater) {
		this.manifestUpdater = updater;
	}

	@Override
	public final void execute() throws MojoExecutionException, MojoFailureException {
		try {
			executeMojo();
		} catch (Exception e) {
			getLog().error(e.getLocalizedMessage());
			throw e;
		}
	}

	/** Execute the mojo.
	 *
	 * @throws MojoExecutionException if an unexpected problem occurs. Throwing this
	 *     exception causes a "BUILD ERROR" message to be displayed.
	 * @throws MojoFailureException if an expected problem (such as a compilation failure)
	 *     occurs. Throwing this exception causes a "BUILD FAILURE" message to be displayed.
	 */
	protected abstract void executeMojo() throws MojoExecutionException, MojoFailureException;

	/** Put the string representation of the properties of this object into the given buffer.
	 *
	 * @param buffer the buffer.
	 */
	public void buildPropertyString(StringBuilder buffer) {
		buffer.append("sarlSpecification = ").append(this.sarlSpecificationVersion).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("sreName = ").append(this.sreName).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("standaloneSRE = ").append(this.standaloneSRE).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		if (this.commandLineOptions != null) {
			this.commandLineOptions.buildPropertyString(buffer);
		}
	}

	/** Replies the current Maven session.
	 *
	 * @return the session
	 */
	protected MavenSession getMavenSession() {
		return this.session;
	}

	/** Replies the current Maven project.
	 *
	 * @return the project
	 */
	protected MavenProject getMavenProject() {
		return this.project;
	}

	/** The name of the main class of the SRE.
	 *
	 * @return the main class, never <code>null</code>.
	 */
	protected String getMainClass() {
		return Objects.toString(this.mainClass, ""); //$NON-NLS-1$
	}

	/** Change the name of the main class of the SRE.
	 *
	 * @param name the main class.
	 */
	protected void setMainClass(String name) {
		this.mainClass = name;
	}

	/** The name of the bootstrap of the SRE.
	 *
	 * @return the bootstrap, or <code>null</code>.
	 */
	protected String getBootstrap() {
		return Strings.emptyToNull(this.bootstrap);
	}

	/** Change the bootstrap of the SRE.
	 *
	 * @param bootstrap the bootstrap.
	 */
	protected void setBootstrap(String bootstrap) {
		this.bootstrap = Strings.emptyToNull(bootstrap);
	}

	/** Replies the version number of the SARL specification supported by the SRE.
	 *
	 * @return the sarlSpecificationVersion
	 */
	protected String getSarlSpecificationVersion() {
		if (this.sarlSpecificationVersion == null || this.sarlSpecificationVersion.isEmpty()) {
			return SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING;
		}
		return this.sarlSpecificationVersion;
	}

	/** Replies the name of the SRE.
	 *
	 * @return the name, never <code>null</code>.
	 */
	protected String getSreName() {
		return Objects.toString(this.sreName, ""); //$NON-NLS-1$
	}

	/** Chagne the name of the SRE.
	 *
	 * @param name the name.
	 */
	protected void setSreName(String name) {
		this.sreName = name;
	}

	/** Replies the arguments for the VM.
	 *
	 * @return the arguments, never <code>null</code>.
	 */
	protected String getVmArguments() {
		return Objects.toString(this.vmArguments, ""); //$NON-NLS-1$
	}

	/** Change the arguments for the VM.
	 *
	 * @param args the arguments.
	 */
	protected void setVmArguments(String args) {
		this.vmArguments = args;
	}

	/** Replies the arguments for the application.
	 *
	 * @return the arguments, never <code>null</code>.
	 */
	protected String getApplicationArguments() {
		return Objects.toString(this.applicationArguments, ""); //$NON-NLS-1$
	}

	/** Change the arguments for the application.
	 *
	 * @param args the arguments.
	 */
	protected void setApplicationArguments(String args) {
		this.applicationArguments = args;
	}

	/** Replies the command line options for the SRE.
	 *
	 * @return the command line options.
	 */
	protected CommandLineOptions getCommandLineOptions() {
		if (this.commandLineOptions == null) {
			this.commandLineOptions = new CommandLineOptions();
		}
		return this.commandLineOptions;
	}

	/** Create the manifest of the SRE.
	 *
	 * @param filename the name of the manifest; or <code>null</code> if default.
	 * @return the created file.
	 * @throws MojoExecutionException if the mojo fails.
	 * @throws MojoFailureException if the generation fails.
	 */
	protected File createSREManifestFile(File filename) throws MojoExecutionException, MojoFailureException {
		try {
			final Manifest manifest = createSREManifest();
			final File manifestFile;
			if (filename == null) {
				final File parent = new File(getMavenProject().getBuild().getOutputDirectory());
				manifestFile = new File(parent.getParentFile(), "SRE-MANIFEST.MF"); //$NON-NLS-1$
			} else {
				manifestFile = filename;
			}

			try (FileOutputStream fos = new FileOutputStream(manifestFile)) {
				manifest.write(fos);
			}

			return manifestFile;
		} catch (MojoFailureException exception) {
			throw exception;
		} catch (IOException exception) {
			throw new MojoFailureException(exception.getLocalizedMessage(), exception);
		}
	}

	/** Create the manifest of the SRE.
	 *
	 * @return the created manifest.
	 * @throws MojoExecutionException if the mojo fails.
	 * @throws MojoFailureException if the generation fails.
	 */
	protected Manifest createSREManifest() throws MojoExecutionException, MojoFailureException {
		final Manifest manifest = new Manifest();

		final String mainClass = getMainClass();
		if (mainClass.isEmpty()) {
			throw new MojoFailureException("the main class of the SRE is missed"); //$NON-NLS-1$
		}

		ManifestUpdater updater = getManifestUpdater();
		if (updater == null) {
			updater = new ManifestUpdater() {

				@Override
				public void addSREAttribute(String name, String value) {
					assert name != null && !name.isEmpty();
					if (value != null && !value.isEmpty()) {
						getLog().debug("Adding to SRE manifest: " + name + " = " + value);  //$NON-NLS-1$//$NON-NLS-2$
						final Map<String, Attributes> entries = manifest.getEntries();
						Attributes attrs = entries.get(SREConstants.MANIFEST_SECTION_SRE);
						if (attrs == null) {
							attrs = new Attributes();
							entries.put(SREConstants.MANIFEST_SECTION_SRE, attrs);
						}
						attrs.put(new Attributes.Name(name), value);
					}
				}

				@Override
				public void addMainAttribute(String name, String value) {
					assert name != null && !name.isEmpty();
					if (value != null && !value.isEmpty()) {
						getLog().debug("Adding to SRE manifest: " + name + " = " + value);  //$NON-NLS-1$//$NON-NLS-2$
						manifest.getMainAttributes().put(new Attributes.Name(name), value);
					}
				}

			};
		}

		buildManifest(updater, mainClass);

		return manifest;
	}

	/** Create the configuration of the SRE with the maven archive format.
	 *
	 * @return the created manifest.
	 * @throws MojoExecutionException if the mojo fails.
	 * @throws MojoFailureException if the generation fails.
	 */
	protected Xpp3Dom createSREConfiguration() throws MojoExecutionException, MojoFailureException {
		final Xpp3Dom xmlConfiguration = new Xpp3Dom("configuration"); //$NON-NLS-1$
		final Xpp3Dom xmlArchive = new Xpp3Dom("archive"); //$NON-NLS-1$
		xmlConfiguration.addChild(xmlArchive);

		final String mainClass = getMainClass();
		if (mainClass.isEmpty()) {
			throw new MojoFailureException("the main class of the SRE is missed"); //$NON-NLS-1$
		}
		final Xpp3Dom xmlManifest = new Xpp3Dom("manifest"); //$NON-NLS-1$
		xmlArchive.addChild(xmlManifest);
		final Xpp3Dom xmlManifestMainClass = new Xpp3Dom("mainClass"); //$NON-NLS-1$
		xmlManifestMainClass.setValue(mainClass);
		xmlManifest.addChild(xmlManifestMainClass);

		final Xpp3Dom xmlSections = new Xpp3Dom("manifestSections"); //$NON-NLS-1$
		xmlArchive.addChild(xmlSections);
		final Xpp3Dom xmlSection = new Xpp3Dom("manifestSection"); //$NON-NLS-1$
		xmlSections.addChild(xmlSection);
		final Xpp3Dom xmlSectionName = new Xpp3Dom("name"); //$NON-NLS-1$
		xmlSectionName.setValue(SREConstants.MANIFEST_SECTION_SRE);
		xmlSection.addChild(xmlSectionName);
		final Xpp3Dom xmlManifestEntries = new Xpp3Dom("manifestEntries"); //$NON-NLS-1$
		xmlSection.addChild(xmlManifestEntries);

		ManifestUpdater updater = getManifestUpdater();
		if (updater == null) {
			updater = new ManifestUpdater() {

				@Override
				public void addSREAttribute(String name, String value) {
					assert name != null && !name.isEmpty();
					if (value != null && !value.isEmpty()) {
						getLog().debug("Adding to SRE manifest: " + name + " = " + value);  //$NON-NLS-1$//$NON-NLS-2$
						final Xpp3Dom xmlManifestEntry = new Xpp3Dom(name);
						xmlManifestEntry.setValue(value);
						xmlManifestEntries.addChild(xmlManifestEntry);
					}
				}

				@Override
				public void addMainAttribute(String name, String value) {
					//
				}

			};
		}

		buildManifest(updater, mainClass);

		return xmlConfiguration;
	}

	private void buildManifest(ManifestUpdater updater, String mainClass) {
		updater.addMainAttribute(SREConstants.MANIFEST_MAIN_CLASS, mainClass);
		updater.addMainAttribute(SREConstants.MANIFEST_CLASS_PATH, ""); //$NON-NLS-1$

		final String name = getSreName();
		if (name.isEmpty()) {
			getLog().debug("Configuring anonymous SRE");  //$NON-NLS-1$
		} else {
			getLog().debug("Configuring SRE \"" + name + "\"");  //$NON-NLS-1$ //$NON-NLS-2$
		}

		final CommandLineOptions cmd = getCommandLineOptions();

		updater.addSREAttribute(SREConstants.MANIFEST_SARL_SPEC_VERSION, getSarlSpecificationVersion());
		updater.addSREAttribute(SREConstants.MANIFEST_SRE_NAME, name);
		updater.addSREAttribute(SREConstants.MANIFEST_CLI_SRE_OFFLINE, cmd.getOffline());
		updater.addSREAttribute(SREConstants.MANIFEST_CLI_RANDOM_CONTEXT_ID, cmd.getRandomContextId());
		updater.addSREAttribute(SREConstants.MANIFEST_CLI_BOOT_AGENT_CONTEXT_ID, cmd.getBootAgentContextId());
		updater.addSREAttribute(SREConstants.MANIFEST_CLI_DEFAULT_CONTEXT_ID, cmd.getDefaultContextId());
		updater.addSREAttribute(SREConstants.MANIFEST_CLI_HIDE_LOGO, cmd.getHideLogo());
		updater.addSREAttribute(SREConstants.MANIFEST_CLI_SHOW_LOGO, cmd.getShowLogo());
		updater.addSREAttribute(SREConstants.MANIFEST_CLI_HIDE_INFO, cmd.getHideInfo());
		updater.addSREAttribute(SREConstants.MANIFEST_CLI_SHOW_INFO, cmd.getShowInfo());
		updater.addSREAttribute(SREConstants.MANIFEST_CLI_NO_MORE_OPTION, cmd.getNoMoreOption());
		updater.addSREAttribute(SREConstants.MANIFEST_CLI_EMBEDDED, cmd.getEmbedded());
		updater.addSREAttribute(SREConstants.MANIFEST_VM_ARGUMENTS, getVmArguments());
		updater.addSREAttribute(SREConstants.MANIFEST_PROGRAM_ARGUMENTS, getApplicationArguments());
	}

	/** An updater for manifest.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public interface ManifestUpdater {

		/** Add an attribute to the SRE section.
		 *
		 * @param name the name of the attribute.
		 * @param value the value of the attribute.
		 */
		void addSREAttribute(String name, String value);

		/** Add an attribute to the main section.
		 *
		 * @param name the name of the attribute.
		 * @param value the value of the attribute.
		 */
		void addMainAttribute(String name, String value);

	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.maven.docs.generator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Provider;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.compiler.batch.XtendBatchCompiler;
import org.eclipse.xtend.lib.macro.file.Path;
import org.eclipse.xtend.maven.MavenProjectAdapter;
import org.eclipse.xtend.maven.XtendTestCompile;
import org.eclipse.xtext.ISetup;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.file.ProjectConfig;
import org.eclipse.xtext.xbase.file.RuntimeWorkspaceConfigProvider;
import org.eclipse.xtext.xbase.file.WorkspaceConfig;
import org.jnario.compiler.HtmlAssetsCompiler;
import org.jnario.compiler.JnarioDocCompiler;
import org.jnario.feature.FeatureStandaloneSetup;
import org.jnario.maven.JnarioMavenProjectResourceSetProvider;
import org.jnario.report.Executable2ResultMapping;
import org.jnario.report.HashBasedSpec2ResultMapping;
import org.jnario.report.SpecResultParser;
import org.jnario.suite.SuiteStandaloneSetup;

/** Maven MOJO that is generating the documentation for the SARL project.
 *
 * <p>Copied from JnarioDocGenerate (version 1.0.1).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Mojo(name = "generate", requiresDependencyResolution = ResolutionScope.TEST)
public class SARLDocGenerate extends XtendTestCompile {

	/**
	 * Location of the generated documentation.
	 */
	@Parameter(defaultValue = "${basedir}/target/jnario-doc", required = true)
	protected String docOutputDirectory;

	/**
	 * Location of the generated JUnit XML reports.
	 */
	@Parameter(defaultValue = "${basedir}/target/surefire-reports", required = true)
	protected String reportsDirectory;

	/**
	 * Location of the generated JUnit XML reports.
	 */
	@Parameter
	protected String sourceDirectory;

	/**
	 * Location of the generated JUnit XML reports.
	 */
	@Parameter(defaultValue = "true")
	protected boolean sectionNumbering;

	/**
	 * The project itself. This parameter is set by maven.
	 */
	@Parameter(required = true, defaultValue = "${project}")
	@SuppressWarnings("hiding")
	protected MavenProject project;

	/**
	 * The project base directory. This parameter is set by maven.
	 */
	@Parameter(required = true, defaultValue = "${project.basedir}")
	protected File projectBaseDir;

	/**
	 * Set this to true to skip compiling Xtend sources.
	 */
	@Parameter(defaultValue = "false")
	@SuppressWarnings("hiding")
	protected boolean skipXtend;

	/**
	 * Xtend-File encoding argument for the compiler.
	 */
	@Parameter(defaultValue = "${project.build.sourceEncoding}")
	@SuppressWarnings("hiding")
	protected String encoding;

	/**
	 * Set this to false to suppress the creation of *._trace files.
	 */
	@Parameter(defaultValue = "true")
	@SuppressWarnings("hiding")
	protected boolean writeTraceFiles;

	@Inject
	private RuntimeWorkspaceConfigProvider workspaceConfigProvider;

	private Provider<ResourceSet> resourceSetProvider;

	@Override
	protected void internalExecute() throws MojoExecutionException {
		try {
			// Only for injecting the attributes from Maven.
			super.project = this.project;
			super.skipXtend = this.skipXtend;
			super.encoding = this.encoding;
			super.writeTraceFiles = this.writeTraceFiles;

			getLog().info("Generating into " + this.docOutputDirectory); //$NON-NLS-1$

			getLog().debug("Set section numbering: " + Boolean.toString(this.sectionNumbering)); //$NON-NLS-1$
			MavenConfig.setSectionNumbering(this.sectionNumbering);

			// the order is important, the suite compiler must be executed last
			final InjectorCollection injectors = new InjectorCollection(
					new SARLSpecStandaloneSetup(),
					new FeatureStandaloneSetup(),
					new SuiteStandaloneSetup());

			generateCssAndJsFiles(injectors);

			getLog().info("Creating Jnario-based resource provider"); //$NON-NLS-1$
			this.resourceSetProvider = new JnarioMavenProjectResourceSetProvider(this.project);

			getLog().info("Creating Jnario specification map"); //$NON-NLS-1$
			final HashBasedSpec2ResultMapping resultMapping = createSpec2ResultMapping(injectors);
			for (final String injectorId : injectors.getIdentifiers()) {
				final Injector injector = injectors.getInjector(injectorId);
				if (injector != null) {
					generateDoc(injectorId, injector, resultMapping);
				}
			}
		} finally {
			cleanClassFolders();
		}
	}

	private void cleanClassFolders() {
		if (this.projectBaseDir != null) {
			final Pattern pattern = Pattern.compile("^classes[0-9]+$"); //$NON-NLS-1$
			for (final File child : this.projectBaseDir.listFiles()) {
				if (child.isDirectory() && pattern.matcher(child.getName()).matches()) {
					getLog().debug("Deleting " + child.getName()); //$NON-NLS-1$
					try {
						FileSystem.delete(child);
					} catch (IOException e) {
						getLog().warn(e);
					}
				}
			}
		}
	}

	/**
	 * @param injectors - the injector list.
	 * @return the mapping
	 * @throws MojoExecutionException when no Surefire report was found.
	 */
	protected HashBasedSpec2ResultMapping createSpec2ResultMapping(InjectorCollection injectors) throws MojoExecutionException {
		final Injector injector = injectors.getInjector(SuiteStandaloneSetup.class.getName());
		final HashBasedSpec2ResultMapping resultMapping = injector.getInstance(HashBasedSpec2ResultMapping.class);
		getLog().debug("Report directory: " + this.reportsDirectory); //$NON-NLS-1$
		final File reportFolder = new File(this.reportsDirectory);
		if (reportFolder.exists()) {
			addExecutionResults(resultMapping, reportFolder);
		} else {
			throw new MojoExecutionException("Surefire Report folder does not exist"); //$NON-NLS-1$
		}
		return resultMapping;
	}

	/**
	 * @param injectors - the injector list.
	 * @throws MojoExecutionException when no Surefire report was found.
	 */
	protected void generateCssAndJsFiles(InjectorCollection injectors) throws MojoExecutionException {
		getLog().info("Generating CSS and JS assets"); //$NON-NLS-1$
		final Injector injector = injectors.getInjector(SARLSpecStandaloneSetup.class.getName());
		final HtmlAssetsCompiler assetsCompiler = injector.getInstance(HtmlAssetsCompiler.class);
		getLog().debug("Output path: " + this.docOutputDirectory); //$NON-NLS-1$
		assetsCompiler.setOutputPath(this.docOutputDirectory);
		assetsCompiler.compile();
	}

	/**
	 * @param resultMapping - the result mapping
	 * @param reportFolder - the report folder.
	 * @throws MojoExecutionException when parsing error.
	 */
	protected void addExecutionResults(HashBasedSpec2ResultMapping resultMapping, File reportFolder)
			throws MojoExecutionException {
		final SpecResultParser specResultParser = new SpecResultParser();
		for (final File file : reportFolder.listFiles(new XmlFiles())) {
			try (FileInputStream is = new FileInputStream(file)) {
				specResultParser.parse(is, resultMapping);
			} catch (Exception e) {
				throw new MojoExecutionException("Exception while parsing spec for: " + file, e); //$NON-NLS-1$
			}
		}
	}

	@Override
	protected void compileTestSources(XtendBatchCompiler xtend2BatchCompiler) throws MojoExecutionException {
		List<String> testCompileSourceRoots = Lists.newArrayList(this.project.getTestCompileSourceRoots());
		final String testClassPath = Strings.concat(File.pathSeparator, getTestClassPath());
		if (this.sourceDirectory != null && !this.sourceDirectory.isEmpty()) {
			testCompileSourceRoots = Collections.singletonList(this.sourceDirectory);
		}
		getLog().debug("source folders: " + testCompileSourceRoots); //$NON-NLS-1$
		compile(xtend2BatchCompiler, testClassPath, testCompileSourceRoots, this.docOutputDirectory);
	}

	private void generateDoc(String id, Injector injector, Executable2ResultMapping resultMapping) throws MojoExecutionException {
		getLog().info("Generating documentation with " + id); //$NON-NLS-1$
		final JnarioDocCompiler docCompiler = injector.getInstance(JnarioDocCompiler.class);
		docCompiler.setExecutable2ResultMapping(resultMapping);
		compileTestSources(docCompiler);
	}

	@Override
	protected void compile(XtendBatchCompiler xtend2BatchCompiler, String classPath, List<String> sourceDirectories,
			String outputPath) throws MojoExecutionException {
		configureWorkspace(sourceDirectories, outputPath);
		this.resourceSetProvider.get().eAdapters().clear();
		xtend2BatchCompiler.setResourceSetProvider(this.resourceSetProvider);
		MavenProjectAdapter.install(this.resourceSetProvider.get(), this.project);
		final Iterable<String> filtered = Iterables.filter(sourceDirectories, FILE_EXISTS);
		if (Iterables.isEmpty(filtered)) {
			getLog().info(
					"skip compiling sources because the configured directory '" //$NON-NLS-1$
					+ Iterables.toString(sourceDirectories)
					+ "' does not exists."); //$NON-NLS-1$
			return;
		}
		getLog().debug("Set temp directory: " + getTempDirectory()); //$NON-NLS-1$
		xtend2BatchCompiler.setTempDirectory(getTempDirectory());
		getLog().debug("Set DeleteTempDirectory: " + false); //$NON-NLS-1$
		xtend2BatchCompiler.setDeleteTempDirectory(false);
		getLog().debug("Set classpath: " + classPath); //$NON-NLS-1$
		xtend2BatchCompiler.setClassPath(classPath);
		getLog().debug("Set source path: " + Strings.concat(File.pathSeparator, Lists.newArrayList(filtered))); //$NON-NLS-1$
		xtend2BatchCompiler.setSourcePath(Strings.concat(File.pathSeparator, Lists.newArrayList(filtered)));
		getLog().debug("Set output path: " + outputPath); //$NON-NLS-1$
		xtend2BatchCompiler.setOutputPath(outputPath);
		getLog().debug("Set encoding: " + this.encoding); //$NON-NLS-1$
		xtend2BatchCompiler.setFileEncoding(this.encoding);
		getLog().debug("Set writeTraceFiles: " + this.writeTraceFiles); //$NON-NLS-1$
		xtend2BatchCompiler.setWriteTraceFiles(this.writeTraceFiles);
		if (!xtend2BatchCompiler.compile()) {
			throw new MojoExecutionException("Error compiling xtend sources in '" //$NON-NLS-1$
					+ Strings.concat(File.pathSeparator, Lists.newArrayList(filtered)) + "'."); //$NON-NLS-1$
		}
	}

	private void configureWorkspace(List<String> sourceDirectories, String outputPath) throws MojoExecutionException {
		final WorkspaceConfig workspaceConfig = new WorkspaceConfig(this.project.getBasedir().getParentFile().getAbsolutePath());
		final ProjectConfig projectConfig = new ProjectConfig(this.project.getBasedir().getName());
		final URI absoluteRootPath = this.project.getBasedir().getAbsoluteFile().toURI();
		final URI relativizedTarget = absoluteRootPath.relativize(new File(outputPath).toURI());
		if (relativizedTarget.isAbsolute()) {
			throw new MojoExecutionException("Output path '" + outputPath //$NON-NLS-1$
					+ "' must be a child of the project folder '" + absoluteRootPath + "'"); //$NON-NLS-1$ //$NON-NLS-2$
		}
		for (final String source : sourceDirectories) {
			final URI relativizedSrc = absoluteRootPath.relativize(new File(source).toURI());
			if (relativizedSrc.isAbsolute()) {
				throw new MojoExecutionException("Source folder " + source //$NON-NLS-1$
						+ " must be a child of the project folder " + absoluteRootPath); //$NON-NLS-1$
			}
			projectConfig.addSourceFolderMapping(relativizedSrc.getPath(), relativizedTarget.getPath());
		}
		workspaceConfig.addProjectConfig(projectConfig);
		this.workspaceConfigProvider.setWorkspaceConfig(workspaceConfig);
		if (getLog().isDebugEnabled()) {
			getLog().debug("WS config root: " + workspaceConfig.getAbsoluteFileSystemPath()); //$NON-NLS-1$
			getLog().debug("Project name: " + projectConfig.getName()); //$NON-NLS-1$
			getLog().debug("Project root path: " + projectConfig.getRootPath()); //$NON-NLS-1$
			for (final Entry<Path, Path> entry : projectConfig.getSourceFolderMappings().entrySet()) {
				getLog().debug("Source path: " + entry.getKey() + " -> " + entry.getValue()); //$NON-NLS-1$//$NON-NLS-2$
			}
		}
	}

	/** Copied from JnarioDocGenerate  (version 1.0.1).
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private final class XmlFiles implements FilenameFilter {

		XmlFiles() {
			//
		}

		@Override
		public boolean accept(File dir, String name) {
			return name.endsWith("xml"); //$NON-NLS-1$
		}

	}

	/** Collection of injectors.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private final class InjectorCollection {

		private final Map<String, Injector> injectors = new HashMap<>();

		private final List<String> order = new ArrayList<>();

		InjectorCollection(ISetup... setups) {
			for (final ISetup setup : setups) {
				final String id = setup.getClass().getName();
				final Injector injector = setup.createInjectorAndDoEMFRegistration();
				this.order.add(id);
				this.injectors.put(id, injector);
			}
		}

		public Injector getInjector(String id) {
			return this.injectors.get(id);
		}

		public Iterable<String> getIdentifiers() {
			return Collections.unmodifiableCollection(this.order);
		}

	}

}

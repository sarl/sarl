/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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


package io.sarl.examples.tests;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import com.google.common.collect.Iterables;
import com.google.inject.Injector;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.Diagnostician;
import org.junit.Assume;
import org.junit.ComparisonFailure;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.osgi.framework.Bundle;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.internal.LangActivator;
import io.sarl.tests.api.AbstractSarlUiTest;

/** Class for testing the examples.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Parameterized.class)
public class ExamplesTest extends AbstractSarlUiTest {

	private final Injector injector;

	private final Bundle bundle;
	
	private final String name;

	private final URL exampleArchive;

	private final String exampleTargetFolder;

	/** Constructor.
	 *
	 * @param bundle the source bundle.
	 * @param name the name of the test.
	 * @param exampleArchive the archive file to open.
	 * @param exampleTargetFolder the target folder to copy inside.
	 */
	public ExamplesTest(Bundle bundle, String name, URL exampleArchive, String exampleTargetFolder) {
		this.injector = LangActivator.getInstance().getInjector("io.sarl.lang.SARL"); //$NON-NLS-1$
		this.bundle = bundle;
		this.name = name;
		this.exampleArchive = exampleArchive;
		this.exampleTargetFolder = exampleTargetFolder;
		this.injector.injectMembers(this);
	}

	@Override
	public String toString() {
		return this.name;
	}

	/** Replies the bundle of the examples.
	 *
	 * @return the bundle.
	 */
	protected Bundle getExampleBundle() {
		return this.bundle;
	}

	/** Replies the archives for the examples.
	 *
	 * @return the archive locations.
	 * @throws Exception in case of error.
	 */
	@Parameters(name = "Example {1}")
	public static Collection<Object[]> getExampleArchives() throws Exception {
		final Bundle bundle = Platform.getBundle("io.sarl.examples.plugin"); //$NON-NLS-1$
		if (bundle == null) {
			throw new Exception("Bundle not found"); //$NON-NLS-1$
		}

		final Set<String> names = new TreeSet<>();
		final Map<String, String> rawSources = new TreeMap<>();
		
		final Enumeration<String> exampleSources = bundle.getEntryPaths("projects"); //$NON-NLS-1$
		if (exampleSources != null) {
			while (exampleSources.hasMoreElements()) {
				final String example = exampleSources.nextElement();
				URL url = bundle.getResource(example);
				if (url != null) {
					final String name = Path.fromPortableString(example).lastSegment();
					rawSources.putIfAbsent(name, example);
					names.add(name);
				}
			}
		}

		final Map<String, URL> archiveSources = new TreeMap<>();

		final Enumeration<String> exampleArchives = bundle.getEntryPaths("contents"); //$NON-NLS-1$
		if (exampleArchives != null) {
			while (exampleArchives.hasMoreElements()) {
				final String example = exampleArchives.nextElement();
				if (example.endsWith(".zip")) { //$NON-NLS-1$
					final URL url = bundle.getResource(example);
					if (url != null) {
						final String name = Path.fromPortableString(url.getPath()).removeFileExtension().lastSegment();
						archiveSources.putIfAbsent(name, url);
						names.add(name);
					}
				}
			}
		}

		if (names.isEmpty()) {
			throw new Exception("no test found"); //$NON-NLS-1$
		}
		
		final List<Object[]> list = new ArrayList<>();

		for (final String name : names) {
			final URL url = archiveSources.get(name);
			final String path = rawSources.get(name);
			list.add(new Object[] {bundle, name, url, path});
		}
		
		return list;
	}

	/** Run the test for the archive.
	 *
	 * @throws Exception in case of error.
	 */
	@Test
	public void archive() throws Exception {
		Assume.assumeTrue(this.exampleArchive != null && this.exampleTargetFolder == null);
		final IProject project = stage0();
		List<IFile> installedFiles = decompress();
		Iterable<IFile> sarlFiles = stage1(project, installedFiles);
		assertNoErrors(sarlFiles);
	}

	/** Run the test for the archive.
	 *
	 * @throws Exception in case of error.
	 */
	@Test
	public void path() throws Exception {
		Assume.assumeTrue(this.exampleTargetFolder != null);
		final IProject project = stage0();
		List<IFile> installedFiles = copy();
		Iterable<IFile> sarlFiles = stage1(project, installedFiles);
		assertNoErrors(sarlFiles);
	}

	/** Copy the given folder.
	 *
	 * @return the extracted files.
	 * @throws Exception in case of error.
	 */
	private List<IFile> copy() throws Exception {
		final List<IFile> result = new ArrayList<>();
		final List<String> folders = new ArrayList<>();
		folders.add(this.exampleTargetFolder);
		while (!folders.isEmpty()) {
			final String folder = folders.remove(0);
			final Enumeration<String> enumeration = this.bundle.getEntryPaths(folder);
			while (enumeration.hasMoreElements()) {
				final String member = enumeration.nextElement();
				URL url = this.bundle.getResource(member);
				if (url != null) {
					url = FileLocator.toFileURL(FileLocator.resolve(url));	
					if (url != null && "file".equals(url.getProtocol())) { //$NON-NLS-1$
						final IPath targetPath = Path.fromPortableString(member).removeFirstSegments(1);
						final File file = FileSystem.convertURLToFile(url);
						if (file.isDirectory()) {
							folders.add(member);
						} else if (file.isFile()) {
							final IFile targetFile = helper().getFile(targetPath);
							if (!isIgnorableFile(targetFile)) {
								try (InputStream inputStream = url.openStream()) {
									try (UncloseableInputStream ustream = new UncloseableInputStream(inputStream)) {
										helper().createFileImpl(targetFile, ustream);
									}
								}
								result.add(targetFile);
							}
						}
					}
				}
			}
		}
		return result;
	}

	private static boolean isIgnorableFile(IFile file) {
		final String name = file.getName();
		return ".classpath".equals(name) || ".project".equals(name); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private IProject stage0() throws Exception {
		return helper().getProject(this.name, true);
	}

	private Iterable<IFile> stage1(IProject project, Iterable<IFile> installedFiles) throws Exception {
		project.refreshLocal(100, null);
		project.build(IncrementalProjectBuilder.CLEAN_BUILD, null);
		helper().awaitAutoBuild();
		return Iterables.filter(
				installedFiles,
				(it) -> "sarl".equals(it.getFileExtension())); //$NON-NLS-1$
	}

	/** Uncompress the given file.
	 *
	 * @return the extracted files.
	 * @throws Exception in case of error.
	 */
	private List<IFile> decompress() throws Exception {
		final List<IFile> result = new ArrayList<>();
		try (final InputStream inputStream = this.exampleArchive.openStream()) {
			try (final ZipInputStream zipInputStream = new ZipInputStream(inputStream)) {
				ZipEntry entry = zipInputStream.getNextEntry();
				while (entry != null) {
					if (!entry.isDirectory()) {
						final IFile currentFile = helper().getFileInRoot(this.name, entry.getName());
						if (!isIgnorableFile(currentFile)) {
							try (UncloseableInputStream ustream = new UncloseableInputStream(zipInputStream)) {
								helper().createFileImpl(currentFile, ustream);
							}
							result.add(currentFile);
						}
					}
					entry = zipInputStream.getNextEntry();
				}
			}
		}
		return result;
	}

	private void assertNoErrors(Iterable<IFile> sarlFiles) throws Exception {
		final StringBuilder errorDescription = new StringBuilder();

		assertNotNull(sarlFiles);
		assertTrue(sarlFiles.iterator().hasNext());

		for (final IFile decompressedFile : sarlFiles) {
			final SarlScript script = helper().sarlScript(decompressedFile);
			final Resource resource = script.eResource();
			assertTrue(decompressedFile.toString() + ": " + //$NON-NLS-1$
					resource.getErrors().toString(), resource.getErrors().isEmpty());
			final Diagnostic diagnostic = Diagnostician.INSTANCE.validate(script);
			if (diagnostic.getSeverity() ==  Diagnostic.ERROR) {
				final List<Diagnostic> diags = new ArrayList<>();
				diags.add(diagnostic);
				while (!diags.isEmpty()) {
					final Diagnostic diag = diags.remove(0);
					if (diag.getSeverity() ==  Diagnostic.ERROR) {
						if (diag.getChildren().isEmpty()) {
							errorDescription.append(diag.toString()).append("\nFile: "); //$NON-NLS-1$
							errorDescription.append(decompressedFile).append("\n------\n"); //$NON-NLS-1$
						} else {
							diags.addAll(diag.getChildren());
						}
					}
				}
			}
		}
		if (errorDescription.length() > 0) {
			throw new ComparisonFailure("Compilation error(s) found", "", errorDescription.toString()); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/** Stream that cannot be closed.
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class UncloseableInputStream extends FilterInputStream {

		UncloseableInputStream(InputStream stream) {
			super(stream);
		}

		@Override
		public void close() throws IOException {
			//
		}
	}

}

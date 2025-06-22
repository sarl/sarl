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

package io.sarl.eclipse.util.classpath;

import java.io.IOException;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaConventions;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.ToolFactory;
import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.util.IClassFileReader;
import org.eclipse.jdt.internal.corext.dom.IASTSharedValues;
import org.eclipse.jdt.internal.corext.util.JavaConventionsUtil;
import org.eclipse.jdt.internal.ui.wizards.ClassPathDetector;
import org.eclipse.jdt.internal.ui.wizards.NewWizardMessages;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.Utilities;
import io.sarl.lang.SARLConfig;

/** Detect the classpath entries according to the Java and SARL conventions.
 *
 * <p>Class mostly inspired from JDT. See {@link ClassPathDetector}.
 * Unfortunately, the {@link ClassPathDetector} class cannot be overridden because
 * most of its logic is defined in protected methods.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 * @see ClassPathDetector
 */
@SuppressWarnings("restriction")
public class SarlClassPathDetector implements IResourceProxyVisitor {

	private final SarlDefaultClassPathProvider defaultCPProvider;

	private final Map<IPath, List<IPath>> sourceFolders;

	private final List<IResource> classFiles;

	private final Set<IPath> jarFiles;

	private final IProject project;

	private IPath resultOutputFolder;

	private IClasspathEntry[] resultClasspath;

	private final IProgressMonitor monitor;

	private final IPath standardSarlSourceFolder;

	private final IPath testSarlSourceFolder;

	private final IPath generationSarlSourceFolder;

	/** Constructor.
	 *
	 * @param project the project to scan.
	 * @param monitor the progress monitor.
	 * @param defaultCPProvider the provider of default classpath.
	 * @throws CoreException if is it impossible to scan the project.
	 */
	public SarlClassPathDetector(IProject project, SarlDefaultClassPathProvider defaultCPProvider,
			IProgressMonitor monitor) throws CoreException {
		this.defaultCPProvider = defaultCPProvider;
		this.sourceFolders = new HashMap<>();
		this.jarFiles = new HashSet<>(10);
		this.classFiles = new ArrayList<>(100);
		this.project = project;

		this.resultClasspath = null;
		this.resultOutputFolder = null;

		this.standardSarlSourceFolder = Path.fromPortableString(SARLConfig.FOLDER_SOURCE_SARL);
		this.testSarlSourceFolder = Path.fromPortableString(SARLConfig.FOLDER_TEST_SOURCE_SARL);
		this.generationSarlSourceFolder = Path.fromPortableString(SARLConfig.FOLDER_SOURCE_GENERATED);

		var mon = monitor;
		if (mon == null) {
			mon = new NullProgressMonitor();
		}
		this.monitor = mon;

		detectClasspath();
	}

	/** Replies the detected output location.
	 *
	 * @return the output location.
	 */
	@Pure
	public IPath getOutputLocation() {
		return this.resultOutputFolder;
	}

	/** Replies the detected classpath.
	 *
	 * @return the classpath.
	 */
	@Pure
	public IClasspathEntry[] getClasspath() {
		if (this.resultClasspath == null) {
			return new IClasspathEntry[0];
		}
		return this.resultClasspath;
	}

	/** Replies if the given name is valid for a Java compilation unit.
	 *
	 * @param name the name to test.
	 * @return {@code true} if the name is valid.
	 */
	@Pure
	protected boolean isValidJavaCUName(String name) {
		return !JavaConventionsUtil.validateCompilationUnitName(name, JavaCore.create(this.project)).matches(IStatus.ERROR);
	}

	/** Replies if the given name is valid for a SARL compilation unit.
	 *
	 * @param name the name to test.
	 * @return {@code true} if the name is valid.
	 */
	@SuppressWarnings("static-method")
	@Pure
	protected boolean isValidSarlCUName(String name) {
		return FileSystem.hasExtension(name, SARLEclipseConfig.SARL_FILE_EXTENSION);
	}

	private static int computeOrderPriority(IPath path) {
		final var len = path.segmentCount();
		if (len >= 2 && "generated-sources".equals(path.segment(len - 2))) { //$NON-NLS-1$
			return 0;
		}
		if (len >= 1 && "sarl".equals(path.segment(len - 1))) { //$NON-NLS-1$
			return 2;
		}
		return 1;
	}

	/** Replies the comparator of classpath entries.
	 *
	 * @return the comparator
	 */
	@SuppressWarnings("static-method")
	@Pure
	public Comparator<IClasspathEntry> getCPEntryComparator() {
		// TODO Define a Collator for SARL?
		final var collator = Collator.getInstance();
		return (e1, e2) -> {
			final var p1 = e1.getPath();
			final var p2 = e2.getPath();
			final var priority1 = computeOrderPriority(p1);
			final var priority2 = computeOrderPriority(p2);
			final var cmp = priority2 - priority1;
			if (cmp != 0) {
				return cmp;
			}
			return collator.compare(p1.toString(), p2.toString());
		};
	}

	/**
	 * Detect the classpath components.
	 *
	 * @throws CoreException in case of any failure.
	 * @throws OperationCanceledException a runtime exception
	 */
	protected void detectClasspath() throws CoreException {
		try {
			this.monitor.beginTask(NewWizardMessages.ClassPathDetector_operation_description, 4);

			this.project.accept(this, IResource.NONE);
			this.monitor.worked(1);

			final var cpEntries = new ArrayList<IClasspathEntry>();

			detectSourceFolders(cpEntries);
			if (this.monitor.isCanceled()) {
				throw new OperationCanceledException();
			}
			this.monitor.worked(1);

			final var outputLocation = detectOutputFolder();
			if (this.monitor.isCanceled()) {
				throw new OperationCanceledException();
			}
			this.monitor.worked(1);

			detectLibraries(cpEntries, outputLocation);
			if (this.monitor.isCanceled()) {
				throw new OperationCanceledException();
			}
			this.monitor.worked(1);

			if (cpEntries.isEmpty() && this.classFiles.isEmpty()) {
				return;
			}

			this.defaultCPProvider.putDefaultClasspathEntriesIn(cpEntries);

			final var entries = cpEntries.toArray(new IClasspathEntry[cpEntries.size()]);
			if (!JavaConventions.validateClasspath(JavaCore.create(this.project), entries, outputLocation).isOK()) {
				return;
			}

			this.resultClasspath = entries;
			this.resultOutputFolder = outputLocation;
		} finally {
			this.monitor.done();
		}
	}

	/** Detect the source folders according to the Java and SARL conventions.
	 *
	 * @param resEntries the list of entries that should be updated.
	 */
	protected void detectSourceFolders(List<IClasspathEntry> resEntries) {
		final var res = new ArrayList<IClasspathEntry>();
		final var sourceFolderSet = this.sourceFolders.keySet();
		var generatedSourceFolderPresent = false;
		var nbCodeFolders = 0;
		for (final var path : sourceFolderSet) {
			final var entry = createSourceFolderEntry(path);
			res.add(entry);
			if (this.generationSarlSourceFolder.equals(path.removeFirstSegments(0))) {
				generatedSourceFolderPresent = true;
			} else {
				++nbCodeFolders;
			}
		}
		// Ensure that the "generated-source" folder is present
		if (!generatedSourceFolderPresent) {
			final var projectPath = this.project.getFullPath();
			final var entry = createSourceFolderEntry(projectPath.append(this.generationSarlSourceFolder));
			res.add(entry);
		}
		// Ensure that at least one source folder may receive code.
		if (nbCodeFolders == 0) {
			final var projectPath = this.project.getFullPath();
			final var entry = createSourceFolderEntry(projectPath.append(this.standardSarlSourceFolder));
			res.add(entry);
		}
		Collections.sort(res, getCPEntryComparator());
		resEntries.addAll(res);
	}

	private IClasspathEntry createSourceFolderEntry(IPath path) {
		final var excluded = new ArrayList<IPath>();
		for (final var other : this.sourceFolders.keySet()) {
			if (!path.equals(other) && path.isPrefixOf(other)) {
				final var pathToExclude = other.removeFirstSegments(path.segmentCount()).addTrailingSeparator();
				excluded.add(pathToExclude);
			}
		}
		final var excludedPaths = excluded.toArray(new IPath[excluded.size()]);
		return JavaCore.newSourceEntry(path, excludedPaths);
	}

	/** Detect the output folder for binary files.
	 *
	 * @return the detected output folder.
	 * @throws CoreException in case of any failure.
	 */
	protected IPath detectOutputFolder() throws CoreException {
		final var classFolders = new HashSet<IPath>();

		for (final var resource : this.classFiles) {
			final var file = (IFile) resource;
			IClassFileReader reader = null;
			try (var content = file.getContents()) {
				reader = ToolFactory.createDefaultClassFileReader(content, IClassFileReader.CLASSFILE_ATTRIBUTES);
			} catch (IOException exception) {
				throw new CoreException(
						SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception));
			}
			if (reader == null) {
				// problematic class file
				continue;
			}
			final var className = reader.getClassName();
			final var sourceAttribute = reader.getSourceFileAttribute();
			if (className != null && sourceAttribute != null && sourceAttribute.getSourceFileName() != null) {
				final var packPath = file.getParent().getFullPath();
				final var idx = CharOperation.lastIndexOf('/', className) + 1;
				final var relPath = new Path(new String(className, 0, idx));
				final var cuPath = relPath.append(new String(sourceAttribute.getSourceFileName()));

				IPath resPath = null;
				if (idx == 0) {
					resPath = packPath;
				} else {
					final var folderPath = getFolderPath(packPath, relPath);
					if (folderPath != null) {
						resPath = folderPath;
					}
				}
				if (resPath != null) {
					final var path = findInSourceFolders(cuPath);
					if (path != null) {
						return resPath;
					}
					classFolders.add(resPath);
				}
			}
		}
		final var projPath = this.project.getFullPath();
		if (this.sourceFolders.size() == 1 && classFolders.isEmpty() && this.sourceFolders.get(projPath) != null) {
			return projPath;
		}
		// Default path
		var path = projPath.append(SARLConfig.FOLDER_BIN);
		while (classFolders.contains(path)) {
			path = new Path(path.toString() + '1');
		}
		return path;
	}

	/** Detect the libraries.
	 *
	 * @param cpEntries the classpath  entries to be updated with libraries.
	 * @param outputLocation the detected output location.
	 */
	protected void detectLibraries(List<IClasspathEntry> cpEntries, IPath outputLocation) {
		final var res = new ArrayList<IClasspathEntry>();
		final var sourceFolderSet = this.sourceFolders.keySet();
		for (final var path : this.jarFiles) {
			if (Utilities.isNested(path, sourceFolderSet.iterator())) {
				continue;
			}
			if (outputLocation != null && outputLocation.isPrefixOf(path)) {
				continue;
			}
			final var entry = JavaCore.newLibraryEntry(path, null, null);
			res.add(entry);
		}
		Collections.sort(res, getCPEntryComparator());
		cpEntries.addAll(res);
	}

	/** Retrieve the source folder for the given path.
	 *
	 * <p>This function assumes that the list of files in the source folders are known.
	 *
	 * @param path the path to check.
	 * @return the source folder or {@code null} if the path is not inside a source folder.
	 */
	@Pure
	protected IPath findInSourceFolders(IPath path) {
		for (final var entry : this.sourceFolders.entrySet()) {
			if (entry.getValue().contains(path)) {
				return entry.getKey();
			}
		}
		return null;
	}

	/** Utility function for updating the given map.
	 *
	 * @param map the map to update.
	 * @param folderPath the folder patth.
	 * @param relPath the sub path.
	 */
	protected static void addToMap(Map<IPath, List<IPath>> map, IPath folderPath, IPath relPath) {
		var list = map.get(folderPath);
		if (list == null) {
			list = new ArrayList<>(50);
			map.put(folderPath, list);
		}
		list.add(relPath);
	}

	/** Replies the folder path.
	 *
	 * @param packPath the path to path.
	 * @param relpath the relative path.
	 * @return the folder path or {@code null} if it cannot be computed.
	 */
	protected static IPath getFolderPath(IPath packPath, IPath relpath) {
		final var remainingSegments = packPath.segmentCount() - relpath.segmentCount();
		if (remainingSegments >= 0) {
			final var common = packPath.removeFirstSegments(remainingSegments);
			if (common.equals(relpath)) {
				return packPath.uptoSegment(remainingSegments);
			}
		}
		return null;
	}

	/** Visit the given Java compilation unit.
	 *
	 * @param jfile the compilation unit.
	 */
	protected void visitJavaCompilationUnit(IFile jfile) {
		final var cu = JavaCore.createCompilationUnitFrom(jfile);
		if (cu != null) {
			final var parser = ASTParser.newParser(IASTSharedValues.SHARED_AST_LEVEL);
			parser.setSource(cu);
			parser.setFocalPosition(0);
			final var root = (CompilationUnit) parser.createAST(null);
			final var packDecl = root.getPackage();

			final var packPath = jfile.getParent().getFullPath();
			final var cuName = jfile.getName();
			if (packDecl == null) {
				addToMap(this.sourceFolders, packPath, new Path(cuName));
			} else {
				final var relPath = new Path(packDecl.getName().getFullyQualifiedName().replace('.', '/'));
				final var folderPath = getFolderPath(packPath, relPath);
				if (folderPath != null) {
					addToMap(this.sourceFolders, folderPath, relPath.append(cuName));
				}
			}
		}
	}

	private IPath findSarlSourceFolder(IPath path) {
		for (final var sourceFolder : this.sourceFolders.keySet()) {
			if (sourceFolder.isPrefixOf(path)) {
				return sourceFolder;
			}
		}
		final var projectPath = path.uptoSegment(1);
		final var stdSarl = projectPath.append(this.standardSarlSourceFolder);
		if (stdSarl.isPrefixOf(path)) {
			return stdSarl;
		}
		final var testSarl = projectPath.append(this.testSarlSourceFolder);
		if (testSarl.isPrefixOf(path)) {
			return testSarl;
		}
		return null;
	}

	/** Visit the given SARL compilation unit.
	 *
	 * @param sfile the compilation unit.
	 */
	protected void visitSarlCompilationUnit(IFile sfile) {
		final var path = sfile.getFullPath();
		final var sourceFolder = findSarlSourceFolder(path);
		if (sourceFolder != null) {
			addToMap(this.sourceFolders, sourceFolder,
					path.removeFirstSegments(sourceFolder.segmentCount()));
		}
	}

	@Override
	public boolean visit(IResourceProxy proxy) {
		if (this.monitor.isCanceled()) {
			throw new OperationCanceledException();
		}
		if (proxy.getType() == IResource.FILE) {
			final var name = proxy.getName();
			if (isValidJavaCUName(name)) {
				visitJavaCompilationUnit((IFile) proxy.requestResource());
			} else if (isValidSarlCUName(name)) {
				visitSarlCompilationUnit((IFile) proxy.requestResource());
			} else if (FileSystem.hasExtension(name, ".class")) { //$NON-NLS-1$
				this.classFiles.add(proxy.requestResource());
			} else if (FileSystem.hasExtension(name, ".jar")) { //$NON-NLS-1$
				this.jarFiles.add(proxy.requestFullPath());
			}
			return false;
		}
		return true;
	}

}

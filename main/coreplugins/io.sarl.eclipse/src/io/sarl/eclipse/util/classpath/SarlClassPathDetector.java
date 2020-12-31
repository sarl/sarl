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

package io.sarl.eclipse.util.classpath;

import java.io.IOException;
import java.io.InputStream;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaConventions;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.ToolFactory;
import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.util.IClassFileReader;
import org.eclipse.jdt.core.util.ISourceAttribute;
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
	@SuppressWarnings("checkstyle:magicnumber")
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

		IProgressMonitor mon = monitor;
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
		final int len = path.segmentCount();
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
		final Collator collator = Collator.getInstance();
		return (e1, e2) -> {
			final IPath p1 = e1.getPath();
			final IPath p2 = e2.getPath();
			final int priority1 = computeOrderPriority(p1);
			final int priority2 = computeOrderPriority(p2);
			final int cmp = priority2 - priority1;
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
	@SuppressWarnings("checkstyle:magicnumber")
	protected void detectClasspath() throws CoreException {
		try {
			this.monitor.beginTask(NewWizardMessages.ClassPathDetector_operation_description, 4);

			this.project.accept(this, IResource.NONE);
			this.monitor.worked(1);

			final List<IClasspathEntry> cpEntries = new ArrayList<>();

			detectSourceFolders(cpEntries);
			if (this.monitor.isCanceled()) {
				throw new OperationCanceledException();
			}
			this.monitor.worked(1);

			final IPath outputLocation = detectOutputFolder();
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

			final IClasspathEntry[] entries = cpEntries.toArray(new IClasspathEntry[cpEntries.size()]);
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
		final List<IClasspathEntry> res = new ArrayList<>();
		final Set<IPath> sourceFolderSet = this.sourceFolders.keySet();
		boolean generatedSourceFolderPresent = false;
		int nbCodeFolders = 0;
		for (final IPath path : sourceFolderSet) {
			final IClasspathEntry entry = createSourceFolderEntry(path);
			res.add(entry);
			if (this.generationSarlSourceFolder.equals(path.removeFirstSegments(0))) {
				generatedSourceFolderPresent = true;
			} else {
				++nbCodeFolders;
			}
		}
		// Ensure that the "generated-source" folder is present
		if (!generatedSourceFolderPresent) {
			final IPath projectPath = this.project.getFullPath();
			final IClasspathEntry entry = createSourceFolderEntry(projectPath.append(this.generationSarlSourceFolder));
			res.add(entry);
		}
		// Ensure that at least one source folder may receive code.
		if (nbCodeFolders == 0) {
			final IPath projectPath = this.project.getFullPath();
			final IClasspathEntry entry = createSourceFolderEntry(projectPath.append(this.standardSarlSourceFolder));
			res.add(entry);
		}
		Collections.sort(res, getCPEntryComparator());
		resEntries.addAll(res);
	}

	private IClasspathEntry createSourceFolderEntry(IPath path) {
		final List<IPath> excluded = new ArrayList<>();
		for (final IPath other : this.sourceFolders.keySet()) {
			if (!path.equals(other) && path.isPrefixOf(other)) {
				final IPath pathToExclude = other.removeFirstSegments(path.segmentCount()).addTrailingSeparator();
				excluded.add(pathToExclude);
			}
		}
		final IPath[] excludedPaths = excluded.toArray(new IPath[excluded.size()]);
		return JavaCore.newSourceEntry(path, excludedPaths);
	}

	/** Detect the output folder for binary files.
	 *
	 * @return the detected output folder.
	 * @throws CoreException in case of any failure.
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	protected IPath detectOutputFolder() throws CoreException {
		final Set<IPath> classFolders = new HashSet<>();

		for (final IResource resource : this.classFiles) {
			final IFile file = (IFile) resource;
			IClassFileReader reader = null;
			try (InputStream content = file.getContents()) {
				reader = ToolFactory.createDefaultClassFileReader(content, IClassFileReader.CLASSFILE_ATTRIBUTES);
			} catch (IOException exception) {
				throw new CoreException(
						SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception));
			}
			if (reader == null) {
				// problematic class file
				continue;
			}
			final char[] className = reader.getClassName();
			final ISourceAttribute sourceAttribute = reader.getSourceFileAttribute();
			if (className != null && sourceAttribute != null && sourceAttribute.getSourceFileName() != null) {
				final IPath packPath = file.getParent().getFullPath();
				final int idx = CharOperation.lastIndexOf('/', className) + 1;
				final IPath relPath = new Path(new String(className, 0, idx));
				final IPath cuPath = relPath.append(new String(sourceAttribute.getSourceFileName()));

				IPath resPath = null;
				if (idx == 0) {
					resPath = packPath;
				} else {
					final IPath folderPath = getFolderPath(packPath, relPath);
					if (folderPath != null) {
						resPath = folderPath;
					}
				}
				if (resPath != null) {
					final IPath path = findInSourceFolders(cuPath);
					if (path != null) {
						return resPath;
					}
					classFolders.add(resPath);
				}
			}
		}
		final IPath projPath = this.project.getFullPath();
		if (this.sourceFolders.size() == 1 && classFolders.isEmpty() && this.sourceFolders.get(projPath) != null) {
			return projPath;
		}
		// Default path
		IPath path = projPath.append(SARLConfig.FOLDER_BIN);
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
		final List<IClasspathEntry> res = new ArrayList<>();
		final Set<IPath> sourceFolderSet = this.sourceFolders.keySet();
		for (final IPath path : this.jarFiles) {
			if (Utilities.isNested(path, sourceFolderSet.iterator())) {
				continue;
			}
			if (outputLocation != null && outputLocation.isPrefixOf(path)) {
				continue;
			}
			final IClasspathEntry entry = JavaCore.newLibraryEntry(path, null, null);
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
		for (final Entry<IPath, List<IPath>> entry : this.sourceFolders.entrySet()) {
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
	@SuppressWarnings("checkstyle:magicnumber")
	protected static void addToMap(Map<IPath, List<IPath>> map, IPath folderPath, IPath relPath) {
		List<IPath> list = map.get(folderPath);
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
		final int remainingSegments = packPath.segmentCount() - relpath.segmentCount();
		if (remainingSegments >= 0) {
			final IPath common = packPath.removeFirstSegments(remainingSegments);
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
		final ICompilationUnit cu = JavaCore.createCompilationUnitFrom(jfile);
		if (cu != null) {
			final ASTParser parser = ASTParser.newParser(IASTSharedValues.SHARED_AST_LEVEL);
			parser.setSource(cu);
			parser.setFocalPosition(0);
			final CompilationUnit root = (CompilationUnit) parser.createAST(null);
			final PackageDeclaration packDecl = root.getPackage();

			final IPath packPath = jfile.getParent().getFullPath();
			final String cuName = jfile.getName();
			if (packDecl == null) {
				addToMap(this.sourceFolders, packPath, new Path(cuName));
			} else {
				final IPath relPath = new Path(packDecl.getName().getFullyQualifiedName().replace('.', '/'));
				final IPath folderPath = getFolderPath(packPath, relPath);
				if (folderPath != null) {
					addToMap(this.sourceFolders, folderPath, relPath.append(cuName));
				}
			}
		}
	}

	private IPath findSarlSourceFolder(IPath path) {
		for (final IPath sourceFolder : this.sourceFolders.keySet()) {
			if (sourceFolder.isPrefixOf(path)) {
				return sourceFolder;
			}
		}
		final IPath projectPath = path.uptoSegment(1);
		final IPath stdSarl = projectPath.append(this.standardSarlSourceFolder);
		if (stdSarl.isPrefixOf(path)) {
			return stdSarl;
		}
		final IPath testSarl = projectPath.append(this.testSarlSourceFolder);
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
		final IPath path = sfile.getFullPath();
		final IPath sourceFolder = findSarlSourceFolder(path);
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
			final String name = proxy.getName();
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

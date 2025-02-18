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

package io.sarl.lang.sarlc.tools;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.SARLConfig;

/**
 * Default implementation of a path detector.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class DefaultPathDetector implements PathDetector {

	private File sarlOutputPath;

	private File classOutputPath;

	private File tempPath;
	
	/** Default constructor, used for injection.
	 */
	@Inject
	public DefaultPathDetector() {
		//
	}

	@Override
	public void setSarlOutputPath(File path) {
		this.sarlOutputPath = path;
	}

	@Override
	public void setClassOutputPath(File path) {
		this.classOutputPath = path;
	}

	@Override
	public void setTempDirectory(File path) {
		this.tempPath = path;
	}

	@Override
	public File getSarlOutputPath() {
		return this.sarlOutputPath;
	}

	@Override
	public File getClassOutputPath() {
		return this.classOutputPath;
	}

	@Override
	public File getTempDirectory() {
		return this.tempPath;
	}

	@Override
	public boolean isResolved() {
		return this.sarlOutputPath != null && this.tempPath != null && this.classOutputPath != null;
	}

	/** Build the list of all the paths to be resolved.
	 *
	 * @param userFiles the list of files to be provided by hand from additional arguments.
	 * @return full list of the files.
	 * @since 0.13
	 */
	protected Iterable<File> buildResolvablePaths(Iterable<File> userFiles) {
		return Iterables.concat(
				userFiles,
				Collections.singleton(this.sarlOutputPath),
				Collections.singleton(this.tempPath),
				Collections.singleton(this.classOutputPath));
	}

	/** Normalize the paths with the given root file if the paths are not yet normalized.
	 *
	 * @param rootFile the common parent path.
	 * @since 0.13
	 */
	protected void normalizePaths(File rootFile) {
		if (this.sarlOutputPath == null) {
			this.sarlOutputPath = toFile(rootFile, SARLConfig.FOLDER_SOURCE_GENERATED);
		}
		if (this.tempPath == null) {
			this.tempPath = toFile(rootFile, SARLConfig.FOLDER_TMP);
		}
		if (this.classOutputPath == null) {
			this.classOutputPath = toFile(rootFile, SARLConfig.FOLDER_BIN);
		}
	}

	/** Make absolute the paths if the paths are not yet absolute.
	 *
	 * @since 0.13
	 * @throws IOException if the current folder cannot be determined.
	 */
	protected void makeAbsolutePaths() throws IOException {
		if (this.sarlOutputPath == null) {
			this.sarlOutputPath = toFile(cwd(), SARLConfig.FOLDER_SOURCE_GENERATED).getCanonicalFile();
		}
		if (this.sarlOutputPath != null && !this.sarlOutputPath.isAbsolute()) {
			this.sarlOutputPath = FileSystem.join(cwd(), this.sarlOutputPath).getCanonicalFile();
		}

		if (this.tempPath == null) {
			this.tempPath = toFile(cwd(), SARLConfig.FOLDER_TMP).getCanonicalFile();
		}
		if (this.tempPath != null && !this.tempPath.isAbsolute()) {
			this.tempPath = FileSystem.join(cwd(), this.tempPath).getCanonicalFile();
		}

		if (this.classOutputPath == null) {
			this.classOutputPath = toFile(cwd(), SARLConfig.FOLDER_BIN).getCanonicalFile();
		}
		if (this.classOutputPath != null && !this.classOutputPath.isAbsolute()) {
			this.classOutputPath = FileSystem.join(cwd(), this.classOutputPath).getCanonicalFile();
		}
	}

	@Override
	public void resolve(List<String>  args) throws IOException {
		if (!isResolved()) {
			final Iterable<File> cliFiles = Iterables.transform(args, it -> toFile(it));
			File root = determineCommonRoot(buildResolvablePaths(cliFiles));
			if (root != null) {
				root = normalize(root);
				normalizePaths(root);
			}
		}
		makeAbsolutePaths();
	}

	/** Normalize the name of a parent root. This function removes from the filename the standard folder names:
	 * {@code src/main/sarl}, {@code src/main/java}, {@code src/test/sarl}, {@code src/it/sarl}.
	 *
	 * @param filename the filename to normalize
	 * @return the normalized filename.
	 * @since 0.13
	 */
	protected static File normalize(File filename) {
		final var path1 = toFile(SARLConfig.FOLDER_SOURCE_SARL).toPath();
		final var path2 = toFile(SARLConfig.FOLDER_SOURCE_JAVA).toPath();
		final var path3 = toFile(SARLConfig.FOLDER_TEST_SOURCE_SARL).toPath();
		final var path4 = toFile(SARLConfig.FOLDER_INTEGRATION_TEST_SOURCE_SARL).toPath();
		final var path = filename.toPath();
		Path toRemove = null;
		if (path.endsWith(path1)) {
			toRemove = path1;
		} else if (path.endsWith(path2)) {
			toRemove = path2;
		} else if (path.endsWith(path3)) {
			toRemove = path3;
		} else if (path.endsWith(path4)) {
			toRemove = path4;
		}
		if (toRemove != null) {
			final var nb = toRemove.getNameCount();
			var res = filename;
			for (var i = 0; i < nb; ++i) {
				res = res.getParentFile();
			}
			return res;
		}
		return filename;
	}

	/** Convert a filename to its equivalent File object.
	 *
	 * @param filename the filename to convert.
	 * @return the file object.
	 * @since 0.13
	 */
	protected static File toFile(String filename) {
		File result = null;
		for (final var element : filename.split("\\/")) { //$NON-NLS-1$
			if (result == null) {
				result = new File(element);
			} else {
				result = new File(result, element);
			}
		}
		return result;
	}

	/** Merge the given root folder and filename to obtain a fill path.
	 * 
	 * @param root the root folder.
	 * @param filename the filename to merge to the root.
	 * @return the merged file.
	 * @since 0.13
	 */
	protected static File toFile(File root, String filename) {
		var result = root;
		for (final var element : filename.split("\\/")) { //$NON-NLS-1$
			result = new File(result, element);
		}
		return result;
	}

	/** Replies the current directory of the application.
	 *
	 * @return the current directory.
	 * @since 0.13
	 */
	protected static File cwd() {
		return new File("").getAbsoluteFile(); //$NON-NLS-1$
	}

	/** Replies the common parent file for the given files.
	 *
	 * @param files the files to analyze.
	 * @return the common parent file, or {@code null} if there is no common parent file.
	 * @since 0.13
	 */
	protected static File determineCommonRoot(Iterable<File> files) {
		LinkedList<String> longuestPrefix = null;

		for (final var file : files) {
			if (file == null) {
				continue;
			}
			final var components = splitFile(file);
			if (longuestPrefix == null) {
				longuestPrefix = components;
			} else {
				var i = 0;
				while (i < longuestPrefix.size() && i < components.size()
						&& Strings.equal(longuestPrefix.get(i), components.get(i))) {
					++i;
				}
				while (i < longuestPrefix.size()) {
					longuestPrefix.removeLast();
				}
				if (longuestPrefix.isEmpty()) {
					return null;
				}
			}
		}

		if (longuestPrefix == null) {
			return null;
		}

		File prefix = null;
		for (final var component : longuestPrefix) {
			if (prefix == null) {
				prefix = new File(component);
			} else {
				prefix = new File(prefix, component);
			}
		}

		return prefix;
	}

	private static LinkedList<String> splitFile(File file) {
		final var elements = new LinkedList<String>();
		var current = file;
		do {
			elements.addFirst(current.getName());
			current = current.getParentFile();
		} while (current != null);
		return elements;
	}

}

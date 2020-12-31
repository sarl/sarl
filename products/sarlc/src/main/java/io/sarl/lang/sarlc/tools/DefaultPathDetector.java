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

package io.sarl.lang.sarlc.tools;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import com.google.common.collect.Iterables;
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
	@SuppressWarnings("checkstyle:npathcomplexity")
	public void resolve(List<String>  args) throws IOException {
		if (this.sarlOutputPath == null || this.tempPath == null || this.classOutputPath == null) {
			final Iterable<File> cliFiles = Iterables.transform(
				args,
				it -> toFile(it));
			File root = determineCommonRoot(Iterables.concat(
					cliFiles,
					Collections.singleton(this.sarlOutputPath),
					Collections.singleton(this.tempPath),
					Collections.singleton(this.classOutputPath)));
			if (root != null) {
				root = normalize(root);
				if (this.sarlOutputPath == null) {
					this.sarlOutputPath = toFile(root, SARLConfig.FOLDER_SOURCE_GENERATED);
				}
				if (this.tempPath == null) {
					this.tempPath = toFile(root, SARLConfig.FOLDER_TMP);
				}
				if (this.classOutputPath == null) {
					this.classOutputPath = toFile(root, SARLConfig.FOLDER_BIN);
				}
			}
		}

		if (this.sarlOutputPath == null) {
			this.sarlOutputPath = toFile(cwd(), SARLConfig.FOLDER_SOURCE_GENERATED).getCanonicalFile();
		}
		if (this.tempPath == null) {
			this.tempPath = toFile(cwd(), SARLConfig.FOLDER_TMP).getCanonicalFile();
		}
		if (this.classOutputPath == null) {
			this.classOutputPath = toFile(cwd(), SARLConfig.FOLDER_BIN).getCanonicalFile();
		}

		if (this.sarlOutputPath != null && !this.sarlOutputPath.isAbsolute()) {
			this.sarlOutputPath = FileSystem.join(cwd(), this.sarlOutputPath).getCanonicalFile();
		}
		if (this.tempPath != null && !this.tempPath.isAbsolute()) {
			this.tempPath = FileSystem.join(cwd(), this.tempPath).getCanonicalFile();
		}
		if (this.classOutputPath != null && !this.classOutputPath.isAbsolute()) {
			this.classOutputPath = FileSystem.join(cwd(), this.classOutputPath).getCanonicalFile();
		}
	}

	private static File normalize(File filename) {
		final Path path1 = toFile(SARLConfig.FOLDER_SOURCE_SARL).toPath();
		final Path path2 = toFile(SARLConfig.FOLDER_SOURCE_JAVA).toPath();
		final Path path3 = toFile(SARLConfig.FOLDER_TEST_SOURCE_SARL).toPath();
		final Path path = filename.toPath();
		Path toRemove = null;
		if (path.endsWith(path1)) {
			toRemove = path1;
		} else if (path.endsWith(path2)) {
			toRemove = path2;
		} else if (path.endsWith(path3)) {
			toRemove = path3;
		}
		if (toRemove != null) {
			final int nb = toRemove.getNameCount();
			File res = filename;
			for (int i = 0; i < nb; ++i) {
				res = res.getParentFile();
			}
			return res;
		}
		return filename;
	}

	private static File toFile(String filename) {
		File result = null;
		for (final String element : filename.split("\\/")) { //$NON-NLS-1$
			if (result == null) {
				result = new File(element);
			} else {
				result = new File(result, element);
			}
		}
		return result;
	}

	private static File toFile(File root, String filename) {
		File result = root;
		for (final String element : filename.split("\\/")) { //$NON-NLS-1$
			result = new File(result, element);
		}
		return result;
	}

	private static File cwd() {
		return new File("").getAbsoluteFile(); //$NON-NLS-1$
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
	private static File determineCommonRoot(Iterable<File> files) {
		LinkedList<String> longuestPrefix = null;

		for (final File file : files) {
			if (file == null) {
				continue;
			}
			final LinkedList<String> components = splitFile(file);
			if (longuestPrefix == null) {
				longuestPrefix = components;
			} else {
				int i = 0;
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
		for (final String component : longuestPrefix) {
			if (prefix == null) {
				prefix = new File(component);
			} else {
				prefix = new File(prefix, component);
			}
		}

		return prefix;
	}

	private static LinkedList<String> splitFile(File file) {
		final LinkedList<String> elements = new LinkedList<>();
		File current = file;
		do {
			elements.addFirst(current.getName());
			current = current.getParentFile();
		} while (current != null);
		return elements;
	}

}

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

package io.sarl.maven.bootiqueapp.utils;

import java.io.File;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import org.arakhne.afc.vmutil.FileSystem;

/** Represents a collection of path with the OS syntax.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SystemPath implements Iterable<File> {

	private final StringBuilder content = new StringBuilder();

	private final List<File> elements = new LinkedList<>();

	/** Constructor.
	 */
	public SystemPath() {
		//
	}

	@Override
	public String toString() {
		return this.content.toString();
	}

	/** Add an path into the collection.
	 *
	 * @param path the path.
	 */
	public void add(String path) {
		if (!Strings.isNullOrEmpty(path)) {
			if (this.content.length() > 0) {
				this.content.append(File.pathSeparator);
			}
			this.content.append(path);
			this.elements.add(FileSystem.convertStringToFile(path));
		}
	}

	/** Add an path into the collection.
	 *
	 * @param path the path.
	 */
	public void add(File path) {
		if (path != null) {
			if (this.content.length() > 0) {
				this.content.append(File.pathSeparator);
			}
			final String absPath = path.getAbsolutePath();
			this.content.append(absPath);
			this.elements.add(path);
		}
	}

	/** Add a set of paths into the collection.
	 *
	 * @param paths the paths.
	 */
	public void addEntries(SystemPath paths) {
		if (paths != null) {
			if (paths.content.length() > 0) {
				if (this.content.length() > 0) {
					this.content.append(File.pathSeparator);
				}
				this.content.append(paths.content);
			}
			this.elements.addAll(paths.elements);
		}
	}

	/** Add a set of paths into the collection.
	 *
	 * @param paths the paths.
	 */
	public void addEntries(String paths) {
		if (paths != null) {
			for (final String entry : paths.split(Pattern.quote(File.pathSeparator))) {
				add(entry);
			}
		}
	}

	/** Replies the elements into the path as a list of files.
	 *
	 * @return the list of files.
	 */
	public List<File> toFileList() {
		return Collections.unmodifiableList(this.elements);
	}

	/** Replies the elements into the path as a list of filenames.
	 *
	 * @return the list of filenames.
	 */
	public List<String> toFilenameList() {
		return Collections.unmodifiableList(Lists.transform(this.elements, it -> it.getAbsolutePath()));
	}

	/** Replies the number of elements into this path.
	 *
	 * @return the number of elements into this path.
	 */
	public int size() {
		return this.elements.size();
	}

	/** Replies the collection of paths is empty.
	 *
	 * @return {@code true} if the collection is empty.
	 */
	public boolean isEmpty() {
		return this.elements.isEmpty();
	}

	/** Clear the system path.
	 */
	public void clear() {
		this.content.setLength(0);
		this.elements.clear();
	}

	@Override
	public Iterator<File> iterator() {
		return Collections.unmodifiableList(this.elements).iterator();
	}

}

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

package io.sarl.sarldoc.utils;

import java.io.File;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.base.Strings;

/** Represents a collection of path with the OS syntax.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SystemPath implements Iterable<String> {

	private final StringBuilder content = new StringBuilder();

	private final List<String> elements = new LinkedList<>();

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
			this.elements.add(path);
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
			this.elements.add(absPath);
		}
	}

	/** Replies the elements into the path.
	 *
	 * @return the elements.
	 */
	public List<String> getElements() {
		return Collections.unmodifiableList(this.elements);
	}

	/** Replies the number of elements into this path.
	 *
	 * @return the number of elements into this path.
	 */
	public int size() {
		return this.elements.size();
	}

	@Override
	public Iterator<String> iterator() {
		return Collections.unmodifiableList(this.elements).iterator();
	}

}

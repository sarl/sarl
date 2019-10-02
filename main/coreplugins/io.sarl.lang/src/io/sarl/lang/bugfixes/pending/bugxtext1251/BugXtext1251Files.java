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

package io.sarl.lang.bugfixes.pending.bugxtext1251;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Deque;
import java.util.LinkedList;

import org.eclipse.xtext.util.Files;

/**
 * Fixing the Xtext PR 1251: Fixing the behavior of the cleanFolder function in Files class.
 *
 * <p>Issue is due to Xtend PR 1251 (https://github.com/eclipse/xtext-core/pull/1251)
 *
 * <p>Fixing the behavior of cleanFolders
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-core/pull/1251"
 */
public final class BugXtext1251Files extends Files {

	private BugXtext1251Files() {
		//
	}

	/** Clean the content of the the given folder.
	 *
	 * @param parentFolder the folder to be cleaned. It must not be {@code null}.
	 * @param filter a filter for selecting the files to be removed. If it is {@code null}, all the files are removed.
	 * @param continueOnError indicates if the cleaning should continue after an error occurs.
	 * @param deleteParentFolder indicates if {@code parentFolder} should be also deleted if it becomes empty.
	 * @return {@code true} if the cleaning process goes through all the folders and files. {@code false} if the process
	 *     has been stopped before its termination. The value {@code false} could be replied only if the value of
	 *     {@code continueOnError} is {@code false}.
	 * @throws FileNotFoundException if the given {@code parentFolder} does not exists.
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	public static boolean cleanFolder(final File parentFolder, final FileFilter filter, boolean continueOnError,
			boolean deleteParentFolder) throws FileNotFoundException {
		if (!parentFolder.exists()) {
			throw new FileNotFoundException(parentFolder.getAbsolutePath());
		}
		final FileFilter myFilter = filter == null ? it -> true : filter;
		//log.debug("Cleaning folder " + parentFolder.toString());
		final File[] contents = parentFolder.listFiles(myFilter);
		if (contents != null) {
			final Deque<File> filesToRemove = new LinkedList<>(Arrays.asList(contents));
			while (!filesToRemove.isEmpty()) {
				final File file = filesToRemove.pop();
				if (file.isDirectory()) {
					final File[] children = file.listFiles(myFilter);
					if (children != null && children.length > 0) {
						// Push back the folder in order to be removed after all its children.
						filesToRemove.push(file);
						// Push the children in order to be removed before the parent folder.
						for (int i = 0; i < children.length; ++i) {
							filesToRemove.push(children[i]);
						}
					} else if (!file.delete()) {
						//log.error("Couldn't delete " + file.getAbsolutePath());
						if (!continueOnError) {
							return false;
						}
					}
				} else {
					if (!file.delete()) {
						//log.error("Couldn't delete " + file.getAbsolutePath());
						if (!continueOnError) {
							return false;
						}
					}
				}
			}
		}
		if (deleteParentFolder) {
			final String[] children = parentFolder.list();
			if (children != null && children.length == 0 && !parentFolder.delete()) {
				//log.error("Couldn't delete " + parentFolder.getAbsolutePath());
				return false;
			}
		}
		return true;
	}

}

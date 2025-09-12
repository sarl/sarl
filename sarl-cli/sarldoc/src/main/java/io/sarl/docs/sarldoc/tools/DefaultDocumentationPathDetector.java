/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.docs.sarldoc.tools;

import java.io.File;
import java.io.IOException;
import java.util.Collections;

import org.arakhne.afc.vmutil.FileSystem;

import com.google.common.collect.Iterables;

import io.sarl.docs.sarldoc.Constants;
import io.sarl.lang.sarlc.tools.DefaultPathDetector;
import jakarta.inject.Inject;

/**
 * Default implementation of a documentation path detector.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class DefaultDocumentationPathDetector extends DefaultPathDetector implements DocumentationPathDetector {

	private File documentationOutputPath;

	/** Default constructor, used for injection.
	 */
	@Inject
	public DefaultDocumentationPathDetector() {
		//
	}

	@Override
	public void setDocumentationOutputPath(File path) {
		this.documentationOutputPath = path;
	}

	@Override
	public File getDocumentationOutputPath() {
		return this.documentationOutputPath;
	}

	@Override
	public boolean isResolved() {
		return super.isResolved() && this.documentationOutputPath != null;
	}

	@Override
	protected Iterable<File> buildResolvablePaths(Iterable<File> userFiles) {
		return Iterables.concat(
				super.buildResolvablePaths(userFiles),
				Collections.singleton(this.documentationOutputPath));
	}

	@Override
	protected void normalizePaths(File rootFile) {
		super.normalizePaths(rootFile);
		if (this.documentationOutputPath == null) {
			this.documentationOutputPath = toFile(rootFile, Constants.FOLDER_DOCUMENTATION);
		}
	}

	@Override
	protected void makeAbsolutePaths() throws IOException {
		super.makeAbsolutePaths();
		if (this.documentationOutputPath == null) {
			this.documentationOutputPath = toFile(cwd(), Constants.FOLDER_DOCUMENTATION).getCanonicalFile();
		}
		if (this.documentationOutputPath != null && !this.documentationOutputPath.isAbsolute()) {
			this.documentationOutputPath = FileSystem.join(cwd(), this.documentationOutputPath).getCanonicalFile();
		}
	}

}

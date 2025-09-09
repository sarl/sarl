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

package io.sarl.docs.sarldoc.tools;

import java.io.File;

import io.sarl.lang.sarlc.tools.PathDetector;

/**
 * Detect the base paths when they are missed.
 * The paths are: <ul>
 * <li>the documentation output path,</li>
 * <li>the SARL output path,</li>
 * <li>the Java class output path, and</li>
 * <li>the working path.</li>
 * </ul>
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version sarldoc 0.15.0 20250909-115750
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarldoc
 * @since 0.13
 */
public interface DocumentationPathDetector extends PathDetector {

	/** Change the documentatio output path.
	 *
	 * @param path the path.
	 */
	void setDocumentationOutputPath(File path);

	/** Replies the documentation output path.
	 *
	 * @return the path.
	 */
	File getDocumentationOutputPath();

}

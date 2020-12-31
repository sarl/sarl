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
import java.util.List;

import com.google.inject.ImplementedBy;

/**
 * Detect the three base paths when they are missed.
 * The paths are: <ul>
 * <li>the SARL output path,</li>
 * <li>the Java class output path, and</li>
 * <li>the working path.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@ImplementedBy(DefaultPathDetector.class)
public interface PathDetector {

	/** Change the SARL output path.
	 *
	 * @param path the path.
	 */
	void setSarlOutputPath(File path);

	/** Change the Java class output path.
	 *
	 * @param path the path.
	 */
	void setClassOutputPath(File path);

	/** Change the path in which the SARL compiler will write temp files.
	 *
	 * @param path the path.
	 */
	void setTempDirectory(File path);

	/** Replies the SARL output path.
	 *
	 * @return the path.
	 */
	File getSarlOutputPath();

	/** Replies the Java class output path.
	 *
	 * @return the path.
	 */
	File getClassOutputPath();

	/** Replies th path in which the SARL compiler will write temp files.
	 *
	 * @return the path.
	 */
	File getTempDirectory();

	/** Resolve the paths.
	 *
	 * @param args the command line arguments to consider.
	 * @throws IOException if one path cannot be canonized.
	 */
	void resolve(List<String> args) throws IOException;

}

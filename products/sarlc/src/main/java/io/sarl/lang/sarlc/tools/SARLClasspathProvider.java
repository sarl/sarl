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

import java.util.logging.Logger;

import com.google.inject.ImplementedBy;

import io.sarl.maven.bootiqueapp.utils.SystemPath;

/**
 * A provider of the class path that must be used for compiling a SARL program.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@ImplementedBy(SarlEmbededSdkClasspathProvider.class)
public interface SARLClasspathProvider {

	/** Replies the class path that must be used for compiling a SARL program.
	 *
	 * @param path the classpath to fill.
	 * @param logger the logger to use for notifying about the process of the task.
	 * @since 0.12
	 */
	void getClassPath(SystemPath path, Logger logger);

	/** Replies the module-path that must be used for compiling a SARL program.
	 *
	 * @param path the module-path to fill.
	 * @param logger the logger to use for notifying about the process of the task.
	 * @since 0.12
	 */
	void getModulePath(SystemPath path, Logger logger);

}

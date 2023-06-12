/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.doclet2.html.summaries;

import java.nio.file.Path;
import java.util.Collection;

import javax.lang.model.element.PackageElement;

import jdk.javadoc.doclet.Reporter;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.DocletOptions;

/** Generator of the type hierarchy for a package.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface PackageTreeSummaryGenerator {

	/** Generate the summary.
	 *
	 * @param packageElement the module to generate a documentation for.
	 * @param cssStylesheets the list of available CSS style sheets.
	 * @param jsScripts the list of available Javascript scripts.
	 * @param environment the generation environment.
	 * @param cliOptions the options provided on the CLI.
	 * @param reporter the tool for reporting errors, warnings and notes.
	 * @throws Exception error during generation.
	 */
	void generate(PackageElement moduleElement, Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception;

}

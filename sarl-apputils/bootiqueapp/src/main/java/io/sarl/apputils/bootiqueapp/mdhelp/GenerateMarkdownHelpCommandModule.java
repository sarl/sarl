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

package io.sarl.apputils.bootiqueapp.mdhelp;

import static io.bootique.BQCoreModule.extend;

import io.bootique.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Injector;
import io.bootique.di.Provides;
import jakarta.inject.Singleton;

/** Module for displaying the help on the standard output using a Markdown format.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class GenerateMarkdownHelpCommandModule implements BQModule {

	@Override
	public void configure(Binder binder) {
		extend(binder).addCommand(GenerateMarkdownHelpCommand.class);
	}

	/** Provide the command for displaying the help options.
	 *
	 * @param injector the injector.
	 * @return the command.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public GenerateMarkdownHelpCommand provideGenerateMarkdownHelpCommand(Injector injector) {
		return new GenerateMarkdownHelpCommand(injector);
	}

}

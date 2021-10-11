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

package io.sarl.lang.sarlc.modules.commands;

import static io.bootique.BQCoreModule.extend;

import javax.inject.Provider;
import javax.inject.Singleton;

import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Provides;
import io.bootique.log.BootLogger;

import io.sarl.lang.extralanguage.IExtraLanguageContributions;
import io.sarl.lang.sarlc.commands.ExtraLanguageListCommand;

/** Module for the command for printing out the available extra-language generators.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class ExtraLanguageListCommandModule implements BQModule {

	@Override
	public void configure(Binder binder) {
		extend(binder).addCommand(ExtraLanguageListCommand.class);
	}

	/** Provide the command for displaying the available extra-language generators.
	 *
	 * @param bootLogger the logger.
	 * @param contributions the provider of the extra-language contributions.
	 * @return the command.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public ExtraLanguageListCommand provideExtraLanguageListCommand(BootLogger bootLogger,
			Provider<IExtraLanguageContributions> contributions) {
		return new ExtraLanguageListCommand(bootLogger, contributions);
	}

}

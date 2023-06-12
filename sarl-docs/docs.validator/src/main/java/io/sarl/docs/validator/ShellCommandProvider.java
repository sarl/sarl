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

package io.sarl.docs.validator;

import java.io.File;

import com.google.inject.ImplementedBy;

/** Provider of shell commands.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
@ImplementedBy(DefaultShellCommandProvider.class)
public interface ShellCommandProvider {

	/** Replies the shell command that is registered with the given name.
	 *
	 * @param commandName the name of the shell command.
	 * @return the command or {@code null} if there is no command registered with the given name.
	 */
	ShellCommand getShellCommand(String commandName);

	/** Register a shell command.
	 *
	 * @param name the name of the shell command.
	 * @param executable the executable file.
	 */
	void register(String name, File executable);

}

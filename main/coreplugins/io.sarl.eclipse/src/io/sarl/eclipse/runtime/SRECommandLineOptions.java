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

package io.sarl.eclipse.runtime;

/**
 * Constants that are representing the different command-line options that a
 * SRE may provide and that are supported by the Eclipse tools.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public final class SRECommandLineOptions {

	/** Field name for CLI option for using the default identifier of the root context.
	 */
	public static final String CLI_DEFAULT_CONTEXT_ID = "CLI-Default-Context-ID"; //$NON-NLS-1$

	/** Field name for CLI option for using the random identifier of the root context.
	 */
	public static final String CLI_RANDOM_CONTEXT_ID = "CLI-Random-Context-ID"; //$NON-NLS-1$

	/** Field name for CLI option for using the boot-agent-based identifier of the root context.
	 */
	public static final String CLI_BOOT_AGENT_CONTEXT_ID = "CLI-BootAgent-Context-ID"; //$NON-NLS-1$

	/** Field name for CLI option that disabling options.
	 */
	public static final String CLI_NO_MORE_OPTION = "CLI-No-More-Option"; //$NON-NLS-1$

	/** Field name for CLI option for marking the SRE as embedded in another application.
	 */
	public static final String CLI_EMBEDDED = "CLI-Embedded"; //$NON-NLS-1$

	/** Field name for CLI option for changing the logging level.
	 * @since 0.12
	 */
	public static final String CLI_LOG = "CLI-Log"; //$NON-NLS-1$

	/** Field name for CLI option's values for the logging option.
	 * The values are separated by comas.
	 * @since 0.12
	 */
	public static final String CLI_LOG_VALUES = "CLI-Log-Values"; //$NON-NLS-1$

	/** Field name for CLI option's default value for the logging option.
	 * @since 0.12
	 */
	public static final String CLI_LOG_DEFAULT_VALUE = "CLI-Log-Default-Value"; //$NON-NLS-1$

	private SRECommandLineOptions() {
		//
	}

}

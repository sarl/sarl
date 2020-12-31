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

package io.sarl.eclipse.launching.runner.application;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;

import io.sarl.eclipse.launching.runner.general.AbstractLaunchProcess;
import io.sarl.eclipse.launching.runner.general.AbstractSARLLaunchConfiguration;
import io.sarl.eclipse.launching.runner.general.ILaunchProcess;
import io.sarl.eclipse.runtime.ISREInstall;

/**
 * Implementation of an eclipse LauncConfigurationDelegate to launch SARL application.
 *
 * <p>This delegate is in charge of running a SARL application with the specific
 * SRE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SARLApplicationLaunchConfiguration extends AbstractSARLLaunchConfiguration {

	@Override
	protected ILaunchProcess createLaunchingProcess(ILaunchConfiguration configuration, String mode, ILaunch launch) {
		return new LaunchProcess(this, configuration, mode, launch);
	}

	@Override
	protected String getProgramArguments(ILaunchConfiguration configuration, ISREInstall sre, String standardProgramArguments) throws CoreException {
		return standardProgramArguments;
	}

	/** Definition of the launching process, split in separated steps for
	 * making easier the cancellation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @sincec 0.7
	 */
	private class LaunchProcess extends AbstractLaunchProcess<SARLApplicationLaunchConfiguration> {

		/** Constructor.
		 * @param owner the owner of the process.
		 * @param configuration the launch configuration.
		 * @param mode the launching mode.
		 * @param launch the launching
		 */
		LaunchProcess(SARLApplicationLaunchConfiguration owner, ILaunchConfiguration configuration, String mode, ILaunch launch) {
			super(owner, configuration, mode, launch);
		}

	}

}

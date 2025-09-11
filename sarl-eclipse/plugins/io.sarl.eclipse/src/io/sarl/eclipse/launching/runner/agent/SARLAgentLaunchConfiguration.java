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

package io.sarl.eclipse.launching.runner.agent;

import static io.sarl.eclipse.launching.config.LaunchConfigurationUtils.join;

import java.util.Objects;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.variables.VariablesPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;

import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstall;
import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.launching.config.ILaunchConfigurationAccessor;
import io.sarl.eclipse.launching.runner.general.AbstractLaunchProcess;
import io.sarl.eclipse.launching.runner.general.AbstractSARLLaunchConfiguration;
import io.sarl.eclipse.launching.runner.general.ILaunchProcess;
import io.sarl.eclipse.runtime.SRECommandLineOptions;
import io.sarl.lang.core.SRE;
import io.sarl.lang.core.util.CliUtilities;

/**
 * Implementation of an eclipse LauncConfigurationDelegate to launch SARL agent.
 *
 * <p>This delegate is in charge of running a SARL agent with the specific
 * SRE.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
public class SARLAgentLaunchConfiguration extends AbstractSARLLaunchConfiguration {

	@Override
	protected ILaunchProcess createLaunchingProcess(ILaunchConfiguration configuration, String mode, ILaunch launch) {
		return new LaunchProcess(this, configuration, mode, launch);
	}

	@Override
	protected String getProgramArguments(ILaunchConfiguration configuration, ISREInstall sre, String standardProgramArguments) throws CoreException {
		// Retrieve the classname of the boot agent.
		final var bootAgent = getAgentName(configuration);

		// Special case: the boot class is a simple one provided within the SARL library.
		if (Objects.equals(sre.getMainClass(), SRE.class.getName())) {
			return join(bootAgent, standardProgramArguments);
		}

		final var substitutor = VariablesPlugin.getDefault().getStringVariableManager();

		// Retrieve the SRE arguments from the SRE configuration
		final var sreArgs1 = substitutor.performStringSubstitution(sre.getSREArguments());

		// Retrieve the SRE arguments from the launch configuration
		final var sreArgs2 = substitutor.performStringSubstitution(getConfigurationAccessor().getSRELaunchingArguments(configuration));

		// Retrieve the extra SRE arguments from the launch configuration
		final var sreArgs3 = substitutor.performStringSubstitution(getConfigurationAccessor().getExtraSRELaunchingArguments(configuration));

		// Add the options corresponding to the general setting of the launch configuration.
		final var cliOptions = sre.getAvailableCommandLineOptions();
		assert cliOptions != null;
		String options = null;

		final var logOption = getConfigurationAccessor().getLogArgumentName(configuration);
		final var logValue = getConfigurationAccessor().getLogArgumentValue(configuration);
		if (!Strings.isNullOrEmpty(logOption) && !Strings.isNullOrEmpty(logValue)) {
			final var fullOption = CliUtilities.getUnixCommandLineOption(logOption, logValue);
			if (!Strings.isNullOrEmpty(fullOption)) {
				options = join(options, fullOption);
			}
		}

		if (getConfigurationAccessor().isEmbeddedSRE(configuration)) {
			options = join(options, cliOptions.get(SRECommandLineOptions.CLI_EMBEDDED));
		}

		final var type = getConfigurationAccessor().getDefaultContextIdentifier(configuration);
		switch (type) {
		case RANDOM_CONTEXT_ID:
			options = join(options, cliOptions.get(SRECommandLineOptions.CLI_RANDOM_CONTEXT_ID));
			break;
		case BOOT_AGENT_CONTEXT_ID:
			options = join(options, cliOptions.get(SRECommandLineOptions.CLI_BOOT_AGENT_CONTEXT_ID));
			break;
		case DEFAULT_CONTEXT_ID:
		default:
			options = join(options, cliOptions.get(SRECommandLineOptions.CLI_DEFAULT_CONTEXT_ID));
			break;
		}

		options = substitutor.performStringSubstitution(options);

		// Add the command line option that mark the difference between the SRE's options and
		// the arguments for the boot agent
		final var noMoreOption = cliOptions.get(SRECommandLineOptions.CLI_NO_MORE_OPTION);

		// Make the complete command line
		return join(sreArgs1, sreArgs2, sreArgs3, options, bootAgent, noMoreOption, standardProgramArguments);
	}

	/**
	 * Returns the main type name specified by the given launch configuration,
	 * or {@code null} if none.
	 *
	 * @param accessor the accessor to the launch configuration attributes.
	 * @param configuration launch configuration
	 * @return the main type name specified by the given launch configuration,
	 *         or {@code null} if none
	 * @throws CoreException if unable to retrieve the attribute
	 * @since 0.11
	 */
	public static String getAgentName(ILaunchConfigurationAccessor accessor, ILaunchConfiguration configuration) throws CoreException {
		final var agentName = accessor.getAgent(configuration);
		if (agentName == null) {
			return null;
		}
		return VariablesPlugin.getDefault().getStringVariableManager()
				.performStringSubstitution(agentName);
	}

	/**
	 * Returns the main type name specified by the given launch configuration,
	 * or {@code null} if none.
	 *
	 * @param configuration launch configuration
	 * @return the main type name specified by the given launch configuration,
	 *         or {@code null} if none
	 * @throws CoreException if unable to retrieve the attribute
	 */
	protected String getAgentName(ILaunchConfiguration configuration) throws CoreException {
		return getAgentName(getConfigurationAccessor(), configuration);
	}

	/**
	 * Verifies a main type name is specified by the given launch configuration,
	 * and returns the main type name.
	 *
	 * @param configuration launch configuration
	 * @throws CoreException if unable to retrieve the attribute or the attribute is
	 *     unspecified
	 */
	protected void verifyAgentName(ILaunchConfiguration configuration) throws CoreException {
		final var name = getAgentName(configuration);
		if (name == null) {
			abort(
					io.sarl.eclipse.launching.dialog.Messages.MainLaunchConfigurationTab_2,
					null,
					SARLEclipseConfig.ERR_UNSPECIFIED_AGENT_NAME);
		}
	}

	/** Definition of the launching process, split in separated steps for
	 * making easier the cancellation.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.1 20250911-224827
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	private static class LaunchProcess extends AbstractLaunchProcess<SARLAgentLaunchConfiguration> {

		/** Constructor.
		 * @param owner the owner of the process.
		 * @param configuration the launch configuration.
		 * @param mode the launching mode.
		 * @param launch the launching
		 */
		LaunchProcess(SARLAgentLaunchConfiguration owner, ILaunchConfiguration configuration, String mode, ILaunch launch) {
			super(owner, configuration, mode, launch);
		}

		@Override
		protected void readConfigurationParameters(IProgressMonitor monitor) throws CoreException {
			super.readConfigurationParameters(monitor);
			getOwner().verifyAgentName(this.configuration);
		}

	}

}

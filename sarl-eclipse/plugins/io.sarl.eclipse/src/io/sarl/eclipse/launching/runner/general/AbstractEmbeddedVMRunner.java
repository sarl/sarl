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

package io.sarl.eclipse.launching.runner.general;

import java.text.MessageFormat;
import java.util.regex.Pattern;

import org.arakhne.afc.bootique.variables.VariableNames;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.launching.IVMRunner;
import org.eclipse.jdt.launching.VMRunnerConfiguration;
import org.eclipse.swt.widgets.Shell;

import io.sarl.eclipse.SARLEclipsePlugin;

/**
 * Abstract implementation of a VM runner that is running in the current Eclipse VM.
 * This implementation is for debugging of SREs.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.12
 */
@SuppressWarnings("restriction")
public abstract class AbstractEmbeddedVMRunner implements IVMRunner {

	/** Standard extension for JAR files.
	 */
	protected static final String JAR_EXTENSION = ".jar"; //$NON-NLS-1$

	/** Standard slash, that is used in URLs a separator.
	 */
	protected static final String SLASH = "/"; //$NON-NLS-1$

	/** Standard slash, that is used in URLs a separator.
	 */
	protected static final String DEFINITION_PATTERN = "^[-/]D([^=]+)=(.*)$"; //$NON-NLS-1$

	@Override
	public final void run(VMRunnerConfiguration configuration, ILaunch launch, IProgressMonitor monitor)
			throws CoreException {
		try {
			final var myJob = createJob(configuration, launch);
			monitor.done();
			if (myJob != null) {
				myJob.schedule();
			}
		} catch (Exception exception) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception));
		}
	}

	/** Create the main class finder.
	 *
	 * @return the main class finder.
	 */
	protected abstract MainClassFinder getMainClassFinder();

	private static void resetProperties() {
		final var props = System.getProperties();
		final var iterator = props.entrySet().iterator();
		while (iterator.hasNext()) {
			final var entry = iterator.next();
			final var name = entry.getKey().toString();
			if (name.startsWith(VariableNames.BOOTIQUE_PROPERTY_PREFIX)) {
				iterator.remove();
			}
		}
	}

	private static void registerProperties(VMRunnerConfiguration configuration) {
		final var props = System.getProperties();
		final var pattern = Pattern.compile(DEFINITION_PATTERN);
		for (final var arg : configuration.getVMArguments()) {
			final var matcher = pattern.matcher(arg);
			if (matcher.matches()) {
				final var name = matcher.group(1);
				final var value = matcher.group(2);
				props.setProperty(name, value);
			}
		}
	}

	/** Create the job that enables to run the SRE into the current Eclipse JVM.
	 *
	 * @param configuration description of the VM configuration (that is set without considering the embedding state of the VM).
	 * @param launch description of the launch.
	 * @return the job.
	 */
	protected Job createJob(VMRunnerConfiguration configuration, ILaunch launch) {
		final var myJob = new Job(launch.getLaunchConfiguration().getName()) {
			@Override
			public IStatus run(IProgressMonitor monitor) {
				monitor.subTask(MessageFormat.format(Messages.AbstractEmbeddedVMRunner_0, launch.getLaunchConfiguration().getName()));
				final MainClassFinder mcFinder = getMainClassFinder();
				try {
					resetProperties();
					registerProperties(configuration);
					//
					mcFinder.initialize(configuration, launch);
					//
					final var mainClass = configuration.getClassToLaunch();
					//
					final var clazz = mcFinder.getMainClass(mainClass);
					//
					final var mainMethod = clazz.getDeclaredMethod("main", String[].class); //$NON-NLS-1$
					mainMethod.invoke(null, (Object) configuration.getProgramArguments());
					return SARLEclipsePlugin.getDefault().createOkStatus();
				} catch (Throwable exception) {
					return SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception);
				} finally {
					mcFinder.release();
					monitor.done();
				}
			}
		};
		return myJob;
	}

	/**
	 * Convenience method to return the active workbench window shell.
	 *
	 * @return active workbench window shell
	 */
	protected static Shell getShell() {
		return JDIDebugUIPlugin.getActiveWorkbenchShell();
	}

	/** Finder of the main class.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.15.1 20250911-224827
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 * @since 0.12
	 */
	protected interface MainClassFinder {

		/** Initialize the finder.
		 *
		 * @param configuration the VM configuration to use.
		 * @param launch the launching tool.
		 */
		void initialize(VMRunnerConfiguration configuration, ILaunch launch);

		/** Find the main class to be launched.
		 *
		 * @param mainClass the fully qualified name of the class to launch.
		 * @return the main class.
		 * @throws Exception if the class cannot be loaded.
		 */
		Class<?> getMainClass(String mainClass) throws Exception;

		/** Release the finder.
		 */
		void release();

	}

}

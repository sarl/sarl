/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.eclipse.launching.runner;

import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.Arrays;

import org.arakhne.afc.vmutil.ClassLoaderFinder;
import org.arakhne.afc.vmutil.DynamicURLClassLoader;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.URISchemeType;
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
 * Implementation of a VM runner that is running in the current Eclipse VM.
 * This implementation is for debugging of SREs.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class EmbeddedVMRunner implements IVMRunner {

	private static final String JAR_EXTENSION = ".jar"; //$NON-NLS-1$

	private static final String SLASH = "/"; //$NON-NLS-1$

	@Override
	public void run(VMRunnerConfiguration configuration, ILaunch launch, IProgressMonitor monitor)
			throws CoreException {
		try {
			final String[] classpath = configuration.getClassPath();
			final URL[] classPathURLs = new URL[classpath.length];
			Arrays.parallelSetAll(classPathURLs, index -> {
				String path = classpath[index];
				final URL url = FileSystem.convertStringToURL(path, false);
				path = url.getFile();
				if (!path.endsWith(JAR_EXTENSION) && !path.endsWith(SLASH)) {
					path = path + SLASH;
				}
				try {
					return new URL(URISchemeType.FILE.name(), null, path);
				} catch (MalformedURLException e) {
					throw new RuntimeException(e);
				}
			});
			monitor.done();
			final Job myJob = new Job(launch.getLaunchConfiguration().getName()) {
				@Override
				public IStatus run(IProgressMonitor monitor) {
					monitor.subTask(MessageFormat.format(Messages.EmbeddedVMRunner_0, launch.getLaunchConfiguration().getName()));
					final DynamicURLClassLoader classLoader = DynamicURLClassLoader.newInstance(
								ClassLoader.getSystemClassLoader(), classPathURLs);
					try {
						ClassLoaderFinder.setPreferredClassLoader(classLoader);
						final String mainClass = configuration.getClassToLaunch();
						final Class<?> clazz = classLoader.loadClass(mainClass);
						final Method mainMethod = clazz.getDeclaredMethod("main", String[].class); //$NON-NLS-1$
						mainMethod.invoke(null, (Object) configuration.getProgramArguments());
						return SARLEclipsePlugin.getDefault().createOkStatus();
					} catch (Throwable exception) {
						return SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception);
					} finally {
						ClassLoaderFinder.popPreferredClassLoader();
						monitor.done();
					}
				}
			};
			myJob.schedule();
		} catch (Exception exception) {
			throw new CoreException(SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception));
		}
	}

	/**
	 * Convenience method to return the active workbench window shell.
	 *
	 * @return active workbench window shell
	 */
	protected static Shell getShell() {
		return JDIDebugUIPlugin.getActiveWorkbenchShell();
	}

}

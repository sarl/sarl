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

package io.sarl.eclipse.launching.runner.general;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;

import org.arakhne.afc.vmutil.ClassLoaderFinder;
import org.arakhne.afc.vmutil.DynamicURLClassLoader;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.URISchemeType;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.jdt.launching.VMRunnerConfiguration;

/**
 * Implementation of a VM runner that is running in the current Eclipse VM assuming it is running on
 * a Java that cannot support modules.
 * This implementation is for debugging of SREs.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class EmbeddedNotModularVMRunner extends AbstractEmbeddedVMRunner {

	@Override
	protected MainClassFinder getMainClassFinder() {
		return new CpMainClassFinder();
	}

	/** Finder of the main class.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	protected static class CpMainClassFinder implements MainClassFinder {

		private ClassLoader classLoader;

		@Override
		public void initialize(VMRunnerConfiguration configuration, ILaunch launch) {
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
			//
			this.classLoader = DynamicURLClassLoader.newInstance(ClassLoader.getSystemClassLoader(), classPathURLs);
			//
			ClassLoaderFinder.setPreferredClassLoader(this.classLoader);
		}

		@Override
		public Class<?> getMainClass(String mainClass) throws Exception {
			return this.classLoader.loadClass(mainClass);
		}

		@Override
		public void release() {
			ClassLoaderFinder.popPreferredClassLoader();
		}

	}

}

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

package io.sarl.lang.sarlc.tools;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.ref.SoftReference;
import java.net.URL;
import java.text.MessageFormat;
import java.util.Iterator;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.jar.JarFile;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.common.base.Strings;
import com.google.inject.Singleton;
import org.arakhne.afc.vmutil.ClasspathUtil;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

import io.sarl.lang.SARLVersion;
import io.sarl.maven.bootiqueapp.utils.SystemPath;

/**
 * Provider of the SARL SDK class path.
 * The SARL libraries are supposed to be embedded into the sarlc archive file, into "embedded-sdk-libs" folder.
 * The files into this folders are extracted into a temporary folder "sarlc-sdk-X.X".
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@Singleton
public class SarlEmbededSdkClasspathProvider implements SARLClasspathProvider {

	private static final String EMBEDDED_SDK_CONTAINER = "embedded-sdk-libs"; //$NON-NLS-1$

	private static final String EMBEDDED_SDK_FOLDER = "sarlc-sdk-" + SARLVersion.SARL_RELEASE_VERSION; //$NON-NLS-1$

	private static final int BUFFER_SIZE = 4096;

	private SoftReference<URL> singleUrl;

	private static void getJvmClasspath(SystemPath path) {
		final Iterator<URL> iterator = ClasspathUtil.getClasspath();
		while (iterator.hasNext()) {
			final URL classpathEntry = iterator.next();
			final File entry = FileSystem.convertURLToFile(classpathEntry);
			path.add(entry);
		}
	}

	private static File getSdkFolder() {
		final File folder = FileSystem.convertStringToFile(System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
		return new File(folder, EMBEDDED_SDK_FOLDER);
	}

	private static boolean uncompressEmbeddedSdk(URL url, File output, Logger logger) {
		final File sdkFile = FileSystem.convertURLToFile(url);
		if (sdkFile == null || !output.mkdirs() || !output.isDirectory()) {
			logger.severe(MessageFormat.format(Messages.SarlEmbededSdkClasspathProvider_3, output.getAbsoluteFile()));
			return false;
		}
		try (JarFile jarFile = new JarFile(sdkFile)) {
			final byte[] buffer = new byte[BUFFER_SIZE];
			final AtomicBoolean found = new AtomicBoolean();
			jarFile.stream().filter(it -> {
				if (it.isDirectory()) {
					return false;
				}
				final String name = it.getName();
				final File filename = FileSystem.convertStringToFile(name);
				return filename.getParentFile() != null && EMBEDDED_SDK_CONTAINER.equals(filename.getParentFile().getName());
			}).forEach(it -> {
				try {
					final String name = it.getName();
					final File filename = FileSystem.convertStringToFile(name);
					final File outFile = FileSystem.join(output, filename).getCanonicalFile();
					outFile.getParentFile().mkdirs();
					try (InputStream istream = new BufferedInputStream(jarFile.getInputStream(it))) {
						try (FileOutputStream fos = new FileOutputStream(outFile)) {
							int len;
							while ((len = istream.read(buffer)) > 0) {
								fos.write(buffer, 0, len);
							}
						}
					}
					found.set(true);
				} catch (IOException exception) {
					throw new RuntimeException(exception);
				}
			});
			return found.get();
		} catch (IOException exception) {
			logger.log(Level.SEVERE, exception.getLocalizedMessage(), exception);
			// Delete the output folder if some error occurs
			try {
				FileSystem.delete(output);
			} catch (Exception exception0) {
				// Silent
			}
			return false;
		}
	}

	private static boolean extractEmbeddedClasspath(URL url, SystemPath path, Logger logger) {
		final File embeddedSdk = getSdkFolder();
		if (!embeddedSdk.exists() && !uncompressEmbeddedSdk(url, embeddedSdk, logger)) {
			return false;
		}
		if (embeddedSdk.isDirectory()) {
			final File[] content = new File(embeddedSdk, EMBEDDED_SDK_CONTAINER).listFiles();
			if (content != null) {
				logger.fine(MessageFormat.format(Messages.SarlEmbededSdkClasspathProvider_0, embeddedSdk.getAbsoluteFile()));
				for (final File library : content) {
					path.add(library);
				}
				return true;
			}
		}
		return false;
	}

	private synchronized URL getSingleArchiveFromClasspath() {
		URL singleUrl = this.singleUrl == null ? null : this.singleUrl.get();
		if (singleUrl == null) {
			final String javaHome = Strings.emptyToNull(System.getProperty("java.home")); //$NON-NLS-1$
			IPath javaHomePath = javaHome == null ? null : Path.fromOSString(javaHome);
			if (javaHomePath != null && "jre".equalsIgnoreCase(javaHomePath.lastSegment())) { //$NON-NLS-1$
				javaHomePath = javaHomePath.removeLastSegments(1);
			}
			final Iterator<URL> iterator = ClasspathUtil.getClasspath();
			while (iterator.hasNext()) {
				final URL url = iterator.next();
				if (singleUrl != null) {
					// If more then 2 jar files are on the classpath, then there is not a single archive.
					return null;
				}
				final File jarFile = FileSystem.convertURLToFile(url);
				if (jarFile != null) {
					if (javaHomePath == null) {
						singleUrl = url;
					} else {
						final IPath jarPath = Path.fromOSString(jarFile.getAbsolutePath());
						if (!javaHomePath.isPrefixOf(jarPath)) {
							singleUrl = url;
						}
					}
				}
			}
			this.singleUrl = new SoftReference<>(singleUrl);
		}
		return singleUrl;
	}

	@Override
	public void getBootClasspath(SystemPath path, Logger logger) {
		final URL url = getSingleArchiveFromClasspath();
		if (url == null) {
			logger.fine(Messages.SarlEmbededSdkClasspathProvider_1);
			getJvmClasspath(path);
		} else if (!extractEmbeddedClasspath(url, path, logger)) {
			logger.fine(Messages.SarlEmbededSdkClasspathProvider_2);
			getJvmClasspath(path);
		}
	}

	@Override
	public void getClassPath(SystemPath path, Logger logger) {
		//
	}

	@Override
	public void getModulePath(SystemPath path, Logger logger) {
		//
	}

}

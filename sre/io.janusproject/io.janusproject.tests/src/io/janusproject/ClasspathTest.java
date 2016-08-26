/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.janusproject;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.File;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.junit.Test;

import io.janusproject.testutils.AbstractJanusTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class ClasspathTest extends AbstractJanusTest {

	@Test
	public void onlyJanusGuavaInClasspath() throws Exception {
		String rawClasspath = System.getProperty("java.class.path"); //$NON-NLS-1$
		assertNotNull(rawClasspath);
		assertNotEquals("", rawClasspath);
		String[] paths = rawClasspath.split(File.pathSeparator);

		for (String path : paths) {
			File file = new File(path);
			if (file.isDirectory()) {
				file = new File(file, "META-INF");
				file = new File(file, "maven");
				file = new File(file, "com.google.guava");
				file = new File(file, "guava");
				file = new File(file, "pom.xml");
				assertFalse("The original Guava library was found in the classpath. Only the Janus fork must be used.",
						file.exists());
			} else {
				try (JarFile jf = new JarFile(file)) {
					JarEntry entry = jf.getJarEntry("META-INF/maven/com.google.guava/guava/pom.xml");
					assertNull("The original Guava library was found in the classpath. Only the Janus fork must be used.", entry);
				}
			}
		}

	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.m2e.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.*;
import io.sarl.m2e.SARLMavenEclipsePlugin;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.osgi.framework.Version;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	SARLMavenEclipsePluginTest.DefaultPlugin.class,
	SARLMavenEclipsePluginTest.PluginInstance.class,
	SARLMavenEclipsePluginTest.Logging.class,
})
@SuppressWarnings("all")
public final class SARLMavenEclipsePluginTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class DefaultPlugin {

		@Before
		public void setUp() {
			SARLMavenEclipsePlugin.setDefault(null);
		}

		@After
		public void tearDown() {
			SARLMavenEclipsePlugin.setDefault(null);
		}

		@Test
		public void getDefault_noBundleInitialization() {
			assertNull(SARLMavenEclipsePlugin.getDefault());
		}

		@Test
		public void getDefault_bundleInitialization() {
			SARLMavenEclipsePlugin plugin = mock(SARLMavenEclipsePlugin.class);
			SARLMavenEclipsePlugin.setDefault(plugin);
			assertSame(plugin, SARLMavenEclipsePlugin.getDefault());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class PluginInstance {

		private SARLMavenEclipsePlugin plugin;
		private SARLMavenEclipsePlugin spy;

		@Before
		public void setUp() {
			SARLMavenEclipsePlugin.setDefault(null);
			this.plugin = new SARLMavenEclipsePlugin();
			this.spy = spy(this.plugin);
		}

		@After
		public void tearDown() {
			this.plugin = null;
			this.spy = null;
			SARLMavenEclipsePlugin.setDefault(null);
		}

		@Test
		public void getDefault() {
			assertSame(this.plugin, SARLMavenEclipsePlugin.getDefault());
		}

		@Test
		public void createStatusIntThrowable() {
			Throwable ex = mock(Throwable.class);
			IStatus status = this.spy.createStatus(IStatus.ERROR, ex);
			assertNotNull(status);
			assertEquals(IStatus.ERROR, status.getSeverity());
			assertSame(ex, status.getException());
		}

		@Test
		public void createStatusIntString() {
			String msg = "my error message";
			IStatus status = this.spy.createStatus(IStatus.ERROR, msg);
			assertNotNull(status);
			assertEquals(IStatus.ERROR, status.getSeverity());
			assertEquals(msg, status.getMessage());
		}

		@Test
		public void parseMavenVersion() {
			Version version = SARLMavenEclipsePlugin.parseMavenVersion("1.2.3");
			assertNotNull(version);
			assertEquals(1, version.getMajor());
			assertEquals(2, version.getMinor());
			assertEquals(3, version.getMicro());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Logging {

		private ILog logger;
		private SARLMavenEclipsePlugin plugin;

		@Before
		public void setUp() {
			SARLMavenEclipsePlugin.setDefault(null);
			this.logger = mock(ILog.class);
			this.plugin = mock(SARLMavenEclipsePlugin.class);
			SARLMavenEclipsePlugin.setDefault(this.plugin);
			when(this.plugin.getILog()).thenReturn(this.logger);
		}

		@After
		public void tearDown() {
			this.logger = null;
			this.plugin = null;
			SARLMavenEclipsePlugin.setDefault(null);
		}

		@Test
		public void logThrowable() {
			Throwable ex = mock(Throwable.class);
			this.plugin.log(ex);
			ArgumentCaptor<IStatus> arg = ArgumentCaptor.forClass(IStatus.class);
			verify(this.logger, times(1)).log(arg.capture());
			assertSame(ex, arg.getValue().getException());
		}

		@Test
		public void logIStatus() {
			IStatus status = mock(IStatus.class);
			this.plugin.log(status);
			ArgumentCaptor<IStatus> arg = ArgumentCaptor.forClass(IStatus.class);
			verify(this.logger, times(1)).log(arg.capture());
			assertSame(status, arg.getValue());
		}

	}

}
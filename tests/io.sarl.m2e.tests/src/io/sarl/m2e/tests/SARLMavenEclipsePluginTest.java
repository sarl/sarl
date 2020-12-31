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
package io.sarl.m2e.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.PrintStream;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.osgi.internal.debug.Debug;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;

import io.sarl.m2e.SARLMavenEclipsePlugin;
import io.sarl.tests.api.AbstractSarlTest;


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
	public static class DefaultPlugin extends AbstractSarlTest {

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
	public static class PluginInstance extends AbstractSarlTest {

		@NonNullByDefault
		private SARLMavenEclipsePlugin plugin;

		@NonNullByDefault
		private SARLMavenEclipsePlugin spy;

		@Before
		public void setUp() {
			SARLMavenEclipsePlugin.setDefault(null);
			this.plugin = new SARLMavenEclipsePlugin();
			this.spy = spy(this.plugin);
		}

		@After
		public void tearDown() {
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

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Logging extends AbstractSarlTest {

		@NonNullByDefault
		private PrintStream old;

		@NonNullByDefault
		private ILog logger;

		@NonNullByDefault
		private SARLMavenEclipsePlugin plugin;

		@Before
		public void setUp() {
			this.old = Debug.out;
			Debug.out = mock(PrintStream.class);
			this.logger = mock(ILog.class);
			// Cannot create a "spyied" version of getILog() since it
			// will be directly called by the underlying implementation.
			// For providing a mock for the ILog, only subclassing is working.
			this.plugin = new SARLMavenEclipsePlugin() {
				public ILog getILog() {
					return logger;
				}
			};
			SARLMavenEclipsePlugin.setDefault(this.plugin);
		}

		@After
		public void tearDown() {
			Debug.out = this.old;
		}

		@Test
		public void logIStatus() {
			IStatus status = mock(IStatus.class);
			this.plugin.getILog().log(status);
			ArgumentCaptor<IStatus> arg = ArgumentCaptor.forClass(IStatus.class);
			verify(this.logger).log(arg.capture());
			assertSame(status, arg.getValue());
		}

		@Test
		public void logThrowable_withoutException() {
			this.plugin.log((Throwable) null);
			ArgumentCaptor<IStatus> arg = ArgumentCaptor.forClass(IStatus.class);
			verify(this.logger).log(arg.capture());
			IStatus status = arg.getValue();
			assertNotNull(status);
			assertEquals("Internal Error", status.getMessage());
			assertEquals(IStatus.ERROR, status.getSeverity());
			assertNull(status.getException());
			assertEquals(SARLMavenEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

		@Test
		public void logThrowable_withException() {
			Throwable ex = mock(Throwable.class);
			when(ex.getMessage()).thenReturn("my message");
			this.plugin.log(ex);
			ArgumentCaptor<IStatus> arg = ArgumentCaptor.forClass(IStatus.class);
			verify(this.logger).log(arg.capture());
			IStatus status = arg.getValue();
			assertNotNull(status);
			assertEquals("Internal Error: my message", status.getMessage());
			assertEquals(IStatus.ERROR, status.getSeverity());
			assertSame(ex, status.getException());
			assertEquals(SARLMavenEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

	}

}
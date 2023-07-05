/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
package io.sarl.eclipse.m2e.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.PrintStream;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.osgi.internal.debug.Debug;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import io.sarl.eclipse.m2e.SARLMavenEclipsePlugin;
import io.sarl.lang.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("M2E SARL Integration")
@Tag("unit")
@Tag("eclipse")
@Tag("eclipse-unit")
@SuppressWarnings("all")
public final class SARLMavenEclipsePluginTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("Defaut plugin")
	public static class DefaultPlugin extends AbstractSarlTest {

		@BeforeEach
		public void setUp() {
			SARLMavenEclipsePlugin.setDefault(null);
		}

		@AfterEach
		public void tearDown() {
			SARLMavenEclipsePlugin.setDefault(null);
		}

		@Test
		@DisplayName("getDefault without bundle init")
		public void getDefault_noBundleInitialization() {
			assertNull(SARLMavenEclipsePlugin.getDefault());
		}

		@Test
		@DisplayName("getDefault with bundle init")
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
	@DisplayName("Plugin instance")
	public static class PluginInstance extends AbstractSarlTest {

		private SARLMavenEclipsePlugin plugin;

		private SARLMavenEclipsePlugin spy;

		@BeforeEach
		public void setUp() {
			SARLMavenEclipsePlugin.setDefault(null);
			this.plugin = new SARLMavenEclipsePlugin();
			this.spy = spy(this.plugin);
		}

		@AfterEach
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
	@DisplayName("Logging")
	public static class Logging extends AbstractSarlTest {

		private PrintStream old;

		private ILog logger;

		private SARLMavenEclipsePlugin plugin;

		@BeforeEach
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

		@AfterEach
		public void tearDown() {
			Debug.out = this.old;
		}

		@Test
		@DisplayName("Log IStatus")
		public void logIStatus() {
			IStatus status = mock(IStatus.class);
			this.plugin.getILog().log(status);
			ArgumentCaptor<IStatus> arg = ArgumentCaptor.forClass(IStatus.class);
			verify(this.logger).log(arg.capture());
			assertSame(status, arg.getValue());
		}

		@Test
		@DisplayName("Log IStatus without exception")
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
		@DisplayName("Log IStatus with exception")
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
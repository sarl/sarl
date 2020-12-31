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
package io.sarl.eclipse.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.io.PrintStream;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.osgi.internal.debug.Debug;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	SARLEclipsePluginTest.LogTests.class,
	SARLEclipsePluginTest.StatusTests.class,
})
@SuppressWarnings("all")
public final class SARLEclipsePluginTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class LogTests extends AbstractSarlTest {

		@NonNullByDefault
		private PrintStream old;

		@NonNullByDefault
		private ILog logger;

		@NonNullByDefault
		private SARLEclipsePlugin plugin;

		@Before
		public void setUp() {
			this.old = Debug.out;
			Debug.out = mock(PrintStream.class);
			this.logger = mock(ILog.class);
			// Cannot create a "spyied" version of getILog() since it
			// will be directly called by the underlying implementation.
			// For providing a mock for the ILog, only subclassing is working.
			this.plugin = new SARLEclipsePlugin() {
				public ILog getILog() {
					return logger;
				}
			};
			SARLEclipsePlugin.setDefault(this.plugin);
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
		public void logErrorMessage() {
			this.plugin.logErrorMessage("my message");
			ArgumentCaptor<IStatus> arg = ArgumentCaptor.forClass(IStatus.class);
			verify(this.logger).log(arg.capture());
			IStatus status = arg.getValue();
			assertNotNull(status);
			assertEquals("my message", status.getMessage());
			assertEquals(IStatus.ERROR, status.getSeverity());
			assertNull(status.getException());
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
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
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
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
			assertEquals("my message", status.getMessage());
			assertEquals(IStatus.ERROR, status.getSeverity());
			assertSame(ex, status.getException());
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

		@Test
		public void logDebugMessageString() {
			this.plugin.logDebugMessage("my message");
			ArgumentCaptor<String> arg = ArgumentCaptor.forClass(String.class);
			verify(Debug.out).println(arg.capture());
			assertEquals("my message", arg.getValue());
		}

		@Test
		public void logDebugMessageStringThrowable_withoutException() {
			this.plugin.logDebugMessage("my message", null);
			ArgumentCaptor<String> arg0 = ArgumentCaptor.forClass(String.class);
			verify(Debug.out).println(arg0.capture());
			assertEquals("my message", arg0.getValue());
		}

		@Test
		public void logDebugMessageStringThrowable_withException() {
			Throwable ex = mock(Throwable.class);
			this.plugin.logDebugMessage("my message", ex);
			ArgumentCaptor<String> arg0 = ArgumentCaptor.forClass(String.class);
			verify(Debug.out).println(arg0.capture());
			assertEquals("my message", arg0.getValue());
			ArgumentCaptor<PrintStream> arg1 = ArgumentCaptor.forClass(PrintStream.class);
			verify(ex).printStackTrace(arg1.capture());
			assertSame(Debug.out, arg1.getValue());
		}

		@Test
		public void logDebugMessageThrowable_withoutException() {
			this.plugin.logDebugMessage((Throwable) null);
			verifyZeroInteractions(Debug.out);
		}

		@Test
		public void logDebugMessageThrowable_withException() {
			Throwable ex = mock(Throwable.class);
			this.plugin.logDebugMessage(ex);
			ArgumentCaptor<PrintStream> arg1 = ArgumentCaptor.forClass(PrintStream.class);
			verify(ex).printStackTrace(arg1.capture());
			assertSame(Debug.out, arg1.getValue());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class StatusTests extends AbstractSarlTest {

		@NonNullByDefault
		private SARLEclipsePlugin plugin;

		@Before
		public void setUp() {
			this.plugin = spy(new SARLEclipsePlugin());
			SARLEclipsePlugin.setDefault(this.plugin);
		}

		@Test
		public void createOkStatus() {
			assertSame(Status.OK_STATUS, this.plugin.createOkStatus());
		}

		@Test
		public void createStatusIntString() {
			IStatus status = this.plugin.createStatus(IStatus.CANCEL, "my message");
			assertNotNull(status);
			assertEquals("my message", status.getMessage());
			assertZero(status.getCode());
			assertEquals(IStatus.CANCEL, status.getSeverity());
			assertNull(status.getException());
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

		@Test
		public void createStatusIntThrowable() {
			Throwable ex = mock(Throwable.class);
			when(ex.getMessage()).thenReturn("my message");
			IStatus status = this.plugin.createStatus(IStatus.CANCEL, ex);
			assertNotNull(status);
			assertEquals("my message", status.getMessage());
			assertZero(status.getCode());
			assertEquals(IStatus.CANCEL, status.getSeverity());
			assertSame(ex, status.getException());
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

		@Test
		public void createStatusIntStringThrowable() {
			Throwable ex = mock(Throwable.class);
			when(ex.getMessage()).thenReturn("my message");
			IStatus status = this.plugin.createStatus(IStatus.CANCEL, ex);
			assertNotNull(status);
			assertZero(status.getCode());
			assertEquals("my message", status.getMessage());
			assertEquals(IStatus.CANCEL, status.getSeverity());
			assertSame(ex, status.getException());
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

		@Test
		public void createStatusIntIntThrowable() {
			Throwable ex = mock(Throwable.class);
			when(ex.getMessage()).thenReturn("my message");
			IStatus status = this.plugin.createStatus(IStatus.CANCEL, 123, ex);
			assertNotNull(status);
			assertEquals(123, status.getCode());
			assertEquals("my message", status.getMessage());
			assertEquals(IStatus.CANCEL, status.getSeverity());
			assertSame(ex, status.getException());
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

		@Test
		public void createStatusIntIntString() {
			IStatus status = this.plugin.createStatus(IStatus.CANCEL, 123, "my message");
			assertNotNull(status);
			assertEquals(123, status.getCode());
			assertEquals("my message", status.getMessage());
			assertEquals(IStatus.CANCEL, status.getSeverity());
			assertNull(status.getException());
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

		@Test
		public void createStatusIntIntStringThrowable() {
			Throwable ex = mock(Throwable.class);
			when(ex.getMessage()).thenReturn("my message");
			IStatus status = this.plugin.createStatus(IStatus.CANCEL, 123, "my message2", ex);
			assertNotNull(status);
			assertEquals(123, status.getCode());
			assertEquals("my message2", status.getMessage());
			assertEquals(IStatus.CANCEL, status.getSeverity());
			assertSame(ex, status.getException());
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

	}

}

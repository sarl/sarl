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
package io.sarl.eclipse.tests;

import static io.sarl.eclipse.tests.Asserts.assertStrictlyNegative;
import static io.sarl.eclipse.tests.Asserts.assertStrictlyPositive;
import static io.sarl.eclipse.tests.Asserts.assertZero;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;
import io.sarl.eclipse.SARLEclipsePlugin;

import java.io.PrintStream;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.osgi.internal.debug.Debug;
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
	SARLEclipsePluginTest.StandardTests.class,
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
	public static class StandardTests {

		@Test
		public void compareTo_null_null() {
			int actual = SARLEclipsePlugin.compareTo(null, null);
			assertZero(actual);
		}

		@Test
		public void compareTo_null_1() {
			assertStrictlyNegative(SARLEclipsePlugin.compareTo(null, 1));
		}

		@Test
		public void compareTo_1_null() {
			assertStrictlyPositive(SARLEclipsePlugin.compareTo(1, null));
		}

		@Test
		public void compareTo_1_1() {
			assertZero(SARLEclipsePlugin.compareTo(1, 1));
		}

		@Test
		public void compareTo_0_1() {
			assertStrictlyNegative(SARLEclipsePlugin.compareTo(0, 1));
		}

		@Test
		public void compareTo_1_0() {
			assertStrictlyPositive(SARLEclipsePlugin.compareTo(1, 0));
		}

		@Test
		public void parseVersion() {
			assertNull(SARLEclipsePlugin.parseVersion(null));
			assertNull(SARLEclipsePlugin.parseVersion(""));
			assertEquals(new Version(1, 2, 3), SARLEclipsePlugin.parseVersion("1.2.3"));
			assertEquals(new Version(1, 2, 3, "something"), SARLEclipsePlugin.parseVersion("1.2.3.something"));
			assertNull(SARLEclipsePlugin.parseVersion("1.2.3-something"));
		}

		@Test
		public void compareVersionToRange_1_null_null() {
			Version v1 = new Version(1, 0, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v1, null, null));
		}

		@Test
		public void compareVersionToRange_1_null_1() {
			Version v1 = new Version(1, 0, 0);
			assertStrictlyPositive(SARLEclipsePlugin.compareVersionToRange(v1, null, v1));
		}

		@Test
		public void compareVersionToRange_1_null_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v1, null, v2));
		}

		@Test
		public void compareVersionToRange_1_null_3() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(SARLEclipsePlugin.compareVersionToRange(v1, null, v3));
		}

		@Test
		public void compareVersionToRange_1_1_null() {
			Version v1 = new Version(1, 0, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v1, v1, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_1_1() {
			Version v1 = new Version(1, 0, 0);
			SARLEclipsePlugin.compareVersionToRange(v1, v1, v1);
		}

		@Test
		public void compareVersionToRange_1_1_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v1, v1, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_1_3() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v1, v1, v3);
		}

		@Test
		public void compareVersionToRange_1_2_null() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertStrictlyNegative(SARLEclipsePlugin.compareVersionToRange(v1, v2, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_2_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			SARLEclipsePlugin.compareVersionToRange(v1, v2, v1);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_2_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			SARLEclipsePlugin.compareVersionToRange(v1, v2, v2);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_2_3() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v1, v2, v3);
		}

		@Test
		public void compareVersionToRange_1_3_null() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v1, v3, null));
		}

		@Test
		public void compareVersionToRange_1_3_1() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(SARLEclipsePlugin.compareVersionToRange(v1, v3, v1));
		}

		@Test
		public void compareVersionToRange_1_3_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v1, v3, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_3_3() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v1, v3, v3);
		}

		@Test
		public void compareVersionToRange_2_null_null() {
			Version v2 = new Version(2, 0, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v2, null, null));
		}

		@Test
		public void compareVersionToRange_2_null_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertStrictlyPositive(SARLEclipsePlugin.compareVersionToRange(v2, null, v1));
		}

		@Test
		public void compareVersionToRange_2_null_2() {
			Version v2 = new Version(2, 0, 0);
			assertStrictlyPositive(SARLEclipsePlugin.compareVersionToRange(v2, null, v2));
		}

		@Test
		public void compareVersionToRange_2_null_3() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(SARLEclipsePlugin.compareVersionToRange(v2, null, v3));
		}

		@Test
		public void compareVersionToRange_2_1_null() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v2, v1, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_1_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			SARLEclipsePlugin.compareVersionToRange(v2, v1, v1);
		}

		@Test
		public void compareVersionToRange_2_1_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertStrictlyPositive(SARLEclipsePlugin.compareVersionToRange(v2, v1, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_1_3() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v2, v1, v3);
		}

		@Test
		public void compareVersionToRange_2_2_null() {
			Version v2 = new Version(2, 0, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v2, v2, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_2_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			SARLEclipsePlugin.compareVersionToRange(v2, v2, v1);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_2_2() {
			Version v2 = new Version(2, 0, 0);
			SARLEclipsePlugin.compareVersionToRange(v2, v2, v2);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_2_3() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v2, v2, v3);
		}

		@Test
		public void compareVersionToRange_2_3_null() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v2, v3, null));
		}

		@Test
		public void compareVersionToRange_2_3_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(SARLEclipsePlugin.compareVersionToRange(v2, v3, v1));
		}

		@Test
		public void compareVersionToRange_2_3_2() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(SARLEclipsePlugin.compareVersionToRange(v2, v3, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_3_3() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v2, v3, v3);
		}

		@Test
		public void compareVersionToRange_3_null_null() {
			Version v3 = new Version(0, 10, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v3, null, null));
		}

		@Test
		public void compareVersionToRange_3_null_1() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v3, null, v1));
		}

		@Test
		public void compareVersionToRange_3_null_2() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v3, null, v2));
		}

		@Test
		public void compareVersionToRange_3_null_3() {
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(SARLEclipsePlugin.compareVersionToRange(v3, null, v3));
		}

		@Test
		public void compareVersionToRange_3_1_null() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyNegative(SARLEclipsePlugin.compareVersionToRange(v3, v1, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_1_1() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v3, v1, v1);
		}

		@Test
		public void compareVersionToRange_3_1_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyNegative(SARLEclipsePlugin.compareVersionToRange(v3, v1, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_1_3() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v3, v1, v3);
		}

		@Test
		public void compareVersionToRange_3_2_null() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyNegative(SARLEclipsePlugin.compareVersionToRange(v3, v2, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_2_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v3, v2, v1);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_2_2() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v3, v2, v2);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_2_3() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v3, v2, v3);
		}

		@Test
		public void compareVersionToRange_3_3_null() {
			Version v3 = new Version(0, 10, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v3, v3, null));
		}

		@Test
		public void compareVersionToRange_3_3_1() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v3, v3, v1));
		}

		@Test
		public void compareVersionToRange_3_3_2() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(SARLEclipsePlugin.compareVersionToRange(v3, v3, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_3_3() {
			Version v3 = new Version(0, 10, 0);
			SARLEclipsePlugin.compareVersionToRange(v3, v3, v3);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class LogTests {

		private PrintStream old;
		private ILog logger;
		private SARLEclipsePlugin plugin;

		@Before
		public void setUp() {
			this.old = Debug.out;
			Debug.out = mock(PrintStream.class);
			this.logger = mock(ILog.class);
			this.plugin = mock(SARLEclipsePlugin.class);
			SARLEclipsePlugin.setDefault(this.plugin);
			when(this.plugin.getILog()).thenReturn(this.logger);
		}

		@After
		public void tearDown() {
			Debug.out = this.old;
			this.plugin = null;
			this.logger = null;
		}

		@Test
		public void logIStatus() {
			IStatus status = mock(IStatus.class);
			SARLEclipsePlugin.log(status);
			ArgumentCaptor<IStatus> arg = ArgumentCaptor.forClass(IStatus.class);
			verify(this.logger).log(arg.capture());
			assertSame(status, arg.getValue());
		}

		@Test
		public void logErrorMessage() {
			SARLEclipsePlugin.logErrorMessage("my message");
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
			SARLEclipsePlugin.log((Throwable) null);
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
			SARLEclipsePlugin.log(ex);
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
			SARLEclipsePlugin.logDebugMessage("my message");
			ArgumentCaptor<String> arg = ArgumentCaptor.forClass(String.class);
			verify(Debug.out).println(arg.capture());
			assertEquals("my message", arg.getValue());
		}

		@Test
		public void logDebugMessageStringThrowable_withoutException() {
			SARLEclipsePlugin.logDebugMessage("my message", null);
			ArgumentCaptor<String> arg0 = ArgumentCaptor.forClass(String.class);
			verify(Debug.out).println(arg0.capture());
			assertEquals("my message", arg0.getValue());
		}

		@Test
		public void logDebugMessageStringThrowable_withException() {
			Throwable ex = mock(Throwable.class);
			SARLEclipsePlugin.logDebugMessage("my message", ex);
			ArgumentCaptor<String> arg0 = ArgumentCaptor.forClass(String.class);
			verify(Debug.out).println(arg0.capture());
			assertEquals("my message", arg0.getValue());
			ArgumentCaptor<PrintStream> arg1 = ArgumentCaptor.forClass(PrintStream.class);
			verify(ex).printStackTrace(arg1.capture());
			assertSame(Debug.out, arg1.getValue());
		}

		@Test
		public void logDebugMessageThrowable_withoutException() {
			SARLEclipsePlugin.logDebugMessage((Throwable) null);
			verifyZeroInteractions(Debug.out);
		}

		@Test
		public void logDebugMessageThrowable_withException() {
			Throwable ex = mock(Throwable.class);
			SARLEclipsePlugin.logDebugMessage(ex);
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
	public static class StatusTests {

		@Test
		public void createOkStatus() {
			assertSame(Status.OK_STATUS, SARLEclipsePlugin.createOkStatus());
		}

		@Test
		public void createStatusIntString() {
			IStatus status = SARLEclipsePlugin.createStatus(IStatus.CANCEL, "my message");
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
			IStatus status = SARLEclipsePlugin.createStatus(IStatus.CANCEL, ex);
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
			IStatus status = SARLEclipsePlugin.createStatus(IStatus.CANCEL, ex);
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
			IStatus status = SARLEclipsePlugin.createStatus(IStatus.CANCEL, 123, ex);
			assertNotNull(status);
			assertEquals(123, status.getCode());
			assertEquals("my message", status.getMessage());
			assertEquals(IStatus.CANCEL, status.getSeverity());
			assertSame(ex, status.getException());
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

		@Test
		public void createStatusIntIntString() {
			IStatus status = SARLEclipsePlugin.createStatus(IStatus.CANCEL, 123, "my message");
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
			IStatus status = SARLEclipsePlugin.createStatus(IStatus.CANCEL, 123, "my message2", ex);
			assertNotNull(status);
			assertEquals(123, status.getCode());
			assertEquals("my message2", status.getMessage());
			assertEquals(IStatus.CANCEL, status.getSeverity());
			assertSame(ex, status.getException());
			assertEquals(SARLEclipsePlugin.PLUGIN_ID, status.getPlugin());
		}

	}

}

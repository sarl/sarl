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
package io.sarl.eclipse.tests.runtime;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.net.URL;
import java.text.MessageFormat;
import java.util.UUID;

import foo.Foo;
import org.eclipse.core.internal.preferences.EclipsePreferences;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.launching.PropertyChangeEvent;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.ISREInstallChangedListener;
import io.sarl.eclipse.runtime.SARLRuntime;
import io.sarl.eclipse.tests.AbstractEclipseTestPreferencesTest;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlUiTest;
import io.sarl.tests.api.TestScope;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @noinstantiate This class is not intended to be instantiated by clients.
 */
@RunWith(Suite.class)
@SuiteClasses({
	SARLRuntimeTest.PreferenceKey.class,
	SARLRuntimeTest.GetterSetter.class,
	SARLRuntimeTest.InputOutput.class,
	SARLRuntimeTest.EventFiring.class,
})
@SuppressWarnings("all")
public final class SARLRuntimeTest {

	protected static ISREInstall[] createSREInstallArray() {
		ISREInstall[] installs = new ISREInstall[15];
		for (int i = 0; i < installs.length; ++i) {
			installs[i] = mock(ISREInstall.class);
			when(installs[i].getId()).thenReturn(SARLRuntime.createUniqueIdentifier());
			if ((i % 2) == 1) {
				when(installs[i].getValidity()).thenReturn(Status.OK_STATUS);
			} else {
				when(installs[i].getValidity()).thenReturn(
						new Status(IStatus.ERROR, SARLEclipsePlugin.PLUGIN_ID, "message"));
			}
		}
		return installs;
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class PreferenceKey extends AbstractSarlTest {

		@Test
		public void getCurrentPreferenceKey() {
			assertEquals(SARLRuntime.DEFAULT_PREFERENCE_KEY, SARLRuntime.getCurrentPreferenceKey());
		}

		@Test
		public void setCurrentPreferenceKey_notNull_0() {
			String key = UUID.randomUUID().toString();
			SARLRuntime.setCurrentPreferenceKey(key);
			assertEquals(key, SARLRuntime.getCurrentPreferenceKey());
		}

		@Test
		public void setCurrentPreferenceKey_notNull_1() {
			SARLRuntime.setCurrentPreferenceKey(AbstractEclipseTestPreferencesTest.TESTING_PREFERENCE_KEY);
			assertEquals(AbstractEclipseTestPreferencesTest.TESTING_PREFERENCE_KEY, SARLRuntime.getCurrentPreferenceKey());
		}

		@Test
		public void setCurrentPreferenceKey_null() {
			String key = UUID.randomUUID().toString();
			SARLRuntime.setCurrentPreferenceKey(key);
			SARLRuntime.setCurrentPreferenceKey(null);
			assertEquals(SARLRuntime.DEFAULT_PREFERENCE_KEY, SARLRuntime.getCurrentPreferenceKey());
		}

		@Test
		public void setCurrentPreferenceKey_empty() {
			String key = UUID.randomUUID().toString();
			SARLRuntime.setCurrentPreferenceKey(key);
			SARLRuntime.setCurrentPreferenceKey("");
			assertEquals(SARLRuntime.DEFAULT_PREFERENCE_KEY, SARLRuntime.getCurrentPreferenceKey());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class GetterSetter extends AbstractEclipseTestPreferencesTest {

		@Test
		public void createUniqueIdentifier() {
			String[] values = new String[50];
			for (int i = 0; i < values.length; ++i) {
				values[i] = SARLRuntime.createUniqueIdentifier();
			}
			for(int i = 0; i < values.length - 1; ++i) {
				for(int j = i + 1; j < values.length; ++j) {
					assertNotEquals("Two identifiers with the same value have been generated.", values[i], values[j]);
				}
			}
		}

		@Test
		public void setSREInstalls_nullMonitor() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			//
			SARLRuntime.setSREInstalls(installs, null);
			//
			ISREInstall[] actual = SARLRuntime.getSREInstalls();
			assertArraySimilar(installs, actual);
		}

		@Test
		public void setSREInstalls_monitor() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			//
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			SARLRuntime.setSREInstalls(installs, monitor);
			//
			ISREInstall[] actual = SARLRuntime.getSREInstalls();
			assertArraySimilar(installs, actual);
			//
			verify(monitor, times(1)).beginTask(ArgumentMatchers.anyString(), ArgumentMatchers.anyInt());
		}

		@Test
		public void getDefaultSREInstall_default() {
			assertNull(SARLRuntime.getDefaultSREInstall());
		}

		@Test
		public void getDefaultSREInstall_afterSetting() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			SARLRuntime.setSREInstalls(installs, null);
			//
			ISREInstall defaultInstall = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultInstall);
			assertTrue(defaultInstall.getValidity().isOK());
			assertArrayContains(installs, defaultInstall);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitor_noSRE_nullMonitor() throws Exception {
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.setDefaultSREInstall(sre, null);
			assertNull(SARLRuntime.getDefaultSREInstall());
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitor_noSRE_monitor() throws Exception {
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.setDefaultSREInstall(sre, monitor);
			assertNull(SARLRuntime.getDefaultSREInstall());
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitor_SRE_unknownSRE_nullMonitor() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.setDefaultSREInstall(sre, null);
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertNotSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitor_SRE_unknownSRE_monitor() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.setDefaultSREInstall(sre, null);
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertNotSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitor_SRE_knownSRE_nullMonitor() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			ISREInstall sre = installs[installs.length / 2];
			SARLRuntime.setDefaultSREInstall(sre, null);
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitor_SRE_knownSRE_monitor() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			ISREInstall sre = installs[installs.length / 2];
			SARLRuntime.setDefaultSREInstall(sre, null);
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_noSRE_nullMonitor_savePreferences() throws Exception {
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, null, true);
			//
			assertNotNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			assertNull(SARLRuntime.getDefaultSREInstall());
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_noSRE_monitor_savePreferences() throws Exception {
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, monitor, true);
			//
			assertNotNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			assertNull(SARLRuntime.getDefaultSREInstall());
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_SRE_unknownSRE_nullMonitor_savePreferences() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, null, true);
			//
			assertNotNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertNotSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_SRE_unknownSRE_monitor_savePreferences() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, null, true);
			//
			assertNotNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertNotSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_SRE_knownSRE_nullMonitor_savePreferences() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			ISREInstall sre = installs[installs.length / 2];
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, null, true);
			//
			assertNotNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_SRE_knownSRE_monitor_savePreferences() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			ISREInstall sre = installs[installs.length / 2];
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, null, true);
			//
			assertNotNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_noSRE_nullMonitor_noSavePreferences() throws Exception {
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, null, false);
			//
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			assertNull(SARLRuntime.getDefaultSREInstall());
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_noSRE_monitor_noSavePreferences() throws Exception {
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, monitor, false);
			//
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			assertNull(SARLRuntime.getDefaultSREInstall());
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_SRE_unknownSRE_nullMonitor_noSavePreferences() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, null, false);
			//
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertNotSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_SRE_unknownSRE_monitor_noSavePreferences() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, null, false);
			//
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertNotSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_SRE_knownSRE_nullMonitor_noSavePreferences() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			ISREInstall sre = installs[installs.length / 2];
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, null, false);
			//
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertSame(sre, defaultSRE);
		}

		@Test
		public void setDefaultSREInstallISREInstallIProgressMonitorBoolean_SRE_knownSRE_monitor_noSavePreferences() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			ISREInstall sre = installs[installs.length / 2];
			SARLRuntime.clearSREConfiguration();
			//
			SARLRuntime.setDefaultSREInstall(sre, null, false);
			//
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertSame(sre, defaultSRE);
		}

		/** @see #reset_inTycho()
		 */
		@Test
		@TestScope(eclipse = true, tycho = false)
		public void reset_inEclipse() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			//
			SARLRuntime.reset();
			//
			String xml = SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null);
			assertEquals(
					"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
					+ "<SREs defaultSRE=\"io.janusproject.plugin.sre\">"
					+ "<SRE class=\"io.janusproject.JanusSREInstall\" id=\"io.janusproject.plugin.sre\" platform=\"true\"/>"
					+ "</SREs>", xml);
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNotNull(defaultSRE);
			assertEquals("io.janusproject.plugin.sre", defaultSRE.getId());
		}

		/** @see #reset_inEclipse()
		 */
		@Test
		@TestScope(eclipse = false, tycho = true)
		public void reset_inTycho() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			//
			SARLRuntime.reset();
			//
			String xml = SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null);
			assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><SREs/>", xml);
			ISREInstall defaultSRE = SARLRuntime.getDefaultSREInstall();
			assertNull(defaultSRE);
		}

		@Test
		public void getSREFromId_noSRE() {
			String id = UUID.randomUUID().toString();
			ISREInstall sre = mock(ISREInstall.class);
			when(sre.getId()).thenReturn(id);
			//
			assertNull(SARLRuntime.getSREFromId(id));
		}

		@Test
		public void getSREFromId_SRE_notRegistered() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			String id = UUID.randomUUID().toString();
			ISREInstall sre = mock(ISREInstall.class);
			when(sre.getId()).thenReturn(id);
			//
			assertNull(SARLRuntime.getSREFromId(id));
		}

		@Test
		public void getSREFromId_SRE_registered() throws Exception {
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			ISREInstall sre = installs[installs.length / 2];
			String id = sre.getId();
			//
			assertSame(sre, SARLRuntime.getSREFromId(id));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class EventFiring extends AbstractEclipseTestPreferencesTest {

		@NonNullByDefault
		private ISREInstallChangedListener listener;

		@Before
		public void setUp() throws Exception {
			super.setUp();
			this.listener = mock(ISREInstallChangedListener.class);
			SARLRuntime.addSREInstallChangedListener(listener);
		}

		@After
		public void tearDown() throws Exception {
			if (this.listener != null) {
				SARLRuntime.removeSREInstallChangedListener(this.listener);
			}
			super.tearDown();
		}

		@Test
		public void fireSREChanged() {
			PropertyChangeEvent event = mock(PropertyChangeEvent.class);
			SARLRuntime.fireSREChanged(event);
			ArgumentCaptor<PropertyChangeEvent> arg = ArgumentCaptor.forClass(PropertyChangeEvent.class);
			verify(this.listener, times(1)).sreChanged(arg.capture());
			assertSame(event, arg.getValue());
		}

		@Test
		public void fireSREAdded() {
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.fireSREAdded(sre);
			ArgumentCaptor<ISREInstall> arg = ArgumentCaptor.forClass(ISREInstall.class);
			verify(this.listener, times(1)).sreAdded(arg.capture());
			assertSame(sre, arg.getValue());
		}

		@Test
		public void fireSRERemoved() {
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.fireSRERemoved(sre);
			ArgumentCaptor<ISREInstall> arg = ArgumentCaptor.forClass(ISREInstall.class);
			verify(this.listener, times(1)).sreRemoved(arg.capture());
			assertSame(sre, arg.getValue());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class InputOutput extends AbstractEclipseTestPreferencesTest {

		@Test
		public void saveSREConfiguration_nullMonitor_noSRE() throws Exception {
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			SARLRuntime.saveSREConfiguration(null);
			String xml = SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null);
			assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><SREs/>", xml);
		}

		@Test
		public void saveSREConfiguration_monitor_noSRE() throws Exception {
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			SARLRuntime.saveSREConfiguration(monitor);
			String xml = SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null);
			assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><SREs/>", xml);
		}

		@Test
		public void saveSREConfiguration_nullMonitor_SRE() throws Exception {
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			//
			SARLRuntime.saveSREConfiguration(null);
			//
			String xml = SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null);
			assertNotNull(xml);
			assertNotEquals("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><SREs/>", xml);
		}

		@Test
		public void saveSREConfiguration_monitor_SRE() throws Exception {
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			//
			SARLRuntime.saveSREConfiguration(monitor);
			//
			String xml = SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null);
			assertNotNull(xml);
			assertNotEquals("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><SREs/>", xml);
		}

		@Test
		public void clearSREConfiguration() throws Exception {
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			ISREInstall[] installs = createSREInstallArray();
			SARLRuntime.setSREInstalls(installs, null);
			SARLRuntime.saveSREConfiguration(monitor);
			assertNotNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
			//
			SARLRuntime.clearSREConfiguration();
			//
			assertNull(SARLEclipsePlugin.getDefault().getPreferences().get(TESTING_PREFERENCE_KEY, null));
		}

		@Test
		public void getSREAsXML() throws Exception {
			ISREInstall sre = mock(ISREInstall.class);
			String id = SARLRuntime.createUniqueIdentifier();
			when(sre.getId()).thenReturn(id);
			doAnswer(new Answer() {
				@Override
				public Object answer(InvocationOnMock invocation) throws Throwable {
					Document document = (Document) invocation.getArguments()[0];
					Element element = (Element) invocation.getArguments()[1];
					Element node = document.createElement("testNode");
					node.setAttribute("attrName", "attrValue");
					element.appendChild(node);
					return null;
				}
			}).when(sre).getAsXML(ArgumentMatchers.any(Document.class), ArgumentMatchers.any(Element.class));
			//
			String xml = SARLRuntime.getSREAsXML(sre);
			assertNotNull(xml);
			assertEquals(MessageFormat.format(
					"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
					+ "<SRE class=\"{0}\" id=\"{1}\" platform=\"false\"><testNode attrName=\"attrValue\"/></SRE>",
					sre.getClass().getName(),
					id), xml);
		}

		@Test
		public void setSREFromXML() throws Exception {
			ISREInstall sre = mock(ISREInstall.class);
			SARLRuntime.setSREFromXML(sre, "<SRE/>");
			ArgumentCaptor<Element> arg = ArgumentCaptor.forClass(Element.class);
			verify(sre, times(1)).setFromXML(arg.capture());
			Element elt = arg.getValue();
			assertNotNull(elt);
			assertEquals("SRE", elt.getNodeName());
		}

		@Test
		public void getSREsAsXML_noSRE_nullMonitor() throws Exception {
			String xml = SARLRuntime.getSREsAsXML(null);
			assertEquals(
					"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
					+ "<SREs/>",
					xml);
		}

		@Test
		public void getSREsAsXML_noSRE_monitor() throws Exception {
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			String xml = SARLRuntime.getSREsAsXML(monitor);
			assertEquals(
					"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
					+ "<SREs/>",
					xml);
		}

		@Test
		public void getSREsAsXML_SRE_nullMonitor() throws Exception {
			ISREInstall sre1 = mock(ISREInstall.class);
			String id1 = SARLRuntime.createUniqueIdentifier();
			when(sre1.getId()).thenReturn(id1);
			IStatus okStatus = SARLEclipsePlugin.getDefault().createOkStatus();
			when(sre1.getValidity()).thenReturn(okStatus);
			ISREInstall sre2 = mock(ISREInstall.class);
			String id2 = SARLRuntime.createUniqueIdentifier();
			when(sre2.getId()).thenReturn(id2);
			when(sre2.getValidity()).thenReturn(okStatus);
			SARLRuntime.setSREInstalls(new ISREInstall[] {
					sre1, sre2
			}, null);
			ISREInstall[] installs = SARLRuntime.getSREInstalls();
			//
			String xml = SARLRuntime.getSREsAsXML(null);
			assertEquals(MessageFormat.format(
					"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
					+ "<SREs defaultSRE=\"{4}\">"
					+ "<SRE class=\"{0}\" id=\"{1}\" platform=\"false\"/>"
					+ "<SRE class=\"{2}\" id=\"{3}\" platform=\"false\"/>"
					+ "</SREs>",
					installs[0].getClass().getName(),
					installs[0].getId(),
					installs[1].getClass().getName(),
					installs[1].getId(),
					installs[0].getId()), xml);
		}

		@Test
		public void getSREsAsXML_SRE_monitor() throws Exception {
			IProgressMonitor monitor = mock(IProgressMonitor.class);
			ISREInstall sre1 = mock(ISREInstall.class);
			String id1 = SARLRuntime.createUniqueIdentifier();
			when(sre1.getId()).thenReturn(id1);
			IStatus okStatus = SARLEclipsePlugin.getDefault().createOkStatus();
			when(sre1.getValidity()).thenReturn(okStatus);
			ISREInstall sre2 = mock(ISREInstall.class);
			String id2 = SARLRuntime.createUniqueIdentifier();
			when(sre2.getId()).thenReturn(id2);
			when(sre2.getValidity()).thenReturn(okStatus);
			SARLRuntime.setSREInstalls(new ISREInstall[] {
					sre1, sre2
			}, null);
			ISREInstall[] installs = SARLRuntime.getSREInstalls();
			//
			String xml = SARLRuntime.getSREsAsXML(monitor);
			assertEquals(MessageFormat.format(
					"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
					+ "<SREs defaultSRE=\"{4}\">"
					+ "<SRE class=\"{0}\" id=\"{1}\" platform=\"false\"/>"
					+ "<SRE class=\"{2}\" id=\"{3}\" platform=\"false\"/>"
					+ "</SREs>",
					installs[0].getClass().getName(),
					installs[0].getId(),
					installs[1].getClass().getName(),
					installs[1].getId(),
					installs[0].getId()), xml);
		}

		private File getUnpackedSRE() throws Exception {
			URL url = Foo.class.getResource("/foo/unpackedSRE/META-INF/MANIFEST.MF");
			assertNotNull(url);
			url = FileLocator.toFileURL(url);
			assertNotNull(url);
			File file = new File(url.getPath());
			file = file.getParentFile().getParentFile();
			assertEquals("unpackedSRE", file.getName());
			return file;
		}

		private File getUnpackedJAR() throws Exception {
			URL url = Foo.class.getResource("/foo/unpackedJAR/META-INF/MANIFEST.MF");
			assertNotNull(url);
			url = FileLocator.toFileURL(url);
			assertNotNull(url);
			File file = new File(url.getPath());
			file = file.getParentFile().getParentFile();
			assertEquals("unpackedJAR", file.getName());
			return file;
		}

		private File getPackedSRE() throws Exception {
			URL url = Foo.class.getResource("/foo/foo2.jar");
			assertNotNull(url);
			url = FileLocator.toFileURL(url);
			assertNotNull(url);
			File file = new File(url.getPath());
			assertEquals("foo2.jar", file.getName());
			return file;
		}

		private File getPackedJAR() throws Exception {
			URL url = Foo.class.getResource("/foo/foo3.jar");
			assertNotNull(url);
			url = FileLocator.toFileURL(url);
			assertNotNull(url);
			File file = new File(url.getPath());
			assertEquals("foo3.jar", file.getName());
			return file;
		}

		@Test
		public void isUnpackedSREFile_unpacked_sreFolder() throws Exception {
			File file = getUnpackedSRE();
			assertTrue(SARLRuntime.isUnpackedSRE(file));
		}

		@Test
		public void isUnpackedSREFile_unpacked_jarFolder() throws Exception {
			File file = getUnpackedJAR();
			assertFalse(SARLRuntime.isUnpackedSRE(file));
		}

		@Test
		public void isUnpackedSREFile_packed_sreJar() throws Exception {
			File file = getPackedSRE();
			assertFalse(SARLRuntime.isUnpackedSRE(file));
		}

		@Test
		public void isUnpackedSREFile_packed_jarJar() throws Exception {
			File file = getPackedJAR();
			assertFalse(SARLRuntime.isUnpackedSRE(file));
		}

		@Test
		public void isPackedSREFile_unpacked_sreFolder() throws Exception {
			File file = getUnpackedSRE();
			assertFalse(SARLRuntime.isPackedSRE(file));
		}

		@Test
		public void isPackedSREFile_unpacked_jarFolder() throws Exception {
			File file = getUnpackedJAR();
			assertFalse(SARLRuntime.isPackedSRE(file));
		}

		@Test
		public void isPackedSREFile_packed_sreJar() throws Exception {
			File file = getPackedSRE();
			assertTrue(SARLRuntime.isPackedSRE(file));
		}

		@Test
		public void isPackedSREFile_packed_jarJar() throws Exception {
			File file = getPackedJAR();
			assertFalse(SARLRuntime.isPackedSRE(file));
		}

		@Test
		public void isUnpackedSREIPath_unpacked_sreFolder() throws Exception {
			File file = getUnpackedSRE();
			IPath path = Path.fromOSString(file.getAbsolutePath());
			assertTrue(SARLRuntime.isUnpackedSRE(path));
		}

		@Test
		public void isUnpackedSREIPath_unpacked_jarFolder() throws Exception {
			File file = getUnpackedJAR();
			IPath path = Path.fromOSString(file.getAbsolutePath());
			assertFalse(SARLRuntime.isUnpackedSRE(path));
		}

		@Test
		public void isUnpackedSREIPath_packed_sreJar() throws Exception {
			File file = getPackedSRE();
			IPath path = Path.fromOSString(file.getAbsolutePath());
			assertFalse(SARLRuntime.isUnpackedSRE(path));
		}

		@Test
		public void isUnpackedSREIPath_packed_jarJar() throws Exception {
			File file = getPackedJAR();
			IPath path = Path.fromOSString(file.getAbsolutePath());
			assertFalse(SARLRuntime.isUnpackedSRE(path));
		}

		@Test
		public void isPackedSREIPath_unpacked_sreFolder() throws Exception {
			File file = getUnpackedSRE();
			IPath path = Path.fromOSString(file.getAbsolutePath());
			assertFalse(SARLRuntime.isPackedSRE(path));
		}

		@Test
		public void isPackedSREIPath_unpacked_jarFolder() throws Exception {
			File file = getUnpackedJAR();
			IPath path = Path.fromOSString(file.getAbsolutePath());
			assertFalse(SARLRuntime.isPackedSRE(path));
		}

		@Test
		public void isPackedSREIPath_packed_sreJar() throws Exception {
			File file = getPackedSRE();
			IPath path = Path.fromOSString(file.getAbsolutePath());
			assertTrue(SARLRuntime.isPackedSRE(path));
		}

		@Test
		public void isPackedSREIPath_packed_jarJar() throws Exception {
			File file = getPackedJAR();
			IPath path = Path.fromOSString(file.getAbsolutePath());
			assertFalse(SARLRuntime.isPackedSRE(path));
		}

	}

}

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

import static org.mockito.Mockito.when;

import javax.inject.Inject;

import org.eclipse.core.internal.preferences.EclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.After;
import org.junit.Before;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.SARLRuntime;
import io.sarl.tests.api.AbstractSarlUiTest;

/** Abstract class that is forcing the Eclipse preferences to be specific for tests.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractEclipseTestPreferencesTest extends AbstractSarlUiTest {

	public static final String TESTING_PREFERENCE_KEY = SARLEclipsePlugin.PLUGIN_ID + ".tests.runtime.PREF_SRE_XML"; //$NON-NLS-1$

	@NonNullByDefault
	protected SARLEclipsePlugin plugin;

	@NonNullByDefault
	protected IEclipsePreferences preferences;

	@NonNullByDefault
	protected Bundle bundle;

	@NonNullByDefault
	protected BundleContext bundleContext;
	
	@Inject
	private ReflectExtensions reflect;
	
	@Before
	public void setUp() throws Exception {
		this.reflect.setStatic(SARLRuntime.class, "enableSreExtensionPoints", Boolean.FALSE);
		this.preferences = new EclipsePreferences();
		this.plugin = spy(new SARLEclipsePlugin());
		SARLEclipsePlugin.setDefault(this.plugin);
		when(this.plugin.getPreferences()).thenReturn(this.preferences);
		this.bundle = mock(Bundle.class);
		when(this.bundle.getSymbolicName()).thenReturn(SARLEclipsePlugin.PLUGIN_ID);
		this.bundleContext = mock(BundleContext.class);
		when(this.bundleContext.getBundle()).thenReturn(this.bundle);
		when(this.bundle.getBundleContext()).thenReturn(this.bundleContext);
		SARLRuntime.setCurrentPreferenceKey(TESTING_PREFERENCE_KEY);
		this.plugin.start(this.bundleContext);
	}

	@After
	public void tearDown() throws Exception {
		if (SARLEclipsePlugin.getDefault() != null) {
			SARLRuntime.setSREInstalls(new ISREInstall[0], null);
			SARLRuntime.clearSREConfiguration();
			SARLRuntime.setCurrentPreferenceKey(null);
			SARLEclipsePlugin.setDefault(null);
		}
		this.preferences.clear();
		this.reflect.setStatic(SARLRuntime.class, "enableSreExtensionPoints", Boolean.TRUE);
	}

}

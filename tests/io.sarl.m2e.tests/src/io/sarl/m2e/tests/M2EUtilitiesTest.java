/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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
import io.sarl.m2e.M2EUtilities;
import io.sarl.m2e.SARLMavenEclipsePlugin;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

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
@SuppressWarnings("all")
public final class M2EUtilitiesTest extends AbstractSarlTest {

	@Nullable
	private SARLMavenEclipsePlugin plugin;
	@Nullable
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
	public void parseMavenVersion() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
	}

}
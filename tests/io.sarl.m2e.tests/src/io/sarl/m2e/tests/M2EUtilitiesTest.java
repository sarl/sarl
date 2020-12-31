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
import static org.mockito.Mockito.spy;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.osgi.framework.Version;

import io.sarl.m2e.M2EUtilities;
import io.sarl.m2e.SARLMavenEclipsePlugin;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public final class M2EUtilitiesTest extends AbstractSarlTest {

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
	public void parseMavenVersion_null() {
		Version version = M2EUtilities.parseMavenVersion(null);
		assertNotNull(version);
		assertEquals(0, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_empty() {
		Version version = M2EUtilities.parseMavenVersion("");
		assertNotNull(version);
		assertEquals(0, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_str() {
		Version version = M2EUtilities.parseMavenVersion("this is a string");
		assertNotNull(version);
		assertEquals(0, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_1234_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.4");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_1234_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.4-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	public void parseMavenVersion_123_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_123_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	public void parseMavenVersion_12_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_12_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(0, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	public void parseMavenVersion_1_standard() {
		Version version = M2EUtilities.parseMavenVersion("1");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_1_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	public void parseMavenVersion_1234x_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.4.x");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_1234x_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.4.x-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	public void parseMavenVersion_123x_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.x");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_123x_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.x-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	public void parseMavenVersion_12x_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2.x");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_12x_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2.x-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(0, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	public void parseMavenVersion_1x_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.x");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_1x_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.x-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	public void parseMavenVersion_x_standard() {
		Version version = M2EUtilities.parseMavenVersion("x");
		assertNotNull(version);
		assertEquals(0, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	public void parseMavenVersion_x_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("x-SNAPSHOT");
		assertNotNull(version);
		assertEquals(0, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

}
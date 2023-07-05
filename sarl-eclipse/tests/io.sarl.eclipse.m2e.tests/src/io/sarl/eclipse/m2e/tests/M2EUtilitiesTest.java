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

import static io.sarl.tests.api.tools.TestAssertions.assertNullOrEmpty;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.spy;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.osgi.framework.Version;

import io.sarl.eclipse.m2e.SARLMavenEclipsePlugin;
import io.sarl.eclipse.m2e.utils.M2EUtilities;
import io.sarl.lang.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("M2EUtilities")
@Tag("unit")
@Tag("eclipse")
@Tag("eclipse-unit")
@SuppressWarnings("all")
public final class M2EUtilitiesTest extends AbstractSarlTest {

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
	@DisplayName("parseMavenVersion(null)")
	public void parseMavenVersion_null() {
		Version version = M2EUtilities.parseMavenVersion(null);
		assertNotNull(version);
		assertEquals(0, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"\")")
	public void parseMavenVersion_empty() {
		Version version = M2EUtilities.parseMavenVersion("");
		assertNotNull(version);
		assertEquals(0, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"this is a string\")")
	public void parseMavenVersion_str() {
		Version version = M2EUtilities.parseMavenVersion("this is a string");
		assertNotNull(version);
		assertEquals(0, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2.3.4\")")
	public void parseMavenVersion_1234_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.4");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2.3.4-SNAPSHOT\")")
	public void parseMavenVersion_1234_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.4-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2.3\")")
	public void parseMavenVersion_123_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2.3-SNAPSHOT\")")
	public void parseMavenVersion_123_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2\")")
	public void parseMavenVersion_12_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2-SNAPSHOT\")")
	public void parseMavenVersion_12_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(0, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1\")")
	public void parseMavenVersion_1_standard() {
		Version version = M2EUtilities.parseMavenVersion("1");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1-SNAPSHOT\")")
	public void parseMavenVersion_1_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2.3.4.x\")")
	public void parseMavenVersion_1234x_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.4.x");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2.3.4.x-SNAPSHOT\")")
	public void parseMavenVersion_1234x_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.4.x-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2.3.x\")")
	public void parseMavenVersion_123x_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.x");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2.3.x-SNAPSHOT\")")
	public void parseMavenVersion_123x_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2.3.x-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(3, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2.x\")")
	public void parseMavenVersion_12x_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.2.x");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.2.x-SNAPSHOT\")")
	public void parseMavenVersion_12x_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.2.x-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(2, version.getMinor());
		assertEquals(0, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.x\")")
	public void parseMavenVersion_1x_standard() {
		Version version = M2EUtilities.parseMavenVersion("1.x");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"1.x-SNAPSHOT\")")
	public void parseMavenVersion_1x_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("1.x-SNAPSHOT");
		assertNotNull(version);
		assertEquals(1, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"x\")")
	public void parseMavenVersion_x_standard() {
		Version version = M2EUtilities.parseMavenVersion("x");
		assertNotNull(version);
		assertEquals(0, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertNullOrEmpty(version.getQualifier());
	}

	@Test
	@DisplayName("parseMavenVersion(\"x-SNAPSHOT\")")
	public void parseMavenVersion_x_snapshot() {
		Version version = M2EUtilities.parseMavenVersion("x-SNAPSHOT");
		assertNotNull(version);
		assertEquals(0, version.getMajor());
		assertEquals(0, version.getMinor());
		assertEquals(0, version.getMicro());
		assertEquals("qualifier", version.getQualifier());
	}

}
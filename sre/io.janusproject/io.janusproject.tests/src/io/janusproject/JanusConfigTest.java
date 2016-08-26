/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.janusproject;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.junit.Test;

import io.janusproject.testutils.AbstractJanusTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class JanusConfigTest extends AbstractJanusTest {

	private static final String DEFAULT_VALUE = UUID.randomUUID().toString();
	private static final float FLOAT_EPSILON = 0.000001f;

	@Test
	public void testGetSystemPropertyString_fromProperties() throws Exception {
		List<Object> names = new ArrayList<>(System.getProperties().keySet());
		for (Object k : names) {
			String sv = System.getProperty(k.toString());
			String v = JanusConfig.getSystemProperty(k.toString());
			assertEquals(sv, v);
			v = JanusConfig.getSystemProperty(k + UUID.randomUUID().toString());
			assertNull(v);
		}
	}

	@Test
	public void testGetSystemPropertyStringString_fromProperties() throws Exception {
		for (Object k : System.getProperties().keySet()) {
			String sv = System.getProperty(k.toString());
			String v = JanusConfig.getSystemProperty(k.toString(), DEFAULT_VALUE);
			assertEquals(sv, v);
			v = JanusConfig.getSystemProperty(k + UUID.randomUUID().toString(), DEFAULT_VALUE);
			assertSame(DEFAULT_VALUE, v);
		}
	}

	@Test
	public void testGetSystemPropertyString_fromEnv() throws Exception {
		List<String> names = new ArrayList<>(System.getenv().keySet());
		for (String k : names) {
			String sv = System.getenv(k.toString());
			String v = JanusConfig.getSystemProperty(k.toString());
			assertEquals(sv, v);
			v = JanusConfig.getSystemProperty(k + UUID.randomUUID().toString());
			assertNull(v);
		}
	}

	@Test
	public void testGetSystemPropertyStringString_fromEnv() throws Exception {
		List<String> names = new ArrayList<>(System.getenv().keySet());
		for (String k : names) {
			String sv = System.getenv(k.toString());
			String v = JanusConfig.getSystemProperty(k.toString(), DEFAULT_VALUE);
			assertEquals(sv, v);
			v = JanusConfig.getSystemProperty(k + UUID.randomUUID().toString(), DEFAULT_VALUE);
			assertSame(DEFAULT_VALUE, v);
		}
	}

	@Test
	public void testGetSystemPropertyAsInteger() throws Exception {
		System.setProperty("janus.unit.test.enum", Integer.toString(234)); //$NON-NLS-1$
		List<Object> names = new ArrayList<>(System.getProperties().keySet());
		for (Object k : names) {
			String sv = JanusConfig.getSystemProperty(k.toString(), ""); //$NON-NLS-1$
			int v = JanusConfig.getSystemPropertyAsInteger(k.toString(), 567);
			if (k.equals("janus.unit.test.enum")) { //$NON-NLS-1$
				assertEquals(234, v);
			} else {
				int rv;
				try {
					rv = Integer.parseInt(sv);
				} catch (Throwable exception) {
					rv = 567;
				}
				assertEquals(rv, v);
			}
		}
		int v = JanusConfig.getSystemPropertyAsInteger("janus.unit.test.enum2", 789); //$NON-NLS-1$
		assertEquals(789, v);
	}

	@Test
	public void testGetSystemPropertyAsFloat() throws Exception {
		System.setProperty("janus.unit.test.enum", Float.toString(234.567f)); //$NON-NLS-1$
		List<Object> names = new ArrayList<>(System.getProperties().keySet());
		for (Object k : names) {
			String sv = JanusConfig.getSystemProperty(k.toString(), ""); //$NON-NLS-1$
			float v = JanusConfig.getSystemPropertyAsFloat(k.toString(), 567.8901f);
			if (k.equals("janus.unit.test.enum")) { //$NON-NLS-1$
				assertEquals(234.567f, v, FLOAT_EPSILON);
			} else {
				float rv;
				try {
					rv = Float.parseFloat(sv);
				} catch (Throwable exception) {
					rv = 567.8901f;
				}
				assertEquals(rv, v, FLOAT_EPSILON);
			}
		}
		float v = JanusConfig.getSystemPropertyAsFloat("janus.unit.test.enum2", 789.0123f); //$NON-NLS-1$
		assertEquals(789.0123f, v, FLOAT_EPSILON);
	}

	@Test
	public void testGetSystemPropertyAsBoolean() throws Exception {
		System.setProperty("janus.unit.test.enum", Boolean.TRUE.toString()); //$NON-NLS-1$
		List<Object> names = new ArrayList<>(System.getProperties().keySet());
		for (Object k : names) {
			String sv = JanusConfig.getSystemProperty(k.toString(), ""); //$NON-NLS-1$
			boolean v = JanusConfig.getSystemPropertyAsBoolean(k.toString(), true);
			if (k.equals("janus.unit.test.enum")) { //$NON-NLS-1$
				assertEquals(true, v);
			} else {
				assertSame(Boolean.parseBoolean(sv), v);
			}
		}
		boolean v = JanusConfig.getSystemPropertyAsBoolean("janus.unit.test.enum2", true); //$NON-NLS-1$
		assertEquals(true, v);
	}

	@Test
	public void testGetSystemPropertyAsEnum() throws Exception {
		System.setProperty("janus.unit.test.enum", UnitTestEnum.UNIT_TEST_ENUM_DEF.name()); //$NON-NLS-1$
		List<Object> names = new ArrayList<>(System.getProperties().keySet());
		for (Object k : names) {
			UnitTestEnum v = JanusConfig.getSystemPropertyAsEnum(UnitTestEnum.class, k.toString(),
					UnitTestEnum.UNIT_TEST_ENUM_ABC);
			if (k.equals("janus.unit.test.enum")) { //$NON-NLS-1$
				assertSame(UnitTestEnum.UNIT_TEST_ENUM_DEF, v);
			} else {
				assertSame(UnitTestEnum.UNIT_TEST_ENUM_ABC, v);
			}
		}
		UnitTestEnum v = JanusConfig.getSystemPropertyAsEnum(UnitTestEnum.class, "janus.unit.test.enum2", //$NON-NLS-1$
				UnitTestEnum.UNIT_TEST_ENUM_ABC);
		assertSame(UnitTestEnum.UNIT_TEST_ENUM_ABC, v);
	}

	@Test
	public void testGetSystemPropertyAsClassObject() throws Exception {
		System.setProperty("janus.unit.test.enum", JanusConfig.class.getName()); //$NON-NLS-1$
		List<Object> names = new ArrayList<>(System.getProperties().keySet());
		for (Object k : names) {
			Class<?> v = JanusConfig.getSystemPropertyAsClass(k.toString(), Boot.class);
			if (k.equals("janus.unit.test.enum")) { //$NON-NLS-1$
				assertEquals(JanusConfig.class, v);
			} else {
				assertNotNull(v);
			}
		}
		Class<?> v = JanusConfig.getSystemPropertyAsClass("janus.unit.test.enum2", JanusConfig.class); //$NON-NLS-1$
		assertEquals(JanusConfig.class, v);
	}

	@Test
	public void testGetSystemPropertyAsClassS() throws Exception {
		System.setProperty("janus.unit.test.enum", JanusConfig.class.getName()); //$NON-NLS-1$
		List<Object> names = new ArrayList<>(System.getProperties().keySet());
		for (Object k : names) {
			Class<? extends JanusConfig> v = JanusConfig.getSystemPropertyAsClass(JanusConfig.class, k.toString(),
					(Class<JanusConfig>) null);
			if (k.equals("janus.unit.test.enum")) { //$NON-NLS-1$
				assertEquals(JanusConfig.class, v);
			} else {
				assertNull(v);
			}
		}
		Class<?> v = JanusConfig.getSystemPropertyAsClass(JanusConfig.class, "janus.unit.test.enum2", JanusConfig.class); //$NON-NLS-1$
		assertEquals(JanusConfig.class, v);
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static enum UnitTestEnum {
		UNIT_TEST_ENUM_ABC, UNIT_TEST_ENUM_DEF;
	}

}

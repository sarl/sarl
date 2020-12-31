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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.LibraryLocation;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import io.sarl.eclipse.runtime.AbstractSREInstall;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class AbstractSREInstallTest extends AbstractSarlTest {

	@NonNullByDefault
	private String id;

	@NonNullByDefault
	private AbstractSREInstall sre;

	@Before
	public void setUp() {
		this.id = UUID.randomUUID().toString();
		this.sre = new SreMock(this.id);
	}

	@Test
	public void getName() {
		this.sre.setName("my name");
		assertEquals("my name", this.sre.getName());
	}


	@Test
	public void testToString() {
		this.sre.setName("my name");
		assertEquals("my name", this.sre.toString());
	}

	@Test
	public void setMinimalSARLVersion_invalid() {
		String v = UUID.randomUUID().toString();
		this.sre.setMinimalSARLVersion(v);
		assertNull(this.sre.getMinimalSARLVersion());
	}

	@Test
	public void setMinimalSARLVersion_valid() {
		String v = "1.2.3";
		this.sre.setMinimalSARLVersion(v);
		assertSame(v, this.sre.getMinimalSARLVersion());
	}

	@Test
	public void setMaximalSARLVersion_invalid() {
		String v = UUID.randomUUID().toString();
		this.sre.setMaximalSARLVersion(v);
		assertNull(this.sre.getMaximalSARLVersion());
	}

	@Test
	public void setMaximalSARLVersion_valid() {
		String v = "1.2.3";
		this.sre.setMaximalSARLVersion(v);
		assertSame(v, this.sre.getMaximalSARLVersion());
	}

	@Test
	public void getId() {
		assertSame(this.id, this.sre.getId());
	}

	@Test
	public void getNotify() {
		this.sre.setNotify(true);
		assertTrue(this.sre.getNotify());
		this.sre.setNotify(false);
		assertFalse(this.sre.getNotify());
		this.sre.setNotify(true);
		assertTrue(this.sre.getNotify());
	}

	@Test
	public void getMainClass() {
		String m = UUID.randomUUID().toString();
		this.sre.setMainClass(m);
		assertSame(m, this.sre.getMainClass());
	}

	@Test
	public void getLibraryLocations() {
		 List<IRuntimeClasspathEntry> m = new ArrayList<>(4);
		this.sre.setClassPathEntries(m);
		assertSame(m, this.sre.getClassPathEntries());
	}

	@Test
	public void getVMSpecificAttributesMap() {
		Map<String, String> m = Collections.singletonMap(UUID.randomUUID().toString(), UUID.randomUUID().toString());
		this.sre.setVMSpecificAttributesMap(m);
		assertNotSame(m, this.sre.getVMSpecificAttributesMap());
		assertEquals(m, this.sre.getVMSpecificAttributesMap());
	}

	@Test
	public void isStandalone() {
		this.sre.setStandalone(true);
		assertTrue(this.sre.isStandalone());
		this.sre.setStandalone(false);
		assertFalse(this.sre.isStandalone());
		this.sre.setStandalone(true);
		assertTrue(this.sre.isStandalone());
	}

	@Test
	public void testClone() {
		AbstractSREInstall c = this.sre.clone();
		assertNotNull(c);
		assertSame(this.id, c.getId());
		assertEquals(this.sre.getName(), c.getName());
		assertEquals(this.sre.getMainClass(), c.getMainClass());
		assertEquals(this.sre.getMaximalSARLVersion(), c.getMaximalSARLVersion());
		assertEquals(this.sre.getMinimalSARLVersion(), c.getMinimalSARLVersion());
	}

	@Test
	public void copy() {
		String id = UUID.randomUUID().toString();
		ISREInstall c = this.sre.copy(id);
		assertNotNull(c);
		assertSame(id, c.getId());
		assertEquals(this.sre.getName(), c.getName());
		assertEquals(this.sre.getMainClass(), c.getMainClass());
		assertEquals(this.sre.getMaximalSARLVersion(), c.getMaximalSARLVersion());
		assertEquals(this.sre.getMinimalSARLVersion(), c.getMinimalSARLVersion());
	}

	@Test
	public void equals() {
		String id2 = UUID.randomUUID().toString();
		AbstractSREInstall sre2 = new SreMock(id2);
		AbstractSREInstall sre3 = new SreMock(this.id);
		assertTrue(this.sre.equals(this.sre));
		assertFalse(this.sre.equals(sre2));
		assertTrue(this.sre.equals(sre3));
	}

	@Test
	public void testHashCode() {
		String id2 = UUID.randomUUID().toString();
		AbstractSREInstall sre2 = new SreMock(id2);
		AbstractSREInstall sre3 = new SreMock(this.id);
		assertEquals(this.id.hashCode(), this.sre.hashCode());
		assertEquals(id2.hashCode(), sre2.hashCode());
		assertEquals(this.id.hashCode(), sre3.hashCode());
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SreMock extends AbstractSREInstall {
		public SreMock(String id) {
			super(id);
		}
		@Override
		public void setFromXML(Element element) throws IOException {
			throw new UnsupportedOperationException();
		}
		@Override
		public String getJVMArguments() {
			throw new UnsupportedOperationException();
		}
		@Override
		public String getSREArguments() {
			throw new UnsupportedOperationException();
		}
		@Override
		public String getNameNoDefault() {
			throw new UnsupportedOperationException();
		}
		@Override
		public String getLocation() {
			throw new UnsupportedOperationException();
		}
		@Override
		public void getAsXML(Document document, Element element) throws IOException {
			throw new UnsupportedOperationException();
		}
		@Override
		public Map<String, String> getAvailableCommandLineOptions() {
			throw new UnsupportedOperationException();
		}
		@Override
		protected void resolveDirtyFields(boolean forceSettings) {
			//
		}
	}

}

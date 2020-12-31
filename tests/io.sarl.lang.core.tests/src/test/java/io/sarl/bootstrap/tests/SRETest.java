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
package io.sarl.bootstrap.tests;

import static io.sarl.tests.api.tools.TestAssertions.assertContains;
import static io.sarl.tests.api.tools.TestAssertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.Iterator;
import java.util.ServiceLoader;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.bootstrap.SRE;
import io.sarl.bootstrap.SREBootstrap;
import io.sarl.lang.core.Agent;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("SRE")
@Tag("unit")
@Tag("core")
public class SRETest extends AbstractSarlTest {

	@BeforeEach
	public void setUp() {
		SRE.resetServiceLoader();
		SRE.setBootstrap(null);
	}
	
	@AfterEach
	public void tearDown() {
		SRE.resetServiceLoader();
		SRE.setBootstrap(null);
	}

	private void installNoService() {
		SRE.getServiceLoader(true);
	}

	private void installService() {
		SRE.getServiceLoader(false);
	}

	@Test
	public void getServiceLoader_noService() {
		installNoService();
		ServiceLoader<SREBootstrap> bootstraps = SRE.getServiceLoader();
		assertContains(bootstraps);
	}

	@Test
	public void getServiceLoader_mockedService() {
		installService();
		ServiceLoader<SREBootstrap> bootstraps = SRE.getServiceLoader();
		Iterator<SREBootstrap> iterator = bootstraps.iterator();
		SREBootstrap bs = iterator.next();
		assertNotNull(bs);
		assertInstanceOf(SREBootstrapMock.class, bs);
		assertFalse(iterator.hasNext());
	}

	@Test
	public void getBootstrap_noService() throws Exception {
		installNoService();
		SREBootstrap bs = SRE.getBootstrap();
		assertNotNull(bs);
		assertFalse(bs.isActive());
		try {
			bs.startAgent(MyAgent.class);
			fail("Expecting exception");
		} catch (IllegalStateException | UnsupportedOperationException ex) {
		}
		try {
			bs.startAgent(1, MyAgent.class);
			fail("Expecting exception");
		} catch (IllegalStateException | UnsupportedOperationException ex) {
		}
	}

	@Test
	public void getBootstrap_mockedService() {
		installService();
		SREBootstrap bs = SRE.getBootstrap();
		assertNotNull(bs);
		assertTrue(bs.isActive());
		assertInstanceOf(SREBootstrapMock.class, bs);
	}

	private static class MyAgent extends Agent {

		public MyAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}
		
	}

}

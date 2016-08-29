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
package io.janusproject.modules.hazelcast;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.hazelcast.logging.ILogger;
import io.janusproject.services.logging.LogService;
import io.janusproject.testutils.AbstractJanusTest;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class HazelcastKernelLoggerFactoryTest extends AbstractJanusTest {

	@Nullable
	private HazelcastKernelLoggerFactory factory;

	@Before
	public void setUp() {
		this.factory = new HazelcastKernelLoggerFactory();
	}

	@Test
	public void getLogService() {
		assertNull(HazelcastKernelLoggerFactory.getLogService());
	}

	@Test
	public void setLogService() {
		LogService serv = Mockito.mock(LogService.class);
		HazelcastKernelLoggerFactory.setLogService(serv);
		assertSame(serv, HazelcastKernelLoggerFactory.getLogService());
	}

	@Test
	public void createLogger() {
		LogService serv = Mockito.mock(LogService.class);

		ILogger log = this.factory.createLogger(UUID.randomUUID().toString());
		assertNotNull(log);

		log.severe("Test"); //$NON-NLS-1$
		Mockito.verifyZeroInteractions(serv);

		HazelcastKernelLoggerFactory.setLogService(serv);
		log.severe("Test"); //$NON-NLS-1$

		ArgumentCaptor<LogRecord> argument = ArgumentCaptor.forClass(LogRecord.class);
		Mockito.verify(serv).log(argument.capture());
		assertEquals(Level.SEVERE, argument.getValue().getLevel());
		assertEquals("Test", argument.getValue().getMessage()); //$NON-NLS-1$
	}

}

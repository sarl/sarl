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
package io.janusproject.tests.modules.hazelcast;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import static org.mockito.Mockito.*;

import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.hazelcast.logging.ILogger;
import io.janusproject.modules.hazelcast.HazelcastKernelLoggerFactory;
import io.janusproject.services.logging.LogService;
import io.janusproject.tests.testutils.AbstractJanusTest;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import io.sarl.tests.api.Nullable;

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
		LogService serv = mock(LogService.class);
		when(serv.getPlatformLogger()).thenReturn(mock(Logger.class));
		HazelcastKernelLoggerFactory.setLogService(serv);
		assertSame(serv, HazelcastKernelLoggerFactory.getLogService());
	}

	@Test
	public void createLogger() throws Exception {
		LogService serv = mock(LogService.class);
		Logger logger = mock(Logger.class);
		when(serv.getKernelLogger()).thenReturn(logger);

		ILogger log = (ILogger) this.reflect.invoke(this.factory, "createLogger", UUID.randomUUID().toString());
		assertNotNull(log);

		log.severe("Test"); //$NON-NLS-1$
		Mockito.verifyZeroInteractions(serv);

		HazelcastKernelLoggerFactory.setLogService(serv);
		log.severe("Test"); //$NON-NLS-1$

		ArgumentCaptor<LogRecord> argument = ArgumentCaptor.forClass(LogRecord.class);
		Mockito.verify(logger).log(argument.capture());
		assertEquals(Level.SEVERE, argument.getValue().getLevel());
		assertEquals("Test", argument.getValue().getMessage()); //$NON-NLS-1$
	}

}

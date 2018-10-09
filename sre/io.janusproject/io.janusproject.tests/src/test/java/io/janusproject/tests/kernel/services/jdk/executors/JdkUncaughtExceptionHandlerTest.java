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
package io.janusproject.tests.kernel.services.jdk.executors;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.*;

import java.util.UUID;
import java.util.concurrent.CancellationException;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

import io.janusproject.kernel.services.jdk.executors.JdkUncaughtExceptionHandler;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.logging.LogService;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class JdkUncaughtExceptionHandlerTest extends AbstractJanusTest {

	@Nullable
	private String loggerName;

	@Nullable
	private LogService logService;

	@Nullable
	private Logger logger;

	@Nullable
	private JdkUncaughtExceptionHandler handler;

	@Before
	public void setUp() {
		this.loggerName = UUID.randomUUID().toString();
		this.logService = mock(LogService.class);
		this.logger = mock(Logger.class);

		when(this.logger.isLoggable(ArgumentMatchers.any(Level.class))).thenReturn(true);
		when(this.logger.getName()).thenReturn(this.loggerName);

		when(this.logService.getKernelLogger()).thenReturn(logger);
		when(this.logService.prepareLogRecord(any(LogRecord.class), anyString(), any(Throwable.class)))
			.thenAnswer(it -> {
				final LogRecord value = it.getArgument(0);
				assert value != null;
				final String name = it.getArgument(1);
				assert name != null;
				value.setLoggerName(name);
				final Throwable ex = it.getArgument(2);
				assert ex != null;
				value.setThrown(ex);
				final StackTraceElement[] trace = ex.getStackTrace();
				if (trace != null && trace.length > 0) {
					final StackTraceElement elt = trace[0];
					assert elt != null;
					value.setSourceClassName(elt.getClassName());
					value.setSourceMethodName(elt.getMethodName());
				}
				return value;
			});

		this.handler = new JdkUncaughtExceptionHandler();
		this.handler.setLogService(this.logService);
	}

	@Test
	public void uncaughtException_Exception() {
		Exception e = new Exception();
		this.handler.uncaughtException(Thread.currentThread(), e);

		ArgumentCaptor<LogRecord> argument = ArgumentCaptor.forClass(LogRecord.class);
		verify(this.logger, times(1)).log(argument.capture());
		assertSame(Level.SEVERE, argument.getValue().getLevel());
		assertSame(e, argument.getValue().getThrown());
		assertEquals(JdkUncaughtExceptionHandlerTest.class.getName(), argument.getValue().getSourceClassName());
		assertEquals("uncaughtException_Exception", argument.getValue().getSourceMethodName()); //$NON-NLS-1$
	}

	@Test
	public void uncaughtException_EarlyExit() {
		try {
			ExecutorService.neverReturn();
			fail("Early exit exception is expected");
		} catch (Exception e) {
			this.handler.uncaughtException(Thread.currentThread(), e);
			verifyZeroInteractions(this.logger);
		}
	}

	@Test
	public void uncaughtException_Cancellation() {
		Exception e = new CancellationException();
		this.handler.uncaughtException(Thread.currentThread(), e);

		ArgumentCaptor<LogRecord> argument = ArgumentCaptor.forClass(LogRecord.class);
		verify(this.logger, times(1)).log(argument.capture());
		assertSame(Level.FINEST, argument.getValue().getLevel());
		assertSame(e, argument.getValue().getThrown());
		assertEquals(JdkUncaughtExceptionHandlerTest.class.getName(), argument.getValue().getSourceClassName());
		assertEquals("uncaughtException_Cancellation", argument.getValue().getSourceMethodName()); //$NON-NLS-1$
	}

	@Test
	public void uncaughtException_Interrupt() {
		Exception e = new InterruptedException();
		this.handler.uncaughtException(Thread.currentThread(), e);

		ArgumentCaptor<LogRecord> argument = ArgumentCaptor.forClass(LogRecord.class);
		verify(this.logger, times(1)).log(argument.capture());
		assertSame(Level.FINEST, argument.getValue().getLevel());
		assertSame(e, argument.getValue().getThrown());
		assertEquals(JdkUncaughtExceptionHandlerTest.class.getName(), argument.getValue().getSourceClassName());
		assertEquals("uncaughtException_Interrupt", argument.getValue().getSourceMethodName()); //$NON-NLS-1$
	}

}

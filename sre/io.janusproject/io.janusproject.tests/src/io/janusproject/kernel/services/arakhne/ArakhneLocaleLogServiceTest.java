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
package io.janusproject.kernel.services.arakhne;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.mockito.Matchers;
import org.mockito.Mock;

import io.janusproject.kernel.services.arakhne.ArakhneLocaleLogService.LoggerCaller;
import io.janusproject.kernel.services.arakhne.ArakhneLocaleLogService.StackTraceLoggerCallerProvider;
import io.janusproject.services.logging.LogService;
import io.janusproject.testutils.AbstractDependentServiceTest;
import io.janusproject.testutils.AvoidServiceStartForTest;
import io.janusproject.testutils.StartServiceForTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({ ArakhneLocaleLogServiceTest.NoFilter.class, ArakhneLocaleLogServiceTest.Filter.class, })
@SuppressWarnings("all")
public class ArakhneLocaleLogServiceTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@StartServiceForTest
	public static class NoFilter extends AbstractDependentServiceTest<ArakhneLocaleLogService> {

		@Mock
		private Logger logger;

		public NoFilter() {
			super(LogService.class);
		}

		@Before
		public void setUp() {
			when(this.logger.isLoggable(Matchers.any(Level.class))).thenReturn(true);
		}

		@Override
		public ArakhneLocaleLogService newService() {
			ArakhneLocaleLogService service = new ArakhneLocaleLogService();
			service.setLoggerCaller(new TestLoggerCallerProvider());
			return service;
		}

		@Override
		public void getServiceDependencies() {
			assertContains(this.service.getServiceDependencies());
		}

		@Override
		public void getServiceWeakDependencies() {
			assertContains(this.service.getServiceWeakDependencies());
		}

		@AvoidServiceStartForTest
		@Test
		public void getLogger() {
			assertSame(this.logger, this.service.getLogger());
		}

		@Test
		public void getFilter() {
			assertNull(this.service.getFilter());
		}

		@Test
		public void logLogRecord() {
			LogRecord record = mock(LogRecord.class);
			this.service.log(record);
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			assertSame(record, arg.getValue());
		}

		@Test
		public void logLevelStringObjectArray_noProperty_noException() {
			this.service.log(Level.CONFIG, "UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.CONFIG, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("logLevelStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void logLevelStringObjectArray_property_noException() {
			this.service.log(Level.CONFIG, "TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.CONFIG, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("logLevelStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void logLevelStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.log(Level.CONFIG, "UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.CONFIG, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("logLevelStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void logLevelStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.log(Level.CONFIG, "TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.CONFIG, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("logLevelStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void logLevelClassStringObjectArray_noProperty_noException() {
			this.service.log(Level.CONFIG, ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.CONFIG, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("logLevelClassStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void logLevelClassStringObjectArray_property_noException() {
			this.service.log(Level.CONFIG, ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.CONFIG, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("logLevelClassStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void logLevelClassStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.log(Level.CONFIG, ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.CONFIG, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("logLevelClassStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void logLevelClassStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.log(Level.CONFIG, ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.CONFIG, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("logLevelClassStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void infoStringObjectArray_noProperty_noException() {
			this.service.info("UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.INFO, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("infoStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void infoStringObjectArray_property_noException() {
			this.service.info("TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.INFO, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("infoStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void infoStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.info("UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.INFO, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("infoStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void infoStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.info("TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.INFO, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("infoStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void infoClassStringObjectArray_noProperty_noException() {
			this.service.info(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.INFO, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("infoClassStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void infoClassStringObjectArray_property_noException() {
			this.service.info(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.INFO, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("infoClassStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void infoClassStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.info(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.INFO, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("infoClassStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void infoClassStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.info(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.INFO, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("infoClassStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void warningStringObjectArray_noProperty_noException() {
			this.service.warning("UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.WARNING, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("warningStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void warningStringObjectArray_property_noException() {
			this.service.warning("TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.WARNING, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("warningStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void warningStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.warning("UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.WARNING, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("warningStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void warningStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.warning("TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.WARNING, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("warningStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void warningClassStringObjectArray_noProperty_noException() {
			this.service.warning(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.WARNING, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("warningClassStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void warningClassStringObjectArray_property_noException() {
			this.service.warning(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.WARNING, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("warningClassStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void warningClassStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.warning(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.WARNING, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("warningClassStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void warningClassStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.warning(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.WARNING, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("warningClassStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void errorStringObjectArray_noProperty_noException() {
			this.service.error("UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.SEVERE, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("errorStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void errorStringObjectArray_property_noException() {
			this.service.error("TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.SEVERE, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("errorStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void errorStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.error("UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.SEVERE, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("errorStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void errorStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.error("TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.SEVERE, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("errorStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void errorClassStringObjectArray_noProperty_noException() {
			this.service.error(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.SEVERE, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("errorClassStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void errorClassStringObjectArray_property_noException() {
			this.service.error(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.SEVERE, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("errorClassStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void errorClassStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.error(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.SEVERE, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("errorClassStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void errorClassStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.error(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.SEVERE, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("errorClassStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void fineInfoStringObjectArray_noProperty_noException() {
			this.service.fineInfo("UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINE, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("fineInfoStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void fineInfoStringObjectArray_property_noException() {
			this.service.fineInfo("TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINE, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("fineInfoStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void fineInfoStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.fineInfo("UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINE, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("fineInfoStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void fineInfoStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.fineInfo("TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINE, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("fineInfoStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void fineInfoClassStringObjectArray_noProperty_noException() {
			this.service.fineInfo(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINE, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("fineInfoClassStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void fineInfoClassStringObjectArray_property_noException() {
			this.service.fineInfo(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINE, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("fineInfoClassStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void fineInfoClassStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.fineInfo(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINE, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("fineInfoClassStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void fineInfoClassStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.fineInfo(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINE, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("fineInfoClassStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void finerInfoStringObjectArray_noProperty_noException() {
			this.service.finerInfo("UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINER, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("finerInfoStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void finerInfoStringObjectArray_property_noException() {
			this.service.finerInfo("TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINER, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("finerInfoStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void finerInfoStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.finerInfo("UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINER, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("finerInfoStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void finerInfoStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.finerInfo("TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINER, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("finerInfoStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void finerInfoClassStringObjectArray_noProperty_noException() {
			this.service.finerInfo(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINER, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("finerInfoClassStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void finerInfoClassStringObjectArray_property_noException() {
			this.service.finerInfo(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINER, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("finerInfoClassStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void finerInfoClassStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.finerInfo(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINER, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("finerInfoClassStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void finerInfoClassStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.finerInfo(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINER, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("finerInfoClassStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void debugStringObjectArray_noProperty_noException() {
			this.service.debug("UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINEST, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("debugStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void debugStringObjectArray_property_noException() {
			this.service.debug("TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINEST, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("debugStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void debugStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.debug("UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINEST, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("debugStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void debugStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.debug("TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINEST, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("debugStringObjectArray_property_exception", record.getSourceMethodName());
		}

		@Test
		public void debugClassStringObjectArray_noProperty_noException() {
			this.service.debug(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINEST, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("debugClassStringObjectArray_noProperty_noException", record.getSourceMethodName());
		}

		@Test
		public void debugClassStringObjectArray_property_noException() {
			this.service.debug(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", "abc", "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINEST, record.getLevel());
			assertEquals("testing message | abc | de", record.getMessage());
			assertNull(record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("debugClassStringObjectArray_property_noException", record.getSourceMethodName());
		}

		@Test
		public void debugClassStringObjectArray_noProperty_exception() {
			Exception ex = new Exception("my ex");
			this.service.debug(ArakhneLocaleLogServiceTest.class, "UNKNOWN_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINEST, record.getLevel());
			assertEquals("UNKNOWN_MESSAGE", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("debugClassStringObjectArray_noProperty_exception", record.getSourceMethodName());
		}

		@Test
		public void debugClassStringObjectArray_property_exception() {
			Exception ex = new Exception("my ex");
			this.service.debug(ArakhneLocaleLogServiceTest.class, "TESTING_MESSAGE", ex, "de");
			ArgumentCaptor<LogRecord> arg = ArgumentCaptor.forClass(LogRecord.class);
			verify(this.logger, times(1)).log(arg.capture());
			LogRecord record = arg.getValue();
			assertEquals(Level.FINEST, record.getLevel());
			assertEquals("testing message | java.lang.Exception: my ex | de", record.getMessage());
			assertSame(ex, record.getThrown());
			assertEquals(getClass().getName(), record.getSourceClassName());
			assertEquals("debugClassStringObjectArray_property_exception", record.getSourceMethodName());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@StartServiceForTest
	public static class Filter extends AbstractDependentServiceTest<ArakhneLocaleLogService> {

		@Mock
		private Logger logger;

		public Filter() {
			super(LogService.class);
		}

		@Before
		public void setUp() {
			when(this.logger.isLoggable(Matchers.any(Level.class))).thenReturn(true);
		}

		@Override
		public ArakhneLocaleLogService newService() {
			ArakhneLocaleLogService service = new ArakhneLocaleLogService();
			service.setLoggerCaller(new TestLoggerCallerProvider());
			return service;
		}

		@Override
		public void getServiceDependencies() {
			assertContains(this.service.getServiceDependencies());
		}

		@Override
		public void getServiceWeakDependencies() {
			assertContains(this.service.getServiceWeakDependencies());
		}

		@Test
		public void setFilter() {
			java.util.logging.Filter filter = mock(java.util.logging.Filter.class);
			this.service.setFilter(filter);
			ArgumentCaptor<java.util.logging.Filter> arg = ArgumentCaptor.forClass(java.util.logging.Filter.class);
			verify(this.logger, times(1)).setFilter(arg.capture());
			assertSame(filter, arg.getValue());
		}

		@Test
		public void getFilter_noSet() {
			java.util.logging.Filter filter = this.service.getFilter();
			assertNull(filter);
		}

		@Test
		public void getFilter_set() {
			java.util.logging.Filter filter = mock(java.util.logging.Filter.class);
			when(this.logger.getFilter()).thenReturn(filter);
			java.util.logging.Filter filter2 = this.service.getFilter();
			assertSame(filter, filter2);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class TestLoggerCallerProvider extends StackTraceLoggerCallerProvider {

		public TestLoggerCallerProvider() {
			//
		}

		@Override
		public LoggerCaller getLoggerCaller() {
			LoggerCaller old = super.getLoggerCaller();
			return new LoggerCaller(ArakhneLocaleLogServiceTest.class, old.getTypeName(), old.getMethod());
		}

	}

}

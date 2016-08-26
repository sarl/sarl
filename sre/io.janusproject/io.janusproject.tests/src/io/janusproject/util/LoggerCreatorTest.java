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
package io.janusproject.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;

import io.janusproject.JanusConfig;
import io.janusproject.testutils.AbstractJanusTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({ "javadoc", "static-method" })
public class LoggerCreatorTest extends AbstractJanusTest {

	@Test
	public void getLevelStrings() {
		String[] t = LoggerCreator.getLevelStrings();
		assertNotNull(t);
		assertEquals(8, t.length);
		assertEquals("none", t[0]); //$NON-NLS-1$
		assertEquals("error", t[1]); //$NON-NLS-1$
		assertEquals("warning", t[2]); //$NON-NLS-1$
		assertEquals("info", t[3]); //$NON-NLS-1$
		assertEquals("fine", t[4]); //$NON-NLS-1$
		assertEquals("finer", t[5]); //$NON-NLS-1$
		assertEquals("finest", t[6]); //$NON-NLS-1$
		assertEquals("all", t[7]); //$NON-NLS-1$
	}

	@Test
	public void parseLoggingLevel() {
		assertSame(Level.ALL, LoggerCreator.parseLoggingLevel("all")); //$NON-NLS-1$
		assertSame(Level.ALL, LoggerCreator.parseLoggingLevel("ALL")); //$NON-NLS-1$
		assertSame(Level.ALL, LoggerCreator.parseLoggingLevel("7")); //$NON-NLS-1$
		//
		assertSame(Level.FINEST, LoggerCreator.parseLoggingLevel("debug")); //$NON-NLS-1$
		assertSame(Level.FINEST, LoggerCreator.parseLoggingLevel("DEBUG")); //$NON-NLS-1$
		assertSame(Level.FINEST, LoggerCreator.parseLoggingLevel("finest")); //$NON-NLS-1$
		assertSame(Level.FINEST, LoggerCreator.parseLoggingLevel("FINEST")); //$NON-NLS-1$
		assertSame(Level.FINEST, LoggerCreator.parseLoggingLevel("6")); //$NON-NLS-1$
		//
		assertSame(Level.FINER, LoggerCreator.parseLoggingLevel("finer")); //$NON-NLS-1$
		assertSame(Level.FINER, LoggerCreator.parseLoggingLevel("FINER")); //$NON-NLS-1$
		assertSame(Level.FINER, LoggerCreator.parseLoggingLevel("5")); //$NON-NLS-1$
		//
		assertSame(Level.FINE, LoggerCreator.parseLoggingLevel("fine")); //$NON-NLS-1$
		assertSame(Level.FINE, LoggerCreator.parseLoggingLevel("FINE")); //$NON-NLS-1$
		assertSame(Level.FINE, LoggerCreator.parseLoggingLevel("config")); //$NON-NLS-1$
		assertSame(Level.FINE, LoggerCreator.parseLoggingLevel("config")); //$NON-NLS-1$
		assertSame(Level.FINE, LoggerCreator.parseLoggingLevel("4")); //$NON-NLS-1$
		//
		assertSame(Level.INFO, LoggerCreator.parseLoggingLevel("info")); //$NON-NLS-1$
		assertSame(Level.INFO, LoggerCreator.parseLoggingLevel("INFO")); //$NON-NLS-1$
		assertSame(Level.INFO, LoggerCreator.parseLoggingLevel("true")); //$NON-NLS-1$
		assertSame(Level.INFO, LoggerCreator.parseLoggingLevel("TRUE")); //$NON-NLS-1$
		assertSame(Level.INFO, LoggerCreator.parseLoggingLevel("3")); //$NON-NLS-1$
		//
		assertSame(Level.WARNING, LoggerCreator.parseLoggingLevel("warning")); //$NON-NLS-1$
		assertSame(Level.WARNING, LoggerCreator.parseLoggingLevel("WARNING")); //$NON-NLS-1$
		assertSame(Level.WARNING, LoggerCreator.parseLoggingLevel("warn")); //$NON-NLS-1$
		assertSame(Level.WARNING, LoggerCreator.parseLoggingLevel("WARN")); //$NON-NLS-1$
		assertSame(Level.WARNING, LoggerCreator.parseLoggingLevel("2")); //$NON-NLS-1$
		//
		assertSame(Level.SEVERE, LoggerCreator.parseLoggingLevel("severe")); //$NON-NLS-1$
		assertSame(Level.SEVERE, LoggerCreator.parseLoggingLevel("SEVERE")); //$NON-NLS-1$
		assertSame(Level.SEVERE, LoggerCreator.parseLoggingLevel("error")); //$NON-NLS-1$
		assertSame(Level.SEVERE, LoggerCreator.parseLoggingLevel("ERROR")); //$NON-NLS-1$
		assertSame(Level.SEVERE, LoggerCreator.parseLoggingLevel("1")); //$NON-NLS-1$
		//
		assertSame(Level.OFF, LoggerCreator.parseLoggingLevel("none")); //$NON-NLS-1$
		assertSame(Level.OFF, LoggerCreator.parseLoggingLevel("NONE")); //$NON-NLS-1$
		assertSame(Level.OFF, LoggerCreator.parseLoggingLevel("false")); //$NON-NLS-1$
		assertSame(Level.OFF, LoggerCreator.parseLoggingLevel("FALSE")); //$NON-NLS-1$
		assertSame(Level.OFF, LoggerCreator.parseLoggingLevel("0")); //$NON-NLS-1$
		//
		assertSame(Level.INFO, LoggerCreator.parseLoggingLevel(UUID.randomUUID().toString()));
		assertSame(Level.INFO, LoggerCreator.parseLoggingLevel(null));
	}

	@Test
	public void fromInt() {
		assertSame(Level.OFF, LoggerCreator.fromInt(-32));
		assertSame(Level.OFF, LoggerCreator.fromInt(-1));
		assertSame(Level.OFF, LoggerCreator.fromInt(0));
		assertSame(Level.SEVERE, LoggerCreator.fromInt(1));
		assertSame(Level.WARNING, LoggerCreator.fromInt(2));
		assertSame(Level.INFO, LoggerCreator.fromInt(3));
		assertSame(Level.FINE, LoggerCreator.fromInt(4));
		assertSame(Level.FINER, LoggerCreator.fromInt(5));
		assertSame(Level.FINEST, LoggerCreator.fromInt(6));
		assertSame(Level.ALL, LoggerCreator.fromInt(7));
		assertSame(Level.ALL, LoggerCreator.fromInt(8));
		assertSame(Level.ALL, LoggerCreator.fromInt(35));
	}

	@Test
	public void toIntLevel() {
		assertEquals(0, LoggerCreator.toInt(Level.OFF));
		assertEquals(1, LoggerCreator.toInt(Level.SEVERE));
		assertEquals(2, LoggerCreator.toInt(Level.WARNING));
		assertEquals(3, LoggerCreator.toInt(Level.INFO));
		assertEquals(3, LoggerCreator.toInt((Level) null));
		assertEquals(4, LoggerCreator.toInt(Level.FINE));
		assertEquals(4, LoggerCreator.toInt(Level.CONFIG));
		assertEquals(5, LoggerCreator.toInt(Level.FINER));
		assertEquals(6, LoggerCreator.toInt(Level.FINEST));
		assertEquals(7, LoggerCreator.toInt(Level.ALL));
	}

	@Test
	public void getLoggingLevelFromProperties() {
		String propertyValue = JanusConfig.getSystemProperty(JanusConfig.VERBOSE_LEVEL_NAME, JanusConfig.VERBOSE_LEVEL_VALUE);
		Level expectedLevel = LoggerCreator.parseLoggingLevel(propertyValue);
		Level level = LoggerCreator.getLoggingLevelFromProperties();
		assertEquals(expectedLevel, level);
	}

	@Test
	public void createLogger() {
		String name = UUID.randomUUID().toString();
		Level expectedLevel = LoggerCreator.getLoggingLevelFromProperties();
		Logger logger = LoggerCreator.createLogger(name);
		assertEquals(expectedLevel, logger.getLevel());
	}

	@Test
	public void createLoggerWithParent() {
		Logger parent = LoggerCreator.createLogger("parent"); //$NON-NLS-1$
		String name = UUID.randomUUID().toString();
		Level expectedLevel = LoggerCreator.getLoggingLevelFromProperties();
		Logger logger = LoggerCreator.createLogger(name, parent);
		assertEquals(expectedLevel, logger.getLevel());
	}

}

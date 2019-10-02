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

package io.janusproject.tests;

import static org.junit.Assert.*;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.Assume.assumeNotNull;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.only;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;

import com.google.inject.Binder;
import com.google.inject.Module;
import com.google.inject.Provides;
import io.janusproject.Boot;
import io.janusproject.Boot.Exiter;
import io.janusproject.JanusConfig;
import io.janusproject.JanusVersion;
import io.janusproject.kernel.Kernel;
import io.janusproject.tests.testutils.AbstractJanusRunTest;
import io.janusproject.tests.testutils.AbstractJanusTest;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Options;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.Resources;
import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;

import io.sarl.core.DefaultContextInteractions;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({ BootTest.SetterTests.class, BootTest.CommandLineParserTests.class, BootTest.CommandLineExecutionTests.class,
		BootTest.StartTests.class, BootTest.BootAgentIdentifierTests.class, BootTest.RunConfigurationTests.class, })
@SuppressWarnings("all")
public class BootTest {

	static final UUID ID = UUID.fromString("63ee52ee-4739-47b1-9e73-0a7986d17bc5");

	protected static String[] args(String... strings) {
		return strings;
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	protected static class AgentMock extends Agent {
		/**
		 */
		public AgentMock() {
			super(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null);
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected static class TestModule implements Module {

		public TestModule() {
			//
		}

		@Override
		public void configure(Binder binder) {
			//
		}

		@Provides
		public Kernel createKernel() {
			Kernel k = mock(Kernel.class);
			when(k.spawn(any(Class.class), anyVararg())).thenReturn(ID);
			when(k.getLogger()).thenReturn(mock(Logger.class));
			return k;
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class SetterTests extends AbstractJanusTest {

		@Test
		public void setOffline_true() {
			Boot.setOffline(true);
			assertTrueProperty(JanusConfig.OFFLINE);
		}

		@Test
		public void setOffline_false() {
			Boot.setOffline(false);
			assertFalseProperty(JanusConfig.OFFLINE);
		}

		@Test
		public void setRandomContextUUID() {
			Boot.setRandomContextUUID();
			assertFalseProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertTrueProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
		}

		@Test
		public void setBootAgentTypeContextUUID() {
			Boot.setBootAgentTypeContextUUID();
			assertTrueProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertFalseProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
		}

		public void setDefaultContextUUID() {
			Boot.setDefaultContextUUID();
			assertFalseProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertFalseProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
		}

		@Test
		public void setVerboseLevel() {
			for (int i = -10; i < 10; ++i) {
				Boot.setVerboseLevel(i);
				assertEquals(Integer.toString(i), System.getProperty(JanusConfig.VERBOSE_LEVEL_NAME));
			}
		}

		@Test
		public void setProperty_withValue() {
			String name = UUID.randomUUID().toString();
			String value = UUID.randomUUID().toString();
			Boot.setProperty(name, value);
			assertEquals(value, System.getProperty(name));
		}

		@Test
		public void setPropertiesFromURL() throws IOException {
			URL resource = Resources.getResource(getClass(), "Test1.properties");
			Assume.assumeNotNull(resource);
			Boot.setPropertiesFrom(resource);
			assertEquals("my value 0", System.getProperty("io.janusproject.tests.MY_PROPERTY_0"));
			assertEquals("my value 1", System.getProperty("io.janusproject.tests.MY_PROPERTY_1"));
			assertEquals("my value 2", System.getProperty("io.janusproject.tests.MY_PROPERTY_2"));
		}

		@Test
		public void setPropertiesFromFile() throws IOException {
			URL resource = Resources.getResource(getClass(), "Test2.properties");
			Assume.assumeNotNull(resource);
			File file = FileSystem.convertURLToFile(resource);
			Assume.assumeNotNull(file);
			Boot.setPropertiesFrom(file);
			assertEquals("my value 3", System.getProperty("io.janusproject.tests.MY_PROPERTY_3"));
			assertEquals("my value 4", System.getProperty("io.janusproject.tests.MY_PROPERTY_4"));
		}

		@Test
		public void getStandardConsoleLogger_default() {
			assertSame(System.out, Boot.getStandardConsoleLogger());
		}

		@Test
		public void getErrorConsoleLogger_default() {
			assertSame(System.err, Boot.getErrorConsoleLogger());
		}

		@Test
		public void setStandardConsoleLogger_null() {
			Boot.setStandardConsoleLogger(null);
			assertSame(System.out, Boot.getStandardConsoleLogger());
		}

		@Test
		public void setErrorConsoleLogger_null() {
			Boot.setErrorConsoleLogger(null);
			assertSame(System.err, Boot.getErrorConsoleLogger());
		}

		@Test
		public void setStandardConsoleLogger_notNull() {
			PrintStream os = mock(PrintStream.class);
			Boot.setStandardConsoleLogger(os);
			assertSame(os, Boot.getStandardConsoleLogger());
		}

		@Test
		public void setErrorConsoleLogger_notNull() {
			PrintStream os = mock(PrintStream.class);
			Boot.setErrorConsoleLogger(os);
			assertSame(os, Boot.getErrorConsoleLogger());
		}

		@Test
		public void setStandardConsoleLogger_notNull_null() {
			PrintStream os = mock(PrintStream.class);
			Boot.setStandardConsoleLogger(os);
			Boot.setStandardConsoleLogger(null);
			assertSame(System.out, Boot.getStandardConsoleLogger());
		}

		@Test
		public void setErrorConsoleLogger_notNull_null() {
			PrintStream os = mock(PrintStream.class);
			Boot.setErrorConsoleLogger(os);
			Boot.setErrorConsoleLogger(null);
			assertSame(System.err, Boot.getErrorConsoleLogger());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class CommandLineParserTests extends AbstractJanusTest {

		@Nullable
		private Options janusOptions;

		@Nullable
		private CommandLineParser parser;

		/**
		 * @throws java.lang.Exception
		 */
		@Before
		public void setUp() throws Exception {
			this.janusOptions = Boot.getOptions();
			this.parser = new DefaultParser();
		}

		@Test
		public void testHelp() throws Exception {
			CommandLine cmd = this.parser.parse(this.janusOptions, args(""));
			assertFalse(cmd.hasOption('h'));
			cmd = this.parser.parse(this.janusOptions, args("-h"));
			assertTrue(cmd.hasOption('h'));
			cmd = this.parser.parse(this.janusOptions, args("-help"));
			assertTrue(cmd.hasOption('h'));
			cmd = this.parser.parse(this.janusOptions, args("--help"));
			assertTrue(cmd.hasOption('h'));

			cmd = this.parser.parse(this.janusOptions, args("-f", "thefile"));
			assertTrue(cmd.hasOption('f'));
			assertEquals("thefile", cmd.getOptionValue('f'));
			cmd = this.parser.parse(this.janusOptions, args("-file", "thefile"));
			assertTrue(cmd.hasOption('f'));
			assertEquals("thefile", cmd.getOptionValue('f'));
			cmd = this.parser.parse(this.janusOptions, args("--file", "thefile"));
			assertTrue(cmd.hasOption('f'));
			assertEquals("thefile", cmd.getOptionValue('f'));

			cmd = this.parser.parse(this.janusOptions, args("-B", "uid"));
			assertTrue(cmd.hasOption('B'));
			assertEquals("uid", cmd.getArgs()[0]);
			cmd = this.parser.parse(this.janusOptions, args("-bootid", "uid"));
			assertTrue(cmd.hasOption('B'));
			assertEquals("uid", cmd.getArgs()[0]);
			cmd = this.parser.parse(this.janusOptions, args("--bootid", "uid"));
			assertTrue(cmd.hasOption('B'));
			assertEquals("uid", cmd.getArgs()[0]);

			cmd = this.parser.parse(this.janusOptions, args("-R", "uid"));
			assertTrue(cmd.hasOption('R'));
			assertEquals("uid", cmd.getArgs()[0]);
			cmd = this.parser.parse(this.janusOptions, args("-randomid", "uid"));
			assertTrue(cmd.hasOption('R'));
			assertEquals("uid", cmd.getArgs()[0]);
			cmd = this.parser.parse(this.janusOptions, args("--randomid", "uid"));
			assertTrue(cmd.hasOption('R'));
			assertEquals("uid", cmd.getArgs()[0]);

			cmd = this.parser.parse(this.janusOptions, args("-W", "uid"));
			assertTrue(cmd.hasOption('W'));
			assertEquals("uid", cmd.getArgs()[0]);
			cmd = this.parser.parse(this.janusOptions, args("-worldid", "uid"));
			assertTrue(cmd.hasOption('W'));
			assertEquals("uid", cmd.getArgs()[0]);
			cmd = this.parser.parse(this.janusOptions, args("--worldid", "uid"));
			assertTrue(cmd.hasOption('W'));
			assertEquals("uid", cmd.getArgs()[0]);

			cmd = this.parser.parse(this.janusOptions, args("-D", "name=value"));
			assertTrue(cmd.hasOption('D'));
			assertArrayEquals(new String[] { "name", "value" }, cmd.getOptionValues('D'));
			cmd = this.parser.parse(this.janusOptions, args("-Dname=value"));
			assertTrue(cmd.hasOption('D'));
			assertArrayEquals(new String[] { "name", "value" }, cmd.getOptionValues('D'));
			cmd = this.parser.parse(this.janusOptions, args("-D", "name", "value"));
			assertTrue(cmd.hasOption('D'));
			assertArrayEquals(new String[] { "name", "value" }, cmd.getOptionValues('D'));
			try {
				this.parser.parse(this.janusOptions, args("-D", "name"));
				fail("Expecting failure");
			} catch (Throwable exception) {
				//
			}
			try {
				this.parser.parse(this.janusOptions, args("-D"));
				fail("Expecting failure");
			} catch (Throwable exception) {
				//
			}
		}

		@Test
		public void testAgentArg() throws Exception {
			CommandLine cmd = this.parser.parse(this.janusOptions, null);

			cmd = this.parser.parse(this.janusOptions, args("-h", "main.Agent"));
			assertEquals(1, cmd.getArgs().length);
			assertEquals("main.Agent", cmd.getArgs()[0]);

			cmd = this.parser.parse(this.janusOptions, args("-h", "main.Agent", "12"));
			assertEquals(2, cmd.getArgs().length);
			assertEquals("main.Agent", cmd.getArgs()[0]);
			assertEquals("12", cmd.getArgs()[1]);

			cmd = this.parser.parse(this.janusOptions, args("-h", "main.Agent", "12", "hola"));
			assertEquals(3, cmd.getArgs().length);
			assertEquals("main.Agent", cmd.getArgs()[0]);
			assertEquals("12", cmd.getArgs()[1]);
			assertEquals("hola", cmd.getArgs()[2]);

		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class CommandLineExecutionTests extends AbstractJanusTest {

		@Mock
		private PrintStream logger;

		@Mock
		private Exiter exiter;

		@Before
		public void setUp() {
			Boot.setStandardConsoleLogger(this.logger);
			Boot.setErrorConsoleLogger(this.logger);
			Boot.setExiter(this.exiter);
			Boot.setProperty("io.janusproject.tests.MY_PROPERTY_0", null);
			Boot.setProperty("io.janusproject.tests.MY_PROPERTY_1", null);
			Boot.setProperty("io.janusproject.tests.MY_PROPERTY_2", null);
		}

		@After
		public void tearDown() {
			Boot.setStandardConsoleLogger(null);
			Boot.setErrorConsoleLogger(null);
			Boot.setExiter(null);
			Boot.setProperty("io.janusproject.tests.MY_PROPERTY_0", null);
			Boot.setProperty("io.janusproject.tests.MY_PROPERTY_1", null);
			Boot.setProperty("io.janusproject.tests.MY_PROPERTY_2", null);
		}

		@Test
		public void option_invalidOption() {
			Object[] freeArgs = Boot.parseCommandLine(args("-B", "-x"));
			assertNull(freeArgs);
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.VERBOSE_LEVEL_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			verify(this.logger, times(3)).write(ArgumentMatchers.any(byte[].class), ArgumentMatchers.anyInt(), ArgumentMatchers.anyInt());
			verify(this.logger, times(3)).flush();
			verifyNoMoreInteractions(this.logger);
			verify(this.exiter, only()).exit();
		}

		@Test
		public void option_noOptionGiven() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_B_valid() {
			Object[] freeArgs = Boot.parseCommandLine(args("-B", "--", "-x", "-y"));
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertTrueProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertFalseProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_B_asArg() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-B", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-B", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_R_valid() {
			Object[] freeArgs = Boot.parseCommandLine(args("-R", "--", "-x", "-y"));
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertFalseProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertTrueProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_R_asArg() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-R", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-R", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_W_valid() {
			Object[] freeArgs = Boot.parseCommandLine(args("-W", "--", "-x", "-y"));
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertFalseProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertFalseProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_W_asArg() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-W", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-W", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_nologo_valid() {
			Object[] freeArgs = Boot.parseCommandLine(args("-nologo", "--", "-x", "-y"));
			assertFalseProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_nologo_asArg() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-nologo", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-nologo", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_o_valid() {
			Object[] freeArgs = Boot.parseCommandLine(args("-o", "--", "-x", "-y"));
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertTrueProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_o_asArg() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-o", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-o", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_e_valid() {
			Exiter old = Boot.getExiter();
			Object[] freeArgs = Boot.parseCommandLine(args("-e", "--", "-x", "-y"));
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
			assertNotSame(old, Boot.getExiter());
		}

		@Test
		public void option_e_asArg() {
			Exiter old = Boot.getExiter();
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-e", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-e", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
			assertSame(old, Boot.getExiter());
		}

		@Test
		public void option_s_valid() {
			Object[] freeArgs = Boot.parseCommandLine(args("-s", "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertNullProperty(JanusConfig.VERBOSE_LEVEL_NAME);
			assertNull(freeArgs);
			verify(this.logger, times(1)).write(ArgumentMatchers.any(byte[].class), ArgumentMatchers.anyInt(), ArgumentMatchers.anyInt());
			verify(this.logger, times(3)).flush();
			verifyNoMoreInteractions(this.logger);
			verify(this.exiter, only()).exit();
		}

		@Test
		public void option_s_asArg() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-s", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-s", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_cli_valid() throws IOException {
			Object[] freeArgs = Boot.parseCommandLine(args("arg1", "--cli", "--", "-x", "arg2", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertNullProperty(JanusConfig.VERBOSE_LEVEL_NAME);
			assertNull(freeArgs);
			verifyCli("arg1", "--cli", "--", "-x", "arg2", "-y");
			verify(this.logger, times(1)).flush();
			verifyNoMoreInteractions(this.logger);
			verify(this.exiter, only()).exit();
		}

		private void verifyCli(String... text) throws IOException {
			ArgumentCaptor<String> arg = ArgumentCaptor.forClass(String.class);
			verify(this.logger, Mockito.times(text.length)).println(arg.capture());
			List<String> list = arg.getAllValues();
			assertEquals("invalid list size", text.length, list.size());
			for (int i = 0; i < text.length; ++i) {
				assertEquals("invalid element #" + i, i + ": " + text[i], list.get(i));
			}
		}

		@Test
		public void option_cli_asArg() {
			Object[] freeArgs = Boot.parseCommandLine(args("arg1", "--", "--cli", "-x", "arg2", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "arg1", "--cli", "-x", "arg2", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_f_valid() throws Exception {
			URL propertyURL = Resources.getResource("io/janusproject/Test1.properties");
			assumeNotNull(propertyURL);
			File propertyFile = FileSystem.convertURLToFile(propertyURL);
			assumeNotNull(propertyFile);
			//
			Object[] freeArgs = Boot.parseCommandLine(args("-f", propertyFile.getAbsolutePath(), "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertProperty("io.janusproject.tests.MY_PROPERTY_0", "my value 0");
			assertProperty("io.janusproject.tests.MY_PROPERTY_1", "my value 1");
			assertProperty("io.janusproject.tests.MY_PROPERTY_2", "my value 2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_f_asArg() throws Exception {
			URL propertyURL = Resources.getResource("io/janusproject/Test1.properties");
			assumeNotNull(propertyURL);
			File propertyFile = FileSystem.convertURLToFile(propertyURL);
			assumeNotNull(propertyFile);
			//
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-f", propertyFile.getAbsolutePath(), "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-f", propertyFile.getAbsolutePath(), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_D_valid_withSeparation() {
			Object[] freeArgs = Boot
					.parseCommandLine(args("-D", "io.janusproject.tests.MY_PROPERTY_1=the value", "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertProperty("io.janusproject.tests.MY_PROPERTY_1", "the value");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_D_valid_withoutSeparation() {
			Object[] freeArgs = Boot.parseCommandLine(args("-Dio.janusproject.tests.MY_PROPERTY_1=the value", "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertProperty("io.janusproject.tests.MY_PROPERTY_1", "the value");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_D_asArg_withSeparation() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-D", "io.janusproject.var0=value1", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-D", "io.janusproject.var0=value1", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_D_asArg_withoutSeparation() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-Dio.janusproject.var0=value1", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-Dio.janusproject.var0=value1", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_q_valid() {
			Object[] freeArgs = Boot.parseCommandLine(args("-q", "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "2");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_q_asArg() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-q", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-q", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_qq_valid() {
			Object[] freeArgs = Boot.parseCommandLine(args("-q", "-q", "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "1");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_v_valid() {
			Object[] freeArgs = Boot.parseCommandLine(args("-v", "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "4");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_v_asArg() {
			Object[] freeArgs = Boot.parseCommandLine(args("--", "-v", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "3");
			assertContains(Arrays.asList(freeArgs), "-v", "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_vv() {
			Object[] freeArgs = Boot.parseCommandLine(args("-v", "-v", "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "5");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_vqv() {
			Object[] freeArgs = Boot.parseCommandLine(args("-v", "-q", "-v", "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "4");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_qvq() {
			Object[] freeArgs = Boot.parseCommandLine(args("-q", "-v", "-q", "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertNullProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "2");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_qqq_forNoLogo() {
			Object[] freeArgs = Boot.parseCommandLine(args("-q", "-q", "-q", "--", "-x", "-y"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			assertFalseProperty(JanusConfig.JANUS_LOGO_SHOW_NAME);
			assertNullProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
			assertNullProperty(JanusConfig.OFFLINE);
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_0");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_1");
			assertNullProperty("io.janusproject.tests.MY_PROPERTY_2");
			assertProperty(JanusConfig.VERBOSE_LEVEL_NAME, "0");
			assertContains(Arrays.asList(freeArgs), "-x", "-y");
			verifyZeroInteractions(this.logger);
			verifyZeroInteractions(this.exiter);
		}

		@Test
		public void option_version() {
			Boot.parseCommandLine(args("-version"));
			// The properties are null since resetProperties() is invoked for resetting the properties in
			// the start-up function inherited from AbstractJanusTest
			ArgumentCaptor<byte[]> array = ArgumentCaptor.forClass(byte[].class);
			ArgumentCaptor<Integer> offset = ArgumentCaptor.forClass(Integer.class);
			ArgumentCaptor<Integer> length = ArgumentCaptor.forClass(Integer.class);
			verify(this.logger, times(1)).write(array.capture(), offset.capture(), length.capture());
			final String message = new String(array.getValue(), offset.getValue(), length.getValue());
			assertEquals("Janus: " + JanusVersion.JANUS_RELEASE_VERSION + getLineSeparator() + "SARL specification: "
					+ SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + getLineSeparator(), message);
			verify(this.logger, times(1)).flush();
			verifyNoMoreInteractions(this.logger);
			verify(this.exiter, only()).exit();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class StartTests extends AbstractJanusTest {

		@Test
		public void startJanus() throws Exception {
			Kernel kernel = Boot.startJanusWithModuleType(TestModule.class, AgentMock.class, "param1", "param2", "param3");

			assertNotNull(kernel);

			ArgumentCaptor<Class> agentType = ArgumentCaptor.forClass(Class.class);
			ArgumentCaptor<String> parameters = ArgumentCaptor.forClass(String.class);
			verify(kernel).spawn(agentType.capture(), parameters.capture());
			assertEquals(AgentMock.class, agentType.getValue());
			assertArrayEquals(new String[] { "param1", "param2", "param3" }, parameters.getAllValues().toArray());

			assertEquals(AgentMock.class.getName(), System.getProperty(JanusConfig.BOOT_AGENT));
			String sid = System.getProperty(JanusConfig.BOOT_AGENT_ID);
			assertNotNull(sid);
			assertEquals(ID.toString(), sid);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class BootAgentIdentifierTests extends AbstractJanusTest {

		@Test
		public void getBootAgentIdentifier_notStarted() throws Exception {
			assertNull(Boot.getBootAgentIdentifier());
		}

		@Test
		public void getBootAgentIdentifier_started() throws Exception {
			Boot.startJanusWithModuleType(TestModule.class, AgentMock.class);
			assertEquals(ID, Boot.getBootAgentIdentifier());
		}

		@Test
		public void getBootAgentIdentifier_startedAgain() throws Exception {
			Boot.startJanusWithModuleType(TestModule.class, AgentMock.class);
			assertEquals(ID, Boot.getBootAgentIdentifier());
			assertEquals(ID, Boot.getBootAgentIdentifier());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class RunConfigurationTests extends AbstractJanusRunTest {

		@Nullable
		private UUID defaultID;

		@Nullable
		private UUID bootID;

		@Before
		public void setUp() {
			this.defaultID = UUID.fromString(JanusConfig.DEFAULT_CONTEXT_ID_VALUE);
			this.bootID = UUID.nameUUIDFromBytes(RCAgent.class.getName().getBytes());
			Boot.setOffline(true);
		}

		@Test
		public void defaultContextUUID() throws Exception {
			Boot.setDefaultContextUUID();
			runJanus(RCAgent.class, false);
			UUID id = getResult(UUID.class, 0);
			assertNotNull(id);
			assertEquals(defaultID, id);
		}

		@Test
		public void bootContextUUID() throws Exception {
			Boot.setBootAgentTypeContextUUID();
			runJanus(RCAgent.class, false);
			UUID id = getResult(UUID.class, 0);
			assertNotNull(id);
			assertEquals(bootID, id);
		}

		@Test
		public void randomContextUUID() throws Exception {
			Boot.setRandomContextUUID();
			runJanus(RCAgent.class, false);
			UUID id = getResult(UUID.class, 0);
			assertNotNull(id);
			assertNotEquals(defaultID, id);
			assertNotEquals(bootID, id);
		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class RCAgent extends TestingAgent {

			public RCAgent(UUID parentID, UUID agentID) {
				super(parentID, agentID);
			}

			@Override
			protected boolean runAgentTest() {
				addResult(getSkill(DefaultContextInteractions.class).getDefaultContext().getID());
				return true;
			}

		}

	}

}

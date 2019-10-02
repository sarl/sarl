/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.janusproject.tests;

import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import javax.annotation.Nullable;

import org.junit.After;
import org.junit.Before;
import org.junit.ComparisonFailure;
import org.junit.Test;

import io.janusproject.Boot;

/** Basic tests of running of Janus.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
@SuppressWarnings("all")
public class RunTest {

	private static final boolean CAPTURE_OUTPUTS = true;
	
	@Nullable
	private PrintStream originalStdout;

	@Nullable
	private PrintStream originalStderr;

	@Nullable
	private ByteArrayOutputStream captureStdout;

	@Nullable
	private ByteArrayOutputStream captureStderr;

	private void restoreOutputs() throws IOException {
		System.setOut(this.originalStdout);
		System.setErr(this.originalStderr);
		if (this.captureStdout != null) {
			this.captureStdout.close();
			this.captureStdout = null;
		}
		if (this.captureStderr != null) {
			this.captureStderr.close();
			this.captureStderr = null;
		}
	}
	
	@Before
	public void setUp() {
		if (CAPTURE_OUTPUTS) {
			this.originalStdout = System.out;
			this.captureStdout = new ByteArrayOutputStream();
			System.setOut(new PrintStream(this.captureStdout));
			this.originalStderr = System.err;
			this.captureStderr = new ByteArrayOutputStream();
			System.setErr(new PrintStream(this.captureStderr));
		}
	}

	@After
	public void tearDown() throws IOException {
		if (CAPTURE_OUTPUTS) {
			restoreOutputs();
		}
	}

	private static void assertStartWith(String expected, ByteArrayOutputStream actual) {
		if (actual == null) {
			fail("The actual stream cannot be null.");
			return;
		}
		final String actualString = new String(actual.toByteArray());
		if (!actualString.startsWith(expected)) {
			throw new ComparisonFailure("String must start with  \"" + expected + "\".", expected, actualString);
		}
	}

	private static void assertEmpty(ByteArrayOutputStream actual) {
		if (actual == null) {
			fail("The actual stream cannot be null.");
			return;
		}
		final String actualString = new String(actual.toByteArray());
		if (!actualString.isEmpty()) {
			throw new ComparisonFailure("String must be empty.", "", actualString);
		}
	}

	@Test
	public void helpOption() throws IOException {
		//restoreOutputs();
		final int retcode = Boot.mainWithExitCode("--help");
		assertEquals(255, retcode);
		if (CAPTURE_OUTPUTS) {
			assertStartWith("usage: janus [OPTIONS] <agent_fully_qualified_name>", this.captureStdout);
			assertEmpty(this.captureStderr);
		}
	}

	@Test
	public void illegalOption() throws IOException {
		//restoreOutputs();
		final int retcode = Boot.mainWithExitCode("-C");
		assertEquals(255, retcode);
		if (CAPTURE_OUTPUTS) {
			assertStartWith("usage: janus [OPTIONS] <agent_fully_qualified_name>", this.captureStdout);
			assertStartWith("Unrecognized option: -C",this.captureStderr);
		}
	}

	@Test
	public void noLogo() throws IOException {
		// Because no agent classname is provided, janus should fails with help message. 
		final int retcode = Boot.mainWithExitCode("--nologo");
		assertEquals(255, retcode);
		if (CAPTURE_OUTPUTS) {
			assertStartWith("usage: janus [OPTIONS] <agent_fully_qualified_name>", this.captureStdout);
			assertEmpty(this.captureStderr);
		}
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.bspl.lang.tests.generation;

import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import io.sarl.bspl.lang.tests.AbstractBsplTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Message generation")
public class ProtocolMessageGenerationTest {

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Protocol modifiers")
	public class ProtocolModifierTest extends AbstractBsplTest {

		@Test
		@DisplayName("Default visibility")
		public void defaultVisibility() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent");
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("Public visibility")
		public void publicVisibility() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"public protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent");
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("Package visibility")
		public void packageVisibility() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"package protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"package event M extends ProtocolEvent");
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Public parameters")
	public class PublicParameterTest extends AbstractBsplTest {

		@Test
		@DisplayName("1 msg, 0 arg")
		public void message1argument0() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent");
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 1 arg in")
		public void message1argument1in() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  R1 -> R2 : M [in A1]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 2 args in")
		public void message1argument2in() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2 : String",
					"  R1 -> R2 : M [in A1, in A2]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : String",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 3 args in")
		public void message1argument3in() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2 : String",
					"  parameter A3 : boolean",
					"  R1 -> R2 : M [in A1, in A2, in A3]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : String",
					"  var A3 : boolean",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 1 arg in, 1 key")
		public void message1argument1in1key() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  R1 -> R2 : M [in A1 key]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 2 args in, 1 key")
		public void message1argument2in1k() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2 : String",
					"  R1 -> R2 : M [in A1 key, in A2]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : String",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 3 args in, 1 key")
		public void message1argument3in1key() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2 : String",
					"  parameter A3 : boolean",
					"  R1 -> R2 : M [in A1 key, in A2, in A3]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : String",
					"  var A3 : boolean",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 1 arg out")
		public void message1argument1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A",
					"  R1 -> R2 : M [out A]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 1 arg in, 1 out")
		public void message1argument1in1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2",
					"  R1 -> R2 : M [in A1, out A2]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 2 args in, 1 out")
		public void message1argument2in1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2 : String",
					"  parameter A3",
					"  R1 -> R2 : M [in A1, in A2, out A3]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : String",
					"  var A3 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 3 args in, 1 out")
		public void message1argument3in1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2 : String",
					"  parameter A3 : boolean",
					"  parameter A4",
					"  R1 -> R2 : M [in A1, in A2, in A3, out A4]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : String",
					"  var A3 : boolean",
					"  var A4 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 1 arg in, 1 key, 1 out")
		public void message1argument1in1key1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2",
					"  R1 -> R2 : M [in A1 key, out A2]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 2 args in, 1 key, 1 arg out")
		public void message1argument2in1k1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2 : String",
					"  parameter A3",
					"  R1 -> R2 : M [in A1 key, in A2, out A3]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : String",
					"  var A3 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 3 args in, 1 key, 1 arg out")
		public void message1argument3in1key1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2 : String",
					"  parameter A3 : boolean",
					"  parameter A4",
					"  R1 -> R2 : M [in A1 key, in A2, in A3, out A4]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : String",
					"  var A3 : boolean",
					"  var A4 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("Unconsistent prototypes")
		public void unconsistentPrototype() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2, R3",
					"  parameter A1 : double",
					"  parameter A2",
					"  R2 -> R1 : M [in A2]",
					"  R1 -> R2 : M [in A1, out A2]",
					"  R1 -> R3 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Private parameters")
	public class PrivateParameterTest extends AbstractBsplTest {

		@Test
		@DisplayName("1 msg, 1 arg in")
		public void message1argument1in() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter A1 : double",
					"  R1 -> R2 : M [in A1]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 2 args in")
		public void message1argument2in() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter A1 : double",
					"  parameter A2 : String",
					"  R1 -> R2 : M [in A1, in A2]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A2 : String",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 3 args in")
		public void message1argument3in() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  private parameter A2 : String",
					"  parameter A3 : boolean",
					"  R1 -> R2 : M [in A1, in A2, in A3]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A3 : boolean",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 1 arg in, 1 key")
		public void message1argument1in1key() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter A1 : double",
					"  R1 -> R2 : M [in A1 key]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 2 args in, 1 key")
		public void message1argument2in1k() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  private parameter A2 : String",
					"  R1 -> R2 : M [in A1 key, in A2]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 3 args in, 1 key")
		public void message1argument3in1key() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2 : String",
					"  private parameter A3 : boolean",
					"  R1 -> R2 : M [in A1 key, in A2, in A3]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : String",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 1 arg out")
		public void message1argument1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter A",
					"  R1 -> R2 : M [out A]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 1 arg in, 1 out")
		public void message1argument1in1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter A1 : double",
					"  private parameter A2",
					"  R1 -> R2 : M [in A1, out A2]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 2 args in, 1 out")
		public void message1argument2in1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  private parameter A2 : String",
					"  parameter A3",
					"  R1 -> R2 : M [in A1, in A2, out A3]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A3 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 3 args in, 1 out")
		public void message1argument3in1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter A1 : double",
					"  parameter A2 : String",
					"  private parameter A3 : boolean",
					"  parameter A4",
					"  R1 -> R2 : M [in A1, in A2, in A3, out A4]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A2 : String",
					"  var A4 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 1 arg in, 1 key, 1 out")
		public void message1argument1in1key1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter A1 : double",
					"  parameter A2",
					"  R1 -> R2 : M [in A1 key, out A2]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A2 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 2 args in, 1 key, 1 arg out")
		public void message1argument2in1k1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  private parameter A2 : String",
					"  parameter A3",
					"  R1 -> R2 : M [in A1 key, in A2, out A3]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A3 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("1 msg, 3 args in, 1 key, 1 arg out")
		public void message1argument3in1key1out() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1 : double",
					"  parameter A2 : String",
					"  private parameter A3 : boolean",
					"  parameter A4",
					"  R1 -> R2 : M [in A1 key, in A2, in A3, out A4]",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A1 : double",
					"  var A2 : String",
					"  var A4 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

		@Test
		@DisplayName("Unconsistent prototypes")
		public void unconsistentPrototype() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2, R3",
					"  private parameter A1 : double",
					"  parameter A2",
					"  R2 -> R1 : M [in A2]",
					"  R1 -> R2 : M [in A1, out A2]",
					"  R1 -> R3 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests.messages",
					"",
					"import io.sarl.bspl.api.protocol.events.ProtocolEvent",
					"",
					"public event M extends ProtocolEvent {",
					"  var A2 : Object",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.messages.M", expected);
		}

	}

}

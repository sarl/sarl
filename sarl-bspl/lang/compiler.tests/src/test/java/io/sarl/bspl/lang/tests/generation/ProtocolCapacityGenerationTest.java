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
@DisplayName("ProtocolCapacity generation")
public class ProtocolCapacityGenerationTest {

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
					"  role R3",
					"  R1 -> R2 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity",
					"import io.sarl.bspl.api.protocol.impl.ProtocolMessage",
					"import io.sarl.bspl.lang.tests.messages.M",
					"import io.sarl.lang.core.annotation.SarlAsynchronousExecution",
					"import java.util.List",
					"",
					"public capacity R1ProtocolCapacity extends ProtocolCapacity {",
					"  @SarlAsynchronousExecution",
					"  def getEnabledMMessages : List<ProtocolMessage<M>>",
					"  @SarlAsynchronousExecution",
					"  def sendMMessage(m : ProtocolMessage<M>)",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.R1ProtocolCapacity", expected);
		}

		@Test
		@DisplayName("Public visibility")
		public void publicVisibility() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"public protocol PROTO {",
					"  role R1, R2",
					"  role R3",
					"  R1 -> R2 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity",
					"import io.sarl.bspl.api.protocol.impl.ProtocolMessage",
					"import io.sarl.bspl.lang.tests.messages.M",
					"import io.sarl.lang.core.annotation.SarlAsynchronousExecution",
					"import java.util.List",
					"",
					"public capacity R1ProtocolCapacity extends ProtocolCapacity {",
					"  @SarlAsynchronousExecution",
					"  def getEnabledMMessages : List<ProtocolMessage<M>>",
					"  @SarlAsynchronousExecution",
					"  def sendMMessage(m : ProtocolMessage<M>)",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.R1ProtocolCapacity", expected);
		}

		@Test
		@DisplayName("Package visibility")
		public void packageVisibility() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"package protocol PROTO {",
					"  role R1, R2",
					"  role R3",
					"  R1 -> R2 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity",
					"import io.sarl.bspl.api.protocol.impl.ProtocolMessage",
					"import io.sarl.bspl.lang.tests.messages.M",
					"import io.sarl.lang.core.annotation.SarlAsynchronousExecution",
					"import java.util.List",
					"",
					"package capacity R1ProtocolCapacity extends ProtocolCapacity {",
					"  @SarlAsynchronousExecution",
					"  def getEnabledMMessages : List<ProtocolMessage<M>>",
					"  @SarlAsynchronousExecution",
					"  def sendMMessage(m : ProtocolMessage<M>)",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.R1ProtocolCapacity", expected);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("General")
	public class GeneralTest extends AbstractBsplTest {

		@Test
		@DisplayName("Capacity for single R1")
		public void onlyOriginBsplProtocol() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  role R3",
					"  R1 -> R2 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity",
					"import io.sarl.bspl.api.protocol.impl.ProtocolMessage",
					"import io.sarl.bspl.lang.tests.messages.M",
					"import io.sarl.lang.core.annotation.SarlAsynchronousExecution",
					"import java.util.List",
					"",
					"public capacity R1ProtocolCapacity extends ProtocolCapacity {",
					"  @SarlAsynchronousExecution",
					"  def getEnabledMMessages : List<ProtocolMessage<M>>",
					"  @SarlAsynchronousExecution",
					"  def sendMMessage(m : ProtocolMessage<M>)",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.R1ProtocolCapacity", expected);
		}
	
		@Test
		@DisplayName("Capacity for R1")
		public void firstOriginBsplProtocol() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  role R3",
					"  R1 -> R2 : M",
					"  R3 -> R2 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity",
					"import io.sarl.bspl.api.protocol.impl.ProtocolMessage",
					"import io.sarl.bspl.lang.tests.messages.M",
					"import io.sarl.lang.core.annotation.SarlAsynchronousExecution",
					"import java.util.List",
					"",
					"public capacity R1ProtocolCapacity extends ProtocolCapacity {",
					"  @SarlAsynchronousExecution",
					"  def getEnabledMMessages : List<ProtocolMessage<M>>",
					"  @SarlAsynchronousExecution",
					"  def sendMMessage(m : ProtocolMessage<M>)",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.R1ProtocolCapacity", expected);
		}
	
		@Test
		@DisplayName("Capacity for R3")
		public void secondOriginBsplProtocol() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  role R3",
					"  R1 -> R2 : M",
					"  R3 -> R2 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity",
					"import io.sarl.bspl.api.protocol.impl.ProtocolMessage",
					"import io.sarl.bspl.lang.tests.messages.M",
					"import io.sarl.lang.core.annotation.SarlAsynchronousExecution",
					"import java.util.List",
					"",
					"public capacity R3ProtocolCapacity extends ProtocolCapacity {",
					"  @SarlAsynchronousExecution",
					"  def getEnabledMMessages : List<ProtocolMessage<M>>",
					"  @SarlAsynchronousExecution",
					"  def sendMMessage(m : ProtocolMessage<M>)",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.R3ProtocolCapacity", expected);
		}
	
		@Test
		@DisplayName("Multiple R1 for same message")
		public void multipleR1SingleMessage() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  role R3",
					"  R1 -> R2 : M",
					"  R3 -> R2 : M",
					"  R1 -> R3 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity",
					"import io.sarl.bspl.api.protocol.impl.ProtocolMessage",
					"import io.sarl.bspl.lang.tests.messages.M",
					"import io.sarl.lang.core.annotation.SarlAsynchronousExecution",
					"import java.util.List",
					"",
					"public capacity R1ProtocolCapacity extends ProtocolCapacity {",
					"  @SarlAsynchronousExecution",
					"  def getEnabledMMessages : List<ProtocolMessage<M>>",
					"  @SarlAsynchronousExecution",
					"  def sendMMessage(m : ProtocolMessage<M>)",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.R1ProtocolCapacity", expected);
		}
	
		@Test
		@DisplayName("Multiple R1 for multiple messages")
		public void multipleR1ManyMessages() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  role R3",
					"  R1 -> R2 : M",
					"  R3 -> R2 : M",
					"  R1 -> R3 : M2",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity",
					"import io.sarl.bspl.api.protocol.impl.ProtocolMessage",
					"import io.sarl.bspl.lang.tests.messages.M",
					"import io.sarl.bspl.lang.tests.messages.M2",
					"import io.sarl.lang.core.annotation.SarlAsynchronousExecution",
					"import java.util.List",
					"",
					"public capacity R1ProtocolCapacity extends ProtocolCapacity {",
					"  @SarlAsynchronousExecution",
					"  def getEnabledMMessages : List<ProtocolMessage<M>>",
					"  @SarlAsynchronousExecution",
					"  def sendMMessage(m : ProtocolMessage<M>)",
					"",
					"  @SarlAsynchronousExecution",
					"  def getEnabledM2Messages : List<ProtocolMessage<M2>>",
					"  @SarlAsynchronousExecution",
					"  def sendM2Message(m : ProtocolMessage<M2>)",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.R1ProtocolCapacity", expected);
		}

	}

}

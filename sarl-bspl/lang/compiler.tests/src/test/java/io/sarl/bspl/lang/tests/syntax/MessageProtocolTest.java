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
package io.sarl.bspl.lang.tests.syntax;

import static io.sarl.bspl.lang.validation.IssueCodes.*;
import static io.sarl.bspl.lang.validation.IssueCodes.INVALID_ROLE_CARDINALITY_ORDER;
import static io.sarl.bspl.lang.validation.IssueCodes.MISSED_ARGUMENT_IN_MESSAGE;
import static io.sarl.bspl.lang.validation.IssueCodes.UNDEFINED_PROTOCOL_PARAMETER;
import static io.sarl.bspl.lang.validation.IssueCodes.UNDEFINED_PROTOCOL_ROLE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.eclipse.xtext.diagnostics.Diagnostic;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import io.sarl.bspl.lang.bspl.BsplPackage;
import io.sarl.bspl.lang.tests.AbstractBsplTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Messages")
public class MessageProtocolTest {

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Base syntax")
	public class BaseSyntaxTest extends AbstractBsplTest {

		@Test
		@DisplayName("1 message")
		public void oneMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());
		}

		@Test
		@DisplayName("2 messages")
		public void twoMessages() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"  R2 -> R1 : M2",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(2, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());

			final var m1 = messages.get(1);
			assertEquals("R2", m1.getFrom());
			assertEquals("R1", m1.getTo());
			assertFalse(m1.isInputTargetRole());
			assertFalse(m1.isOutputTargetRole());
			assertEquals("M2", m1.getMessage());
			assertEquals(0, m1.getArguments().size());
		}

		@Test
		@DisplayName("3 messages")
		public void threeMessages() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"  R2 -> R1 : M2",
					"  R1 -> R1 : M3",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(3, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());

			final var m1 = messages.get(1);
			assertEquals("R2", m1.getFrom());
			assertEquals("R1", m1.getTo());
			assertFalse(m1.isInputTargetRole());
			assertFalse(m1.isOutputTargetRole());
			assertEquals("M2", m1.getMessage());
			assertEquals(0, m1.getArguments().size());

			final var m2 = messages.get(2);
			assertEquals("R1", m2.getFrom());
			assertEquals("R1", m2.getTo());
			assertFalse(m2.isInputTargetRole());
			assertFalse(m2.isOutputTargetRole());
			assertEquals("M3", m2.getMessage());
			assertEquals(0, m2.getArguments().size());
		}

		@Test
		@DisplayName("Unknown FROM role")
		public void unknownFromRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  R2 -> R1 : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined source role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Unknown TO role")
		public void unknownToRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined destination role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Duplicate message")
		public void duplicateMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					DUPLICATE_PROTOCOL_MESSAGE,
					"Duplicate message M from R1 to R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Not duplicate message")
		public void notDuplicateMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2, R3",
					"  R1 -> R2 : M",
					"  R1 -> R3 : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					REQUIRED_OUT_PARAMETER,
					"Out parameter must be declared for protocol PROTO")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					REQUIRED_OUT_PARAMETER_IN_MESSAGES,
					"Out parameter must be defined in at least one message for protocol PROTO")
				.assertNoErrors();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Role cardinalities")
	public class CardinalityTest extends AbstractBsplTest {

		@Test
		@DisplayName("Invalid min max values")
		public void invalidValues() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1 [8..1], R2",
					"  R1 -> R2 : M",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertEquals(1, r0.getMin());
			assertEquals(8, r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Base w/ prefix IN modifier")
	public class PrefixInModifierTest extends AbstractBsplTest {

		@Test
		@DisplayName("1 message")
		public void oneMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> in R2 : M",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertTrue(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());
		}

		@Test
		@DisplayName("2 messages")
		public void twoMessages() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> in R2 : M",
					"  R2 -> R1 : M2",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(2, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertTrue(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());

			final var m1 = messages.get(1);
			assertEquals("R2", m1.getFrom());
			assertEquals("R1", m1.getTo());
			assertFalse(m1.isInputTargetRole());
			assertFalse(m1.isOutputTargetRole());
			assertEquals("M2", m1.getMessage());
			assertEquals(0, m1.getArguments().size());
		}

		@Test
		@DisplayName("3 messages")
		public void threeMessages() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"  R2 -> in R1 : M2",
					"  R1 -> R1 : M3",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(3, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());

			final var m1 = messages.get(1);
			assertEquals("R2", m1.getFrom());
			assertEquals("R1", m1.getTo());
			assertTrue(m1.isInputTargetRole());
			assertFalse(m1.isOutputTargetRole());
			assertEquals("M2", m1.getMessage());
			assertEquals(0, m1.getArguments().size());

			final var m2 = messages.get(2);
			assertEquals("R1", m2.getFrom());
			assertEquals("R1", m2.getTo());
			assertFalse(m2.isInputTargetRole());
			assertFalse(m2.isOutputTargetRole());
			assertEquals("M3", m2.getMessage());
			assertEquals(0, m2.getArguments().size());
		}

		@Test
		@DisplayName("Unknown FROM role")
		public void unknownFromRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  R2 -> in R1 : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined source role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Unknown TO role")
		public void unknownToRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  R1 -> in R2 : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined destination role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Duplicate message")
		public void duplicateMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> in R2 : M",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					DUPLICATE_PROTOCOL_MESSAGE,
					"Duplicate message M from R1 to R2 in the protocol PROTO");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Base w/ prefix OUT modifier")
	public class PrefixOutModifierTest extends AbstractBsplTest {

		@Test
		@DisplayName("1 message")
		public void oneMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> out R2 : M",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertTrue(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());
		}

		@Test
		@DisplayName("Invalid modifier position #1")
		public void invalidModifierPosition() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 out : M",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertTrue(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());
		}

		@Test
		@DisplayName("2 messages")
		public void twoMessages() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> out R2 : M",
					"  R2 -> R1 : M2",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(2, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertTrue(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());

			final var m1 = messages.get(1);
			assertEquals("R2", m1.getFrom());
			assertEquals("R1", m1.getTo());
			assertFalse(m1.isInputTargetRole());
			assertFalse(m1.isOutputTargetRole());
			assertEquals("M2", m1.getMessage());
			assertEquals(0, m1.getArguments().size());
		}

		@Test
		@DisplayName("3 messages")
		public void threeMessages() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"  R2 -> out R1 : M2",
					"  R1 -> R1 : M3",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(3, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());

			final var m1 = messages.get(1);
			assertEquals("R2", m1.getFrom());
			assertEquals("R1", m1.getTo());
			assertFalse(m1.isInputTargetRole());
			assertTrue(m1.isOutputTargetRole());
			assertEquals("M2", m1.getMessage());
			assertEquals(0, m1.getArguments().size());

			final var m2 = messages.get(2);
			assertEquals("R1", m2.getFrom());
			assertEquals("R1", m2.getTo());
			assertFalse(m2.isInputTargetRole());
			assertFalse(m2.isOutputTargetRole());
			assertEquals("M3", m2.getMessage());
			assertEquals(0, m2.getArguments().size());
		}

		@Test
		@DisplayName("Unknown FROM role")
		public void unknownFromRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  R2 -> out R1 : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined source role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Unknown TO role")
		public void unknownToRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  R1 -> out R2 : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined destination role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Duplicate message")
		public void duplicateMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> out R2 : M",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					DUPLICATE_PROTOCOL_MESSAGE,
					"Duplicate message M from R1 to R2 in the protocol PROTO");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Base w/ postfix IN modifier")
	public class PostfixInModifierTest extends AbstractBsplTest {

		@Test
		@DisplayName("1 message")
		public void oneMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 in : M",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertTrue(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());
		}

		@Test
		@DisplayName("2 messages")
		public void twoMessages() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 in : M",
					"  R2 -> R1 : M2",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(2, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertTrue(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());

			final var m1 = messages.get(1);
			assertEquals("R2", m1.getFrom());
			assertEquals("R1", m1.getTo());
			assertFalse(m1.isInputTargetRole());
			assertFalse(m1.isOutputTargetRole());
			assertEquals("M2", m1.getMessage());
			assertEquals(0, m1.getArguments().size());
		}

		@Test
		@DisplayName("3 messages")
		public void threeMessages() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"  R2 -> R1 in : M2",
					"  R1 -> R1 : M3",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(3, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());

			final var m1 = messages.get(1);
			assertEquals("R2", m1.getFrom());
			assertEquals("R1", m1.getTo());
			assertTrue(m1.isInputTargetRole());
			assertFalse(m1.isOutputTargetRole());
			assertEquals("M2", m1.getMessage());
			assertEquals(0, m1.getArguments().size());

			final var m2 = messages.get(2);
			assertEquals("R1", m2.getFrom());
			assertEquals("R1", m2.getTo());
			assertFalse(m2.isInputTargetRole());
			assertFalse(m2.isOutputTargetRole());
			assertEquals("M3", m2.getMessage());
			assertEquals(0, m2.getArguments().size());
		}

		@Test
		@DisplayName("Unknown FROM role")
		public void unknownFromRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  R2 -> R1 in : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined source role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Unknown TO role")
		public void unknownToRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  R1 -> R2 in : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined destination role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Duplicate message")
		public void duplicateMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 in : M",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					DUPLICATE_PROTOCOL_MESSAGE,
					"Duplicate message M from R1 to R2 in the protocol PROTO");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Base w/ postfix OUT modifier")
	public class PostfixOutModifierTest extends AbstractBsplTest {

		@Test
		@DisplayName("1 message")
		public void oneMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 out : M",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertTrue(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());
		}

		@Test
		@DisplayName("2 messages")
		public void twoMessages() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 out : M",
					"  R2 -> R1 : M2",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(2, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertTrue(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());

			final var m1 = messages.get(1);
			assertEquals("R2", m1.getFrom());
			assertEquals("R1", m1.getTo());
			assertFalse(m1.isInputTargetRole());
			assertFalse(m1.isOutputTargetRole());
			assertEquals("M2", m1.getMessage());
			assertEquals(0, m1.getArguments().size());
		}

		@Test
		@DisplayName("3 messages")
		public void threeMessages() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 : M",
					"  R2 -> R1 out : M2",
					"  R1 -> R1 : M3",
					"}");
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());
	
			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertNull(r0.getMax());
			
			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertNull(r1.getMax());
			//
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(3, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(0, m0.getArguments().size());

			final var m1 = messages.get(1);
			assertEquals("R2", m1.getFrom());
			assertEquals("R1", m1.getTo());
			assertFalse(m1.isInputTargetRole());
			assertTrue(m1.isOutputTargetRole());
			assertEquals("M2", m1.getMessage());
			assertEquals(0, m1.getArguments().size());

			final var m2 = messages.get(2);
			assertEquals("R1", m2.getFrom());
			assertEquals("R1", m2.getTo());
			assertFalse(m2.isInputTargetRole());
			assertFalse(m2.isOutputTargetRole());
			assertEquals("M3", m2.getMessage());
			assertEquals(0, m2.getArguments().size());
		}

		@Test
		@DisplayName("Unknown FROM role")
		public void unknownFromRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  R2 -> R1 out : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined source role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Unknown TO role")
		public void unknownToRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  R1 -> R2 out : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined destination role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Duplicate message")
		public void duplicateMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R2 out : M",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					DUPLICATE_PROTOCOL_MESSAGE,
					"Duplicate message M from R1 to R2 in the protocol PROTO");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Arguments")
	public class ArgumentsTest extends AbstractBsplTest {

		@Test
		@DisplayName("1 argument")
		public void oneArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [P1]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertFalse(arg0.isKey());
			assertTrue(arg0.isInput());
			assertTrue(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertTrue(arg0.isAny());
		}

		@Test
		@DisplayName("1 IN argument")
		public void oneInArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [in P1]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertFalse(arg0.isKey());
			assertTrue(arg0.isInput());
			assertFalse(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertFalse(arg0.isAny());
		}

		@Test
		@DisplayName("1 OUT argument")
		public void oneOutArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [out P1]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertFalse(arg0.isKey());
			assertFalse(arg0.isInput());
			assertTrue(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertFalse(arg0.isAny());
		}

		@Test
		@DisplayName("1 NIL argument")
		public void oneNilArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [nil P1]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertFalse(arg0.isKey());
			assertFalse(arg0.isInput());
			assertFalse(arg0.isOutput());
			assertTrue(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertFalse(arg0.isAny());
		}

		@Test
		@DisplayName("1 OPT argument")
		public void oneOptArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [opt P1]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertFalse(arg0.isKey());
			assertFalse(arg0.isInput());
			assertFalse(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertTrue(arg0.isOptional());
			assertFalse(arg0.isAny());
		}

		@Test
		@DisplayName("1 ANY argument")
		public void oneAnyArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [any P1]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertFalse(arg0.isKey());
			assertTrue(arg0.isInput());
			assertTrue(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertTrue(arg0.isAny());
		}

		@Test
		@DisplayName("1 argument with invalid modifier")
		public void oneArgumentInvalidModifier() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [xxx P1]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_PARAMETER,
					"Undefined parameter xxx in the protocol PROTO");
		}

		@Test
		@DisplayName("1 KEY argument")
		public void oneKeyArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [P1 key]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertTrue(arg0.isKey());
			assertTrue(arg0.isInput());
			assertTrue(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertTrue(arg0.isAny());
		}

		@Test
		@DisplayName("1 IN KEY argument")
		public void oneInKeyArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [in P1 key]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertTrue(arg0.isKey());
			assertTrue(arg0.isInput());
			assertFalse(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertFalse(arg0.isAny());
		}

		@Test
		@DisplayName("1 OUT KEY argument")
		public void oneOutKeyArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [out P1 key]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertTrue(arg0.isKey());
			assertFalse(arg0.isInput());
			assertTrue(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertFalse(arg0.isAny());
		}

		@Test
		@DisplayName("1 NIL KEY argument")
		public void oneNilKeyArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [nil P1 key]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertTrue(arg0.isKey());
			assertFalse(arg0.isInput());
			assertFalse(arg0.isOutput());
			assertTrue(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertFalse(arg0.isAny());
		}

		@Test
		@DisplayName("1 OPT KEY argument")
		public void oneOptKeyArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [opt P1 key]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertTrue(arg0.isKey());
			assertFalse(arg0.isInput());
			assertFalse(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertTrue(arg0.isOptional());
			assertFalse(arg0.isAny());
		}

		@Test
		@DisplayName("1 ANY KEY argument")
		public void oneAnyKeyArgument() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [any P1 key]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(1, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertTrue(arg0.isKey());
			assertTrue(arg0.isInput());
			assertTrue(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertTrue(arg0.isAny());
		}

		@Test
		@DisplayName("1 KEY argument with invalid modifier")
		public void oneKeyArgumentInvalidModifier() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M [xxx P1 key]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"missing ']' at 'P1'");
		}

		@Test
		@DisplayName("2 arguments")
		public void twoArguments() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  parameter P2 : String",
					"  R1 -> R2 : M [P1, P2]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(2, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertFalse(arg0.isKey());
			assertTrue(arg0.isInput());
			assertTrue(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertTrue(arg0.isAny());

			final var arg1 = m0.getArguments().get(1);
			assertEquals("P2", arg1.getName());
			assertFalse(arg1.isKey());
			assertTrue(arg1.isInput());
			assertTrue(arg1.isOutput());
			assertFalse(arg1.isNil());
			assertFalse(arg1.isOptional());
			assertTrue(arg1.isAny());
		}

		@Test
		@DisplayName("2 IN KEY arguments")
		public void twoInKeyArguments() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  parameter P2 : String",
					"  R1 -> R2 : M [in P1, P2 key]",
					"}");
			final var messages = bspl.getBsplProtocols().get(0).getMessages();
			assertEquals(1, messages.size());
	
			final var m0 = messages.get(0);
			assertEquals("R1", m0.getFrom());
			assertEquals("R2", m0.getTo());
			assertFalse(m0.isInputTargetRole());
			assertFalse(m0.isOutputTargetRole());
			assertEquals("M", m0.getMessage());
			assertEquals(2, m0.getArguments().size());
			
			final var arg0 = m0.getArguments().get(0);
			assertEquals("P1", arg0.getName());
			assertFalse(arg0.isKey());
			assertTrue(arg0.isInput());
			assertFalse(arg0.isOutput());
			assertFalse(arg0.isNil());
			assertFalse(arg0.isOptional());
			assertFalse(arg0.isAny());

			final var arg1 = m0.getArguments().get(1);
			assertEquals("P2", arg1.getName());
			assertTrue(arg1.isKey());
			assertTrue(arg1.isInput());
			assertTrue(arg1.isOutput());
			assertFalse(arg1.isNil());
			assertFalse(arg1.isOptional());
			assertTrue(arg1.isAny());
		}

		@Test
		@DisplayName("Unconsistent message prototype #1")
		public void unconsistentMessagePrototype1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1",
					"  parameter A2",
					"  R1 -> R2 : M [in A1, out A2]",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					MISSED_ARGUMENT_IN_MESSAGE,
					"Missing definition of argument A1 for message M")
			.assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					MISSED_ARGUMENT_IN_MESSAGE,
					"Missing definition of argument A2 for message M");
		}

		@Test
		@DisplayName("Unconsistent message prototype #2")
		public void unconsistentMessagePrototype2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1",
					"  parameter A2",
					"  R1 -> R2 : M [in A1, out A2]",
					"  R1 -> R2 : M [out A2]",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					MISSED_ARGUMENT_IN_MESSAGE,
					"Missing definition of argument A1 for message M");
		}

		@Test
		@DisplayName("Unconsistent message prototype #3")
		public void unconsistentMessagePrototype3() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter A1",
					"  parameter A2",
					"  R1 -> R2 : M [in A1, out A2]",
					"  R1 -> R2 : M [in A1, in A2]",
					"}");
			validate(bspl).assertNoWarnings(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					MISSED_ARGUMENT_IN_MESSAGE);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Singh Definition Compliance")
	public class SinghDefinitionComplianceTest extends AbstractBsplTest {

		@Test
		@DisplayName("Duplicate message #1")
		public void duplicateMessage1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"PROTO {",
					"  role R1, R2",
					"  parameter in P1",
					"  R1 -> R2 : M",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					DUPLICATE_PROTOCOL_MESSAGE,
					"Duplicate message M from R1 to R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Duplicate message #2")
		public void duplicateMessage2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"PROTO {",
					"  role R1, R2",
					"  parameter in P1",
					"  R1 -> R2 : M [ in P1 ]",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					DUPLICATE_PROTOCOL_MESSAGE,
					"Duplicate message M from R1 to R2 in the protocol PROTO");
		}

	}

}

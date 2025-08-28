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

import static io.sarl.bspl.lang.validation.IssueCodes.DUPLICATE_PROTOCOL_ROLE;
import static io.sarl.bspl.lang.validation.IssueCodes.MISSED_PROTOCOL_MESSAGE;
import static io.sarl.bspl.lang.validation.IssueCodes.UNDEFINED_PROTOCOL_ROLE;
import static io.sarl.bspl.lang.validation.IssueCodes.UNNECESSARY_ROLE_CARDINALITY;
import static io.sarl.bspl.lang.validation.IssueCodes.UNUSED_PROTOCOL_ROLE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

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
@DisplayName("Protocol Role")
public class RoleProtocolTest {

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Simple syntax")
	public class OnlyRoleTest extends AbstractBsplTest {

		@Test
		@DisplayName("Original BSPL syntax")
		public void onlyOriginBsplProtocol() throws Exception {
			var bspl = specification(
					"PROTO {",
					"  R1, R2",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
	
		}
	
		@Test
		@DisplayName("SARL BSPL syntax 1")
		public void onlySarlBsplProtocol_1() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1, R2",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
		}
	
		@Test
		@DisplayName("SARL BSPL syntax 2")
		public void onlySarlBsplProtocol_2() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  role R1, R2",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
		}
	
		@Test
		@DisplayName("SARL BSPL syntax 3")
		public void onlySarlBsplProtocol_3() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  role R1",
					"  role R2",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
		}

		@Test
		@DisplayName("Duplicate role 1 line")
		public void duplicateRole_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R1",
					"  R1 -> R1 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					DUPLICATE_PROTOCOL_ROLE,
					"Duplicate definition of the role R1 in the protocol PROTO");
	
		}

		@Test
		@DisplayName("Duplicate role 2 lines")
		public void duplicateRole_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"  role R1",
					"  R1 -> R1 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					DUPLICATE_PROTOCOL_ROLE,
					"Duplicate definition of the role R1 in the protocol PROTO");
	
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Max cardinality")
	public class MaxCardinalityTest extends AbstractBsplTest {

		@Test
		@DisplayName("Single line notation w/o keyword")
		public void maxCardinality_1() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [5], R2 [6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertEquals(6, r1.getMax());
		}
	
		@Test
		@DisplayName("Single line notation w/ keyword")
		public void maxCardinality_2() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  role R1 [5], R2 [6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertEquals(6, r1.getMax());
		}
	
		@Test
		@DisplayName("Multiline notation w/ keyword")
		public void maxCardinality_3() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  role R1 [5]",
					"  role R2 [6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertEquals(6, r1.getMax());
		}

		@Test
		@DisplayName("Multiline notation w/o keyword")
		public void maxCardinality_4() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [5]",
					"  R2 [6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertEquals(6, r1.getMax());
		}

		@Test
		@DisplayName("Multiline notation w/ w/o keyword")
		public void maxCardinality_5() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  role R1 [5]",
					"  R2 [6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertEquals(6, r1.getMax());
		}

		@Test
		@DisplayName("Multiline notation w/0 w/ keyword")
		public void maxCardinality_6() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [5]",
					"  role R2 [6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertNull(r1.getMin());
			assertEquals(6, r1.getMax());
		}

		@Test
		@DisplayName("Zero max")
		public void zeroMax() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [0]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message")
				.assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					UNNECESSARY_ROLE_CARDINALITY,
					"Unexpected specification of the maximum cardinality for the role 'R1'");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(1, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(0, r0.getMax());
		}

		@Test
		@DisplayName("Negative max")
		public void negativeMax() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [-4]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input '-'");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(1, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(4, r0.getMax());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Min-Max cardinality")
	public class MinMaxCardinalityTest extends AbstractBsplTest {

		@Test
		@DisplayName("Single line notation w/o keyword")
		public void singleLine_1() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [4..5], R2 [1..6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertEquals(4, r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertEquals(1, r1.getMin());
			assertEquals(6, r1.getMax());
		}

		@Test
		@DisplayName("Single line notation w/ keyword")
		public void singleLine_2() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  role R1 [4..5], R2 [1..6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertEquals(4, r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertEquals(1, r1.getMin());
			assertEquals(6, r1.getMax());
		}

		@Test
		@DisplayName("Multiple lines notation w/o keyword")
		public void multipleLines_1() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [4..5]",
					"  R2 [1..6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertEquals(4, r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertEquals(1, r1.getMin());
			assertEquals(6, r1.getMax());
		}

		@Test
		@DisplayName("Multiple lines notation w/ keyword")
		public void multipleLines_2() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  role R1 [4..5]",
					"  role R2 [1..6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertEquals(4, r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertEquals(1, r1.getMin());
			assertEquals(6, r1.getMax());
		}

		@Test
		@DisplayName("Multiple lines notation w/ w/o keyword")
		public void multipleLines_3() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  role R1 [4..5]",
					"  R2 [1..6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertEquals(4, r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertEquals(1, r1.getMin());
			assertEquals(6, r1.getMax());
		}

		@Test
		@DisplayName("Multiple lines notation w/o w/ keyword")
		public void multipleLines_4() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [4..5]",
					"  role R2 [1..6]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(2, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertEquals(4, r0.getMin());
			assertEquals(5, r0.getMax());

			final var r1 = roles.get(1);
			assertEquals("R2", r1.getName());
			assertEquals(1, r1.getMin());
			assertEquals(6, r1.getMax());
		}

		@Test
		@DisplayName("Zero-positive cardinalities")
		public void cardinalities_0() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [0..5]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message")
				.assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					UNNECESSARY_ROLE_CARDINALITY,
					"Unnecessary specification of the minimum cardinality for the role 'R1'");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(1, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertEquals(0, r0.getMin());
			assertEquals(5, r0.getMax());
		}

		@Test
		@DisplayName("Zero-zero cardinalities")
		public void cardinalities_1() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [0..0]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message")
				.assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					UNNECESSARY_ROLE_CARDINALITY,
					"Unnecessary specification of the minimum cardinality for the role 'R1'")
				.assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					UNNECESSARY_ROLE_CARDINALITY,
					"Unexpected specification of the maximum cardinality for the role 'R1'");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(1, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertEquals(0, r0.getMin());
			assertEquals(0, r0.getMax());
		}

		@Test
		@DisplayName("Zero-negative cardinalities")
		public void cardinalities_2() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [0..-1]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input '-'")
				.assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					UNNECESSARY_ROLE_CARDINALITY,
					"Unnecessary specification of the minimum cardinality for the role 'R1'");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(1, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertEquals(0, r0.getMin());
			assertEquals(1, r0.getMax());
		}


		@Test
		@DisplayName("Negative-positive cardinalities")
		public void cardinalities_3() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [-1..5]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input '-'");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(1, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(1, r0.getMax());
		}

		@Test
		@DisplayName("Negative-zero cardinalities")
		public void cardinalities_4() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [-4..0]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input '-'");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(1, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(4, r0.getMax());
		}

		@Test
		@DisplayName("Negative-negative cardinalities")
		public void cardinalities_5() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  R1 [-8..-1]",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input '-'");
			//
			final var roles = bspl.getBsplProtocols().get(0).getRoles();
			assertEquals(1, roles.size());

			final var r0 = roles.get(0);
			assertEquals("R1", r0.getName());
			assertNull(r0.getMin());
			assertEquals(8, r0.getMax());
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
		@DisplayName("Unused role")
		public void unecessaryRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"PROTO {",
					"  role R1, R2, R3",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					UNUSED_PROTOCOL_ROLE,
					"Unused role R3 in protocol PROTO");
		}

		@Test
		@DisplayName("Missing role")
		public void missingRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"PROTO {",
					"  role R1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined destination role R2 in the protocol PROTO");
		}

		@Test
		@DisplayName("Duplicated role #1")
		public void duplicatedRole1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"PROTO {",
					"  role R1, R2, R1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					DUPLICATE_PROTOCOL_ROLE,
					"Duplicate definition of the role R1 in the protocol PROTO");
		}

		@Test
		@DisplayName("Duplicated role #2")
		public void duplicatedRole2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"PROTO {",
					"  role R1, R2",
					"  role R1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocolRole(),
					DUPLICATE_PROTOCOL_ROLE,
					"Duplicate definition of the role R1 in the protocol PROTO");
		}

	}

}

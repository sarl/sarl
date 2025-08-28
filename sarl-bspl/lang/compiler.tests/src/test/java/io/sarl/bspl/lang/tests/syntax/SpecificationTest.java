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

import static io.sarl.bspl.lang.validation.IssueCodes.DUPLICATE_PROTOCOL_NAME;
import static io.sarl.bspl.lang.validation.IssueCodes.EMPTY_PACKAGE_DECLARATION;
import static io.sarl.bspl.lang.validation.IssueCodes.EMPTY_PROTOCOL;
import static io.sarl.bspl.lang.validation.IssueCodes.MISSED_PROTOCOL_MESSAGE;
import static io.sarl.bspl.lang.validation.IssueCodes.*;
import static io.sarl.bspl.lang.validation.IssueCodes.*;
import static org.eclipse.xtext.xbase.validation.IssueCodes.*;

import org.eclipse.xtext.xtype.XtypePackage;
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
@DisplayName("BSPL specification")
public class SpecificationTest {

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
		@DisplayName("Public visibility")
		public void publicVisibility() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"public PROTO {",
					"  role A, B",
					"  parameter out o",
					"  A -> B : M [out o]",
					"}");
			validate(bspl).assertNoErrors();
		}
		
		@Test
		@DisplayName("Package visibility")
		public void packageVisibility() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"package PROTO {",
					"  role A, B",
					"  parameter out o",
					"  A -> B : M [out o]",
					"}");
			validate(bspl).assertNoErrors();
		}

		@Test
		@DisplayName("Public+package visibility")
		public void publicPackageVisibility() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"public package PROTO {",
					"  role A, B",
					"  parameter out o",
					"  A -> B : M [out o]",
					"}");
			validate(bspl)
			.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					INVALID_PROTOCOL_MODIFIER,
					"Invalid modifier 'package' for protocol PROTO");
		}

		@Test
		@DisplayName("Package+public visibility")
		public void packagePublicVisibility() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"package public PROTO {",
					"  role A, B",
					"  parameter out o",
					"  A -> B : M [out o]",
					"}");
			validate(bspl)
			.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					INVALID_PROTOCOL_MODIFIER,
					"Invalid modifier 'public' for protocol PROTO");
		}

		@Test
		@DisplayName("Public+public visibility")
		public void publicPublicVisibility() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"public public PROTO {",
					"  role A, B",
					"  parameter out o",
					"  A -> B : M [out o]",
					"}");
			validate(bspl)
			.assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					UNNECESSARY_PROTOCOL_MODIFIER,
					"Duplicate modifier 'public' for protocol PROTO");
		}

		@Test
		@DisplayName("Package+package visibility")
		public void packagePackageVisibility() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"package package PROTO {",
					"  role A, B",
					"  parameter out o",
					"  A -> B : M [out o]",
					"}");
			validate(bspl)
			.assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					UNNECESSARY_PROTOCOL_MODIFIER,
					"Duplicate modifier 'package' for protocol PROTO");
		}

		@Test
		@DisplayName("Public+package+public visibility")
		public void publicPackagePublicVisibility() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"public package public PROTO {",
					"  role A, B",
					"  parameter out o",
					"  A -> B : M [out o]",
					"}");
			validate(bspl)
			.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					INVALID_PROTOCOL_MODIFIER,
					"Invalid modifier 'package' for protocol PROTO")
			.assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					UNNECESSARY_PROTOCOL_MODIFIER,
					"Duplicate modifier 'public' for protocol PROTO");
		}

		@Test
		@DisplayName("Package+public+package visibility")
		public void packagePublicPackageVisibility() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"package public package PROTO {",
					"  role A, B",
					"  parameter out o",
					"  A -> B : M [out o]",
					"}");
			validate(bspl)
			.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					INVALID_PROTOCOL_MODIFIER,
					"Invalid modifier 'public' for protocol PROTO")
			.assertWarning(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					UNNECESSARY_PROTOCOL_MODIFIER,
					"Duplicate modifier 'package' for protocol PROTO");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("0 protocol")
	@Nested
	public class NoProtocolTest extends AbstractBsplTest {

		@Test
		@DisplayName("empty specification file")
		public void emptyFile() throws Exception {
			var bspl = specification("");
			validate(bspl)
				.assertWarning(
						BsplPackage.eINSTANCE.getBsplProtocolSpecification(),
						EMPTY_PACKAGE_DECLARATION,
						"No package declaration provided");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("1 protocol")
	@Nested
	public class OneProtocolTest extends AbstractBsplTest {

		@Test
		@DisplayName("w/o package w/o import")
		public void withoutPackageWithoutImport() throws Exception {
			var bspl = specification("protocol PROTO { }");
			validate(bspl)
				.assertWarning(
						BsplPackage.eINSTANCE.getBsplProtocolSpecification(),
						EMPTY_PACKAGE_DECLARATION,
						"No package declaration provided")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO is defined without roles, messages and parameters");
		}
	
		@Test
		@DisplayName("w/ package w/o import")
		public void withPackageWithoutImport() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake protocol",
					"PROTO { }");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO is defined without roles, messages and parameters");
		}
	
		@Test
		@DisplayName("w/o package w/ import")
		public void withoutPackageWithImport() throws Exception {
			var bspl = specification(
					"import java.lang.Integer",
					"protocol PROTO { }");
			validate(bspl)
				.assertWarning(
						BsplPackage.eINSTANCE.getBsplProtocolSpecification(),
						EMPTY_PACKAGE_DECLARATION,
						"No package declaration provided")
				.assertWarning(
						XtypePackage.eINSTANCE.getXImportDeclaration(),
						IMPORT_UNUSED,
						"The import 'java.lang.Integer' is never used")
				.assertError(
						BsplPackage.eINSTANCE.getBsplProtocol(),
						EMPTY_PROTOCOL,
						"Protocol PROTO is defined without roles, messages and parameters");
		}
	
		@Test
		@DisplayName("w/ package w/ import")
		public void withPackageWithImport() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"import java.lang.Integer",
					"protocol PROTO { }");
			validate(bspl)
				.assertWarning(
					XtypePackage.eINSTANCE.getXImportDeclaration(),
					IMPORT_UNUSED,
					"The import 'java.lang.Integer' is never used")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO is defined without roles, messages and parameters");
		}

		@Test
		@DisplayName("No role No message")
		public void noRoleNoMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"}");
			validate(bspl)
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO is defined without roles, messages and parameters")
				.assertError(
						BsplPackage.eINSTANCE.getBsplProtocol(),
						REQUIRED_OUT_PARAMETER,
						"Out parameter must be declared for protocol PROTO")
				.assertError(
						BsplPackage.eINSTANCE.getBsplProtocol(),
						REQUIRED_OUT_PARAMETER_IN_MESSAGES,
						"Out parameter must be defined in at least one message for protocol PROTO")
				.assertNoErrors();
		}

		@Test
		@DisplayName("No role")
		public void noRole() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 -> R1 : M",
					"}");
			validate(bspl)
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined source role R1 in the protocol PROTO")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined destination role R1 in the protocol PROTO")
				.assertError(
						BsplPackage.eINSTANCE.getBsplProtocol(),
						MISSED_PROTOCOL_ROLE,
						"Protocol PROTO must contain at least one definition of a role")
				.assertError(
						BsplPackage.eINSTANCE.getBsplProtocol(),
						REQUIRED_OUT_PARAMETER,
						"Out parameter must be declared for protocol PROTO")
				.assertError(
						BsplPackage.eINSTANCE.getBsplProtocol(),
						REQUIRED_OUT_PARAMETER_IN_MESSAGES,
						"Out parameter must be defined in at least one message for protocol PROTO")
				.assertNoErrors();
		}

		@Test
		@DisplayName("No message")
		public void noMessage() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1",
					"}");
			validate(bspl)
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message")
				.assertWarning(
						BsplPackage.eINSTANCE.getBsplProtocolRole(),
						UNUSED_PROTOCOL_ROLE,
						"Unused role R1 in protocol PROTO");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("2 protocols")
	@Nested
	public class TwoProtocolTest extends AbstractBsplTest {

		@Test
		@DisplayName("w/o package w/o import")
		public void withoutPackageWithoutImport() throws Exception {
			var bspl = specification("protocol PROTO1 { } protocol PROTO2 { }");
			validate(bspl)
				.assertWarning(
						BsplPackage.eINSTANCE.getBsplProtocolSpecification(),
						EMPTY_PACKAGE_DECLARATION,
						"No package declaration provided")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO1 is defined without roles, messages and parameters")
				.assertError(
						BsplPackage.eINSTANCE.getBsplProtocol(),
						EMPTY_PROTOCOL,
						"Protocol PROTO2 is defined without roles, messages and parameters");
		}
	
		@Test
		@DisplayName("w/ package w/o import")
		public void withPackageWithoutImport() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake protocol",
					"PROTO1 { } PROTO2 { }");
			validate(bspl)
			.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO1 is defined without roles, messages and parameters")
			.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO2 is defined without roles, messages and parameters");
		}
	
		@Test
		@DisplayName("w/o package w/ import")
		public void withoutPackageWithImport() throws Exception {
			var bspl = specification(
					"import java.lang.Integer",
					"protocol PROTO1 { } protocol PROTO2 { }");
			validate(bspl)
				.assertWarning(
						BsplPackage.eINSTANCE.getBsplProtocolSpecification(),
						EMPTY_PACKAGE_DECLARATION,
						"No package declaration provided")
				.assertWarning(
						XtypePackage.eINSTANCE.getXImportDeclaration(),
						IMPORT_UNUSED,
						"The import 'java.lang.Integer' is never used")
				.assertError(
						BsplPackage.eINSTANCE.getBsplProtocol(),
						EMPTY_PROTOCOL,
						"Protocol PROTO1 is defined without roles, messages and parameters")
				.assertError(
						BsplPackage.eINSTANCE.getBsplProtocol(),
						EMPTY_PROTOCOL,
						"Protocol PROTO2 is defined without roles, messages and parameters");
		}
	
		@Test
		@DisplayName("w/ package w/ import")
		public void withPackageWithImport() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"import java.lang.Integer",
					"protocol PROTO1 { } protocol PROTO2 { }");
			validate(bspl)
				.assertWarning(
					XtypePackage.eINSTANCE.getXImportDeclaration(),
					IMPORT_UNUSED,
					"The import 'java.lang.Integer' is never used")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO1 is defined without roles, messages and parameters")
				.assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO2 is defined without roles, messages and parameters");
		}

		@Test
		@DisplayName("Duplicate protocol")
		public void duplicateProtocol() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.compiler.tests.fake",
					"protocol PROTO1 {",
					"  role R1",
					"  R1 -> R1 : M",
					"}",
					"protocol PROTO1 {",
					"  role R2, R3",
					"  R2 -> R3 : M3",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocol(),
					DUPLICATE_PROTOCOL_NAME,
					"Duplicate protocol specification for PROTO1");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Role definition")
	public class RoleDefinitionTest extends AbstractBsplTest {

		@Test
		@DisplayName("Unknown sender role")
		public void unknownSenderRole() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  role R1, R2",
					"  R3 -> R2 : Message",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined source role R3", "protocol PROTO");

		}

		@Test
		@DisplayName("Unknown receiver role")
		public void unknownReceiverRole() throws Exception {
			var bspl = specification(
					"protocol PROTO {",
					"  role R1, R2",
					"  R1 -> R3 : Message",
					"}");
			validate(bspl).assertError(
					BsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined destination role R3", "protocol PROTO");

		}

	}

}

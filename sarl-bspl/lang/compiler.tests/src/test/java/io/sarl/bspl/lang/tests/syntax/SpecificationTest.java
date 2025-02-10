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
import static org.eclipse.xtext.xbase.validation.IssueCodes.IMPORT_UNUSED;

import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import io.sarl.bspl.lang.sarl_bspl.Sarl_bsplPackage;
import io.sarl.bspl.lang.tests.AbstractSarlBsplTest;

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
	@DisplayName("0 protocol")
	@Nested
	public class NoProtocolTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("empty specification file")
		public void emptyFile() throws Exception {
			var bspl = specification("");
			validate(bspl)
				.assertWarning(
						Sarl_bsplPackage.eINSTANCE.getBsplProtocolSpecification(),
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
	public class OneProtocolTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("w/o package w/o import")
		public void withoutPackageWithoutImport() throws Exception {
			var bspl = specification("protocol PROTO { }");
			validate(bspl)
				.assertWarning(
						Sarl_bsplPackage.eINSTANCE.getBsplProtocolSpecification(),
						EMPTY_PACKAGE_DECLARATION,
						"No package declaration provided")
				.assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
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
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
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
						Sarl_bsplPackage.eINSTANCE.getBsplProtocolSpecification(),
						EMPTY_PACKAGE_DECLARATION,
						"No package declaration provided")
				.assertWarning(
						XtypePackage.eINSTANCE.getXImportDeclaration(),
						IMPORT_UNUSED,
						"The import 'java.lang.Integer' is never used")
				.assertError(
						Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
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
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
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
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO is defined without roles, messages and parameters")
				.assertNoIssues();
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
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined source role R1 in the protocol PROTO")
				.assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolMessage(),
					UNDEFINED_PROTOCOL_ROLE,
					"Undefined destination role R1 in the protocol PROTO")
				.assertError(
						Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
						MISSED_PROTOCOL_ROLE,
						"Protocol PROTO must contain at least one definition of a role")
				.assertNoIssues();
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
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					MISSED_PROTOCOL_MESSAGE,
					"Protocol PROTO must contain at least one definition of a message")
				.assertNoIssues();
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
	public class TwoProtocolTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("w/o package w/o import")
		public void withoutPackageWithoutImport() throws Exception {
			var bspl = specification("protocol PROTO1 { } protocol PROTO2 { }");
			validate(bspl)
				.assertWarning(
						Sarl_bsplPackage.eINSTANCE.getBsplProtocolSpecification(),
						EMPTY_PACKAGE_DECLARATION,
						"No package declaration provided")
				.assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO1 is defined without roles, messages and parameters")
				.assertError(
						Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
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
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO1 is defined without roles, messages and parameters")
			.assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
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
						Sarl_bsplPackage.eINSTANCE.getBsplProtocolSpecification(),
						EMPTY_PACKAGE_DECLARATION,
						"No package declaration provided")
				.assertWarning(
						XtypePackage.eINSTANCE.getXImportDeclaration(),
						IMPORT_UNUSED,
						"The import 'java.lang.Integer' is never used")
				.assertError(
						Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
						EMPTY_PROTOCOL,
						"Protocol PROTO1 is defined without roles, messages and parameters")
				.assertError(
						Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
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
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					EMPTY_PROTOCOL,
					"Protocol PROTO1 is defined without roles, messages and parameters")
				.assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
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
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					DUPLICATE_PROTOCOL_NAME,
					"Duplicate protocol specification for PROTO1");
		}

	}

}

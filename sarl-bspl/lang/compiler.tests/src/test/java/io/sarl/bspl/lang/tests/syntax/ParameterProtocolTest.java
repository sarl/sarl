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

import static io.sarl.bspl.lang.validation.IssueCodes.MISSED_PARAMETER_TYPE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
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
@DisplayName("Protocol parameter")
public class ParameterProtocolTest {

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Default public parameter w/o type")
	public class DefaultPublicTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("Default parameter")
		public void defaultParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN parameter")
		public void inParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter in P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid default IN parameter")
		public void invalidDefaultInParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 in",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'in'");
		}

		@Test
		@DisplayName("OUT parameter")
		public void outParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter out P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT parameter")
		public void invalidOutParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 out",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'out'");
		}

		@Test
		@DisplayName("NIL parameter")
		public void nilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter nil P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL parameter")
		public void invalidNilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 nil",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'nil'");
		}

		@Test
		@DisplayName("OPT parameter")
		public void optParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter opt P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT parameter")
		public void invalidOptParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 opt",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'opt'");
		}

		@Test
		@DisplayName("ANY parameter")
		public void anyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter any P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
				.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY parameter")
		public void invalidAnyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 any",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'any'");
		}

		@Test
		@DisplayName("Invalid private modifier #1")
		public void invalidPrivateParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 private",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '->'");
		}

		@Test
		@DisplayName("Invalid private modifier #2")
		public void invalidPrivateParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter private P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'private'");
		}

		@Test
		@DisplayName("KEY parameter")
		public void keyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN KEY parameter")
		public void inKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter in P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("OUT KEY parameter")
		public void outKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter out P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("NIL KEY parameter")
		public void nilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter nil P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("OPT KEY parameter")
		public void optKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter opt P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("ANY KEY parameter")
		public void anyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter any P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
				.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid key modifier #1")
		public void invalidKeyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter key P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'key'");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Default public parameter w/ type")
	public class DefaultPublicTypeTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("Default parameter")
		public void defaultParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN parameter")
		public void inParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter in P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN parameter #1")
		public void invalidInParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 in : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'in'");
		}

		@Test
		@DisplayName("Invalid IN parameter #2")
		public void invalidInParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double in",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'in'");
		}

		@Test
		@DisplayName("OUT parameter")
		public void outParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter out P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT parameter #1")
		public void invalidOutParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 out : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'out'");
		}

		@Test
		@DisplayName("Invalid OUT parameter #2")
		public void invalidOutParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double out",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'out'");
		}

		@Test
		@DisplayName("NIL parameter")
		public void nilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter nil P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL parameter #1")
		public void invalidNilParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 nil : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'nil'");
		}

		@Test
		@DisplayName("Invalid NIL parameter #2")
		public void invalidNilParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double nil",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'nil'");
		}

		@Test
		@DisplayName("OPT parameter")
		public void optParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter opt P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT parameter #1")
		public void invalidOptParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 opt : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'opt'");
		}

		@Test
		@DisplayName("Invalid OPT parameter #2")
		public void invalidOptParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double opt",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'opt'");
		}

		@Test
		@DisplayName("ANY parameter")
		public void anyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter any P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY parameter #1")
		public void invalidAnyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 any : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'any'");
		}

		@Test
		@DisplayName("Invalid ANY parameter #2")
		public void invalidAnyParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double any",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'any'");
		}

		@Test
		@DisplayName("Invalid private modifier #1")
		public void invalidPrivateParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 private : double",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input ':'");
		}

		@Test
		@DisplayName("Invalid private modifier #2")
		public void invalidPrivateParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double private",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '->'");
		}

		@Test
		@DisplayName("KEY parameter")
		public void keyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid KEY parameter")
		public void invalidKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("IN KEY parameter")
		public void inKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter in P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN KEY parameter")
		public void invalidInKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter in P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("OUT KEY parameter")
		public void outKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter out P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT KEY parameter")
		public void invalidOutKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter out P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("NIL KEY parameter")
		public void nilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter nil P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL KEY parameter")
		public void invalidNilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter nil P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("OPT KEY parameter")
		public void optKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter opt P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT KEY parameter")
		public void invaliOptKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter opt P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("ANY KEY parameter")
		public void anyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter any P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertNull(param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY KEY parameter")
		public void invalidAnyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter any P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("Invalid key modifier #1")
		public void invalidKeyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  parameter key P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'key'");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Public parameter w/o type")
	public class PublicTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("Public parameter modifiers")
		public void publicParameterModifiers() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Public modifier")
		public void publicModifier() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN parameter")
		public void inParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter in P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN parameter")
		public void invalidInParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 in",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'in'");
		}

		@Test
		@DisplayName("OUT parameter")
		public void outParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter out P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT parameter")
		public void invalidOutParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 out",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'out'");
		}

		@Test
		@DisplayName("NIL parameter")
		public void nilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter nil P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL parameter")
		public void invalidNilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 nil",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'nil'");
		}

		@Test
		@DisplayName("OPT parameter")
		public void optParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter opt P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT parameter")
		public void invalidOptParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 opt",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'opt'");
		}

		@Test
		@DisplayName("ANY parameter")
		public void anyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter any P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
				.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY parameter")
		public void invalidAnyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 any",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'any'");
		}

		@Test
		@DisplayName("Invalid private modifier #1")
		public void invalidPrivateParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 private",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '->'");
		}

		@Test
		@DisplayName("Invalid private modifier #2")
		public void invalidPrivateParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter private P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'private'");
		}

		@Test
		@DisplayName("KEY parameter")
		public void keyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN KEY parameter")
		public void inKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter in P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("OUT KEY parameter")
		public void outKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter out P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("NIL KEY parameter")
		public void nilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter nil P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("OPT KEY parameter")
		public void optKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter opt P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("ANY KEY parameter")
		public void anyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter any P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
				.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid key modifier #1")
		public void invalidKeyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter key P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'key'");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Public parameter w/ type")
	public class PublicTypeTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("Public parameter modifiers")
		public void publicParameterModifiers() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Public modifier")
		public void publicModifier() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN parameter")
		public void inParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter in P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN parameter #1")
		public void invalidInParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 in : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'in'");
		}

		@Test
		@DisplayName("Invalid IN parameter #2")
		public void invalidInParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 : double in",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'in'");
		}

		@Test
		@DisplayName("OUT parameter")
		public void outParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter out P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT parameter #1")
		public void invalidOutParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 out : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'out'");
		}

		@Test
		@DisplayName("Invalid OUT parameter #2")
		public void invalidOutParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 : double out",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'out'");
		}

		@Test
		@DisplayName("NIL parameter")
		public void nilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter nil P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL parameter #1")
		public void invalidNilParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 nil : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'nil'");
		}

		@Test
		@DisplayName("Invalid NIL parameter #2")
		public void invalidNilParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 : double nil",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'nil'");
		}

		@Test
		@DisplayName("OPT parameter")
		public void optParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter opt P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT parameter #1")
		public void invalidOptParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 opt : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'opt'");
		}

		@Test
		@DisplayName("Invalid OPT parameter #2")
		public void invalidOptParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 : double opt",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'opt'");
		}

		@Test
		@DisplayName("ANY parameter")
		public void anyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter any P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY parameter #1")
		public void invalidAnyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 any : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'any'");
		}

		@Test
		@DisplayName("Invalid ANY parameter #2")
		public void invalidAnyParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 : double any",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'any'");
		}

		@Test
		@DisplayName("Invalid private modifier #1")
		public void invalidPrivateParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 private : double",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input ':'");
		}

		@Test
		@DisplayName("Invalid private modifier #2")
		public void invalidPrivateParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 : double private",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '->'");
		}

		@Test
		@DisplayName("KEY parameter")
		public void keyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid KEY parameter")
		public void invalidKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("IN KEY parameter")
		public void inKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter in P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN KEY parameter")
		public void invalidInKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter in P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("OUT KEY parameter")
		public void outKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter out P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT KEY parameter")
		public void invalidOutKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter out P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("NIL KEY parameter")
		public void nilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter nil P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL KEY parameter")
		public void invalidNilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter nil P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("OPT KEY parameter")
		public void optKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter opt P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT KEY parameter")
		public void invaliOptKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter opt P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("ANY KEY parameter")
		public void anyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter any P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PUBLIC, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PUBLIC, param.getVisibility());
			assertTrue(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY KEY parameter")
		public void invalidAnyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter any P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("Invalid key modifier #1")
		public void invalidKeyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  public parameter key P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'key'");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Protected parameter w/o type")
	public class ProtectedTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("Protected parameter modifiers")
		public void protectedParameterModifiers() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Protected modifier")
		public void protectedModifier() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN parameter")
		public void inParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter in P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN parameter")
		public void invalidInParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 in",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'in'");
		}

		@Test
		@DisplayName("OUT parameter")
		public void outParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter out P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT parameter")
		public void invalidOutParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 out",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'out'");
		}

		@Test
		@DisplayName("NIL parameter")
		public void nilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter nil P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL parameter")
		public void invalidNilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 nil",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'nil'");
		}

		@Test
		@DisplayName("OPT parameter")
		public void optParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter opt P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT parameter")
		public void invalidOptParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 opt",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'opt'");
		}

		@Test
		@DisplayName("ANY parameter")
		public void anyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter any P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
				.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY parameter")
		public void invalidAnyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 any",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'any'");
		}

		@Test
		@DisplayName("Invalid private modifier #1")
		public void invalidPrivateParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 private",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '->'");
		}

		@Test
		@DisplayName("Invalid private modifier #2")
		public void invalidPrivateParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter private P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'private'");
		}

		@Test
		@DisplayName("KEY parameter")
		public void keyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN KEY parameter")
		public void inKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter in P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("OUT KEY parameter")
		public void outKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter out P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("NIL KEY parameter")
		public void nilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter nil P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("OPT KEY parameter")
		public void optKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter opt P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("ANY KEY parameter")
		public void anyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter any P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
				.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid key modifier #1")
		public void invalidKeyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter key P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'key'");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Protected parameter w/ type")
	public class ProtectedTypeTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("Protected parameter modifiers")
		public void protectedParameterModifiers() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Protected modifier")
		public void protectedModifier() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN parameter")
		public void inParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter in P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN parameter #1")
		public void invalidInParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 in : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'in'");
		}

		@Test
		@DisplayName("Invalid IN parameter #2")
		public void invalidInParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 : double in",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'in'");
		}

		@Test
		@DisplayName("OUT parameter")
		public void outParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter out P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT parameter #1")
		public void invalidOutParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 out : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'out'");
		}

		@Test
		@DisplayName("Invalid OUT parameter #2")
		public void invalidOutParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 : double out",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'out'");
		}

		@Test
		@DisplayName("NIL parameter")
		public void nilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter nil P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL parameter #1")
		public void invalidNilParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 nil : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'nil'");
		}

		@Test
		@DisplayName("Invalid NIL parameter #2")
		public void invalidNilParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 : double nil",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'nil'");
		}

		@Test
		@DisplayName("OPT parameter")
		public void optParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter opt P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT parameter #1")
		public void invalidOptParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 opt : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'opt'");
		}

		@Test
		@DisplayName("Invalid OPT parameter #2")
		public void invalidOptParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 : double opt",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'opt'");
		}

		@Test
		@DisplayName("ANY parameter")
		public void anyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter any P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY parameter #1")
		public void invalidAnyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 any : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'any'");
		}

		@Test
		@DisplayName("Invalid ANY parameter #2")
		public void invalidAnyParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 : double any",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'any'");
		}

		@Test
		@DisplayName("Invalid private modifier #1")
		public void invalidPrivateParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 private : double",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input ':'");
		}

		@Test
		@DisplayName("Invalid private modifier #2")
		public void invalidPrivateParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 : double private",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '->'");
		}

		@Test
		@DisplayName("KEY parameter")
		public void keyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid KEY parameter")
		public void invalidKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("IN KEY parameter")
		public void inKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter in P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN KEY parameter")
		public void invalidInKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter in P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("OUT KEY parameter")
		public void outKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter out P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT KEY parameter")
		public void invalidOutKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter out P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("NIL KEY parameter")
		public void nilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter nil P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL KEY parameter")
		public void invalidNilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter nil P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("OPT KEY parameter")
		public void optKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter opt P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT KEY parameter")
		public void invaliOptKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter opt P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("ANY KEY parameter")
		public void anyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter any P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PROTECTED, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PROTECTED, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertTrue(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY KEY parameter")
		public void invalidAnyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter any P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("Invalid key modifier #1")
		public void invalidKeyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  protected parameter key P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'key'");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Package parameter w/o type")
	public class PackageTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("Package parameter modifiers")
		public void packageParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Package modifier")
		public void packageModifier() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN parameter")
		public void inParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter in P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN parameter")
		public void invalidInParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 in",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'in'");
		}

		@Test
		@DisplayName("OUT parameter")
		public void outParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter out P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT parameter")
		public void invalidOutParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 out",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'out'");
		}

		@Test
		@DisplayName("NIL parameter")
		public void nilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter nil P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL parameter")
		public void invalidNilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 nil",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'nil'");
		}

		@Test
		@DisplayName("OPT parameter")
		public void optParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter opt P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT parameter")
		public void invalidOptParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 opt",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'opt'");
		}

		@Test
		@DisplayName("ANY parameter")
		public void anyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter any P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
				.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY parameter")
		public void invalidAnyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 any",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'any'");
		}

		@Test
		@DisplayName("Invalid private modifier #1")
		public void invalidPrivateParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 private",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '->'");
		}

		@Test
		@DisplayName("Invalid private modifier #2")
		public void invalidPrivateParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter private P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'private'");
		}

		@Test
		@DisplayName("KEY parameter")
		public void keyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN KEY parameter")
		public void inKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter in P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("OUT KEY parameter")
		public void outKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter out P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("NIL KEY parameter")
		public void nilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter nil P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("OPT KEY parameter")
		public void optKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter opt P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("ANY KEY parameter")
		public void anyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter any P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
				.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid key modifier #1")
		public void invalidKeyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter key P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'key'");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Package parameter w/ type")
	public class PackageTypeTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("Package parameter modifiers")
		public void packageParameterModifiers() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Package modifier")
		public void packageModifier() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN parameter")
		public void inParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter in P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN parameter #1")
		public void invalidInParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 in : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'in'");
		}

		@Test
		@DisplayName("Invalid IN parameter #2")
		public void invalidInParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 : double in",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'in'");
		}

		@Test
		@DisplayName("OUT parameter")
		public void outParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter out P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT parameter #1")
		public void invalidOutParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 out : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'out'");
		}

		@Test
		@DisplayName("Invalid OUT parameter #2")
		public void invalidOutParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 : double out",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'out'");
		}

		@Test
		@DisplayName("NIL parameter")
		public void nilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter nil P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL parameter #1")
		public void invalidNilParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 nil : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'nil'");
		}

		@Test
		@DisplayName("Invalid NIL parameter #2")
		public void invalidNilParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 : double nil",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'nil'");
		}

		@Test
		@DisplayName("OPT parameter")
		public void optParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter opt P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT parameter #1")
		public void invalidOptParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 opt : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'opt'");
		}

		@Test
		@DisplayName("Invalid OPT parameter #2")
		public void invalidOptParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 : double opt",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'opt'");
		}

		@Test
		@DisplayName("ANY parameter")
		public void anyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter any P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY parameter #1")
		public void invalidAnyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 any : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'any'");
		}

		@Test
		@DisplayName("Invalid ANY parameter #2")
		public void invalidAnyParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 : double any",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'any'");
		}

		@Test
		@DisplayName("Invalid private modifier #1")
		public void invalidPrivateParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 private : double",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input ':'");
		}

		@Test
		@DisplayName("Invalid private modifier #2")
		public void invalidPrivateParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 : double private",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '->'");
		}

		@Test
		@DisplayName("KEY parameter")
		public void keyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid KEY parameter")
		public void invalidKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("IN KEY parameter")
		public void inKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter in P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN KEY parameter")
		public void invalidInKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter in P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("OUT KEY parameter")
		public void outKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter out P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT KEY parameter")
		public void invalidOutKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter out P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("NIL KEY parameter")
		public void nilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter nil P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL KEY parameter")
		public void invalidNilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter nil P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("OPT KEY parameter")
		public void optKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter opt P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT KEY parameter")
		public void invaliOptKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter opt P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("ANY KEY parameter")
		public void anyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter any P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.DEFAULT, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.DEFAULT, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertTrue(param.isPackageVisibility());
			assertFalse(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY KEY parameter")
		public void invalidAnyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter any P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("Invalid key modifier #1")
		public void invalidKeyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  package parameter key P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'key'");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Private parameter w/o type")
	public class PrivateTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("Private parameter modifiers")
		public void privateParameterModifiers() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Private modifier")
		public void privateModifier() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN parameter")
		public void inParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter in P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN parameter")
		public void invalidInParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 in",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'in'");
		}

		@Test
		@DisplayName("OUT parameter")
		public void outParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter out P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT parameter")
		public void invalidOutParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 out",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'out'");
		}

		@Test
		@DisplayName("NIL parameter")
		public void nilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter nil P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL parameter")
		public void invalidNilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 nil",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'nil'");
		}

		@Test
		@DisplayName("OPT parameter")
		public void optParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter opt P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT parameter")
		public void invalidOptParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 opt",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'opt'");
		}

		@Test
		@DisplayName("ANY parameter")
		public void anyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter any P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
				.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY parameter")
		public void invalidAnyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 any",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'any'");
		}

		@Test
		@DisplayName("Invalid private modifier #1")
		public void invalidPrivateParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 private",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '->'");
		}

		@Test
		@DisplayName("Invalid private modifier #2")
		public void invalidPrivateParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter private P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'private'");
		}

		@Test
		@DisplayName("KEY parameter")
		public void keyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN KEY parameter")
		public void inKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter in P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("OUT KEY parameter")
		public void outKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter out P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("NIL KEY parameter")
		public void nilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter nil P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("OPT KEY parameter")
		public void optKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter opt P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
			.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("ANY KEY parameter")
		public void anyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter any P1 key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertWarning(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					MISSED_PARAMETER_TYPE,
					"Missed type for the parameter P1")
				.assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertNull(param.getType());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid key modifier #1")
		public void invalidKeyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter key P1",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'key'");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Private parameter w/ type")
	public class PrivateTypeTest extends AbstractSarlBsplTest {

		@Test
		@DisplayName("Private parameter modifiers")
		public void privateParameterModifiers() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Private modifier")
		public void privateModifier() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("IN parameter")
		public void inParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter in P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN parameter #1")
		public void invalidInParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 in : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'in'");
		}

		@Test
		@DisplayName("Invalid IN parameter #2")
		public void invalidInParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 : double in",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'in'");
		}

		@Test
		@DisplayName("OUT parameter")
		public void outParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter out P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT parameter #1")
		public void invalidOutParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 out : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'out'");
		}

		@Test
		@DisplayName("Invalid OUT parameter #2")
		public void invalidOutParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 : double out",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'out'");
		}

		@Test
		@DisplayName("NIL parameter")
		public void nilParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter nil P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL parameter #1")
		public void invalidNilParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 nil : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'nil'");
		}

		@Test
		@DisplayName("Invalid NIL parameter #2")
		public void invalidNilParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 : double nil",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'nil'");
		}

		@Test
		@DisplayName("OPT parameter")
		public void optParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter opt P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT parameter #1")
		public void invalidOptParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 opt : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'opt'");
		}

		@Test
		@DisplayName("Invalid OPT parameter #2")
		public void invalidOptParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 : double opt",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'opt'");
		}

		@Test
		@DisplayName("ANY parameter")
		public void anyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter any P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertFalse(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY parameter #1")
		public void invalidAnyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 any : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input 'any'");
		}

		@Test
		@DisplayName("Invalid ANY parameter #2")
		public void invalidAnyParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 : double any",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"no viable alternative at input 'any'");
		}

		@Test
		@DisplayName("Invalid private modifier #1")
		public void invalidPrivateParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 private : double",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input ':'");
		}

		@Test
		@DisplayName("Invalid private modifier #2")
		public void invalidPrivateParameter_2() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 : double private",
					"  R1 -> R2 : M",
					"}");
			// Two parameters are recognized: P1 and R1
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input '->'");
		}

		@Test
		@DisplayName("KEY parameter")
		public void keyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid KEY parameter")
		public void invalidKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("IN KEY parameter")
		public void inKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter in P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertTrue(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid IN KEY parameter")
		public void invalidInKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter in P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("OUT KEY parameter")
		public void outKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter out P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertTrue(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OUT KEY parameter")
		public void invalidOutKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter out P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("NIL KEY parameter")
		public void nilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter nil P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertTrue(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid NIL KEY parameter")
		public void invalidNilKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter nil P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("OPT KEY parameter")
		public void optKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter opt P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertFalse(param.isAny());
			assertFalse(param.isNil());
			assertTrue(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid OPT KEY parameter")
		public void invaliOptKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter opt P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("ANY KEY parameter")
		public void anyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter any P1 : double key",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertNoIssues();
			//
			var param = bspl.getBsplProtocols().get(0).getParameters().get(0);
			assertEquals("double", param.getType().getIdentifier());
			assertEquals("P1", param.getName());
			assertFalse(param.isInput());
			assertTrue(param.isKey());
			assertTrue(param.isAny());
			assertFalse(param.isNil());
			assertFalse(param.isOptional());
			assertFalse(param.isOutput());
			assertSame(JvmVisibility.PRIVATE, param.getSpecifiedVisibility());
			assertSame(JvmVisibility.PRIVATE, param.getVisibility());
			assertFalse(param.isPublicVisibility());
			assertFalse(param.isProtectedVisibility());
			assertFalse(param.isPackageVisibility());
			assertTrue(param.isPrivateVisibility());
		}

		@Test
		@DisplayName("Invalid ANY KEY parameter")
		public void invalidAnyKeyParameter() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter any P1 key : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocol(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"mismatched input ':'");
		}

		@Test
		@DisplayName("Invalid key modifier #1")
		public void invalidKeyParameter_1() throws Exception {
			var bspl = specification(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  private parameter key P1 : double",
					"  R1 -> R2 : M",
					"}");
			validate(bspl).assertError(
					Sarl_bsplPackage.eINSTANCE.getBsplProtocolParameter(),
					Diagnostic.SYNTAX_DIAGNOSTIC,
					"extraneous input 'key'");
		}

	}

}

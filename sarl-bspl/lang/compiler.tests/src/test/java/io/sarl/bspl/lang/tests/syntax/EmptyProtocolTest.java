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

import static io.sarl.bspl.lang.validation.IssueCodes.EMPTY_PROTOCOL;

import org.junit.jupiter.api.DisplayName;
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
@DisplayName("Empty protocol")
public class EmptyProtocolTest extends AbstractBsplTest {

	@Test
	@DisplayName("Original BSPL syntax")
	public void onlyOriginBsplProtocol() throws Exception {
		var bspl = specification(
				"PROTO {",
				"}");
		validate(bspl).assertError(
				BsplPackage.eINSTANCE.getBsplProtocol(),
				EMPTY_PROTOCOL,
				"Protocol PROTO is defined without roles, messages and parameters");

	}

	@Test
	@DisplayName("Only SARL BSPL protocol")
	public void onlySarlBsplProtocol() throws Exception {
		var bspl = specification(
				"protocol PROTO {",
				"}");
		validate(bspl).assertError(
				BsplPackage.eINSTANCE.getBsplProtocol(),
				EMPTY_PROTOCOL,
				"Protocol PROTO is defined without roles, messages and parameters");
	}

	@Test
	@DisplayName("Only SARL BSPL protocol - public")
	public void onlySarlBsplProtocol_public() throws Exception {
		var bspl = specification(
				"public protocol PROTO {",
				"}");
		validate(bspl).assertError(
				BsplPackage.eINSTANCE.getBsplProtocol(),
				EMPTY_PROTOCOL,
				"Protocol PROTO is defined without roles, messages and parameters");
	}

	@Test
	@DisplayName("Only SARL BSPL protocol - package")
	public void onlySarlBsplProtocol_package() throws Exception {
		var bspl = specification(
				"package protocol PROTO {",
				"}");
		validate(bspl).assertError(
				BsplPackage.eINSTANCE.getBsplProtocol(),
				EMPTY_PROTOCOL,
				"Protocol PROTO is defined without roles, messages and parameters");
	}

}

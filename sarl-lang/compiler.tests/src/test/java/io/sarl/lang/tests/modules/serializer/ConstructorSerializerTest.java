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
package io.sarl.lang.tests.modules.serializer;

import static io.sarl.lang.tests.api.tools.TestEObjects.agent;
import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 */
@DisplayName("serialization: new")
@Tag("core")
@Tag("serialization")
@SuppressWarnings("all")
public class ConstructorSerializerTest extends AbstractSerializerTest {

	@Test
	public void noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"new() { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void noParDelim_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"new { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void param_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"new(b : int) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void params_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"new(b : int, c : float) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramDef_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"new(b : int = 5) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDef_noSuper_0() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"new(b : int = 6, c : float) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDef_noSuper_1() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"new(b : int, c : float = 5.9f) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDef_noSuper_2() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"new(b : int = 8, c : float = 5.9f) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsVar_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"new(b : int, c : float*) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDefVar_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"new(b : int = 6, c : float*) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void noParDelim_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"new { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void param_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"new(b : int) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void params_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"new(b : int, c : float) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramDef_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"new(b : int = 5) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDef_super_0() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"new(b : int = 6, c : float) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDef_super_1() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"new(b : int, c : float = 5.9f) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDef_super_2() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"new(b : int = 8, c : float = 5.9f) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsVar_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"new(b : int, c : float*) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDefVar_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"new(b : int = 6, c : float*) { super(null, null) }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

}

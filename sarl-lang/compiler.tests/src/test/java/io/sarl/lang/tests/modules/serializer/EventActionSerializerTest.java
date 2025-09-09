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
 * @author $Author: sgalland$
 * @version compiler.tests 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 */
@DisplayName("serialization: fires in action prototypes")
@Tag("core")
@Tag("serialization")
@SuppressWarnings("all")
public class EventActionSerializerTest extends AbstractSerializerTest {

	@Test
	public void noPar_noReturn_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void noParam_noReturn_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct() fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void param_noReturn_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct(a : int) fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramDef_noReturn_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct(a : int = 5) fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramVar_noReturn_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct(a : int*) fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDef_noReturn_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct(a : int = 6, b : float) fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsVar_noReturn_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct(a : int = 6, b : float*) fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void noPar_return_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void noParam_return_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct() : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void param_return_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct(a : int) : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramDef_return_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct(a : int = 5) : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramVar_return_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct(a : int*) : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDef_return_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct(a : int = 6, b : float) : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsVar_return_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"def fct(a : int = 6, b : float*) : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void noPar_noReturn_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void noParam_noReturn_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct() fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void param_noReturn_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct(a : int) fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramDef_noReturn_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct(a : int = 5) fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramVar_noReturn_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct(a : int*) fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDef_noReturn_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct(a : int = 6, b : float) fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsVar_noReturn_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct(a : int = 6, b : float*) fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void noPar_return_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void noParam_return_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct() : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void param_return_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct(a : int) : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramDef_return_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct(a : int = 5) : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramVar_return_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct(a : int*) : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsDef_return_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct(a : int = 6, b : float) : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void paramsVar_return_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {", //$NON-NLS-1$
				"def fct(a : int = 6, b : float*) : float fires foo.Event1, foo.Event2 { 1 }", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

}

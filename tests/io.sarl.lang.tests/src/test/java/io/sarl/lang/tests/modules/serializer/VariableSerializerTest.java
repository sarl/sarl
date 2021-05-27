/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

import static io.sarl.tests.api.tools.TestEObjects.agent;
import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

/**
 * @author <a href="http://www.ciad-lab.fr/author-10836/">St&eacute;phane Galland</a>
 * @version io.sarl.lang.tests 0.12.0 20210527-171007
 * @mavengroupid io.sarl.lang
 * @mavenartifactid io.sarl.lang.tests
 */
@DisplayName("serialization: var")
@Tag("core")
@Tag("serialization")
public class VariableSerializerTest extends AbstractSerializerTest {

	@Test
	public void variable_expr_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {",
				"var foo = 6.0f",
				"}");
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void variable_type_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {",
				"var foo : float",
				"}");
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void variable_typeExpr_noSuper() throws Exception {
		String s = multilineString(
				"agent Foo {",
				"var foo : float = 6.0f",
				"}");
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void variable_expr_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {",
				"var foo = 6.0f",
				"}");
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void variable_type_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {",
				"var foo : float",
				"}");
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void variable_typeExpr_super() throws Exception {
		String s = multilineString(
				"agent Foo extends foo.ecore.SubAgent {",
				"var foo : float = 6.0f",
				"}");
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

}

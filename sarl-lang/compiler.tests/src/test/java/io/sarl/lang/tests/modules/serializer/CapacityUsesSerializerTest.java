/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
 * @version compiler.tests 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 */
@DisplayName("serialization: uses")
@Tag("core")
@Tag("serialization")
@SuppressWarnings("javadoc")
public class CapacityUsesSerializerTest extends AbstractSerializerTest {

	@Test
	public void one_one() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"uses foo.ecore.SubCapacity", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void one_two() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"uses foo.ecore.SubCapacity, foo.ecore.SubCapacity2", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void two_one() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"uses foo.ecore.SubCapacity", //$NON-NLS-1$
				"uses foo.ecore.SubCapacity2", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

	@Test
	public void two_two() throws Exception {
		String s = multilineString(
				"agent Foo {", //$NON-NLS-1$
				"uses foo.ecore.SubCapacity, foo.ecore.SubCapacity2", //$NON-NLS-1$
				"uses foo.ecore.SubCapacity3", //$NON-NLS-1$
				"}"); //$NON-NLS-1$
		this.object = agent(getParseHelper(), getValidationHelper(), s);
		assertSerialize(s);
	}

}

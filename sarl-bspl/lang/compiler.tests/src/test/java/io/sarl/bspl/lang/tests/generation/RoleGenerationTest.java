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
package io.sarl.bspl.lang.tests.generation;

import static io.sarl.bspl.lang.validation.IssueCodes.EMPTY_PROTOCOL;
import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import io.sarl.bspl.lang.sarl_bspl.Sarl_bsplPackage;
import io.sarl.bspl.lang.tests.AbstractSarlBsplTest;
import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Role generation")
public class RoleGenerationTest extends AbstractSarlBsplTest {

	@Test
	@DisplayName("Role enumeration")
	public void onlyOriginBsplProtocol() throws Exception {
		var source = multilineString(
				"package io.sarl.bspl.lang.tests",
				"protocol PROTO {",
				"  role R1, R2",
				"  role R3",
				"  R1 -> R2 : M",
				"}");
		var expected = multilineString(
				""
				);
		getBsplCompileHelper().assertCompilesTo(source, expected);
	}

}

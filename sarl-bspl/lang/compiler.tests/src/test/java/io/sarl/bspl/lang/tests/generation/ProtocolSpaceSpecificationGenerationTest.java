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

import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import io.sarl.bspl.lang.tests.AbstractBsplTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Sapce specification generation")
public class ProtocolSpaceSpecificationGenerationTest extends AbstractBsplTest {

	@Test
	@DisplayName("SARL syntax - default visibility")
	public void sarlDefault() throws Exception {
		var source = multilineString(
				"package io.sarl.bspl.lang.tests",
				"protocol PROTO {",
				"  role R1, R2",
				"  role R3",
				"  R1 -> R2 : M",
				"}");
		var expected = multilineString(
				"/* This file was automatically generated. Do not change its content. */",
				"",
				"package io.sarl.bspl.lang.tests",
				"",
				"import io.sarl.bspl.api.protocol.impl.AbstractProtocolSpaceSpecification",
				"import io.sarl.bspl.api.protocol.impl.ProtocolRole",
				"",
				"public class PROTOSpaceSpecification extends AbstractProtocolSpaceSpecification {",
				"  override getRoles : ProtocolRole[] {",
				"    PROTORole.values",
				"  }",
				"}"
				);
		getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTOSpaceSpecification", expected);
	}

	@Test
	@DisplayName("SARL syntax - public visibility")
	public void sarlPublic() throws Exception {
		var source = multilineString(
				"package io.sarl.bspl.lang.tests",
				"public protocol PROTO {",
				"  role R1, R2",
				"  role R3",
				"  R1 -> R2 : M",
				"}");
		var expected = multilineString(
				"/* This file was automatically generated. Do not change its content. */",
				"",
				"package io.sarl.bspl.lang.tests",
				"",
				"import io.sarl.bspl.api.protocol.impl.AbstractProtocolSpaceSpecification",
				"import io.sarl.bspl.api.protocol.impl.ProtocolRole",
				"",
				"public class PROTOSpaceSpecification extends AbstractProtocolSpaceSpecification {",
				"  override getRoles : ProtocolRole[] {",
				"    PROTORole.values",
				"  }",
				"}"
				);
		getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTOSpaceSpecification", expected);
	}

	@Test
	@DisplayName("SARL syntax - package visibility")
	public void sarlPackage() throws Exception {
		var source = multilineString(
				"package io.sarl.bspl.lang.tests",
				"package protocol PROTO {",
				"  role R1, R2",
				"  role R3",
				"  R1 -> R2 : M",
				"}");
		var expected = multilineString(
				"/* This file was automatically generated. Do not change its content. */",
				"",
				"package io.sarl.bspl.lang.tests",
				"",
				"import io.sarl.bspl.api.protocol.impl.AbstractProtocolSpaceSpecification",
				"import io.sarl.bspl.api.protocol.impl.ProtocolRole",
				"",
				"package class PROTOSpaceSpecification extends AbstractProtocolSpaceSpecification {",
				"  override getRoles : ProtocolRole[] {",
				"    PROTORole.values",
				"  }",
				"}"
				);
		getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTOSpaceSpecification", expected);
	}

	@Test
	@DisplayName("BSPL syntax - default visibility")
	public void bsplDefault() throws Exception {
		var source = multilineString(
				"package io.sarl.bspl.lang.tests",
				"PROTO {",
				"  role R1, R2",
				"  role R3",
				"  R1 -> R2 : M",
				"}");
		var expected = multilineString(
				"/* This file was automatically generated. Do not change its content. */",
				"",
				"package io.sarl.bspl.lang.tests",
				"",
				"import io.sarl.bspl.api.protocol.impl.AbstractProtocolSpaceSpecification",
				"import io.sarl.bspl.api.protocol.impl.ProtocolRole",
				"",
				"public class PROTOSpaceSpecification extends AbstractProtocolSpaceSpecification {",
				"  override getRoles : ProtocolRole[] {",
				"    PROTORole.values",
				"  }",
				"}"
				);
		getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTOSpaceSpecification", expected);
	}

	@Test
	@DisplayName("BSPL syntax - public visibility")
	public void bsplPublic() throws Exception {
		var source = multilineString(
				"package io.sarl.bspl.lang.tests",
				"public PROTO {",
				"  role R1, R2",
				"  role R3",
				"  R1 -> R2 : M",
				"}");
		var expected = multilineString(
				"/* This file was automatically generated. Do not change its content. */",
				"",
				"package io.sarl.bspl.lang.tests",
				"",
				"import io.sarl.bspl.api.protocol.impl.AbstractProtocolSpaceSpecification",
				"import io.sarl.bspl.api.protocol.impl.ProtocolRole",
				"",
				"public class PROTOSpaceSpecification extends AbstractProtocolSpaceSpecification {",
				"  override getRoles : ProtocolRole[] {",
				"    PROTORole.values",
				"  }",
				"}"
				);
		getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTOSpaceSpecification", expected);
	}

	@Test
	@DisplayName("BSPL syntax - package visibility")
	public void bsplPackage() throws Exception {
		var source = multilineString(
				"package io.sarl.bspl.lang.tests",
				"package PROTO {",
				"  role R1, R2",
				"  role R3",
				"  R1 -> R2 : M",
				"}");
		var expected = multilineString(
				"/* This file was automatically generated. Do not change its content. */",
				"",
				"package io.sarl.bspl.lang.tests",
				"",
				"import io.sarl.bspl.api.protocol.impl.AbstractProtocolSpaceSpecification",
				"import io.sarl.bspl.api.protocol.impl.ProtocolRole",
				"",
				"package class PROTOSpaceSpecification extends AbstractProtocolSpaceSpecification {",
				"  override getRoles : ProtocolRole[] {",
				"    PROTORole.values",
				"  }",
				"}"
				);
		getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTOSpaceSpecification", expected);
	}

}

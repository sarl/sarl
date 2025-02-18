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
				"/* This file was automatically generated. Do not change its content. */",
				"",
				"package io.sarl.bspl.lang.tests;",
				"",
				"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
				"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
				"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
				"import io.sarl.bspl.lang.tests.proto_adapters.R1ProtocolCapacity;",
				"import io.sarl.bspl.lang.tests.proto_adapters.R1ProtocolReactiveBehavior;",
				"import io.sarl.bspl.lang.tests.proto_adapters.R1ProtocolSkill;",
				"import io.sarl.bspl.lang.tests.proto_adapters.R2ProtocolCapacity;",
				"import io.sarl.bspl.lang.tests.proto_adapters.R2ProtocolReactiveBehavior;",
				"import io.sarl.bspl.lang.tests.proto_adapters.R2ProtocolSkill;",
				"import io.sarl.bspl.lang.tests.proto_adapters.R3ProtocolCapacity;",
				"import io.sarl.bspl.lang.tests.proto_adapters.R3ProtocolReactiveBehavior;",
				"import io.sarl.bspl.lang.tests.proto_adapters.R3ProtocolSkill;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.Behavior;",
				"import io.sarl.lang.core.Skill;",
				"",
				"public enum PROTORole implements ProtocolRole {",
				"  R1 {",
				"    public Class<? extends ProtocolCapacity> getProtocolCapacity() {",
				"      return R1ProtocolCapacity.class;",
				"    }",
				"    public Skill getProtocolSkill(ProtocolSpace space) {",
				"      return new R1ProtocolSkill(space);",
				"    }",
				"    public Behavior getProtocolBehavior(Agent ag) {",
				"      return new R1ProtocolReactiveBehavior(ag);",
				"    }",
				"    public int  getMinCardinality() {",
				"      return 0;",
				"    }",
				"    public int  getMaxCardinality() {",
				"      return Integer.MAX_VALUE;",
				"    }",
				"  },",
				"  R2 {",
				"    public Class<? extends ProtocolCapacity> getProtocolCapacity() {",
				"      return R2ProtocolCapacity.class;",
				"    }",
				"    public Skill getProtocolSkill(ProtocolSpace space) {",
				"      return new R2ProtocolSkill(space);",
				"    }",
				"    public Behavior getProtocolBehavior(Agent ag) {",
				"      return new R2ProtocolReactiveBehavior(ag);",
				"    }",
				"    public int  getMinCardinality() {",
				"      return 0;",
				"    }",
				"    public int  getMaxCardinality() {",
				"      return Integer.MAX_VALUE;",
				"    }",
				"  },",
				"  R3 {",
				"    public Class<? extends ProtocolCapacity> getProtocolCapacity() {",
				"      return R3ProtocolCapacity.class;",
				"    }",
				"    public Skill getProtocolSkill(ProtocolSpace space) {",
				"      return new R3ProtocolSkill(space);",
				"    }",
				"    public Behavior getProtocolBehavior(Agent ag) {",
				"      return new R3ProtocolReactiveBehavior(ag);",
				"    }",
				"    public int  getMinCardinality() {",
				"      return 0;",
				"    }",
				"    public int  getMaxCardinality() {",
				"      return Integer.MAX_VALUE;",
				"    }",
				"  };",
				"}"
				);
		getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
	}

}

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

import static io.sarl.bspl.lang.validation.IssueCodes.MISSED_PROTOCOL_MESSAGE;
import static io.sarl.bspl.lang.validation.IssueCodes.UNNECESSARY_ROLE_CARDINALITY;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.eclipse.xtext.diagnostics.Diagnostic;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
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
@DisplayName("Role generation")
public class ProtocolRoleEnumerationGenerationTest {

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Protocol Modifiers")
	public class ProtocolModifierTest extends AbstractBsplTest {

		@Test
		@DisplayName("Default visibility")
		public void defaultVisibility() throws Exception {
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
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Public visibility")
		public void publicVisibility() throws Exception {
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
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}


		@Test
		@DisplayName("Package visibility")
		public void packageVisibility() throws Exception {
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
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Behavior;",
					"import io.sarl.lang.core.Skill;",
					"",
					"enum PROTORole implements ProtocolRole {",
					"  R1 {",
					"    public Class<? extends ProtocolCapacity> getProtocolCapacity() {",
					"      return R1ProtocolCapacity.class;",
					"    }",
					"    public Skill getProtocolSkill(ProtocolSpace space) {",
					"      return new R1ProtocolSkill(space);",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Declarations")
	public class DeclarationTest extends AbstractBsplTest {

		@Test
		@DisplayName("Monodirectional")
		public void monodirectional() throws Exception {
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
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}
	
		@Test
		@DisplayName("Transitive monodirectional")
		public void transitiveMonodirectional() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  role R3",
					"  R1 -> R2 : M",
					"  R2 -> R3 : M2",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"  },",
					"  R3 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R3ProtocolReactiveBehavior(ag);",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}
	
		@Test
		@DisplayName("Bidirectional")
		public void bidirectional() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  role R3",
					"  R1 -> R2 : M",
					"  R2 -> R1 : M2",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}
	
		@Test
		@DisplayName("Transitive bidirectional")
		public void transitiveBidirectional() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1, R2",
					"  role R3",
					"  R1 -> R2 : M",
					"  R2 -> R3 : M2",
					"  R3 -> R2 : M3",
					"  R2 -> R1 : M4",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Max cardinality")
	public class MaxCardinalityTest extends AbstractBsplTest {

		@Test
		@DisplayName("Single line notation w/o keyword")
		public void maxCardinality_1() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [5], R2 [6]",
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
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}
	
		@Test
		@DisplayName("Single line notation w/ keyword")
		public void maxCardinality_2() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1 [5], R2 [6]",
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
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}
	
		@Test
		@DisplayName("Multiline notation w/ keyword")
		public void maxCardinality_3() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1 [5]",
					"  role R2 [6]",
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
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Multiline notation w/o keyword")
		public void maxCardinality_4() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [5]",
					"  R2 [6]",
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
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Multiline notation w/ w/o keyword")
		public void maxCardinality_5() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1 [5]",
					"  R2 [6]",
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
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Multiline notation w/0 w/ keyword")
		public void maxCardinality_6() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [5]",
					"  role R2 [6]",
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
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Zero max")
		public void zeroMax() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"PROTO {",
					"  R1 [0]",
					"  R1 -> R1 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"    public int  getMaxCardinality() {",
					"      return 0;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Negative max")
		public void negativeMax() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [-4]",
					"  R1 -> R1 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"    public int  getMaxCardinality() {",
					"      return 4;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Min-Max cardinality")
	public class MinMaxCardinalityTest extends AbstractBsplTest {

		@Test
		@DisplayName("Single line notation w/o keyword")
		public void singleLine_1() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [4..5], R2 [1..6]",
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
					"    public int  getMinCardinality() {",
					"      return 4;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMinCardinality() {",
					"      return 1;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Single line notation w/ keyword")
		public void singleLine_2() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1 [4..5], R2 [1..6]",
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
					"    public int  getMinCardinality() {",
					"      return 4;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMinCardinality() {",
					"      return 1;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Multiple lines notation w/o keyword")
		public void multipleLines_1() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [4..5]",
					"  R2 [1..6]",
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
					"    public int  getMinCardinality() {",
					"      return 4;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMinCardinality() {",
					"      return 1;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Multiple lines notation w/ keyword")
		public void multipleLines_2() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1 [4..5]",
					"  role R2 [1..6]",
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
					"    public int  getMinCardinality() {",
					"      return 4;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMinCardinality() {",
					"      return 1;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Multiple lines notation w/ w/o keyword")
		public void multipleLines_3() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  role R1 [4..5]",
					"  R2 [1..6]",
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
					"    public int  getMinCardinality() {",
					"      return 4;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMinCardinality() {",
					"      return 1;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Multiple lines notation w/o w/ keyword")
		public void multipleLines_4() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [4..5]",
					"  role R2 [1..6]",
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
					"    public int  getMinCardinality() {",
					"      return 4;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 5;",
					"    }",
					"  },",
					"  R2 {",
					"    public Behavior getProtocolBehavior(Agent ag) {",
					"      return new R2ProtocolReactiveBehavior(ag);",
					"    }",
					"    public int  getMinCardinality() {",
					"      return 1;",
					"    }",
					"    public int  getMaxCardinality() {",
					"      return 6;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Zero-positive cardinalities")
		public void cardinalities_0() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [0..5]",
					"  R1 -> R1 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"      return 5;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Zero-zero cardinalities")
		public void cardinalities_1() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [0..0]",
					"  R1 -> R1 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"      return 0;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Zero-negative cardinalities")
		public void cardinalities_2() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [0..-1]",
					"  R1 -> R1 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"      return 1;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Negative-positive cardinalities")
		public void cardinalities_3() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [-1..5]",
					"  R1 -> R1 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"    public int  getMaxCardinality() {",
					"      return 1;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Negative-zero cardinalities")
		public void cardinalities_4() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [-4..0]",
					"  R1 -> R1 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"    public int  getMaxCardinality() {",
					"      return 4;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

		@Test
		@DisplayName("Negative-negative cardinalities")
		public void cardinalities_5() throws Exception {
			var source = multilineString(
					"package io.sarl.bspl.lang.tests",
					"protocol PROTO {",
					"  R1 [-8..-1]",
					"  R1 -> R1 : M",
					"}");
			var expected = multilineString(
					"/* This file was automatically generated. Do not change its content. */",
					"",
					"package io.sarl.bspl.lang.tests;",
					"",
					"import io.sarl.bspl.api.protocol.impl.ProtocolCapacity;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolRole;",
					"import io.sarl.bspl.api.protocol.impl.ProtocolSpace;",
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
					"    public int  getMaxCardinality() {",
					"      return 8;",
					"    }",
					"  };",
					"}"
					);
			getCompileHelper().assertCompilesTo(source, "io.sarl.bspl.lang.tests.PROTORole", expected);
		}

	}

}

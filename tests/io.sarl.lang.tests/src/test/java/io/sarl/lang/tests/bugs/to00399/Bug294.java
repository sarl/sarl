/*
 * Copyright 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.bugs.to00399;

import static io.sarl.tests.api.AbstractSarlTest.assertContains;
import static io.sarl.tests.api.AbstractSarlTest.multilineString;
import static org.junit.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.Collection;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	Bug294.CompilerTests.class,
})
@SuppressWarnings("all")
public class Bug294 {

	private static final String capacityCode = multilineString(
			"capacity PhysicEnvironment {",
			"	def influenceKinematic(linearInfluence : String = null, angularInfluence : float = 0f, otherInfluences : Object*)",
			"	def influenceSteering(linearInfluence : String = null, angularInfluence : float = 0f, otherInfluences : Object*)",
			"}");

	private static final String skillCode = multilineString(
			"skill StandardPhysicEnvironment implements PhysicEnvironment {",
			"	def influenceKinematic(linearInfluence : String = null, angularInfluence : float = 0f, otherInfluences : Object*) {",
			"	}",
			"	def influenceSteering(linearInfluence : String = null, angularInfluence : float = 0f, otherInfluences : Object*) {",
			"	}",
			"}");

	private static void assertActionKeys(Iterable<ActionPrototype> actual, String... expected) {
		assertNotNull(actual);
		Collection<Object> la = new ArrayList<>();
		for (ActionPrototype key : actual) {
			la.add(key.toString());
		}
		assertContains(la, expected);
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class CompilerTests extends AbstractSarlTest {

		@Test
		public void testCompiler() throws Exception {
			final String expectedPhysicEnvironment = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface PhysicEnvironment extends Capacity {",
					"  @DefaultValueSource",
					"  public abstract void influenceKinematic(@DefaultValue(\"PhysicEnvironment#INFLUENCEKINEMATIC_0\") final String linearInfluence, @DefaultValue(\"PhysicEnvironment#INFLUENCEKINEMATIC_1\") final float angularInfluence, final Object... otherInfluences);",
					"  ",
					"  /**",
					"   * Default value for the parameter linearInfluence",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"null\")",
					"  public static final String $DEFAULT_VALUE$INFLUENCEKINEMATIC_0 = null;",
					"  ",
					"  /**",
					"   * Default value for the parameter angularInfluence",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"0f\")",
					"  public static final float $DEFAULT_VALUE$INFLUENCEKINEMATIC_1 = 0f;",
					"  ",
					"  @DefaultValueSource",
					"  public abstract void influenceSteering(@DefaultValue(\"PhysicEnvironment#INFLUENCESTEERING_0\") final String linearInfluence, @DefaultValue(\"PhysicEnvironment#INFLUENCESTEERING_1\") final float angularInfluence, final Object... otherInfluences);",
					"  ",
					"  /**",
					"   * Default value for the parameter linearInfluence",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"null\")",
					"  public static final String $DEFAULT_VALUE$INFLUENCESTEERING_0 = null;",
					"  ",
					"  /**",
					"   * Default value for the parameter angularInfluence",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"0f\")",
					"  public static final float $DEFAULT_VALUE$INFLUENCESTEERING_1 = 0f;",
					"  ",
					"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
					"  @SyntheticMember",
					"  public default void influenceKinematic(final Object... otherInfluences) {",
					"    influenceKinematic($DEFAULT_VALUE$INFLUENCEKINEMATIC_0, $DEFAULT_VALUE$INFLUENCEKINEMATIC_1, otherInfluences);",
					"  }",
					"  ",
					"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
					"  @SyntheticMember",
					"  public default void influenceKinematic(final float angularInfluence, final Object... otherInfluences) {",
					"    influenceKinematic($DEFAULT_VALUE$INFLUENCEKINEMATIC_0, angularInfluence, otherInfluences);",
					"  }",
					"  ",
					"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
					"  @SyntheticMember",
					"  public default void influenceKinematic(final String linearInfluence, final Object... otherInfluences) {",
					"    influenceKinematic(linearInfluence, $DEFAULT_VALUE$INFLUENCEKINEMATIC_1, otherInfluences);",
					"  }",
					"  ",
					"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
					"  @SyntheticMember",
					"  public default void influenceSteering(final Object... otherInfluences) {",
					"    influenceSteering($DEFAULT_VALUE$INFLUENCESTEERING_0, $DEFAULT_VALUE$INFLUENCESTEERING_1, otherInfluences);",
					"  }",
					"  ",
					"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
					"  @SyntheticMember",
					"  public default void influenceSteering(final float angularInfluence, final Object... otherInfluences) {",
					"    influenceSteering($DEFAULT_VALUE$INFLUENCESTEERING_0, angularInfluence, otherInfluences);",
					"  }",
					"  ",
					"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
					"  @SyntheticMember",
					"  public default void influenceSteering(final String linearInfluence, final Object... otherInfluences) {",
					"    influenceSteering(linearInfluence, $DEFAULT_VALUE$INFLUENCESTEERING_1, otherInfluences);",
					"  }",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  public static class ContextAwareCapacityWrapper<C extends PhysicEnvironment> extends Capacity.ContextAwareCapacityWrapper<C> implements PhysicEnvironment {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public void influenceKinematic(final String linearInfluence, final float angularInfluence, final Object... otherInfluences) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.influenceKinematic(linearInfluence, angularInfluence, otherInfluences);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void influenceSteering(final String linearInfluence, final float angularInfluence, final Object... otherInfluences) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.influenceSteering(linearInfluence, angularInfluence, otherInfluences);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void influenceKinematic(final Object... otherInfluences) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.influenceKinematic(otherInfluences);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void influenceKinematic(final float angularInfluence, final Object... otherInfluences) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.influenceKinematic(angularInfluence, otherInfluences);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void influenceKinematic(final String linearInfluence, final Object... otherInfluences) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.influenceKinematic(linearInfluence, otherInfluences);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void influenceSteering(final Object... otherInfluences) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.influenceSteering(otherInfluences);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void influenceSteering(final float angularInfluence, final Object... otherInfluences) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.influenceSteering(angularInfluence, otherInfluences);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void influenceSteering(final String linearInfluence, final Object... otherInfluences) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.influenceSteering(linearInfluence, otherInfluences);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					"");
			final String expectedStandardPhysicEnvironment = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class StandardPhysicEnvironment extends Skill implements PhysicEnvironment {",
					"  @DefaultValueSource",
					"  public void influenceKinematic(@DefaultValue(\"StandardPhysicEnvironment#INFLUENCEKINEMATIC_0\") final String linearInfluence, @DefaultValue(\"StandardPhysicEnvironment#INFLUENCEKINEMATIC_1\") final float angularInfluence, final Object... otherInfluences) {",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter linearInfluence",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"null\")",
					"  private static final String $DEFAULT_VALUE$INFLUENCEKINEMATIC_0 = null;",
					"  ",
					"  /**",
					"   * Default value for the parameter angularInfluence",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"0f\")",
					"  private static final float $DEFAULT_VALUE$INFLUENCEKINEMATIC_1 = 0f;",
					"  ",
					"  @DefaultValueSource",
					"  public void influenceSteering(@DefaultValue(\"StandardPhysicEnvironment#INFLUENCESTEERING_0\") final String linearInfluence, @DefaultValue(\"StandardPhysicEnvironment#INFLUENCESTEERING_1\") final float angularInfluence, final Object... otherInfluences) {",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter linearInfluence",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"null\")",
					"  private static final String $DEFAULT_VALUE$INFLUENCESTEERING_0 = null;",
					"  ",
					"  /**",
					"   * Default value for the parameter angularInfluence",
					"   */",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"0f\")",
					"  private static final float $DEFAULT_VALUE$INFLUENCESTEERING_1 = 0f;",
					"  ",
					"  @SyntheticMember",
					"  public StandardPhysicEnvironment() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public StandardPhysicEnvironment(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					"");

			getCompileHelper().compile(multilineString(capacityCode, skillCode), (r) -> {
					assertEquals(expectedPhysicEnvironment, r.getGeneratedCode("PhysicEnvironment"));
					assertEquals(expectedStandardPhysicEnvironment, r.getGeneratedCode("StandardPhysicEnvironment"));
				});
		}

	}

}

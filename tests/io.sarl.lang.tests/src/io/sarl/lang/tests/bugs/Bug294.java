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
package io.sarl.lang.tests.bugs;

import static io.sarl.tests.api.AbstractSarlTest.assertContains;
import static io.sarl.tests.api.AbstractSarlTest.multilineString;
import static org.junit.Assert.*;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.actionprototype.ActionPrototypeProvider;
import io.sarl.lang.jvmmodel.JvmModelInferrerProber;
import io.sarl.lang.jvmmodel.JvmModelInferrerProber.Step;
import io.sarl.lang.jvmmodel.SARLJvmModelInferrer;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.JvmModelInferrerProberInjectorProvider;
import io.sarl.tests.api.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.google.common.base.Optional;
import com.google.inject.Binder;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Provides;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	Bug294.GenerateCodeForFeaturesTests.class,
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

	/** Testing the function <code>SARLJvmModelInferrer.generateCodeForFeatures</code>.
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@InjectWith(JvmModelInferrerProberInjectorProvider.class)
	public static class GenerateCodeForFeaturesTests extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Inject
		private ActionPrototypeProvider sarlSignatureProvider;

		@Inject
		private CommonTypeComputationServices services;

		@Nullable
		private Notifier context;

		@Inject
		private JvmModelInferrerProber prober;

		@Before
		public void setUp() throws Exception {
			final Notifier[] context = new Notifier[1];
			this.compiler.compile(
					multilineString(capacityCode, skillCode),
					new IAcceptor<CompilationTestHelper.Result>() {
						@Override
						public void accept(Result t) {
							context[0] = t.getResourceSet().getResources().get(0);
						}
					});
			this.context = context[0];
		}

		@Test
		public void step0_modelUtil() throws Exception {
			Map<ActionPrototype, JvmOperation> finalOperations = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_0,
					"StandardPhysicEnvironment#finalOperations",
					Map.class);
			assertNotNull(finalOperations);
			assertTrue(finalOperations.isEmpty());
			//
			Map<ActionPrototype, JvmOperation>  overridableOperations = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_0,
					"StandardPhysicEnvironment#overridableOperations",
					Map.class);
			assertNotNull(overridableOperations);
			assertActionKeys(overridableOperations.keySet(),
					"attributesToString()",
					"getOwner()",
					"getSkill(java.lang.Class<S>)",
					"install()",
					"operator_mappedTo(java.lang.Class<? extends io.sarl.lang.core.Capacity>,S)",
					"toString()",
					"uninstall()");
			//
			Map<ActionPrototype, JvmOperation>  operationsToImplement = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_0,
					"StandardPhysicEnvironment#operationsToImplement",
					Map.class);
			assertNotNull(operationsToImplement);
			assertActionKeys(operationsToImplement.keySet(),
					"influenceKinematic(java.lang.String,float,java.lang.Object*)",
					"influenceKinematic(float,java.lang.Object*)",
					"influenceKinematic(java.lang.String,java.lang.Object*)",
					"influenceKinematic(java.lang.Object*)",
					"influenceSteering(java.lang.String,float,java.lang.Object*)",
					"influenceSteering(float,java.lang.Object*)",
					"influenceSteering(java.lang.String,java.lang.Object*)",
					"influenceSteering(java.lang.Object*)");
		}

		@Test
		public void step1_putSarlDefinitions() throws Exception {
			Map<ActionPrototype, JvmOperation> finalOperations = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_1,
					"StandardPhysicEnvironment#finalOperations",
					Map.class);
			assertNotNull(finalOperations);
			assertTrue(finalOperations.isEmpty());
			//
			Map<ActionPrototype, JvmOperation>  overridableOperations = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_1,
					"StandardPhysicEnvironment#overridableOperations",
					Map.class);
			assertNotNull(overridableOperations);
			assertActionKeys(overridableOperations.keySet(),
					"attributesToString()",
					"getOwner()",
					"getSkill(java.lang.Class<S>)",
					"install()",
					"operator_mappedTo(java.lang.Class<? extends io.sarl.lang.core.Capacity>,S)",
					"toString()",
					"uninstall()",
					"influenceKinematic(java.lang.String,float,java.lang.Object*)",
					"influenceSteering(java.lang.String,float,java.lang.Object*)");
			//
			Map<ActionPrototype, JvmOperation>  operationsToImplement = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_1,
					"StandardPhysicEnvironment#operationsToImplement",
					Map.class);
			assertNotNull(operationsToImplement);
			assertActionKeys(operationsToImplement.keySet(),
					"influenceKinematic(float,java.lang.Object*)",
					"influenceKinematic(java.lang.String,java.lang.Object*)",
					"influenceKinematic(java.lang.Object*)",
					"influenceSteering(float,java.lang.Object*)",
					"influenceSteering(java.lang.String,java.lang.Object*)",
					"influenceSteering(java.lang.Object*)");
			//
			Integer actionIndex = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_1,
					"StandardPhysicEnvironment#actionIndex",
					Integer.class);
			assertNotNull(actionIndex);
			assertEquals(2, actionIndex.intValue());
			//
			Integer behaviorUnitIndex = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_1,
					"StandardPhysicEnvironment#behaviorUnitIndex",
					Integer.class);
			assertNotNull(behaviorUnitIndex);
			assertEquals(0, behaviorUnitIndex.intValue());
			//
			Boolean hasConstructor = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_1,
					"StandardPhysicEnvironment#hasConstructor",
					Boolean.class);
			assertNotNull(hasConstructor);
			assertFalse(hasConstructor.booleanValue());
		}

		@Test
		public void step2_addCapacityDelegators() throws Exception {
			Map<ActionPrototype, JvmOperation> finalOperations = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_2,
					"StandardPhysicEnvironment#finalOperations",
					Map.class);
			assertNotNull(finalOperations);
			assertTrue(finalOperations.isEmpty());
			//
			Map<ActionPrototype, JvmOperation>  overridableOperations = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_2,
					"StandardPhysicEnvironment#overridableOperations",
					Map.class);
			assertNotNull(overridableOperations);
			assertActionKeys(overridableOperations.keySet(),
					"attributesToString()",
					"getOwner()",
					"getSkill(java.lang.Class<S>)",
					"install()",
					"operator_mappedTo(java.lang.Class<? extends io.sarl.lang.core.Capacity>,S)",
					"toString()",
					"uninstall()",
					"influenceKinematic(java.lang.String,float,java.lang.Object*)",
					"influenceSteering(java.lang.String,float,java.lang.Object*)");
			//
			Map<ActionPrototype, JvmOperation>  operationsToImplement = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_2,
					"StandardPhysicEnvironment#operationsToImplement",
					Map.class);
			assertNotNull(operationsToImplement);
			assertActionKeys(operationsToImplement.keySet(),
					"influenceKinematic(float,java.lang.Object*)",
					"influenceKinematic(java.lang.String,java.lang.Object*)",
					"influenceKinematic(java.lang.Object*)",
					"influenceSteering(float,java.lang.Object*)",
					"influenceSteering(java.lang.String,java.lang.Object*)",
					"influenceSteering(java.lang.Object*)");
			//
			Integer actionIndex = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_2,
					"StandardPhysicEnvironment#actionIndex",
					Integer.class);
			assertNotNull(actionIndex);
			assertEquals(2, actionIndex.intValue());
			//
			Integer behaviorUnitIndex = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_2,
					"StandardPhysicEnvironment#behaviorUnitIndex",
					Integer.class);
			assertNotNull(behaviorUnitIndex);
			assertEquals(0, behaviorUnitIndex.intValue());
			//
			Boolean hasConstructor = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_2,
					"StandardPhysicEnvironment#hasConstructor",
					Boolean.class);
			assertNotNull(hasConstructor);
			assertFalse(hasConstructor.booleanValue());
		}

		@Test
		public void step3_addMissedFunctions() throws Exception {
			Map<ActionPrototype, JvmOperation> finalOperations = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_3,
					"StandardPhysicEnvironment#finalOperations",
					Map.class);
			assertNotNull(finalOperations);
			assertTrue(finalOperations.isEmpty());
			//
			Map<ActionPrototype, JvmOperation>  overridableOperations = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_3,
					"StandardPhysicEnvironment#overridableOperations",
					Map.class);
			assertNotNull(overridableOperations);
			assertActionKeys(overridableOperations.keySet(),
					"attributesToString()",
					"getOwner()",
					"getSkill(java.lang.Class<S>)",
					"install()",
					"operator_mappedTo(java.lang.Class<? extends io.sarl.lang.core.Capacity>,S)",
					"toString()",
					"uninstall()",
					"influenceKinematic(java.lang.String,float,java.lang.Object*)",
					"influenceSteering(java.lang.String,float,java.lang.Object*)",
					"influenceKinematic(float,java.lang.Object*)",
					"influenceKinematic(java.lang.String,java.lang.Object*)",
					"influenceKinematic(java.lang.Object*)",
					"influenceSteering(float,java.lang.Object*)",
					"influenceSteering(java.lang.String,java.lang.Object*)",
					"influenceSteering(java.lang.Object*)");
			//
			Map<ActionPrototype, JvmOperation>  operationsToImplement = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_3,
					"StandardPhysicEnvironment#operationsToImplement",
					Map.class);
			assertNotNull(operationsToImplement);
			assertTrue(operationsToImplement.isEmpty());
			//
			Integer actionIndex = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_3,
					"StandardPhysicEnvironment#actionIndex",
					Integer.class);
			assertNotNull(actionIndex);
			assertEquals(2, actionIndex.intValue());
			//
			Integer behaviorUnitIndex = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_3,
					"StandardPhysicEnvironment#behaviorUnitIndex",
					Integer.class);
			assertNotNull(behaviorUnitIndex);
			assertEquals(0, behaviorUnitIndex.intValue());
			//
			Boolean hasConstructor = this.prober.get(
					Step.GENERATE_CODE_FOR_FEATURES_3,
					"StandardPhysicEnvironment#hasConstructor",
					Boolean.class);
			assertNotNull(hasConstructor);
			assertFalse(hasConstructor.booleanValue());
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class CompilerTests extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void testCompiler() throws Exception {
			this.compiler.assertCompilesTo(
					multilineString(capacityCode, skillCode),
					multilineString(
							"MULTIPLE FILES WERE GENERATED",
							"",
							"File 1 : /myProject/src-gen/PhysicEnvironment.java",
							"",
							"import io.sarl.lang.annotation.DefaultValue;",
							"import io.sarl.lang.annotation.DefaultValueSource;",
							"import io.sarl.lang.annotation.DefaultValueUse;",
							"import io.sarl.lang.annotation.Generated;",
							"import io.sarl.lang.core.Capacity;",
							"",
							"@SuppressWarnings(\"all\")",
							"public interface PhysicEnvironment extends Capacity {",
							"  @DefaultValueSource",
							"  public abstract void influenceKinematic(@DefaultValue(\"PhysicEnvironment#INFLUENCEKINEMATIC_0\") final String linearInfluence, @DefaultValue(\"PhysicEnvironment#INFLUENCEKINEMATIC_1\") final float angularInfluence, final Object... otherInfluences);",
							"  ",
							"  /**",
							"   * Default value for the parameter linearInfluence",
							"   */",
							"  @Generated(\" null\")",
							"  public final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCEKINEMATIC_0 = null;",
							"  ",
							"  /**",
							"   * Default value for the parameter angularInfluence",
							"   */",
							"  @Generated(\" 0f\")",
							"  public final static float ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCEKINEMATIC_1 = 0f;",
							"  ",
							"  @DefaultValueSource",
							"  public abstract void influenceSteering(@DefaultValue(\"PhysicEnvironment#INFLUENCESTEERING_0\") final String linearInfluence, @DefaultValue(\"PhysicEnvironment#INFLUENCESTEERING_1\") final float angularInfluence, final Object... otherInfluences);",
							"  ",
							"  /**",
							"   * Default value for the parameter linearInfluence",
							"   */",
							"  @Generated(\" null\")",
							"  public final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCESTEERING_0 = null;",
							"  ",
							"  /**",
							"   * Default value for the parameter angularInfluence",
							"   */",
							"  @Generated(\" 0f\")",
							"  public final static float ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCESTEERING_1 = 0f;",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public abstract void influenceKinematic(final Object... otherInfluences);",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public abstract void influenceKinematic(final float angularInfluence, final Object... otherInfluences);",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public abstract void influenceKinematic(final String linearInfluence, final Object... otherInfluences);",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public abstract void influenceSteering(final Object... otherInfluences);",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public abstract void influenceSteering(final float angularInfluence, final Object... otherInfluences);",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public abstract void influenceSteering(final String linearInfluence, final Object... otherInfluences);",
							"}",
							"",
							"File 2 : /myProject/src-gen/StandardPhysicEnvironment.java",
							"",
							"import io.sarl.lang.annotation.DefaultValue;",
							"import io.sarl.lang.annotation.DefaultValueSource;",
							"import io.sarl.lang.annotation.DefaultValueUse;",
							"import io.sarl.lang.annotation.Generated;",
							"import io.sarl.lang.core.Agent;",
							"import io.sarl.lang.core.Skill;",
							"",
							"@SuppressWarnings(\"all\")",
							"public class StandardPhysicEnvironment extends Skill implements PhysicEnvironment {",
							"  @DefaultValueSource",
							"  public void influenceKinematic(@DefaultValue(\"StandardPhysicEnvironment#INFLUENCEKINEMATIC_0\") final String linearInfluence, @DefaultValue(\"StandardPhysicEnvironment#INFLUENCEKINEMATIC_1\") final float angularInfluence, final Object... otherInfluences) {",
							"  }",
							"  ",
							"  /**",
							"   * Default value for the parameter linearInfluence",
							"   */",
							"  @Generated(\" null\")",
							"  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCEKINEMATIC_0 = null;",
							"  ",
							"  /**",
							"   * Default value for the parameter angularInfluence",
							"   */",
							"  @Generated(\" 0f\")",
							"  private final static float ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCEKINEMATIC_1 = 0f;",
							"  ",
							"  @DefaultValueSource",
							"  public void influenceSteering(@DefaultValue(\"StandardPhysicEnvironment#INFLUENCESTEERING_0\") final String linearInfluence, @DefaultValue(\"StandardPhysicEnvironment#INFLUENCESTEERING_1\") final float angularInfluence, final Object... otherInfluences) {",
							"  }",
							"  ",
							"  /**",
							"   * Default value for the parameter linearInfluence",
							"   */",
							"  @Generated(\" null\")",
							"  private final static String ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCESTEERING_0 = null;",
							"  ",
							"  /**",
							"   * Default value for the parameter angularInfluence",
							"   */",
							"  @Generated(\" 0f\")",
							"  private final static float ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCESTEERING_1 = 0f;",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public final void influenceKinematic(final Object... otherInfluences) {",
							"    influenceKinematic(___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCEKINEMATIC_0, ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCEKINEMATIC_1, otherInfluences);",
							"  }",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public final void influenceKinematic(final float angularInfluence, final Object... otherInfluences) {",
							"    influenceKinematic(___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCEKINEMATIC_0, angularInfluence, otherInfluences);",
							"  }",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public final void influenceKinematic(final String linearInfluence, final Object... otherInfluences) {",
							"    influenceKinematic(linearInfluence, ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCEKINEMATIC_1, otherInfluences);",
							"  }",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public final void influenceSteering(final Object... otherInfluences) {",
							"    influenceSteering(___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCESTEERING_0, ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCESTEERING_1, otherInfluences);",
							"  }",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public final void influenceSteering(final float angularInfluence, final Object... otherInfluences) {",
							"    influenceSteering(___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCESTEERING_0, angularInfluence, otherInfluences);",
							"  }",
							"  ",
							"  @DefaultValueUse(\"java.lang.String,float,java.lang.Object*\")",
							"  @Generated",
							"  public final void influenceSteering(final String linearInfluence, final Object... otherInfluences) {",
							"    influenceSteering(linearInfluence, ___FORMAL_PARAMETER_DEFAULT_VALUE_INFLUENCESTEERING_1, otherInfluences);",
							"  }",
							"  ",
							"  /**",
							"   * Construct a skill. The owning agent is unknown.",
							"   */",
							"  @Generated",
							"  public StandardPhysicEnvironment() {",
							"    super();",
							"  }",
							"  ",
							"  /**",
							"   * Construct a skill.",
							"   * @param owner - agent that is owning this skill.",
							"   */",
							"  @Generated",
							"  public StandardPhysicEnvironment(final Agent owner) {",
							"    super(owner);",
							"  }",
							"}",
							"",
							""));
		}

	}

}

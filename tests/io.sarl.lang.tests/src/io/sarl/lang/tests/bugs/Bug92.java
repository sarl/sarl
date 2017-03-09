/*
 * Copyright (C) 2014-2017 the original authors or authors.
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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import com.google.inject.Inject;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmPrimitiveType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.TypesFactory;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug92 extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

	protected static void assertInstance(Class<?> expected, Object actual) {
		if (actual != null && !expected.isInstance(actual)) {
			fail("Unexpected type of object. The object must be a " + expected.getName() + " but it is: " + actual);
		}
	}

	protected static JvmType createType(Class<?> t) {
		JvmGenericType result = TypesFactory.eINSTANCE.createJvmGenericType();
		result.setSimpleName(t.getSimpleName());
		result.setPackageName(t.getPackage().getName());
		return result;
	}

	protected static JvmType createPrimitiveType(Class<?> t) {
		JvmPrimitiveType result = TypesFactory.eINSTANCE.createJvmPrimitiveType();
		result.setSimpleName(t.getName());
		return result;
	}

	@Test
	public void attributeDeclarationSyntax_inferredDouble() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  var myDouble = 0d",
				"}"
				));
		validate(mas).assertNoErrors();
		assertEquals(1, mas.getXtendTypes().size());
		assertInstance(SarlAgent.class, mas.getXtendTypes().get(0));
		SarlAgent ag = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals(1, ag.getMembers().size());
		assertInstance(SarlField.class, ag.getMembers().get(0));
		SarlField attr = (SarlField) ag.getMembers().get(0);
		assertEquals("myDouble", attr.getName());
		assertFalse(attr.isFinal());
		assertNull(attr.getType());
		assertInstance(XNumberLiteral.class, attr.getInitialValue());
		XNumberLiteral literal = (XNumberLiteral) attr.getInitialValue();
		assertEquals("0d", literal.getValue());
	}

	@Test
	public void attributeDeclarationSyntax_Double() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  var myDouble : Double = 0d",
				"}"
				));
		validate(mas).assertNoErrors();
		assertEquals(1, mas.getXtendTypes().size());
		assertInstance(SarlAgent.class, mas.getXtendTypes().get(0));
		SarlAgent ag = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals(1, ag.getMembers().size());
		assertInstance(SarlField.class, ag.getMembers().get(0));
		SarlField attr = (SarlField) ag.getMembers().get(0);
		assertEquals("myDouble", attr.getName());
		assertFalse(attr.isFinal());
		assertInstance(JvmParameterizedTypeReference.class, attr.getType());
		JvmParameterizedTypeReference type = (JvmParameterizedTypeReference) attr.getType();
		assertEquals(createType(Double.class).getQualifiedName(), type.getType().getQualifiedName());
		assertInstance(XNumberLiteral.class, attr.getInitialValue());
		XNumberLiteral literal = (XNumberLiteral) attr.getInitialValue();
		assertEquals("0d", literal.getValue());
	}

	@Test
	public void attributeDeclarationSyntax_double() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  var myDouble : double = 0d",
				"}"
				));
		validate(mas).assertNoErrors();
		assertEquals(1, mas.getXtendTypes().size());
		assertEquals(1, mas.getXtendTypes().size());
		assertInstance(SarlAgent.class, mas.getXtendTypes().get(0));
		SarlAgent ag = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals(1, ag.getMembers().size());
		assertInstance(SarlField.class, ag.getMembers().get(0));
		SarlField attr = (SarlField) ag.getMembers().get(0);
		assertEquals("myDouble", attr.getName());
		assertFalse(attr.isFinal());
		assertInstance(JvmParameterizedTypeReference.class, attr.getType());
		JvmParameterizedTypeReference type = (JvmParameterizedTypeReference) attr.getType();
		assertEquals(createPrimitiveType(double.class).getQualifiedName(), type.getType().getQualifiedName());
		assertInstance(XNumberLiteral.class, attr.getInitialValue());
		XNumberLiteral literal = (XNumberLiteral) attr.getInitialValue();
		assertEquals("0d", literal.getValue());
	}

	@Test
	public void attributeDeclarationCompiler_inferredDouble() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"  var myDouble = 0d",
						"}"
						),
				multilineString(
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SuppressWarnings(\"all\")",
						"public class A1 extends Agent {",
						"  protected double myDouble = 0d;",
						"  ",
						"  @SyntheticMember",
						"  public A1(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public A1(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
	}

	@Test
	public void attributeDeclarationCompiler_Double() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"  var myDouble : Double = 0d",
						"}"
						),
				multilineString(
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SuppressWarnings(\"all\")",
						"public class A1 extends Agent {",
						"  protected Double myDouble = Double.valueOf(0d);",
						"  ",
						"  @SyntheticMember",
						"  public A1(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public A1(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
	}

	@Test
	public void attributeDeclarationCompiler_double() throws Exception {
		this.compiler.assertCompilesTo(
				multilineString(
						"agent A1 {",
						"  var myDouble : double = 0d",
						"}"
						),
				multilineString(
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SuppressWarnings(\"all\")",
						"public class A1 extends Agent {",
						"  protected double myDouble = 0d;",
						"  ",
						"  @SyntheticMember",
						"  public A1(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public A1(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
	}

	@Test
	public void originialCode_withDoubleType() throws Exception {
		final String source = multilineString(
				"capacity ComputeEnergyCapacity {",
				"  def getEnergy(currentTime : Double, deltaTime : Double, wantedEnergy : Double) : Double",
				"  def setVoltage(currentVoltage : Double)",
				"}",
				"agent EntityAgent {",
				"}",
				"agent DeviceAgent extends EntityAgent {",
				"  uses ComputeEnergyCapacity",
				"  requires ComputeEnergyCapacity",
				"  var busTime : Double = 0d",
				"  var wantedIntensity : Double",
				"}",
				"");
		final String expected1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.AgentTrait;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface ComputeEnergyCapacity extends Capacity {",
				"  public abstract Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy);",
				"  ",
				"  public abstract void setVoltage(final Double currentVoltage);",
				"  ",
				"  public static class ContextAwareCapacityWrapper<C extends ComputeEnergyCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements ComputeEnergyCapacity {",
				"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
				"      super(capacity, caller);",
				"    }",
				"    ",
				"    public Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy) {",
				"      try {",
				"        ensureCallerInLocalThread();",
				"        return this.capacity.getEnergy(currentTime, deltaTime, wantedEnergy);",
				"      } finally {",
				"        resetCallerInLocalThread();",
				"      }",
				"    }",
				"    ",
				"    public void setVoltage(final Double currentVoltage) {",
				"      try {",
				"        ensureCallerInLocalThread();",
				"        this.capacity.setVoltage(currentVoltage);",
				"      } finally {",
				"        resetCallerInLocalThread();",
				"      }",
				"    }",
				"  }",
				"}",
				"");
		final String expected2 = multilineString(
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.core.Skill;",
				"import io.sarl.lang.util.ClearableReference;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class DeviceAgent extends EntityAgent {",
				"  protected Double busTime = Double.valueOf(0d);",
				"  ",
				"  protected Double wantedIntensity;",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(ComputeEnergyCapacity.class)",
				"  @SyntheticMember",
				"  private transient ClearableReference<Skill> $CAPACITY_USE$COMPUTEENERGYCAPACITY;",
				"  ",
				"  @Inline(value = \"$castSkill(ComputeEnergyCapacity.class, (this.$CAPACITY_USE$COMPUTEENERGYCAPACITY == null || this.$CAPACITY_USE$COMPUTEENERGYCAPACITY.get() == null) ? (this.$CAPACITY_USE$COMPUTEENERGYCAPACITY = $getSkill(ComputeEnergyCapacity.class)) : this.$CAPACITY_USE$COMPUTEENERGYCAPACITY)\", imported = ComputeEnergyCapacity.class)",
				"  @SyntheticMember",
				"  @Pure",
				"  private ComputeEnergyCapacity $CAPACITY_USE$COMPUTEENERGYCAPACITY$CALLER() {",
				"    if (this.$CAPACITY_USE$COMPUTEENERGYCAPACITY == null || this.$CAPACITY_USE$COMPUTEENERGYCAPACITY.get() == null) {",
				"      this.$CAPACITY_USE$COMPUTEENERGYCAPACITY = $getSkill(ComputeEnergyCapacity.class);",
				"    }",
				"    return $castSkill(ComputeEnergyCapacity.class, this.$CAPACITY_USE$COMPUTEENERGYCAPACITY);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public DeviceAgent(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Inject",
				"  public DeviceAgent(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
				"    super(arg0, arg1, arg2);",
				"  }",
				"}",
				"");
		final String expected3 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class EntityAgent extends Agent {",
				"  @SyntheticMember",
				"  public EntityAgent(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Inject",
				"  public EntityAgent(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
				"    super(arg0, arg1, arg2);",
				"  }",
				"}",
				"");

		this.compiler.compile(source, (r) -> {
				assertEquals(expected1,r.getGeneratedCode("ComputeEnergyCapacity"));
				assertEquals(expected2,r.getGeneratedCode("DeviceAgent"));
				assertEquals(expected3,r.getGeneratedCode("EntityAgent"));
			});
	}

	@Test
	public void originialCode_withoutDoubleType() throws Exception {
		final String source = multilineString(
				"capacity ComputeEnergyCapacity {",
				"  def getEnergy(currentTime : Double, deltaTime : Double, wantedEnergy : Double) : Double",
				"  def setVoltage(currentVoltage : Double)",
				"}",
				"agent EntityAgent {",
				"}",
				"agent DeviceAgent extends EntityAgent {",
				"  uses ComputeEnergyCapacity",
				"  requires ComputeEnergyCapacity",
				"  var busTime = 0d",
				"  var wantedIntensity : Double",
				"}"
				);
		final String expected1 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.AgentTrait;",
				"import io.sarl.lang.core.Capacity;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public interface ComputeEnergyCapacity extends Capacity {",
				"  public abstract Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy);",
				"  ",
				"  public abstract void setVoltage(final Double currentVoltage);",
				"  ",
				"  public static class ContextAwareCapacityWrapper<C extends ComputeEnergyCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements ComputeEnergyCapacity {",
				"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
				"      super(capacity, caller);",
				"    }",
				"    ",
				"    public Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy) {",
				"      try {",
				"        ensureCallerInLocalThread();",
				"        return this.capacity.getEnergy(currentTime, deltaTime, wantedEnergy);",
				"      } finally {",
				"        resetCallerInLocalThread();",
				"      }",
				"    }",
				"    ",
				"    public void setVoltage(final Double currentVoltage) {",
				"      try {",
				"        ensureCallerInLocalThread();",
				"        this.capacity.setVoltage(currentVoltage);",
				"      } finally {",
				"        resetCallerInLocalThread();",
				"      }",
				"    }",
				"  }",
				"}",
				"");
		final String expected2 = multilineString(
				"import io.sarl.lang.annotation.ImportedCapacityFeature;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.core.Skill;",
				"import io.sarl.lang.util.ClearableReference;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Extension;",
				"import org.eclipse.xtext.xbase.lib.Inline;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class DeviceAgent extends EntityAgent {",
				"  protected double busTime = 0d;",
				"  ",
				"  protected Double wantedIntensity;",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(ComputeEnergyCapacity.class)",
				"  @SyntheticMember",
				"  private transient ClearableReference<Skill> $CAPACITY_USE$COMPUTEENERGYCAPACITY;",
				"  ",
				"  @Inline(value = \"$castSkill(ComputeEnergyCapacity.class, (this.$CAPACITY_USE$COMPUTEENERGYCAPACITY == null || this.$CAPACITY_USE$COMPUTEENERGYCAPACITY.get() == null) ? (this.$CAPACITY_USE$COMPUTEENERGYCAPACITY = $getSkill(ComputeEnergyCapacity.class)) : this.$CAPACITY_USE$COMPUTEENERGYCAPACITY)\", imported = ComputeEnergyCapacity.class)",
				"  @SyntheticMember",
				"  @Pure",
				"  private ComputeEnergyCapacity $CAPACITY_USE$COMPUTEENERGYCAPACITY$CALLER() {",
				"    if (this.$CAPACITY_USE$COMPUTEENERGYCAPACITY == null || this.$CAPACITY_USE$COMPUTEENERGYCAPACITY.get() == null) {",
				"      this.$CAPACITY_USE$COMPUTEENERGYCAPACITY = $getSkill(ComputeEnergyCapacity.class);",
				"    }",
				"    return $castSkill(ComputeEnergyCapacity.class, this.$CAPACITY_USE$COMPUTEENERGYCAPACITY);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public DeviceAgent(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Inject",
				"  public DeviceAgent(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
				"    super(arg0, arg1, arg2);",
				"  }",
				"}",
				"");
		final String expected3 = multilineString(
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class EntityAgent extends Agent {",
				"  @SyntheticMember",
				"  public EntityAgent(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Inject",
				"  public EntityAgent(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
				"    super(arg0, arg1, arg2);",
				"  }",
				"}",
				"");

		this.compiler.compile(source, (r) -> {
				assertEquals(expected1,r.getGeneratedCode("ComputeEnergyCapacity"));
				assertEquals(expected2,r.getGeneratedCode("DeviceAgent"));
				assertEquals(expected3,r.getGeneratedCode("EntityAgent"));
			});
	}

}

/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.bugs

import com.google.inject.Inject
import io.sarl.lang.SARLInjectorProvider
import io.sarl.lang.sarl.Agent
import io.sarl.lang.sarl.Attribute
import io.sarl.lang.sarl.SarlScript
import org.eclipse.xtext.common.types.JvmGenericType
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference
import org.eclipse.xtext.common.types.JvmPrimitiveType
import org.eclipse.xtext.common.types.JvmType
import org.eclipse.xtext.common.types.TypesFactory
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.junit4.util.ParseHelper
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.eclipse.xtext.xbase.XNumberLiteral
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import org.junit.Test
import org.junit.runner.RunWith

import static org.junit.Assert.*

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class Bug92 {
	@Inject extension ParseHelper<SarlScript>
	@Inject extension ValidationTestHelper
	@Inject extension CompilationTestHelper

	def protected static void assertInstance(Class<?> expected, Object actual) {
		if (actual!=null && !expected.isInstance(actual)) {
			fail("Unexpected type of object. The object must be a "+expected.name+" but it is: "+actual)
		}
	}
	
	def protected static JvmType createType(Class<?> t) {
		var JvmGenericType result = TypesFactory.eINSTANCE.createJvmGenericType
		result.setSimpleName(t.simpleName)
		result.setPackageName(t.package.name)
		return result
	}
	
	def protected static JvmType createPrimitiveType(Class<?> t) {
		var JvmPrimitiveType result = TypesFactory.eINSTANCE.createJvmPrimitiveType
		result.setSimpleName(t.name)
		return result
	}

	@Test
	def void attributeDeclarationSyntax_inferredDouble() {
		val mas = '''
			agent A1 {
				var myDouble = 0d
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
		assertInstance(Agent, mas.elements.get(0))
		var Agent ag = mas.elements.get(0) as Agent
		assertEquals(1, ag.features.size)
		assertInstance(Attribute, ag.features.get(0))
		var Attribute attr = ag.features.get(0) as Attribute
		assertEquals("myDouble", attr.name)
		assertTrue(attr.writeable)
		assertNull(attr.type)
		assertInstance(XNumberLiteral, attr.initialValue)
		var XNumberLiteral literal = attr.initialValue as XNumberLiteral
		assertEquals("0d", literal.value)
	}

	@Test
	def void attributeDeclarationSyntax_Double() {
		val mas = '''
			agent A1 {
				var myDouble : Double = 0d
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
		assertInstance(Agent, mas.elements.get(0))
		var Agent ag = mas.elements.get(0) as Agent
		assertEquals(1, ag.features.size)
		assertInstance(Attribute, ag.features.get(0))
		var Attribute attr = ag.features.get(0) as Attribute
		assertEquals("myDouble", attr.name)
		assertTrue(attr.writeable)
		assertInstance(JvmParameterizedTypeReference, attr.type)
		var JvmParameterizedTypeReference type = attr.type as JvmParameterizedTypeReference
		assertEquals(createType(Double).qualifiedName, type.type.qualifiedName)
		assertInstance(XNumberLiteral, attr.initialValue)
		var XNumberLiteral literal = attr.initialValue as XNumberLiteral
		assertEquals("0d", literal.value)
	}

	@Test
	def void attributeDeclarationSyntax_double() {
		val mas = '''
			agent A1 {
				var myDouble : double = 0d
			}
		'''.parse
		mas.assertNoErrors
		assertEquals(1, mas.elements.size)
		assertEquals(1, mas.elements.size)
		assertInstance(Agent, mas.elements.get(0))
		var Agent ag = mas.elements.get(0) as Agent
		assertEquals(1, ag.features.size)
		assertInstance(Attribute, ag.features.get(0))
		var Attribute attr = ag.features.get(0) as Attribute
		assertEquals("myDouble", attr.name)
		assertTrue(attr.writeable)
		assertInstance(JvmParameterizedTypeReference, attr.type)
		var JvmParameterizedTypeReference type = attr.type as JvmParameterizedTypeReference
		assertEquals(createPrimitiveType(double).qualifiedName, type.type.qualifiedName)
		assertInstance(XNumberLiteral, attr.initialValue)
		var XNumberLiteral literal = attr.initialValue as XNumberLiteral
		assertEquals("0d", literal.value)
	}

	@Test
	def void attributeDeclarationCompiler_inferredDouble() {
		'''
			agent A1 {
				var myDouble = 0d
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  protected double myDouble = 0d;
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   */
			  @Generated
			  public A1(final UUID parentID) {
			    super(parentID, null);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
			   */
			  @Generated
			  public A1(final UUID parentID, final UUID agentID) {
			    super(parentID, agentID);
			  }
			}
		''')
	}

	@Test
	def void attributeDeclarationCompiler_Double() {
		'''
			agent A1 {
				var myDouble : Double = 0d
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  protected Double myDouble = Double.valueOf(0d);
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   */
			  @Generated
			  public A1(final UUID parentID) {
			    super(parentID, null);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
			   */
			  @Generated
			  public A1(final UUID parentID, final UUID agentID) {
			    super(parentID, agentID);
			  }
			}
		''')
	}

	@Test
	def void attributeDeclarationCompiler_double() {
		'''
			agent A1 {
				var myDouble : double = 0d
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class A1 extends Agent {
			  protected double myDouble = 0d;
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   */
			  @Generated
			  public A1(final UUID parentID) {
			    super(parentID, null);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
			   */
			  @Generated
			  public A1(final UUID parentID, final UUID agentID) {
			    super(parentID, agentID);
			  }
			}
		''')
	}
	
	@Test
	def void originialCode_withDoubleType() {
		val source = '''
			capacity ComputeEnergyCapacity {
				def getEnergy(currentTime : Double, deltaTime : Double, wantedEnergy : Double) : Double
				def setVoltage(currentVoltage : Double)
			}
			agent EntityAgent {
			}
			agent DeviceAgent extends EntityAgent {
				uses ComputeEnergyCapacity
				requires ComputeEnergyCapacity
				var busTime : Double = 0d
				var wantedIntensity : Double
			}
		'''
		val expected1 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface ComputeEnergyCapacity extends Capacity {
			  public abstract Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy);
			  
			  public abstract void setVoltage(final Double currentVoltage);
			}
		'''
		val expected2 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.annotation.ImportedCapacityFeature;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class DeviceAgent extends EntityAgent {
			  protected Double busTime = Double.valueOf(0d);
			  
			  protected Double wantedIntensity;
			  
			  /**
			   * See the capacity {@link ComputeEnergyCapacity#getEnergy(java.lang.Double,java.lang.Double,java.lang.Double)}.
			   * 
			   * @see ComputeEnergyCapacity#getEnergy(java.lang.Double,java.lang.Double,java.lang.Double).
			   */
			  @Generated
			  @ImportedCapacityFeature(ComputeEnergyCapacity.class)
			  protected Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy) {
			    return getSkill(ComputeEnergyCapacity.class).getEnergy(currentTime, deltaTime, wantedEnergy);
			  }
			  
			  /**
			   * See the capacity {@link ComputeEnergyCapacity#setVoltage(java.lang.Double)}.
			   * 
			   * @see ComputeEnergyCapacity#setVoltage(java.lang.Double).
			   */
			  @Generated
			  @ImportedCapacityFeature(ComputeEnergyCapacity.class)
			  protected void setVoltage(final Double currentVoltage) {
			    getSkill(ComputeEnergyCapacity.class).setVoltage(currentVoltage);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   */
			  @Generated
			  public DeviceAgent(final UUID parentID) {
			    super(parentID, null);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
			   */
			  @Generated
			  public DeviceAgent(final UUID parentID, final UUID agentID) {
			    super(parentID, agentID);
			  }
			}
		'''
		val expected3 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class EntityAgent extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   */
			  @Generated
			  public EntityAgent(final UUID parentID) {
			    super(parentID, null);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
			   */
			  @Generated
			  public EntityAgent(final UUID parentID, final UUID agentID) {
			    super(parentID, agentID);
			  }
			}
		'''

		source.compile [CompilationTestHelper.Result r |
			assertEquals(expected1,r.getGeneratedCode("ComputeEnergyCapacity"))
			assertEquals(expected2,r.getGeneratedCode("DeviceAgent"))
			assertEquals(expected3,r.getGeneratedCode("EntityAgent"))
		]
	}

	@Test
	def void originialCode_withoutDoubleType() {
		val source = '''
			capacity ComputeEnergyCapacity {
				def getEnergy(currentTime : Double, deltaTime : Double, wantedEnergy : Double) : Double
				def setVoltage(currentVoltage : Double)
			}
			agent EntityAgent {
			}
			agent DeviceAgent extends EntityAgent {
				uses ComputeEnergyCapacity
				requires ComputeEnergyCapacity
				var busTime = 0d
				var wantedIntensity : Double
			}
		'''
		val expected1 = '''
			import io.sarl.lang.core.Capacity;
			
			@SuppressWarnings("all")
			public interface ComputeEnergyCapacity extends Capacity {
			  public abstract Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy);
			  
			  public abstract void setVoltage(final Double currentVoltage);
			}
		'''
		val expected2 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.annotation.ImportedCapacityFeature;
			import java.util.UUID;

			@SuppressWarnings("all")
			public class DeviceAgent extends EntityAgent {
			  protected double busTime = 0d;
			  
			  protected Double wantedIntensity;
			  
			  /**
			   * See the capacity {@link ComputeEnergyCapacity#getEnergy(java.lang.Double,java.lang.Double,java.lang.Double)}.
			   * 
			   * @see ComputeEnergyCapacity#getEnergy(java.lang.Double,java.lang.Double,java.lang.Double).
			   */
			  @Generated
			  @ImportedCapacityFeature(ComputeEnergyCapacity.class)
			  protected Double getEnergy(final Double currentTime, final Double deltaTime, final Double wantedEnergy) {
			    return getSkill(ComputeEnergyCapacity.class).getEnergy(currentTime, deltaTime, wantedEnergy);
			  }
			  
			  /**
			   * See the capacity {@link ComputeEnergyCapacity#setVoltage(java.lang.Double)}.
			   * 
			   * @see ComputeEnergyCapacity#setVoltage(java.lang.Double).
			   */
			  @Generated
			  @ImportedCapacityFeature(ComputeEnergyCapacity.class)
			  protected void setVoltage(final Double currentVoltage) {
			    getSkill(ComputeEnergyCapacity.class).setVoltage(currentVoltage);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   */
			  @Generated
			  public DeviceAgent(final UUID parentID) {
			    super(parentID, null);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
			   */
			  @Generated
			  public DeviceAgent(final UUID parentID, final UUID agentID) {
			    super(parentID, agentID);
			  }
			}
		'''
		val expected3 = '''
			import io.sarl.lang.annotation.Generated;
			import io.sarl.lang.core.Agent;
			import java.util.UUID;
			
			@SuppressWarnings("all")
			public class EntityAgent extends Agent {
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   */
			  @Generated
			  public EntityAgent(final UUID parentID) {
			    super(parentID, null);
			  }
			  
			  /**
			   * Construct an agent.
			   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.
			   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.
			   */
			  @Generated
			  public EntityAgent(final UUID parentID, final UUID agentID) {
			    super(parentID, agentID);
			  }
			}
		'''

		source.compile [CompilationTestHelper.Result r |
			assertEquals(expected1,r.getGeneratedCode("ComputeEnergyCapacity"))
			assertEquals(expected2,r.getGeneratedCode("DeviceAgent"))
			assertEquals(expected3,r.getGeneratedCode("EntityAgent"))
		]
	}

}

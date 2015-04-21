/*
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.compilation;

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class VarDeclarationCompilerTest extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

	@Test
	public void variableDeclaration_attributeScope() throws Exception {
		String source = multilineString(
				"import java.util.List",
				"agent A1 {",
				"  var list : List<Integer>",
				"  var i = 45",
				"  var j : double = 45",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Agent;",
				"import java.util.List;",
				"import java.util.UUID;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected List<Integer> list;",
				"  ",
				"  protected int i = 45;",
				"  ",
				"  protected double j = 45;",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void variableDeclaration_localScope() throws Exception {
		String source = multilineString(
				"import java.util.List",
				"agent A1 {",
				"  def myaction {",
				"    var i : List<Integer>",
				"    var j = 45",
				"    var k : double = 45",
				"    System.out.println(i)",
				"    System.out.println(j)",
				"    System.out.println(k)",
				"  }",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Agent;",
				"import java.util.List;",
				"import java.util.UUID;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  public void myaction() {",
				"    List<Integer> i = null;",
				"    int j = 45;",
				"    double k = 45;",
				"    System.out.println(i);",
				"    System.out.println(j);",
				"    System.out.println(k);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void valueDeclaration_attributeScope() throws Exception {
		String source = multilineString(
				"import java.util.List",
				"agent A1 {",
				"  val list : List<Integer> = null",
				"  val i = 45",
				"  val j : double = 45",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Agent;",
				"import java.util.List;",
				"import java.util.UUID;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected final List<Integer> list = null;",
				"  ",
				"  protected final int i = 45;",
				"  ",
				"  protected final double j = 45;",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void valueDeclaration_localScope() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def myaction {",
				"    val j = 45",
				"    val k : double = 45",
				"    System.out.println(j)",
				"    System.out.println(k)",
				"  }",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Agent;",
				"import java.util.UUID;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  public void myaction() {",
				"    final int j = 45;",
				"    final double k = 45;",
				"    System.out.println(j);",
				"    System.out.println(k);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void forLoop_inferredType() throws Exception {
		String source = multilineString(
				"import java.util.List",
				"agent A1 {",
				"  var list : List<Integer>",
				"  def myaction {",
				"    for( i : list) {",
				"      System.out.println(i)",
				"    }",
				"  }",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Agent;",
				"import java.util.List;",
				"import java.util.UUID;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected List<Integer> list;",
				"  ",
				"  public void myaction() {",
				"    for (final Integer i : this.list) {",
				"      System.out.println(i);",
				"    }",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void forLoop_explicitType() throws Exception {
		String source = multilineString(
				"import java.util.List",
				"agent A1 {",
				"  var list : List<Integer>",
				"  def myaction {",
				"    for( i as Number : list) {",
				"      System.out.println(i)",
				"    }",
				"  }",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Agent;",
				"import java.util.List;",
				"import java.util.UUID;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected List<Integer> list;",
				"  ",
				"  public void myaction() {",
				"    for (final Number i : this.list) {",
				"      System.out.println(i);",
				"    }",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void catch_oneType() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def myaction {",
				"    try {",
				"      System.out.println(\"G\")",
				"    }",
				"    catch(e : Throwable) {",
				"      System.out.println(e)",
				"    }",
				"  }",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Agent;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Exceptions;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  public void myaction() {",
				"    try {",
				"      System.out.println(\"G\");",
				"    } catch (final Throwable _t) {",
				"      if (_t instanceof Throwable) {",
				"        final Throwable e = (Throwable)_t;",
				"        System.out.println(e);",
				"      } else {",
				"        throw Exceptions.sneakyThrow(_t);",
				"      }",
				"    }",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void multicatch_oneType() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def myaction {",
				"    try {",
				"      System.out.println(\"G\")",
				"    }",
				"    catch(e : Exception) {",
				"      System.out.println(e)",
				"    }",
				"    catch(e : Throwable) {",
				"      System.out.println(e)",
				"    }",
				"  }",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Agent;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Exceptions;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  public void myaction() {",
				"    try {",
				"      System.out.println(\"G\");",
				"    } catch (final Throwable _t) {",
				"      if (_t instanceof Exception) {",
				"        final Exception e = (Exception)_t;",
				"        System.out.println(e);",
				"      } else if (_t instanceof Throwable) {",
				"        final Throwable e_1 = (Throwable)_t;",
				"        System.out.println(e_1);",
				"      } else {",
				"        throw Exceptions.sneakyThrow(_t);",
				"      }",
				"    }",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void closure_twoParams() throws Exception {
		String source = multilineString(
				"agent A1 {",
				"  def mycall(a : int, f : (Float,Integer) => float) : float {",
				"    return a + f.apply(5.45f, 6)",
				"  }",
				"  def myaction : void {",
				"    mycall(4) [ a : Float, b : Integer |",
				"      2f * a.floatValue + b.intValue",
				"    ]",
				"  }",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Agent;",
				"import java.util.UUID;",
				"import org.eclipse.xtext.xbase.lib.Functions.Function2;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  public float mycall(final int a, final Function2<? super Float, ? super Integer, ? extends Float> f) {",
				"    Float _apply = f.apply(Float.valueOf(5.45f), Integer.valueOf(6));",
				"    return (a + (_apply).floatValue());",
				"  }",
				"  ",
				"  public void myaction() {",
				"    final Function2<Float, Integer, Float> _function = new Function2<Float, Integer, Float>() {",
				"      public Float apply(final Float a, final Integer b) {",
				"        float _floatValue = a.floatValue();",
				"        float _multiply = (2f * _floatValue);",
				"        int _intValue = b.intValue();",
				"        return Float.valueOf((_multiply + _intValue));",
				"      }",
				"    };",
				"    this.mycall(4, _function);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

}

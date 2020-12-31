/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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
package io.sarl.lang.tests.general.compilation.general;

import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: var")
@Tag("core")
@Tag("compileToJava")
public class VarDeclarationCompilerTest extends AbstractSarlTest {

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
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.List;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private List<Integer> list;",
				"  ",
				"  private int i = 45;",
				"  ",
				"  private double j = 45;",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    A1 other = (A1) obj;",
				"    if (other.i != this.i)",
				"      return false;",
				"    if (Double.doubleToLongBits(other.j) != Double.doubleToLongBits(this.j))",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.i);",
				"    result = prime * result + Double.hashCode(this.j);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
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
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.List;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void myaction() {",
				"    List<Integer> i = null;",
				"    int j = 45;",
				"    double k = 45;",
				"    System.out.println(i);",
				"    System.out.println(j);",
				"    System.out.println(k);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
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
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.List;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private final List<Integer> list = null;",
				"  ",
				"  private final int i = 45;",
				"  ",
				"  private final double j = 45;",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    A1 other = (A1) obj;",
				"    if (other.i != this.i)",
				"      return false;",
				"    if (Double.doubleToLongBits(other.j) != Double.doubleToLongBits(this.j))",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.i);",
				"    result = prime * result + Double.hashCode(this.j);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
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
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void myaction() {",
				"    final int j = 45;",
				"    final double k = 45;",
				"    System.out.println(j);",
				"    System.out.println(k);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
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
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.List;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private List<Integer> list;",
				"  ",
				"  protected void myaction() {",
				"    for (final Integer i : this.list) {",
				"      System.out.println(i);",
				"    }",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
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
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.List;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  private List<Integer> list;",
				"  ",
				"  protected void myaction() {",
				"    for (final Number i : this.list) {",
				"      System.out.println(i);",
				"    }",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
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
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Exceptions;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void myaction() {",
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
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
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
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Exceptions;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected void myaction() {",
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
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
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
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Functions.Function2;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected float mycall(final int a, final Function2<? super Float, ? super Integer, ? extends Float> f) {",
				"    Float _apply = f.apply(Float.valueOf(5.45f), Integer.valueOf(6));",
				"    return (a + ((_apply) == null ? 0 : (_apply).floatValue()));",
				"  }",
				"  ",
				"  protected void myaction() {",
				"    final Function2<Float, Integer, Float> _function = (Float a, Integer b) -> {",
				"      float _floatValue = a.floatValue();",
				"      float _multiply = (2f * _floatValue);",
				"      int _intValue = b.intValue();",
				"      return Float.valueOf((_multiply + _intValue));",
				"    };",
				"    this.mycall(4, _function);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public A1(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				""
				);
		getCompileHelper().assertCompilesTo(source, expected);
	}

}

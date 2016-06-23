/*
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.lang.tests.compilation.general;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLMapExtensionsCompilerTest extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

	@Test
	public void operator_addMapPair_0() throws Exception {
		String source = multilineString(
			"package io.sarl.lang.tests.ste",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	map += str -> num",
				"}",
			"}"
		);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Map;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pair;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Map<String, Integer> map;",
				"  ",
				"  protected String str = \"a\";",
				"  ",  
				"  protected int num = 4;",
				"  ",
				"  protected Object myaction0() {",
				"    Pair<String, Integer> _mappedTo = Pair.<String, Integer>of(this.str, Integer.valueOf(this.num));",
				"    return this.map.put(_mappedTo.getKey(), _mappedTo.getValue());",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void operator_addMapPair_1() throws Exception {
		String source = multilineString(
			"package io.sarl.lang.tests.ste",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction1 : Object {",
				"	var p = str -> num",
				"	map += p",
				"}",
			"}"
		);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Map;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pair;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Map<String, Integer> map;",
				"  ",
				"  protected String str = \"a\";",
				"  ",  
				"  protected int num = 4;",
				"  ",
				"  protected Object myaction1() {",
				"    Integer _xblockexpression = null;",
				"    {",
				"      Pair<String, Integer> p = Pair.<String, Integer>of(this.str, Integer.valueOf(this.num));",
				"      _xblockexpression = this.map.put(p.getKey(), p.getValue());",
				"    }",
				"    return _xblockexpression;",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void operator_addMapMap_0() throws Exception {
		String source = multilineString(
			"package io.sarl.lang.tests.ste",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Number>",
				"var map2 : Map<String, Integer>",
				"def myaction0 {",
				"	map1 += map2",
				"}",
			"}"
		);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Map;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Map<String, Number> map1;",
				"  ",
				"  protected Map<String, Integer> map2;",
				"  ",
				"  protected void myaction0() {",
				"    this.map1.putAll(this.map2);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void operator_plusMapPair_0() throws Exception {
		String source = multilineString(
			"package io.sarl.lang.tests.ste",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	r = map + (str -> num)",
				"}",
			"}"
		);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Map;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pair;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Map<String, Integer> map;",
				"  ",
				"  protected Map<String, Integer> r;",
				"  ",
				"  protected String str = \"a\";",
				"  ",
				"  protected int num = 4;",
				"  ",
				"  protected Object myaction0() {",
				"    Pair<String, Integer> _mappedTo = Pair.<String, Integer>of(this.str, Integer.valueOf(this.num));",
				"    Map<String, Integer> _plus = SARLMapExtensions.union(this.map, Collections.singletonMap(_mappedTo.getKey(), _mappedTo.getValue()));",
				"    return this.r = _plus;",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void operator_plusMapPair_1() throws Exception {
		String source = multilineString(
			"package io.sarl.lang.tests.ste",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	var p = str -> num",
				"	r = map + p",
				"}",
			"}"
		);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Map;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pair;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Map<String, Integer> map;",
				"  ",
				"  protected Map<String, Integer> r;",
				"  ",
				"  protected String str = \"a\";",
				"  ",
				"  protected int num = 4;",
				"  ",
				"  protected Object myaction0() {",
				"    Map<String, Integer> _xblockexpression = null;",
				"    {",
				"      Pair<String, Integer> p = Pair.<String, Integer>of(this.str, Integer.valueOf(this.num));",
				"      Map<String, Integer> _plus = SARLMapExtensions.union(this.map, Collections.singletonMap(p.getKey(), p.getValue()));",
				"      _xblockexpression = this.r = _plus;",
				"    }",
				"    return _xblockexpression;",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void operator_plusMapMap_0() throws Exception {
		String source = multilineString(
			"package io.sarl.lang.tests.ste",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Integer>",
				"var map2 : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"def myaction0 : Object {",
				"	r = map1 + map2",
				"}",
			"}"
		);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Map;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Map<String, Integer> map1;",
				"  ",
				"  protected Map<String, Integer> map2;",
				"  ",
				"  protected Map<String, Integer> r;",
				"  ",
				"  protected Object myaction0() {",
				"    Map<String, Integer> _plus = SARLMapExtensions.union(this.map1, this.map2);",
				"    return this.r = _plus;",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void operator_plusMapMap_1() throws Exception {
		String source = multilineString(
			"package io.sarl.lang.tests.ste",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Integer>",
				"var map2 : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"def myaction0 : Object {",
				"	r = map2 + map1",
				"}",
			"}"
		);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Map;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Map<String, Integer> map1;",
				"  ",
				"  protected Map<String, Integer> map2;",
				"  ",
				"  protected Map<String, Integer> r;",
				"  ",
				"  protected Object myaction0() {",
				"    Map<String, Integer> _plus = SARLMapExtensions.union(this.map2, this.map1);",
				"    return this.r = _plus;",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void operator_plusMapMap_2() throws Exception {
		String source = multilineString(
			"package io.sarl.lang.tests.ste",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Integer>",
				"var map2 : Map<String, Number>",
				"var r : Map<String, Number>",
				"def myaction0 : Object {",
				"	r = map2 + map1",
				"}",
			"}"
		);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Map;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Map<String, Integer> map1;",
				"  ",
				"  protected Map<String, Number> map2;",
				"  ",
				"  protected Map<String, Number> r;",
				"  ",
				"  protected Object myaction0() {",
				"    Map<String, Number> _plus = SARLMapExtensions.union(this.map2, this.map1);",
				"    return this.r = _plus;",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void operator_removeMapK_0() throws Exception {
		String source = multilineString(
			"package io.sarl.lang.tests.ste",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	map -= str",
				"}",
			"}"
		);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import java.util.Map;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Map<String, Integer> map;",
				"  ",
				"  protected Map<String, Integer> r;",
				"  ",
				"  protected String str = \"a\";",
				"  ",
				"  protected int num = 4;",
				"  ",
				"  protected Object myaction0() {",
				"    return this.map.remove(this.str);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void operator_minusMapK_0() throws Exception {
		String source = multilineString(
			"package io.sarl.lang.tests.ste",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	r = map - str",
				"}",
			"}"
		);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
				"import io.sarl.lang.scoping.batch.SARLMapExtensions;",
				"import java.util.Map;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"import javax.inject.Inject;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Map<String, Integer> map;",
				"  ",
				"  protected Map<String, Integer> r;",
				"  ",
				"  protected String str = \"a\";",
				"  ",
				"  protected int num = 4;",
				"  ",
				"  protected Object myaction0() {",
				"    Map<String, Integer> _minus = SARLMapExtensions.<String, Integer>operator_minus(this.map, this.str);",
				"    return this.r = _minus;",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param builtinCapacityProvider - provider of the built-in capacities.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Inject",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
				"    super(builtinCapacityProvider, parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

}

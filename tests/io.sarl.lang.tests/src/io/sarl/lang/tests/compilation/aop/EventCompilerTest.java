/*
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.tests.compilation.aop;

import static org.junit.Assert.assertEquals;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class EventCompilerTest extends AbstractSarlTest {
	@Inject
	private CompilationTestHelper compiler;

	@Test
	public void basicCompile() throws Exception {
		String source = "event E1 { }";
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  /**",
				"   * Construct an event. The source of the event is unknown.",
				"   */",
				"  @Generated",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  /**",
				"   * Construct an event.",
				"   * @param source - address of the agent that is emitting this event.",
				"   */",
				"  @Generated",
				"  public E1(final Address source) {",
				"    super(source);",
				"  }",
				"  ",
				"  @Generated",
				"  private final static long serialVersionUID = 588368462L;",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void withVarAttributesCompile() throws Exception {
		String source = multilineString(
				"event E1 {",
				"  var name : String",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Address;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  public String name;",
				"  ",
				"  /**",
				"   * Construct an event. The source of the event is unknown.",
				"   */",
				"  @Generated",
				"  public E1() {",
				"    super();",
				"  }",
				"  ",
				"  /**",
				"   * Construct an event.",
				"   * @param source - address of the agent that is emitting this event.",
				"   */",
				"  @Generated",
				"  public E1(final Address source) {",
				"    super(source);",
				"  }",
				"  ",
				"  @Override",
				"  @Generated",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    E1 other = (E1) obj;",
				"    if (this.name == null) {",
				"      if (other.name != null)",
				"        return false;",
				"    } else if (!this.name.equals(other.name))",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Generated",
				"  public int hashCode() {",
				"    final int prime = 31;",
				"    int result = super.hashCode();",
				"    result = prime * result + ((this.name== null) ? 0 : this.name.hashCode());",
				"    return result;",
				"  }",
				"  ",
				"  /**",
				"   * Returns a String representation of the E1 event's attributes only.",
				"   */",
				"  @Generated",
				"  protected String attributesToString() {",
				"    StringBuilder result = new StringBuilder(super.attributesToString());",
				"    result.append(\"name  = \").append(this.name);",
				"    return result.toString();",
				"  }",
				"  ",
				"  @Generated",
				"  private final static long serialVersionUID = 1787001662L;",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void inheritanceCompile() throws Exception {
		String source = multilineString(
				"event E1 {",
				"	var name : String",
				"}",
				"",
				"event E2 extends E1{",
				"}"
				);
		final String expectedE2 = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Address;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class E2 extends E1 {",
				"  /**",
				"   * Construct an event. The source of the event is unknown.",
				"   */",
				"  @Generated",
				"  public E2() {",
				"    super();",
				"  }",
				"  ",
				"  /**",
				"   * Construct an event.",
				"   * @param source - address of the agent that is emitting this event.",
				"   */",
				"  @Generated",
				"  public E2(final Address source) {",
				"    super(source);",
				"  }",
				"  ",
				"  @Generated",
				"  private final static long serialVersionUID = 2189L;",
				"}",
				""
				);
		this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
			@Override
			public void accept(Result r) {
				assertEquals(expectedE2,r.getGeneratedCode("E2"));
			}
		});
	}

	@Test
	public void noStaticField() throws Exception {
		String source = multilineString(
				"event E1 {",
				"	val titi : int = 4",
				"	val toto : int",
				"	new(a : int) {",
				"		this.toto = a",
				"	}",
				"}"
				);
		String expected = multilineString(
				"import io.sarl.lang.annotation.Generated;",
				"import io.sarl.lang.core.Event;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class E1 extends Event {",
				"  public final int titi = 4;",
				"  ",
				"  public final int toto;",
				"  ",
				"  public E1(final int a) {",
				"    this.toto = a;",
				"  }",
				"  ",
				"  @Override",
				"  @Generated",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    E1 other = (E1) obj;",
				"    if (other.titi != this.titi)",
				"      return false;",
				"    if (other.toto != this.toto)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Generated",
				"  public int hashCode() {",
				"    final int prime = 31;",
				"    int result = super.hashCode();",
				"    result = prime * result + this.titi;",
				"    result = prime * result + this.toto;",
				"    return result;",
				"  }",
				"  ",
				"  /**",
				"   * Returns a String representation of the E1 event's attributes only.",
				"   */",
				"  @Generated",
				"  protected String attributesToString() {",
				"    StringBuilder result = new StringBuilder(super.attributesToString());",
				"    result.append(\"titi  = \").append(this.titi);",
				"    result.append(\"toto  = \").append(this.toto);",
				"    return result.toString();",
				"  }",
				"  ",
				"  @Generated",
				"  private final static long serialVersionUID = 598944340L;",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}
}

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
package io.sarl.lang.tests.compilation.general;

import static io.sarl.lang.scoping.batch.SARLTimeExtensions.days;
import static io.sarl.lang.scoping.batch.SARLTimeExtensions.hours;
import static io.sarl.lang.scoping.batch.SARLTimeExtensions.milliseconds;
import static io.sarl.lang.scoping.batch.SARLTimeExtensions.minutes;
import static io.sarl.lang.scoping.batch.SARLTimeExtensions.seconds;
import static io.sarl.lang.scoping.batch.SARLTimeExtensions.weeks;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;
import org.junit.runner.RunWith;
import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLTimeExtensionsCompilerTest extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

	@Test
	public void testMilliseconds() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.milliseconds",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.core.Agent;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Integer.valueOf(1234);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void testSeconds() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.seconds",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.core.Agent;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf(Integer.valueOf(1234) * TimeExtensionsConstants.MILLIS_IN_SECOND);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void testMinutes() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.minutes",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.core.Agent;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf(Integer.valueOf(1234) * TimeExtensionsConstants.MILLIS_IN_MINUTE);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void testHours() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.hours",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.core.Agent;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf(Integer.valueOf(1234) * TimeExtensionsConstants.MILLIS_IN_HOUR);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void testDays() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.days",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.core.Agent;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf(Integer.valueOf(1234) * TimeExtensionsConstants.MILLIS_IN_DAY);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void testWeeks() throws Exception {
		String source = multilineString(
				"package io.sarl.lang.tests.ste",
				"agent A1 {",
					"def myaction0 : Object {",
					"	1234.weeks",
					"}",
				"}"
			);
		String expected = multilineString(
				"package io.sarl.lang.tests.ste;",
				"",
				"import io.sarl.lang.core.Agent;",
				"import java.util.UUID;",
				"import javax.annotation.Generated;",
				"",
				"@SuppressWarnings(\"all\")",
				"public class A1 extends Agent {",
				"  protected Object myaction0() {",
				"    return Long.valueOf(Integer.valueOf(1234) * TimeExtensionsConstants.MILLIS_IN_WEEK);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID) {",
				"    super(parentID, null);",
				"  }",
				"  ",
				"  /**",
				"   * Construct an agent.",
				"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
				"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
				"   */",
				"  @Generated(\"io.sarl.lang.jvmmodel.SARLJvmModelInferrer\")",
				"  public A1(final UUID parentID, final UUID agentID) {",
				"    super(parentID, agentID);",
				"  }",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

}

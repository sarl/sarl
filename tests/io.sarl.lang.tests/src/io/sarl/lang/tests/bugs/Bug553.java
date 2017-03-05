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

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug553 extends AbstractSarlTest {

	private static final String SNIPSET = multilineString(
			"package io.sarl.lang.tests.bug553",
			"event MyEvent",
			"agent TestAgent {",
			"  on MyEvent {",
			"    var ms = 5.seconds",
			"  }",
			"}");
	
	private static final String EXPECTED = multilineString(
			"package io.sarl.lang.tests.bug553;",
			"",
			"import io.sarl.lang.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.tests.bug553.MyEvent;",
			"import java.util.Collection;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SuppressWarnings(\"all\")",
			"public class TestAgent extends Agent {",
			"  @SyntheticMember",
			"  private void $behaviorUnit$MyEvent$0(final MyEvent occurrence) {",
			"    long ms = (5) * 1000;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @PerceptGuardEvaluator",
			"  private void $guardEvaluator$MyEvent(final MyEvent occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"    assert occurrence != null;",
			"    assert ___SARLlocal_runnableCollection != null;",
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$MyEvent$0(occurrence));",
			"  }",
			"  ",
			"  /**",
			"   * Construct an agent.",
			"   * @param builtinCapacityProvider - provider of the built-in capacities.",
			"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
			"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
			"   */",
			"  @Inject",
			"  @SyntheticMember",
			"  public TestAgent(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
			"    super(builtinCapacityProvider, parentID, agentID);",
			"  }",
			"}",
			"");
	@Inject
	private CompilationTestHelper compiler;
	
	@Test
	public void validation() throws Exception {
		SarlScript mas = file(SNIPSET);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compilation() throws Exception {
		this.compiler.compile(SNIPSET, (it) -> {
			assertEquals(EXPECTED, it.getGeneratedCode("io.sarl.lang.tests.bug553.TestAgent"));
		});
	}

}

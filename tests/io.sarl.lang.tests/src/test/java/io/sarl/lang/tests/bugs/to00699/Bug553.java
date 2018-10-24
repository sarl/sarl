/*
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.lang.tests.bugs.to00699;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
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
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.tests.bug553.MyEvent;",
			"import java.util.Collection;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class TestAgent extends Agent {",
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
			"  @SyntheticMember",
			"  public TestAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Deprecated",
			"  @Inject",
			"  public TestAgent(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"  ", 
			"  @SyntheticMember", 
			"  @Inject", 
			"  public TestAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
			"    super(arg0, arg1, arg2);", 
			"  }",
			"}",
			"");
	
	@Test
	public void validation() throws Exception {
		SarlScript mas = file(SNIPSET);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compilation() throws Exception {
		getCompileHelper().compile(SNIPSET, (it) -> {
			assertEquals(EXPECTED, it.getGeneratedCode("io.sarl.lang.tests.bug553.TestAgent"));
		});
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.api.core.tests.bugs;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid agent parameter syntax.
 *
 * <p>https://github.com/sarl/sarl/issues/1134
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1134"
 */
@DisplayName("Bug #1134")
@SuppressWarnings("all")
@Tag("core")
public class Bug1134Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1134",
			"import io.sarl.api.core.Initialize",
			"agent Bug1134Case {",
			"  on Initialize {",
			"    var value = occurrence.parameters.get(0) as Double",
			"    useTheValue(value)",
			"  }",
			"  def useTheValue(v : Double) {",
			"  }",
			"}");

	private static final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1134;",
			"",
			"import io.sarl.api.core.Initialize;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import java.util.Collection;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Bug1134Case extends Agent {",
			"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
			"    Object _get = occurrence.parameters[0];",
			"    Double value = (_get == null ? null : PrimitiveCastExtensions.toDouble(_get));",
			"    this.useTheValue(value);",
			"  }",
			"",
			"  protected void useTheValue(final Double v) {",
			"  }",
			"",
			"  @SyntheticMember",
			"  @PerceptGuardEvaluator",
			"  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {",
			"    assert occurrence != null;",
			"    assert ___SARLlocal_runnableCollection != null;",
			"    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Override",
			"  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {",
			"    super.$getSupportedEvents(toBeFilled);",
			"    toBeFilled.add(Initialize.class);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Override",
			"  public boolean $isSupportedEvent(final Class<? extends Event> event) {",
			"    if (Initialize.class.isAssignableFrom(event)) {",
			"      return true;",
			"    }",
			"    return false;",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Override",
			"  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {",
			"    super.$evaluateBehaviorGuards(event, callbacks);",
			"    if (event instanceof Initialize occurrence) {",
			"      $guardEvaluator$Initialize(occurrence, callbacks);",
			"    }",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Bug1134Case(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Bug1134Case(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parse: Invalid agent parameter syntax")
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		var mas = file(getParseHelper(), SARL_CODE_01);
		var validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@DisplayName("Compile: Invalid agent parameter syntax")
	@Tag("compileToJava")
	public void compiling01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			var actual = it.getGeneratedCode("io.sarl.lang.tests.bug1134.Bug1134Case");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

}
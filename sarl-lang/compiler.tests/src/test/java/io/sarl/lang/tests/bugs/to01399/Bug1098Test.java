/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.tests.bugs.to01399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.eclipse.xtext.xbase.validation.IssueCodes.STATIC_ACCESS_TO_INSTANCE_MEMBER;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Add automation value conversion from Object to numbers.
 *
 * <p>https://github.com/sarl/sarl/issues/1098
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1098"
 */
@DisplayName("Bug #1098")
@SuppressWarnings("all")
@Tag("core")
public class Bug1098Test extends AbstractSarlTest {

	private final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1098",
			"event Initialize {",
			"  var parameters : Object[]",
			"}",
			"agent Test {",
			"   protected var par : Double",
			"   on Initialize {",
			"     synchronized(this) {",
			"       this.par = occurrence.parameters.get(0) as Double",
			"     }",
			"   }",
			"}");

	private final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1098;",
			"",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.Event;",
			"import io.sarl.lang.core.annotation.PerceptGuardEvaluator;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import io.sarl.lang.core.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import jakarta.inject.Inject;",
			"import java.util.Collection;",
			"import java.util.Objects;",
			"import java.util.Set;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class Test extends Agent {",
			"  protected Double par;",
			"",
			"  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {",
			"    synchronized (this) {",
			"      Object _get = occurrence.parameters[0];",
			"      this.par = (_get == null ? null : PrimitiveCastExtensions.toDouble(_get));",
			"    }",
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
			"  public void $evaluateBehaviorGuards(final Class<?> eventType, final Object event, final Collection<Runnable> callbacks) {",
			"    assert eventType != null;",
			"    assert event != null;",
			"    super.$evaluateBehaviorGuards(eventType, event, callbacks);",
			"    if (Initialize.class.equals(eventType)) {",
			"      final var occurrence = (Initialize) event;",
			"      $guardEvaluator$Initialize(occurrence, callbacks);",
			"    }",
			"  }",
			"",
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
			"    Test other = (Test) obj;",
			"    if (other.par == null) {",
			"      if (this.par != null)",
			"        return false;",
			"    } else if (this.par == null)",
			"      return false;",
			"    if (other.par != null && Double.doubleToLongBits(other.par.doubleValue()) != Double.doubleToLongBits(this.par.doubleValue()))",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Objects.hashCode(this.par);",
			"    return result;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public Test(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"",
			"  @SyntheticMember",
			"  @Inject",
			"  public Test(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing")
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
						TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
						io.sarl.lang.validation.IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@Test
	@DisplayName("Compiling")
	@Tag("compileToJava")
	public void compiling01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			assertEquals(JAVA_CODE_01, it.getGeneratedCode("io.sarl.lang.tests.bug1098.Test"));
		});
	}

}

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
package io.sarl.lang.tests.general.parsing.general;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: Thread support")
@Tag("core")
public class ThreadTypeTest {

	@Nested
	public class SleepFunction extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void inAgent_inFunction_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"  def fct(x : int) : int {",
					"    Thread::sleep(5)",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inAgent_inEventHandler_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event Evt",
					"agent A1 {",
					"  on Evt {",
					"    Thread::sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inAgent_inConstructor_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"  new (parent : UUID, me : UUID) {",
					"    super(parent, me)",
					"    Thread::sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inAgent_inFunction_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"agent A1 {",
					"  def fct(x : int) : int {",
					"    sleep(5)",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inAgent_inEventHandler_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"event Evt",
					"agent A1 {",
					"  on Evt {",
					"    sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inAgent_inConstructor_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"agent A1 {",
					"  new (parent : UUID, me : UUID) {",
					"    super(parent, me)",
					"    sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inAgent_inFunction_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static extension java.lang.Thread.*",
					"agent A1 {",
					"  def fct(x : int) : int {",
					"    5.sleep",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inAgent_inEventHandler_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static extension java.lang.Thread.*",
					"event Evt",
					"agent A1 {",
					"  on Evt {",
					"    5.sleep",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inAgent_inConstructor_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static extension java.lang.Thread.*",
					"agent A1 {",
					"  new (parent : UUID, me : UUID) {",
					"    super(parent, me)",
					"    5.sleep",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_inFunction_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"  def fct(x : int) : int {",
					"    Thread::sleep(5)",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inBehavior_inEventHandler_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event Evt",
					"behavior B1 {",
					"  on Evt {",
					"    Thread::sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inBehavior_inConstructor_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"  new (parent : Agent) {",
					"    super(parent)",
					"    Thread::sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_inFunction_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"behavior B1 {",
					"  def fct(x : int) : int {",
					"    sleep(5)",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inBehavior_inEventHandler_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"event Evt",
					"behavior B1 {",
					"  on Evt {",
					"    sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inBehavior_inConstructor_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"import static java.lang.Thread.*",
					"behavior B1 {",
					"  new (parent : Agent) {",
					"    super(parent)",
					"    sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_inFunction_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static extension java.lang.Thread.*",
					"behavior B1 {",
					"  def fct(x : int) : int {",
					"    5.sleep",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inBehavior_inEventHandler_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static extension java.lang.Thread.*",
					"event Evt",
					"behavior B1 {",
					"  on Evt {",
					"    5.sleep",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inBehavior_inConstructor_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"import static extension java.lang.Thread.*",
					"behavior B1 {",
					"  new (parent : Agent) {",
					"    super(parent)",
					"    5.sleep",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_inFunction_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"skill S1 {",
					"  def fct(x : int) : int {",
					"    Thread::sleep(5)",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inSkill_inEventHandler_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event Evt",
					"skill S1 {",
					"  on Evt {",
					"    Thread::sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inSkill_inConstructor_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"skill S1 {",
					"  new {",
					"    Thread::sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_inFunction_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"skill S1 {",
					"  def fct(x : int) : int {",
					"    sleep(5)",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inSkill_inEventHandler_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"event Evt",
					"skill S1 {",
					"  on Evt {",
					"    sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inSkill_inConstructor_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"skill S1 {",
					"  new {",
					"    sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_inFunction_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static extension java.lang.Thread.*",
					"skill S1 {",
					"  def fct(x : int) : int {",
					"    5.sleep",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inSkill_inEventHandler_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static extension java.lang.Thread.*",
					"event Evt",
					"skill S1 {",
					"  on Evt {",
					"    5.sleep",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inSkill_inConstructor_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static extension java.lang.Thread.*",
					"skill S1 {",
					"  new {",
					"    5.sleep",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inClass_inFunction_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"class C1 {",
					"  def fct(x : int) : int {",
					"    Thread::sleep(5)",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inClass_inConstructor_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"class C1 {",
					"  new {",
					"    Thread::sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inClass_inFunction_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"class C1 {",
					"  def fct(x : int) : int {",
					"    sleep(5)",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inClass_inConstructor_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"class C1 {",
					"  new {",
					"    sleep(5)",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

		@Test
		@Tag("sarlValidation")
		public void inClass_inFunction_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static extension java.lang.Thread.*",
					"class C1 {",
					"  def fct(x : int) : int {",
					"    5.sleep",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inClass_inConstructor_03() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static extension java.lang.Thread.*",
					"class C1 {",
					"  new {",
					"    5.sleep",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"sleep");
		}

	}

	@Nested
	public class YieldFunction extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void inAgent_inFunction_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"  def fct(x : int) : int {",
					"    Thread::yield",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inAgent_inEventHandler_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event Evt",
					"agent A1 {",
					"  on Evt {",
					"    Thread::yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inAgent_inConstructor_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"  new (parent : UUID, me : UUID) {",
					"    super(parent, me)",
					"    Thread::yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}

		@Test
		@Tag("sarlValidation")
		public void inAgent_inFunction_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"agent A1 {",
					"  def fct(x : int) : int {",
					"    yield",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inAgent_inEventHandler_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"event Evt",
					"agent A1 {",
					"  on Evt {",
					"    yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inAgent_inConstructor_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"agent A1 {",
					"  new (parent : UUID, me : UUID) {",
					"    super(parent, me)",
					"    yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_inFunction_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"  def fct(x : int) : int {",
					"    Thread::yield",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inBehavior_inEventHandler_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event Evt",
					"behavior B1 {",
					"  on Evt {",
					"    Thread::yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inBehavior_inConstructor_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"  new (parent : Agent) {",
					"    super(parent)",
					"    Thread::yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}

		@Test
		@Tag("sarlValidation")
		public void inBehavior_inFunction_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"behavior B1 {",
					"  def fct(x : int) : int {",
					"    yield",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inBehavior_inEventHandler_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"event Evt",
					"behavior B1 {",
					"  on Evt {",
					"    yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inBehavior_inConstructor_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"import static java.lang.Thread.*",
					"behavior B1 {",
					"  new (parent : Agent) {",
					"    super(parent)",
					"    yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_inFunction_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"skill S1 {",
					"  def fct(x : int) : int {",
					"    Thread::yield",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inSkill_inEventHandler_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event Evt",
					"skill S1 {",
					"  on Evt {",
					"    Thread::yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inSkill_inConstructor_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"skill S1 {",
					"  new {",
					"    Thread::yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}

		@Test
		@Tag("sarlValidation")
		public void inSkill_inFunction_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"skill S1 {",
					"  def fct(x : int) : int {",
					"    yield",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inSkill_inEventHandler_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"event Evt",
					"skill S1 {",
					"  on Evt {",
					"    yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inSkill_inConstructor_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"skill S1 {",
					"  new {",
					"    yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}

		@Test
		@Tag("sarlValidation")
		public void inClass_inFunction_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"class C1 {",
					"  def fct(x : int) : int {",
					"    Thread::yield",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inClass_inConstructor_01() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"class C1 {",
					"  new {",
					"    Thread::yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}

		@Test
		@Tag("sarlValidation")
		public void inClass_inFunction_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"class C1 {",
					"  def fct(x : int) : int {",
					"    yield",
					"    return x + 1",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}
	
		@Test
		@Tag("sarlValidation")
		public void inClass_inConstructor_02() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.Thread.*",
					"class C1 {",
					"  new {",
					"    yield",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"yield");
		}

	}

}

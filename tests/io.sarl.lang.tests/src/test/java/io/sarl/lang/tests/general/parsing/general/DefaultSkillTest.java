/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtend.lib.annotations.EqualsHashCode;
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import com.google.common.base.Strings;
import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class DefaultSkillTest extends AbstractSarlTest {
		
	@Test
	public void valid() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.core.DefaultSkill",
				"@DefaultSkill(typeof(S1))",
				"capacity C1 {",
				"  def myFct",
				"}",
				"skill S1 implements C1 {",
				"  def myFct {}",
				"}"
				));
		validate(mas).assertNoErrors();
	}

	@Test
	public void notASkill() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.core.DefaultSkill",
				"@DefaultSkill(typeof(S1))",
				"capacity C1 {",
				"  def myFct",
				"}",
				"class S1 {",
				"}"
				));
		validate(mas).assertError(
				XbasePackage.eINSTANCE.getXTypeLiteral(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
				"Type mismatch");
	}

	@Test
	public void notACapacityImplementation() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.core.DefaultSkill",
				"@DefaultSkill(typeof(S1))",
				"capacity C1 {",
				"  def myFct",
				"}",
				"capacity C2 {",
				"  def myFct2",
				"}",
				"skill S1 implements C2 {",
				"  def myFct2 {}",
				"}"
				));
		validate(mas).assertError(
				XbasePackage.eINSTANCE.getXTypeLiteral(),
				IssueCodes.INVALID_DEFAULT_SKILL_ANNOTATION,
				"Invalid annotation value. S1 is not an implementation of C1");
	}

	@Test
	public void validSubType() throws Exception {
		SarlScript mas = file(multilineString(
				"import io.sarl.lang.core.DefaultSkill",
				"@DefaultSkill(typeof(S1))",
				"capacity C1 {",
				"  def myFct",
				"}",
				"capacity C2 extends C1 {",
				"  def myFct2",
				"}",
				"skill S1 implements C2 {",
				"  def myFct {}",
				"  def myFct2 {}",
				"}"
				));
		validate(mas).assertNoErrors();
	}

}

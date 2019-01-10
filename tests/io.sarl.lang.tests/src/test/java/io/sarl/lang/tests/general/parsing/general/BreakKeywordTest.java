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

import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Test;

import com.google.common.base.Strings;
import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class BreakKeywordTest extends AbstractSarlTest {

	@Test
	public void insideFunction() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct {",
				"    break",
				"  }",
				"}"
				));
		validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBreakExpression(),
				IssueCodes.INVALID_USE_OF_LOOP_BREAKING_KEYWORD,
				"Invalid use of the break keyword");
	}

	@Test
	public void insideIfThen() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    if (a == 1) {",
				"      break",
				"    }",
				"  }",
				"}"
				));
		validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBreakExpression(),
				IssueCodes.INVALID_USE_OF_LOOP_BREAKING_KEYWORD,
				"Invalid use of the break keyword");
	}

	@Test
	public void insideField() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  var field = [",
				"    break",
				"  ]",
				"}"
				));
		validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBreakExpression(),
				IssueCodes.INVALID_USE_OF_LOOP_BREAKING_KEYWORD,
				"Invalid use of the break keyword");
	}

	@Test
	public void insideWhileWithoutBranch() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    var b = a",
				"    while (b > 0) {",
				"      break",
				"      b--",
				"    }",
				"  }",
				"}"
				));
		validate(mas).assertError(
				XbasePackage.eINSTANCE.getXPostfixOperation(),
				org.eclipse.xtext.xbase.validation.IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

	@Test
	public void insideWhileWithBranch() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    var b = a",
				"    while (b > 0) {",
				"      if (a == 5) break",
				"      b--",
				"    }",
				"  }",
				"}"
				));
		validate(mas).assertNoIssues();
	}

	@Test
	public void insideDoWhileWithoutBranch() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    var b = a",
				"    do {",
				"      break",
				"      b--",
				"    } while (b > 0)",
				"  }",
				"}"
				));
		validate(mas).assertError(
				XbasePackage.eINSTANCE.getXPostfixOperation(),
				org.eclipse.xtext.xbase.validation.IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

	@Test
	public void insideDoWhileWithBranch() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    var b = a",
				"    do {",
				"      if (a == 5) break",
				"      b--",
				"    } while (b > 0)",
				"  }",
				"}"
				));
		validate(mas).assertNoIssues();
	}

	@Test
	public void insideForWithoutBranch() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(b : 0..a) {",
				"      break",
				"    }",
				"  }",
				"}"
				));
		validate(mas).assertNoIssues();
	}

	@Test
	public void insideForWithBranch() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(b : 0..a) {",
				"      if (b == 5) break",
				"    }",
				"  }",
				"}"
				));
		validate(mas).assertNoIssues();
	}

	@Test
	public void insideBasicForWithoutBranch() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(var b = 0; b < a; b++) {",
				"      break",
				"    }",
				"  }",
				"}"
				));
		validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlBreakExpression(),
				IssueCodes.DISCOURAGED_LOOP_BREAKING_KEYWORD_USE,
				"Discouraged use of the break keyword inside a basic loop");
	}

	@Test
	public void insideBasicForWithBranch() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(var b = 0; b < a; b++) {",
				"      if (b == 5) break",
				"    }",
				"  }",
				"}"
				));
		validate(mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlBreakExpression(),
				IssueCodes.DISCOURAGED_LOOP_BREAKING_KEYWORD_USE,
				"Discouraged use of the break keyword inside a basic loop");
	}

	@Test
	public void unreachableCode() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(a : int) {",
				"    for(b : 0..a) {",
				"      if (b == 5) {",
				"        break",
				"        println(b)",
				"      }",
				"    }",
				"  }",
				"}"
				));
		validate(mas).assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				org.eclipse.xtext.xbase.validation.IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

}

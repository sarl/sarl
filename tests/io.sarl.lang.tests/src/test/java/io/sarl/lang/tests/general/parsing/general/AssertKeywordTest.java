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
public class AssertKeywordTest extends AbstractSarlTest {

	@Test
	public void assertTrue() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert true",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(mas).assertNoErrors();
	}

	@Test
	public void assertBooleanTrue() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert Boolean::TRUE",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(mas).assertNoErrors();
	}

	@Test
	public void assertFalse() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert false",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(mas).assertNoErrors();
	}

	@Test
	public void assertBooleanFalse() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert Boolean::FALSE",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(mas).assertNoErrors();
	}

	@Test
	public void assertOnParameter() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert x > 0",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(mas).assertNoErrors();
	}

	@Test
	public void assertComplexBooleanExpression() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    var y = x + 1",
				"    assert x > 0 && y < 100",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(mas).assertNoErrors();
	}

	@Test
	public void assertTrueWithMessage() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    assert true, \"Hello world!\"",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(mas).assertNoErrors();
	}

	@Test
	public void assertComplexBooleanExpressionWithMessage() throws Exception {
		SarlScript mas = file(multilineString(
				"agent A1 {",
				"  def fct(x : int) : int {",
				"    var y = x + 1",
				"    assert x > 0 && y < 100, \"That's all, folks!\"",
				"    return x + 1",
				"  }",
				"}"
				));
		validate(mas).assertNoErrors();
	}

}

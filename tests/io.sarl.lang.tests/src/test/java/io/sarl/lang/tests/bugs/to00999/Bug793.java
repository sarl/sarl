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

package io.sarl.lang.tests.bugs.to00999;

import static org.junit.Assert.*;

import java.util.ArrayList;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xbase.validation.UIStrings;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.util.Utils;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class for issue: Impossible d'assigner une valeur negative a une variable short
 *
 * <p>https://github.com/sarl/sarl/issues/793
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/793"
 * @see "https://github.com/eclipse/xtext-lib/issues/68"
 */
@SuppressWarnings("all")
public class Bug793 extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"class XXX {",
			"  def fct : short {",
			"    var temp_node_i : short = -1",
			"    temp_node_i",
			"  }",
			"}");
	
	private static final String SNIPSET2 = multilineString(
			"class XXX {",
			"  def fct : short {",
			"    var temp_node_i : short; temp_node_i = -1 as short",
			"    temp_node_i",
			"  }",
			"}");

	private static final String SNIPSET3 = multilineString(
			"class XXX {",
			"  def fct : short {",
			"    var temp_node_i : short = 1",
			"    temp_node_i",
			"  }",
			"}");

	private static final String SNIPSET4 = multilineString(
			"class XXX {",
			"  def fct : short {",
			"    var temp_node_i : short = 1 as short",
			"    temp_node_i",
			"  }",
			"}");

	private static final String SNIPSET5 = multilineString(
			"class XXX {",
			"  var temp_node_i : short = -1",
			"}");
	
	private static final String SNIPSET6 = multilineString(
			"class XXX {",
			"  var temp_node_i : short = -1 as short",
			"}");

	private static final String SNIPSET7 = multilineString(
			"class XXX {",
			"  var temp_node_i : short = 1",
			"}");

	private static final String SNIPSET8 = multilineString(
			"class XXX {",
			"  var temp_node_i : short = 1 as short",
			"}");

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(SNIPSET1);
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.INCOMPATIBLE_TYPES,
				"cannot convert from int to short");
	}

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(SNIPSET2);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}
	
	@Test
	public void parsing_03() throws Exception {
		SarlScript mas = file(SNIPSET3);
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXNumberLiteral(),
				IssueCodes.INCOMPATIBLE_TYPES,
				"cannot convert from int to short");
	}

	@Test
	public void parsing_04() throws Exception {
		SarlScript mas = file(SNIPSET4);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void parsing_05() throws Exception {
		SarlScript mas = file(SNIPSET5);
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXUnaryOperation(),
				IssueCodes.INCOMPATIBLE_TYPES,
				"cannot convert from int to short");
	}

	@Test
	public void parsing_06() throws Exception {
		SarlScript mas = file(SNIPSET6);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}
	
	@Test
	public void parsing_07() throws Exception {
		SarlScript mas = file(SNIPSET7);
		final Validator validator = validate(mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXNumberLiteral(),
				IssueCodes.INCOMPATIBLE_TYPES,
				"cannot convert from int to short");
	}

	@Test
	public void parsing_08() throws Exception {
		SarlScript mas = file(SNIPSET8);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

}


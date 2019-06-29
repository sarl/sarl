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

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Add quick fix for raw type warning.
 *
 * <p>https://github.com/sarl/sarl/issues/735
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({Bug735.NoIgnore.class, Bug735.Ignore.class})
@SuppressWarnings("all")
public class Bug735 {
	
	public static class NoIgnore extends AbstractSarlTest {

		private static final String SARL_PARAMETER = multilineString(
				"package io.sarl.lang.tests.bug735",
				"class Test {",
				"  def test(t : Class) : Object {",
				"    t.newInstance",
				"  }",
				"}");
		
		@Test
		public void validatingParameter() throws Exception {
			SarlScript mas = file(SARL_PARAMETER);
			final Validator validator = validate(mas);
			validator.assertWarning(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE,
					"Class is a raw type");
		}
	
		private static final String SARL_FIELD = multilineString(
				"package io.sarl.lang.tests.bug735",
				"class Test {",
				"  var f : Class",
				"  def test : Class<?> { f }",
				"}");
	
		@Test
		public void validatingField() throws Exception {
			SarlScript mas = file(SARL_FIELD);
			final Validator validator = validate(mas);
			validator.assertWarning(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE,
					"Class is a raw type");
		}
	
		private static final String SARL_RETURNTYPE = multilineString(
				"package io.sarl.lang.tests.bug735",
				"class Test {",
				"  var f : Class<?>",
				"  def test : Class { f }",
				"}");
	
		@Test
		public void validatingReturnType() throws Exception {
			SarlScript mas = file(SARL_RETURNTYPE);
			final Validator validator = validate(mas);
			validator.assertWarning(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE,
					"Class is a raw type");
		}
	
		private static final String SARL_LOCALVARIABLE = multilineString(
				"package io.sarl.lang.tests.bug735",
				"class Test {",
				"  def test : Class<?> {",
				"     var f : Class = null",
				"     f",
				"  }",
				"}");
	
		@Test
		public void validatingLocalVariable() throws Exception {
			SarlScript mas = file(SARL_LOCALVARIABLE);
			final Validator validator = validate(mas);
			validator.assertWarning(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE,
					"Class is a raw type");
		}
	
		private static final String SARL_EXTENDS = multilineString(
				"package io.sarl.lang.tests.bug735",
				"import java.util.Iterator",
				"interface Test extends Iterator {",
				"}");
	
		@Test
		public void validatingExtends() throws Exception {
			SarlScript mas = file(SARL_EXTENDS);
			final Validator validator = validate(mas);
			validator.assertWarning(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE,
					"Iterator is a raw type");
		}
	
		private static final String SARL_IMPLEMENTS = multilineString(
				"package io.sarl.lang.tests.bug735",
				"import java.util.Iterator",
				"abstract class Test implements Iterator {",
				"}");
	
		@Test
		public void validatingImplements() throws Exception {
			SarlScript mas = file(SARL_IMPLEMENTS);
			final Validator validator = validate(mas);
			validator.assertWarning(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE,
					"Iterator is a raw type");
		}
	
		private static final String SARL_INFERRED_RETURNTYPE = multilineString(
				"package io.sarl.lang.tests.bug735",
				"class Test {",
				"  var f : Class",
				"  def test { f }",
				"}");
	
		@Test
		public void validatingInferredReturnType() throws Exception {
			SarlScript mas = file(SARL_INFERRED_RETURNTYPE);
			final Validator validator = validate(mas);
			validator.assertWarning(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE,
					"Class is a raw type");
		}
	
		private static final String SARL_INFERRED_FIELD = multilineString(
				"package io.sarl.lang.tests.bug735",
				"import java.util.Iterator",
				"class It implements Iterator {",
				"  def hasNext : boolean { false }",
				"  def next : Object { null }",
				"}",
				"class Test {",
				"  var f  = new It",
				"  def test { f }",
				"}");
	
		@Test
		public void validatingInferredField() throws Exception {
			SarlScript mas = file(SARL_INFERRED_FIELD);
			final Validator validator = validate(mas);
			validator.assertWarning(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE,
					"Iterator is a raw type");
		}

	}

	public static class Ignore extends AbstractSarlTest {

		private static final String SARL_PARAMETER = multilineString(
				"package io.sarl.lang.tests.bug735",
				"class Test {",
				"  @SuppressWarnings(\"org.eclipse.xtext.xbase.validation.IssueCodes.raw_type\")",
				"  def test(t : Class) : Object {",
				"    t.newInstance",
				"  }",
				"}");
		
		@Test
		public void validatingParameter() throws Exception {
			SarlScript mas = file(SARL_PARAMETER);
			final Validator validator = validate(mas);
			validator.assertNoIssues();
		}
	
		private static final String SARL_FIELD = multilineString(
				"package io.sarl.lang.tests.bug735",
				"class Test {",
				"  @SuppressWarnings(\"org.eclipse.xtext.xbase.validation.IssueCodes.raw_type\")",
				"  var f : Class",
				"  def test : Class<?> { f }",
				"}");
	
		@Test
		public void validatingField() throws Exception {
			SarlScript mas = file(SARL_FIELD);
			final Validator validator = validate(mas);
			validator.assertNoIssues();
		}
	
		private static final String SARL_RETURNTYPE = multilineString(
				"package io.sarl.lang.tests.bug735",
				"class Test {",
				"  var f : Class<?>",
				"  @SuppressWarnings(\"org.eclipse.xtext.xbase.validation.IssueCodes.raw_type\")",
				"  def test : Class { f }",
				"}");
	
		@Test
		public void validatingReturnType() throws Exception {
			SarlScript mas = file(SARL_RETURNTYPE);
			final Validator validator = validate(mas);
			validator.assertNoIssues();
		}
	
		private static final String SARL_LOCALVARIABLE = multilineString(
				"package io.sarl.lang.tests.bug735",
				"class Test {",
				"  @SuppressWarnings(\"org.eclipse.xtext.xbase.validation.IssueCodes.raw_type\")",
				"  def test : Class<?> {",
				"    var f : Class = null",
				"    f",
				"  }",
				"}");
	
		@Test
		public void validatingLocalVariable() throws Exception {
			SarlScript mas = file(SARL_LOCALVARIABLE);
			final Validator validator = validate(mas);
			validator.assertNoIssues();
		}
	
		private static final String SARL_EXTENDS = multilineString(
				"package io.sarl.lang.tests.bug735",
				"import java.util.Iterator",
				"@SuppressWarnings(\"org.eclipse.xtext.xbase.validation.IssueCodes.raw_type\")",
				"interface Test extends Iterator {",
				"}");
	
		@Test
		public void validatingExtends() throws Exception {
			SarlScript mas = file(SARL_EXTENDS);
			final Validator validator = validate(mas);
			validator.assertNoIssues();
		}
	
		private static final String SARL_IMPLEMENTS = multilineString(
				"package io.sarl.lang.tests.bug735",
				"import java.util.Iterator",
				"@SuppressWarnings(\"org.eclipse.xtext.xbase.validation.IssueCodes.raw_type\")",
				"abstract class Test implements Iterator {",
				"}");
	
		@Test
		public void validatingImplements() throws Exception {
			SarlScript mas = file(SARL_IMPLEMENTS);
			final Validator validator = validate(mas);
			validator.assertNoIssues();
		}
	
		private static final String SARL_INFERRED_RETURNTYPE = multilineString(
				"package io.sarl.lang.tests.bug735",
				"class Test {",
				"  @SuppressWarnings(\"org.eclipse.xtext.xbase.validation.IssueCodes.raw_type\")",
				"  var f : Class",
				"  @SuppressWarnings(\"org.eclipse.xtext.xbase.validation.IssueCodes.raw_type\")",
				"  def test { f }",
				"}");
	
		@Test
		public void validatingInferredReturnType() throws Exception {
			SarlScript mas = file(SARL_INFERRED_RETURNTYPE);
			final Validator validator = validate(mas);
			validator.assertNoIssues();
		}
	
		private static final String SARL_INFERRED_FIELD = multilineString(
				"package io.sarl.lang.tests.bug735",
				"import java.util.Iterator",
				"@SuppressWarnings(\"org.eclipse.xtext.xbase.validation.IssueCodes.raw_type\")",
				"class It implements Iterator {",
				"  def hasNext : boolean { false }",
				"  def next : Object { null }",
				"}",
				"class Test {",
				"  var f  = new It",
				"  def test { f }",
				"}");
	
		@Test
		public void validatingInferredField() throws Exception {
			SarlScript mas = file(SARL_INFERRED_FIELD);
			final Validator validator = validate(mas);
			validator.assertNoIssues();
		}

	}

}

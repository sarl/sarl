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
import static io.sarl.tests.api.tools.TestUtils.multilineString2;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import java.io.IOException;

import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.compiler.IAppendable;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.output.FakeTreeAppendable;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import com.google.inject.Inject;
import com.google.inject.Injector;

import foo.ecore.SubEvent;
import io.sarl.lang.codebuilder.CodeBuilderFactory;
import io.sarl.lang.codebuilder.appenders.SarlSkillSourceAppender;
import io.sarl.lang.codebuilder.appenders.ScriptSourceAppender;
import io.sarl.lang.codebuilder.builders.ISarlBehaviorUnitBuilder;
import io.sarl.lang.codebuilder.builders.ISarlCapacityBuilder;
import io.sarl.lang.codebuilder.builders.ISarlSkillBuilder;
import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.core.Skill;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;


/** Testing class for issue: Specification and implementation of communication protocols.
 *
 * <p>https://github.com/sarl/sarl/issues/1130
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1130"
 */
@DisplayName("Bug #1130")
@SuppressWarnings("all")
@Tag("core")
@Tag("unit")
public class Bug1130Test {

	@Nested
	@DisplayName("BSPL keywords outside regular keywords")
	public class IgnoredKeywordsTest extends AbstractSarlTest { 

		private final String SARL_CODE_OUT = multilineString(
				"package io.sarl.lang.tests.bug1130",
				"class A {",
				"   def prog : Object {",
				"     var out = \"abc\"",
				"     return out",
				"   }",
				"}");

		@Test
		@DisplayName("out")
		@Tag("sarlParsing")
		public void parsing_out() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_OUT);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_IN = multilineString(
				"package io.sarl.lang.tests.bug1130",
				"class A {",
				"   def prog : Object {",
				"     var in = \"abc\"",
				"     return in",
				"   }",
				"}");

		@Test
		@DisplayName("in")
		@Tag("sarlParsing")
		public void parsing_in() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_IN);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_NIL = multilineString(
				"package io.sarl.lang.tests.bug1130",
				"class A {",
				"   def prog : Object {",
				"     var nil = \"abc\"",
				"     return nil",
				"   }",
				"}");

		@Test
		@DisplayName("nil")
		@Tag("sarlParsing")
		public void parsing_nil() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_NIL);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_OPT = multilineString(
				"package io.sarl.lang.tests.bug1130",
				"class A {",
				"   def prog : Object {",
				"     var opt = \"abc\"",
				"     return opt",
				"   }",
				"}");

		@Test
		@DisplayName("opt")
		@Tag("sarlParsing")
		public void parsing_opt() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_OPT);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_ANY = multilineString(
				"package io.sarl.lang.tests.bug1130",
				"class A {",
				"   def prog : Object {",
				"     var any = \"abc\"",
				"     return any",
				"   }",
				"}");

		@Test
		@DisplayName("any")
		@Tag("sarlParsing")
		public void parsing_any() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_ANY);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_KEY = multilineString(
				"package io.sarl.lang.tests.bug1130",
				"class A {",
				"   def prog : Object {",
				"     var key = \"abc\"",
				"     return key",
				"   }",
				"}");

		@Test
		@DisplayName("key")
		@Tag("sarlParsing")
		public void parsing_key() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_KEY);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_PARAMETER = multilineString(
				"package io.sarl.lang.tests.bug1130",
				"class A {",
				"   def prog : Object {",
				"     var parameter = \"abc\"",
				"     return parameter",
				"   }",
				"}");

		@Test
		@DisplayName("parameter")
		@Tag("sarlParsing")
		public void parsing_parameter() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_PARAMETER);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.tests.api.tools;

import java.util.List;

import com.google.inject.Injector;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
import org.eclipse.xtext.validation.Issue;

import io.sarl.lang.sarl.SarlScript;

/** Set of utilities for validating SARL code.
 *
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class TestValidator {

	private TestValidator() {
		//
	}

	/** Validate the given file and reply the validator.
	 *
	 * @param validationHelper the helper to the SARL validator.
	 * @param injector the injector used by SARL.
	 * @param script the script to validate.
	 * @return the validator.
	 */
	public static Validator validate(ValidationTestHelper validationHelper, Injector injector, SarlScript script) {
		return validate(validationHelper, injector, script.eResource());
	}

	/** Validate the given resource and replies the validator.
	 *
	 * @param validationHelper the helper to the SARL validator.
	 * @param injector the injector used by SARL.
	 * @param resource the resource to validate.
	 * @return the validator.
	 */
	public static Validator validate(ValidationTestHelper validationHelper, Injector injector, Resource resource) {
		assert validationHelper != null;
		assert injector != null;
		Validator validator = new XtextValidator(resource, validationHelper);
		injector.injectMembers(validator);
		return validator;
	}

	/** Validation helper on a specific resource.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public interface Validator {

		List<Issue> getIssues();

		Validator assertNoIssues();

		Validator assertNoErrors();

		Validator assertNoError(String issuecode);

		Validator assertNoErrors(EClass objectType, String code, String... messageParts);

		Validator assertNoErrors(String code);

		Validator assertNoIssues(EClass objectType);

		Validator assertNoIssue(EClass objectType, String issuecode);

		Validator assertError(EClass objectType, String code, String... messageParts);

		Validator assertIssue(EClass objectType, String code, Severity severity, String... messageParts);

		Validator assertNoIssues(EClass objectType, String code, Severity severity, String... messageParts);

		Validator assertWarning(EClass objectType, String code, String... messageParts);

		Validator assertNoWarnings(EClass objectType, String code, String... messageParts);

	}

	/** Wrapper for the validation helper on a specific resource.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class XtextValidator implements Validator {

		private Resource resource;

		private ValidationTestHelper testHelper;

		/** Constructor.
		 *
		 * @param resource the resource to validate.
		 * @param testHelper the validator.
		 */
		public XtextValidator(Resource resource, ValidationTestHelper testHelper) {
			this.resource = resource;
			this.testHelper = testHelper;
		}

		@Override
		protected void finalize() throws Throwable {
			this.resource = null;
			this.testHelper = null;
		}

		public List<Issue> getIssues() {
			return this.testHelper.validate(this.resource);
		}

		public Validator assertNoIssues() {
			this.testHelper.assertNoIssues(this.resource);
			return this;
		}

		public Validator assertNoErrors() {
			this.testHelper.assertNoErrors(this.resource);
			return this;
		}

		public Validator assertNoError(String issuecode) {
			this.testHelper.assertNoError(this.resource, issuecode);
			return this;
		}

		public Validator assertNoErrors(EClass objectType, String code, String... messageParts) {
			this.testHelper.assertNoErrors(this.resource, objectType, code, messageParts);
			return this;
		}

		public Validator assertNoErrors(String code) {
			this.testHelper.assertNoErrors(this.resource, code);
			return this;
		}

		public Validator assertNoIssues(EClass objectType) {
			this.testHelper.assertNoIssues(this.resource, objectType);
			return this;
		}

		public Validator assertNoIssue(EClass objectType, String issuecode) {
			this.testHelper.assertNoIssue(this.resource, objectType, issuecode);
			return this;
		}

		public Validator assertError(EClass objectType, String code, String... messageParts) {
			this.testHelper.assertError(this.resource, objectType, code, messageParts);
			return this;
		}

		public Validator assertIssue(EClass objectType, String code, Severity severity, String... messageParts) {
			this.testHelper.assertIssue(this.resource, objectType, code, severity, messageParts);
			return this;
		}

		public Validator assertNoIssues(EClass objectType, String code, Severity severity, String... messageParts) {
			this.testHelper.assertNoIssues(this.resource, objectType, code, severity, messageParts);
			return this;
		}

		public Validator assertWarning(EClass objectType, String code, String... messageParts) {
			this.testHelper.assertWarning(this.resource, objectType, code, messageParts);
			return this;
		}

		public Validator assertNoWarnings(EClass objectType, String code, String... messageParts) {
			this.testHelper.assertNoWarnings(this.resource, objectType, code, messageParts);
			return this;
		}

	}
}

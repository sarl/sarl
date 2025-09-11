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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.tests.api.tools;

import static com.google.common.collect.Iterables.filter;
import static com.google.common.collect.Iterables.isEmpty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Issue;
import org.junit.ComparisonFailure;

import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;
import com.google.inject.Injector;

/** Set of utilities for validating SARL code.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
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
	public static Validator validate(SarlValidationTestHelper validationHelper, Injector injector, XtendFile script) {
		return validate(validationHelper, injector, script.eResource());
	}

	/** Validate the given resource and replies the validator.
	 *
	 * @param validationHelper the helper to the SARL validator.
	 * @param injector the injector used by SARL.
	 * @param resource the resource to validate.
	 * @return the validator.
	 */
	public static Validator validate(SarlValidationTestHelper validationHelper, Injector injector, Resource resource) {
		assert validationHelper != null;
		assert injector != null;
		var validator = new XtextValidator(resource, validationHelper);
		injector.injectMembers(validator);
		return validator;
	}

	/** Validation helper on a specific resource. This validator keep track of all the resources.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public interface Validator {

		/** Replies the list of issues that are not yet consumed by the validator.
		 *
		 * @return the list of issues that are not yet consumed by the validator.
		 */
		List<Issue> getIssues();

		/** Check if the list of issues is empty.
		 *
		 * @return the validator.
		 */
		Validator assertNoIssues();

		/** Check if the list of issues contains no error.
		 *
		 * @return the validator.
		 */
		Validator assertNoErrors();

		/** Check if the list of issues contains no error with the given code.
		 *
		 * @param issuecode the issue code to match.
		 * @return the validator.
		 */
		Validator assertNoError(String issuecode);

		/** Check if the list of issues contains no error with the given description.
		 *
		 * @param objectType the type of object on which the error must not be attached.
		 * @param code the issue code not to match.
		 * @param messageParts the parts of the message that must not be found.
		 * @return the validator.
		 */
		Validator assertNoErrors(EClass objectType, String code, String... messageParts);

		/** Check if the list of issues contains no error with the given code.
		 *
		 * @param code the issue code to match.
		 * @return the validator.
		 */
		Validator assertNoErrors(String code);

		/** Check if the list of issues has no issue on an element of the given type.
		 *
		 * @param objectType the type of the element to not match.
		 * @return the validator.
		 */
		Validator assertNoIssues(EClass objectType);

		/** Check if the list of issues has no issue on an element of the given type and the given code.
		 *
		 * @param objectType the type of the element to not match.
		 * @param issuecode the issue code to not match.
		 * @return the validator.
		 */
		Validator assertNoIssue(EClass objectType, String issuecode);

		/** Check if the list of issues contains an error with the given description.
		 * The matching errors are removed from the validator memory.
		 *
		 * @param objectType the type of object on which the error must be attached.
		 * @param code the issue code to match.
		 * @param messageParts the parts of the message that must be found.
		 * @return the validator.
		 */
		Validator assertError(EClass objectType, String code, String... messageParts);

		/** Check if the list of issues contains an error with the given description.
		 * The matching errors are removed from the validator memory.
		 *
		 * @param objectType the type of object on which the error must be attached.
		 * @param messageParts the parts of the message that must be found.
		 * @return the validator.
		 * @since 0.15
		 */
		Validator assertErrorWithoutCode(EClass objectType, String... messageParts);

		/** Check if the list of issues contains an issue with the given description.
		 * The matching errors are removed from the validator memory.
		 *
		 * @param objectType the type of object on which the issue must be attached.
		 * @param code the issue code to match.
		 * @param severity expected severity.
		 * @param messageParts the parts of the message that must be found.
		 * @return the validator.
		 */
		Validator assertIssue(EClass objectType, String code, Severity severity, String... messageParts);

		/** Check if the list of issues contains no issue with the given description.
		 *
		 * @param objectType the type of object on which the issue must not be attached.
		 * @param code the issue code not to match.
		 * @param severity expected severity.
		 * @param messageParts the parts of the message that must not be found.
		 * @return the validator.
		 */
		Validator assertNoIssues(EClass objectType, String code, Severity severity, String... messageParts);

		/** Check if the list of issues contains a warning with the given description.
		 * The matching errors are removed from the validator memory.
		 *
		 * @param objectType the type of object on which the issue must be attached.
		 * @param code the issue code to match.
		 * @param messageParts the parts of the message that must be found.
		 * @return the validator.
		 */
		Validator assertWarning(EClass objectType, String code, String... messageParts);

		/** Check if the list of issues contains a warning with the given description.
		 * The matching errors are removed from the validator memory.
		 *
		 * @param objectType the type of object on which the issue must be attached.
		 * @param code the issue code to match.
		 * @param offset the position of the warning from the beginning of the resource.
		 * @param messageParts the parts of the message that must be found.
		 * @return the validator.
		 */
		Validator assertWarning(EClass objectType, String code, int offset, String... messageParts);

		/** Check if the list of issues contains no warning with the given description.
		 *
		 * @param objectType the type of object on which the warning must not be attached.
		 * @param code the issue code not to match.
		 * @param messageParts the parts of the message that must not be found.
		 * @return the validator.
		 */
		Validator assertNoWarnings(EClass objectType, String code, String... messageParts);

	}

	/** Wrapper for the validation helper on a specific resource.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class XtextValidator implements Validator {

		private Resource resource;

		private SarlValidationTestHelper testHelper;

		private List<Issue> issues;

		/** Constructor.
		 *
		 * @param resource the resource to validate.
		 * @param testHelper the validator.
		 */
		public XtextValidator(Resource resource, SarlValidationTestHelper testHelper) {
			this.resource = resource;
			this.testHelper = testHelper;
		}

		@Override
		@Deprecated(since = "0.10", forRemoval = true)
		protected void finalize() throws Throwable {
			this.resource = null;
			this.testHelper = null;
			this.issues = null;
		}

		@Override
		public List<Issue> getIssues() {
			if (this.issues == null) {
				this.issues = this.testHelper.validate(this.resource);
			}
			return this.issues;
		}

		@Override
		public Validator assertNoIssues() {
			final var issues = getIssues();
			if (!isEmpty(issues)) {
				final var actual = this.testHelper.getIssuesAsString(this.resource, issues, new StringBuilder()).toString();
				throw new ComparisonFailure("Expected no issues, but got :" + actual, //$NON-NLS-1$
						"", actual); //$NON-NLS-1$
			}
			return this;
		}

		@Override
		public Validator assertNoErrors() {
			final var issues = getIssues();
			final var fissues = filter(issues, input -> Severity.ERROR == input.getSeverity());
			if (!isEmpty(fissues)) {
				final var actual = this.testHelper.getIssuesAsString(this.resource, issues, new StringBuilder()).toString();
				throw new ComparisonFailure("Expected no errors, but got :" + actual, //$NON-NLS-1$
						"", actual); //$NON-NLS-1$
			}
			return this;
		}

		@Override
		public Validator assertNoError(String issuecode) {
			final var issues = getIssues();
			final var fissues = filter(issues, input -> issuecode.equals(input.getCode()));
			if (!isEmpty(fissues)) {
				fail("Expected no error '" + issuecode + "' but got " //$NON-NLS-1$ //$NON-NLS-2$
						+ this.testHelper.getIssuesAsString(this.resource, issues, new StringBuilder()));
			}
			return this;
		}

		private Validator assertNoIssues(Severity severity, EClass objectType, String code, String... messageParts) {
			final var issues = getIssues();
			final var fissues = this.testHelper.matchIssues(this.resource, objectType, code, -1, -1,
					severity, issues, messageParts);
			if (!Iterables.isEmpty(fissues)) {
				final var message = new StringBuilder("Expected no ") //$NON-NLS-1$
					.append(severity)
					.append(" '") //$NON-NLS-1$
					.append(code)
					.append("' on ") //$NON-NLS-1$
					.append(objectType.getName())
					.append(" but got\n"); //$NON-NLS-1$
				this.testHelper.getIssuesAsString(this.resource, issues, message);
				assertEquals(Joiner.on('\n').join(messageParts), message.toString());
				fail(message.toString());
			}
			return this;
		}

		@Override
		public Validator assertNoErrors(EClass objectType, String code, String... messageParts) {
			return assertNoIssues(Severity.ERROR, objectType, code, messageParts);
		}

		@Override
		public Validator assertNoErrors(String code) {
			return assertNoIssues(Severity.ERROR, EcorePackage.Literals.EOBJECT, code);
		}

		@Override
		public Validator assertNoIssues(EClass objectType) {
			final var issues = getIssues();
			final var fissues = filter(issues, input -> {
					final EObject object = this.resource.getEObject(input.getUriToProblem().fragment());
					if (objectType.isInstance(object)) {
						return true;
					}
					return false;
				});
			if (!isEmpty(fissues)) {
				fail("Expected no error on instances of  '" + objectType.getName() + "' but got " //$NON-NLS-1$ //$NON-NLS-2$
						+ this.testHelper.getIssuesAsString(this.resource, fissues, new StringBuilder()));
			}
			return this;
		}

		@Override
		public Validator assertNoIssue(EClass objectType, String issuecode) {
			final var issues = getIssues();
			final var fissues = filter(issues, input -> {
					if (issuecode.equals(input.getCode())) {
						final var object = this.resource.getEObject(input.getUriToProblem().fragment());
						if (objectType.isInstance(object)) {
							return true;
						}
					}
					return false;
				});
			if (!isEmpty(fissues)) {
				fail("Expected no error '" + issuecode + "' but got " //$NON-NLS-1$ //$NON-NLS-2$
						+ this.testHelper.getIssuesAsString(this.resource, fissues, new StringBuilder()));
			}
			return this;
		}

		@Override
		public Validator assertNoIssues(EClass objectType, String code, Severity severity, String... messageParts) {
			return assertNoIssues(severity, objectType, code, messageParts);
		}

		@Override
		public Validator assertNoWarnings(EClass objectType, String code, String... messageParts) {
			return assertNoIssues(Severity.WARNING, objectType, code, messageParts);
		}

		private void assertIssue(Severity severity, EClass objectType, String code, int offset, String... messageParts) {
			assertNotNull(objectType);
			final var issues = getIssues();
			final var fissues = this.testHelper.matchIssues(this.resource, objectType, code, offset,
					-1, severity, issues, messageParts);
			if (isEmpty(fissues)) {
				final var message = new StringBuilder("Expected ") //$NON-NLS-1$
					.append(severity);
				if (!Strings.isEmpty(code)) {
					message.append("'") //$NON-NLS-1$
						.append(code)
						.append("' "); //$NON-NLS-1$
				} else {
					message.append("error "); //$NON-NLS-1$
				}
				message.append("on ") //$NON-NLS-1$
					.append(objectType.getName())
					.append(" but got\n"); //$NON-NLS-1$
				message.append(this.testHelper.getIssuesAsString(this.resource, issues, message));
				assertEquals(Joiner.on('\n').join(messageParts), message.toString());
				fail(message.toString());
			} else {
				final var issueTab = Iterables.toArray(fissues, Issue.class);
				for (final var removableIssue : issueTab) {
					this.issues.remove(removableIssue);
				}
			}
		}

		@Override
		public Validator assertError(EClass objectType, String code, String... messageParts) {
			assertIssue(Severity.ERROR, objectType, code, -1, messageParts);
			return this;
		}
		
		@Override
		public Validator assertErrorWithoutCode(EClass objectType, String... messageParts) {
			assertIssue(Severity.ERROR, objectType, null, -1, messageParts);
			return this;
		}

		@Override
		public Validator assertIssue(EClass objectType, String code, Severity severity, String... messageParts) {
			assertIssue(severity, objectType, code, -1, messageParts);
			return this;
		}

		@Override
		public Validator assertWarning(EClass objectType, String code, String... messageParts) {
			assertIssue(Severity.WARNING, objectType, code, -1, messageParts);
			return this;
		}

		@Override
		public Validator assertWarning(EClass objectType, String code, int offset, String... messageParts) {
			assertIssue(Severity.WARNING, objectType, code, offset, messageParts);
			return this;
		}

	}

}

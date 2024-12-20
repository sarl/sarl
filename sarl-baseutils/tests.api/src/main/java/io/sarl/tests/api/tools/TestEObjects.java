/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.testing.validation.ValidationTestHelper;
import org.eclipse.xtext.validation.Issue;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;

/** Set of additional utilities for created testing EObject, except those related to SARL concepts.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class TestEObjects {

	private TestEObjects() {
		//
	}

	/** Validate the given file and reply the issues.
	 *
	 * @param validationHelper the validation test helper.
	 * @param file the resource to validate.
	 * @return the list of issues.
	 */
	public static List<Issue> issues(ValidationTestHelper validationHelper, XtendFile file) {
		return issues(validationHelper, file.eResource());
	}

	/** Validate the given resource and reply the issues.
	 *
	 * @param validationHelper the validation test helper.
	 * @param resource the resource to validate.
	 * @return the list of issues.
	 */
	public static List<Issue> issues(ValidationTestHelper validationHelper, Resource resource) {
		assert validationHelper != null;
		return validationHelper.validate(resource);
	}

	/** Create an instance of class.
	 *
	 * @param <T> the type of the script.
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the content of the file.
	 * @param resourceSet the set of resources in which the file is created.
	 * @return the SARL script extracted from the file content.
	 * @throws Exception 
	 * @since 0.9
	 */
	public static <T extends XtendFile> T file(ParseHelper<T> parser, ValidationTestHelper validationHelper,
			String string, ResourceSet resourceSet) throws Exception {
		assert parser != null;
		T script;
		if (resourceSet == null) {
			script = parser.parse(string);
		} else {
			script = parser.parse(string, resourceSet);
		}
		if (validationHelper != null) {
			var resource = script.eResource();
			var resourceSet0 = resource.getResourceSet();
			if (resourceSet0 instanceof XtextResourceSet xrs) {
				xrs.setClasspathURIContext(TestEObjects.class);
			}
			assertEquals(0, resource.getErrors().size(), () -> resource.getErrors().toString());
			var issues = Collections2.filter(issues(validationHelper, resource), new Predicate<Issue>() {
				@Override
				public boolean apply(Issue input) {
					return input.getSeverity() == Severity.ERROR;
				}
			});
			if (!issues.isEmpty()) {
				fail("Expected no errors, but got :" + getIssuesAsString(resource, issues, new StringBuilder()).toString()); //$NON-NLS-1$
			}
		}
		return script;
	}

	private static StringBuilder getIssuesAsString(Resource resource, Iterable<Issue> issues, StringBuilder result) {
		for (Issue issue : issues) {
			URI uri = issue.getUriToProblem();
			result.append(issue.getSeverity());
			result.append(" ("); //$NON-NLS-1$
			result.append(issue.getCode());
			result.append(") '"); //$NON-NLS-1$
			result.append(issue.getMessage());
			result.append("'"); //$NON-NLS-1$
			if (uri != null) {
				EObject eObject = resource.getResourceSet().getEObject(uri, true);
				result.append(" on "); //$NON-NLS-1$
				result.append(eObject.eClass().getName());
			}
			result.append(", offset " + issue.getOffset() + ", length " + issue.getLength()); //$NON-NLS-1$ //$NON-NLS-2$
			result.append("\n"); //$NON-NLS-1$
		}
		return result;
	}

	/** Create an instance of class.
	 *
	 * @param <T> the type of the script.
	 * @param parser the SARL parser.
	 * @param validationHelper the validation test helper. If it is {@code null}, no validation.
	 * @param string the content of the file.
	 * @return the SARL script extracted from the file content.
	 * @throws Exception 
	 */
	public static <T extends XtendFile> T file(ParseHelper<T> parser, ValidationTestHelper validationHelper, String string) throws Exception {
		return file(parser, validationHelper, string, null);
	}

	/** Create a script.
	 *
	 * @param <T> the type of the script.
	 * @param parser the SARL parser.
	 * @param string the content of the file.
	 * @return the SARL script extracted from the file content.
	 * @throws Exception 
	 */
	public static <T extends XtendFile> T file(ParseHelper<T> parser, String string) throws Exception {
		return file(parser, null, string);
	}

	/** Build a path.
	 *
	 * @param path path elements.
	 * @return the path.
	 * @since 0.13
	 */
	public static IPath path(String... path) {
		assert(path != null && path.length > 0);
		IPath p = new Path(path[0]);
		for(var i=1; i<path.length; ++i) {
			p = p.append(path[i]);
		}
		return p;
	}

	/** Build a path.
	 *
	 * @param path path elements.
	 * @return the path.
	 * @since 0.13
	 */
	public static String pathStr(String... path) {
		return path(path).toOSString();
	}

	/** Replies the default package name where unit test files are located.
	 * 
	 * @return the package name.
	 * @since 0.13
	 */
	public static String getDefaultTestPackage() {
		return "io.sarl.tests"; //$NON-NLS-1$
	}

}

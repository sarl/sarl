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
package io.sarl.lang.tests.modules.formatting2;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collections;

import javax.inject.Inject;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.resource.IResourceFactory;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.StringInputStream;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.formatting2.FormatterFacade;
import io.sarl.tests.api.AbstractSarlTest;

/** Test of a SARL formatter facade.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Formatter facade")
@Tag("core")
@Tag("codeFormat")
public class FormatterFacadeTest extends AbstractSarlTest {

	@Inject
	private FormatterFacade facade;

	@Inject
	private IResourceFactory resourceFactory;

	@Test
	@DisplayName("Format string 0")
	public void formatString0() {
		String source = "event E1 { var i : int }"
				+ "agent A1 {"
				+ "private var myval=1 "
				+ "on E1 [ occurrence.i===1 ] {"
				+ "System.out.println(occurrence)"
				+ "}"
				+ "private def myfct{}"
				+ "}";
		String expected = multilineString(
				"event E1 {",
				"	var i : int",
				"}",
				"",
				"agent A1 {",
				"	private var myval = 1",
				"",
				"	on E1 [occurrence.i === 1] {",
				"		System.out.println(occurrence)",
				"	}",
				"",
				"	private def myfct {",
				"	}",
				"}",
				"");
		String actual = this.facade.format(source);
		assertEquals(expected, actual);
	}

	@Test
	@DisplayName("Format string 1")
	public void formatString1() {
		String source = "/*Top comment*/agent Myagent {}";
		String expected = multilineString(
				"/* Top comment",
				" */",
				"agent Myagent {",
				"}",
				"");
		String actual = this.facade.format(source);
		assertEquals(expected, actual);
	}

	@Test
	@DisplayName("Format string 2")
	public void formatString2() {
		String source = multilineString(
				"/*Top comment.",
				"Second line.",
				"    Third line.*/agent Myagent {}");
		String expected = multilineString(
				"/* Top comment.",
				" * Second line.",
				" * Third line.",
				" */",
				"agent Myagent {",
				"}",
				"");
		String actual = this.facade.format(source);
		assertEquals(expected, actual);
	}

	@Test
	@DisplayName("Format string 3")
	public void formatString3() {
		String source = multilineString(
				"/* Top comment.",
				" * Second line.",
				" * Third line.",
				" */agent Myagent {",
				"}");
		String expected = multilineString(
				"/* Top comment.",
				" * Second line.",
				" * Third line.",
				" */",
				"agent Myagent {",
				"}",
				"");
		String actual = this.facade.format(source);
		assertEquals(expected, actual);
	}

	private void assertResourceContentFormat(String source, String expected) throws IOException {
		final ResourceSet resourceSet = new XtextResourceSet();
		final URI createURI = URI.createURI("synthetic://to-be-formatted.sarl"); //$NON-NLS-1$
		final XtextResource resource = (XtextResource) this.resourceFactory.createResource(createURI);
		resourceSet.getResources().add(resource);
		try (StringInputStream stringInputStream = new StringInputStream(source)) {
			resource.load(stringInputStream, Collections.emptyMap());
		}
		this.facade.format(resource);
		final String actual; 
		try (ByteArrayOutputStream stringOutputStream = new ByteArrayOutputStream()) {
			resource.save(stringOutputStream, Collections.emptyMap());
			stringOutputStream.flush();
			actual = stringOutputStream.toString();
		}
		assertEquals(expected, actual);
	}

	@Test
	@DisplayName("Format resource")
	@Disabled
	public void formatResource0() throws IOException {
		String source = "event E1 { var i : int }"
				+ "agent A1 {"
				+ "private var myval=1 "
				+ "on E1 [ occurrence.i===1 ] {"
				+ "System.out.println(occurrence)"
				+ "}"
				+ "private def myfct{}"
				+ "}";
		String expected = multilineString(
				"event E1 {",
				"	var i : int",
				"}",
				"agent A1 {",
				"	private var myval = 1 ",
				"	on E1 [occurrence.i === 1] {",
				"		System.out.println(occurrence)",
				"	}",
				"	private def myfct {",
				"	}",
				"}",
				"");
		assertResourceContentFormat(source, expected);
	}

}

/*
 * Copyright (C) 2014-2015 the original authors or authors.
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
package io.sarl.lang.tests.formatting2;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Named;

import com.google.inject.Inject;
import junit.framework.TestSuite;
import org.eclipse.xtext.junit4.formatter.FormatterTestRequest;
import org.eclipse.xtext.junit4.formatter.FormatterTester;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.junit.Test;
import org.junit.internal.builders.AllDefaultPossibilitiesBuilder;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.model.InitializationError;

import io.sarl.tests.api.AbstractSarlTest;

/** Tests for formatting events.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class EventFormatterTest extends AbstractFormatterTest {
	
	@Test
	public void empty() throws Exception {
		String source = "event  EntityX{}";
		String expected = multilineString(
				"event EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void modifiers() throws Exception {
		String source = "public    static    event EntityX{}";
		String expected = multilineString(
				"public static event EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void twoAnnotations() throws Exception {
		String source = "@Pure@Beta    event EntityX{}";
		String expected = multilineString(
				"@Pure @Beta event EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void threeAnnotations() throws Exception {
		String source = "@Pure@Beta\n@Hello    event EntityX{}";
		String expected = multilineString(
				"@Pure @Beta",
				"@Hello event EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void annotationValue() throws Exception {
		String source = "@SuppressWarnings(        value= \"name\"   )event EntityX{}";
		String expected = multilineString(
				"@SuppressWarnings(value = \"name\") event EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void annotationImplicitValue() throws Exception {
		String source = "@SuppressWarnings(   \"name\"   )event EntityX{}";
		String expected = multilineString(
				"@SuppressWarnings(\"name\") event EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void extend() throws Exception {
		String source = "event EntityX    extends    EntityY {}";
		String expected = multilineString(
				"event EntityX extends EntityY {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void memberIndent() throws Exception {
		String source = "event EntityX{var x:int}";
		String expected = multilineString(
				"event EntityX {",
				"	var x : int",
				"}",
				"");
		assertFormatted(source, expected);
	}

}
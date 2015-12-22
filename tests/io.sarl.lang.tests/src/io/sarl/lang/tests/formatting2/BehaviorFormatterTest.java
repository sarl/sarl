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

/** Tests for formatting agents.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class BehaviorFormatterTest extends AbstractFormatterTest {
	
	@Test
	public void empty() throws Exception {
		String source = "behavior  EntityX{}";
		String expected = multilineString(
				"behavior EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void modifiers() throws Exception {
		String source = "public    static    behavior EntityX{}";
		String expected = multilineString(
				"public static behavior EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void twoAnnotations() throws Exception {
		String source = "@Pure@Beta    behavior EntityX{}";
		String expected = multilineString(
				"@Pure @Beta behavior EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void threeAnnotations() throws Exception {
		String source = "@Pure@Beta\n@Hello    behavior EntityX{}";
		String expected = multilineString(
				"@Pure @Beta",
				"@Hello behavior EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void annotationValue() throws Exception {
		String source = "@SuppressWarnings(        value= \"name\"   )behavior EntityX{}";
		String expected = multilineString(
				"@SuppressWarnings(value = \"name\") behavior EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void annotationImplicitValue() throws Exception {
		String source = "@SuppressWarnings(   \"name\"   )behavior EntityX{}";
		String expected = multilineString(
				"@SuppressWarnings(\"name\") behavior EntityX {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void extend() throws Exception {
		String source = "behavior EntityX    extends    EntityY {}";
		String expected = multilineString(
				"behavior EntityX extends EntityY {",
				"}",
				"");
		assertFormatted(source, expected);
	}

	@Test
	public void memberIndent() throws Exception {
		String source = "behavior EntityX{var x:int}";
		String expected = multilineString(
				"behavior EntityX {",
				"	var x : int",
				"}",
				"");
		assertFormatted(source, expected);
	}

}
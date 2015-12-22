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

/** Tests for formatting behavior units.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class BehaviorUnitFormatterTest extends AbstractMemberFormatterTest {

	@Test
	public void simple() throws Exception {
		String source = unformattedCode("on Event{System.out.println(occurrence)}");
		String expected = formattedCode(
				"	on Event {",
				"		System.out.println(occurrence)",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void trueGuard() throws Exception {
		String source = unformattedCode("on Event[true]{System.out.println(occurrence)}");
		String expected = formattedCode(
				"	on Event [true] {",
				"		System.out.println(occurrence)",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void falseGuard() throws Exception {
		String source = unformattedCode("on Event[false]{System.out.println(occurrence)}");
		String expected = formattedCode(
				"	on Event [false] {",
				"		System.out.println(occurrence)",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void guard() throws Exception {
		String source = unformattedCode("on Event[occurrence.forMe]{System.out.println(occurrence)}");
		String expected = formattedCode(
				"	on Event [occurrence.forMe] {",
				"		System.out.println(occurrence)",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void twoAnnotations() throws Exception {
		String source = unformattedCode("@Pure@Beta on Event{}");
		String expected = formattedCode(
				"	@Pure @Beta on Event {",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void threeAnnotations() throws Exception {
		String source = unformattedCode("@Pure@Beta\n@Hello on Event{}");
		String expected = formattedCode(
				"	@Pure @Beta",
				"	@Hello on Event {",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void annotationValue() throws Exception {
		String source = unformattedCode("@SuppressWarnings(        value= \"name\"   )on Event{}");
		String expected = formattedCode(
				"	@SuppressWarnings(value = \"name\") on Event {",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void annotationImplicitValue() throws Exception {
		String source = unformattedCode("@SuppressWarnings(   \"name\"   )on Event{}");
		String expected = formattedCode(
				"	@SuppressWarnings(\"name\") on Event {",
				"	}");
		assertFormatted(source, expected);
	}

}
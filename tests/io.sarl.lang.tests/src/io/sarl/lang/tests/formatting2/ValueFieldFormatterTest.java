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

/** Tests for formatting fields.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class ValueFieldFormatterTest extends AbstractMemberFormatterTest {

	@Test
	public void type() throws Exception {
		String source = unformattedCode("val xxx:int");
		String expected = formattedCode("	val xxx : int");
		assertFormatted(source, expected);
	}

	@Test
	public void types() throws Exception {
		String source = unformattedCode("val xxx:int val yyy:boolean");
		String expected = formattedCode(
				"	val xxx : int",
				"	val yyy : boolean");
		assertFormatted(source, expected);
	}

	@Test
	public void initialValue() throws Exception {
		String source = unformattedCode("val xxx=5");
		String expected = formattedCode("	val xxx = 5");
		assertFormatted(source, expected);
	}

	@Test
	public void initialValues() throws Exception {
		String source = unformattedCode("val xxx=5 val yyy=true");
		String expected = formattedCode(
				"	val xxx = 5",
				"	val yyy = true");
		assertFormatted(source, expected);
	}

	@Test
	public void initialValueType() throws Exception {
		String source = unformattedCode("val xxx=5 val yyy : boolean");
		String expected = formattedCode(
				"	val xxx = 5",
				"	val yyy : boolean");
		assertFormatted(source, expected);
	}

	@Test
	public void typeInitialValue() throws Exception {
		String source = unformattedCode("val xxx:int val yyy=true");
		String expected = formattedCode(
				"	val xxx : int",
				"	val yyy = true");
		assertFormatted(source, expected);
	}

	@Test
	public void typeComaType() throws Exception {
		String source = unformattedCode("val xxx:int  ;  val yyy:boolean");
		String expected = formattedCode(
				"	val xxx : int;",
				"	val yyy : boolean");
		assertFormatted(source, expected);
	}

	@Test
	public void typeInit() throws Exception {
		String source = unformattedCode("val xxx:int=45");
		String expected = formattedCode("	val xxx : int = 45");
		assertFormatted(source, expected);
	}

	@Test
	public void modifiers() throws Exception {
		String source = unformattedCode("protected   final    val xxx:int=45");
		String expected = formattedCode("	protected final val xxx : int = 45");
		assertFormatted(source, expected);
	}

	@Test
	public void twoAnnotations() throws Exception {
		String source = unformattedCode("@Pure@Beta val xxx:int=45");
		String expected = formattedCode("	@Pure @Beta val xxx : int = 45");
		assertFormatted(source, expected);
	}

	@Test
	public void threeAnnotations() throws Exception {
		String source = unformattedCode("@Pure@Beta\n@Hello val xxx:int=45");
		String expected = formattedCode(
				"	@Pure @Beta",
				"	@Hello val xxx : int = 45");
		assertFormatted(source, expected);
	}

	@Test
	public void annotationValue() throws Exception {
		String source = unformattedCode("@SuppressWarnings(        value= \"name\"   )val xxx:int=45");
		String expected = formattedCode("	@SuppressWarnings(value = \"name\") val xxx : int = 45");
		assertFormatted(source, expected);
	}

	@Test
	public void annotationImplicitValue() throws Exception {
		String source = unformattedCode("@SuppressWarnings(   \"name\"   )val xxx:int=45");
		String expected = formattedCode("	@SuppressWarnings(\"name\") val xxx : int = 45");
		assertFormatted(source, expected);
	}

}
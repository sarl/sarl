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

/** Tests for formatting several XExpressions.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class XExpressionFormatterTest extends AbstractMemberFormatterTest {

	@Test
	public void localVariableType() throws Exception {
		String source = unformattedCode("def test{var x:int}");
		String expected = formattedCode(
				"	def test {",
				"		var x : int",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void localVariableInitialValue() throws Exception {
		String source = unformattedCode("def test{var x=5}");
		String expected = formattedCode(
				"	def test {",
				"		var x = 5",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void localVariableModifier() throws Exception {
		String source = unformattedCode("def test{extension     var    x:int}");
		String expected = formattedCode(
				"	def test {",
				"		extension var x : int",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void localValueType() throws Exception {
		String source = unformattedCode("def test{val x:int}");
		String expected = formattedCode(
				"	def test {",
				"		val x : int",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void localValueInitialValue() throws Exception {
		String source = unformattedCode("def test{val x=5}");
		String expected = formattedCode(
				"	def test {",
				"		val x = 5",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void localValueModifier() throws Exception {
		String source = unformattedCode("def test{extension     val    x:int}");
		String expected = formattedCode(
				"	def test {",
				"		extension val x : int",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void jvmFormalParameter_inClosure() throws Exception {
		String source = unformattedCode("def test{var proc = [x:int,y:boolean|x]}");
		String expected = formattedCode(
				"	def test {",
				"		var proc = [x : int, y : boolean|x]",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void fullJvmFormalParameter_inCatch() throws Exception {
		String source = unformattedCode("def test{try{}catch(zz:Exception){}}");
		String expected = formattedCode(
				"	def test {",
				"		try {",
				"		} catch (zz : Exception) {",
				"		}",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void forLoop_inferred() throws Exception {
		String source = unformattedCode("def test{for(x:1..3){}}");
		String expected = formattedCode(
				"	def test {",
				"		for (x : 1 .. 3) {",
				"		}",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void forLoop_inferred_extension() throws Exception {
		String source = unformattedCode("def test{for(extension    x:1..3){}}");
		String expected = formattedCode(
				"	def test {",
				"		for (extension x : 1 .. 3) {",
				"		}",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void forLoop_explicit() throws Exception {
		String source = unformattedCode("def test{for(x    as   int:1..3){}}");
		String expected = formattedCode(
				"	def test {",
				"		for (x as int : 1 .. 3) {",
				"		}",
				"	}");
		assertFormatted(source, expected);
	}

	@Test
	public void forLoop_explicit_extension() throws Exception {
		String source = unformattedCode("def test{for(extension    x    as    int:1..3){}}");
		String expected = formattedCode(
				"	def test {",
				"		for (extension x as int : 1 .. 3) {",
				"		}",
				"	}");
		assertFormatted(source, expected);
	}

}
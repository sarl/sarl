/*
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.lang.tests.compilation.general;

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.XtextPackage;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.google.inject.Inject;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class GeneralSyntaxTest extends AbstractSarlTest {

	@Inject
	private CompilationTestHelper compiler;

	@Test
	public void noParamNoReturnActionInClass() throws Exception {
		String source = multilineString(
				"abstract class Light {",
				"	def turnOn",
				"	def turnOff",
				"}",
				"");
		String expected = multilineString(
				"@SuppressWarnings(\"all\")",
				"public abstract class Light {",
				"  public abstract void turnOn();",
				"  ",
				"  public abstract void turnOff();",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

	@Test
	public void noParamNoReturnActionInInterface() throws Exception {
		String source = multilineString(
				"interface Light {",
				"	def turnOn",
				"	def turnOff",
				"}",
				"");
		String expected = multilineString(
				"@SuppressWarnings(\"all\")",
				"public interface Light {",
				"  public abstract void turnOn();",
				"  ",
				"  public abstract void turnOff();",
				"}",
				""
				);
		this.compiler.assertCompilesTo(source, expected);
	}

}

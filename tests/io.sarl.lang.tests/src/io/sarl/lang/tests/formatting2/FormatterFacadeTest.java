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
package io.sarl.lang.tests.formatting2;

import javax.inject.Inject;

import static org.junit.Assert.*;
import org.junit.Test;

import io.sarl.lang.formatting2.FormatterFacade;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.TestScope;

/** Test of a SARL formatter facade.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class FormatterFacadeTest extends AbstractSarlTest {

	@Inject
	private FormatterFacade facade;

	@Test
	public void test() {
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
		String actual = this.facade.format(source);
		assertEquals(expected, actual);
	}

}
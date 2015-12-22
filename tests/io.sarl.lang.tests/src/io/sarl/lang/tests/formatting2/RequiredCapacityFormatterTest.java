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

/** Tests for formatting required capacities.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class RequiredCapacityFormatterTest extends AbstractMemberFormatterTest {

	@Test
	public void one() throws Exception {
		String source = unformattedCode("requires    Capacity1");
		String expected = formattedCode("	requires Capacity1");
		assertFormatted(source, expected);
	}

	@Test
	public void two() throws Exception {
		String source = unformattedCode("requires Capacity1,Capacity2");
		String expected = formattedCode("	requires Capacity1, Capacity2");
		assertFormatted(source, expected);
	}

	@Test
	public void three() throws Exception {
		String source = unformattedCode("requires Capacity1,Capacity2,    Capacity3");
		String expected = formattedCode("	requires Capacity1, Capacity2, Capacity3");
		assertFormatted(source, expected);
	}

	@Test
	public void twoStatements_two() throws Exception {
		String source = unformattedCode("requires Capacity1 requires Capacity2");
		String expected = formattedCode(
				"	requires Capacity1",
				"",
				"	requires Capacity2");
		assertFormatted(source, expected);
	}

	@Test
	public void twoStatements_three() throws Exception {
		String source = unformattedCode("requires Capacity1 requires Capacity2,    Capacity3");
		String expected = formattedCode(
				"	requires Capacity1",
				"",
				"	requires Capacity2, Capacity3");
		assertFormatted(source, expected);
	}

}
/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.maven.docs.generator.tests.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assume.assumeNotNull;

import java.io.File;
import java.net.URL;
import java.util.List;

import com.google.common.io.Resources;
import com.google.inject.Injector;
import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.MutableTriple;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.util.Strings;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.maven.docs.parser.SarlDocumentationParser;
import io.sarl.maven.docs.parser.SarlDocumentationParser.ParsingException;
import io.sarl.maven.docs.parser.SarlDocumentationParser.Tag;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@RunWith(Suite.class)
@SuiteClasses({
	SarlDocumentationParserTest.TransformTest.class,
	SarlDocumentationParserTest.ValidationTest.class,
})
@SuppressWarnings("all")
public class SarlDocumentationParserTest {

	private static File file(String basename) {
		URL url = Resources.getResource(SarlDocumentationParserTest.class, basename);
		assumeNotNull(url);
		File file = FileSystem.convertURLToFile(url);
		assumeNotNull(file);
		return file;
	}

	public static class TransformTest {

		private SarlDocumentationParser parser;

		@Before
		public void setUp() {
			Injector injector = SARLStandaloneSetup.doSetup();
			this.parser = injector.getInstance(SarlDocumentationParser.class);
			this.parser.setBlockCodeTemplate(SarlDocumentationParser.getBasicCodeBlockFormatter());
		}

		@Test(expected = ParsingException.class)
		public void noFile() throws Exception {
			File file = new File("nofile.txt");
			this.parser.transform(file);
		}
	
		@Test
		public void includer01() throws Exception {
			File file = file("includer.txt");
			String value = this.parser.transform(file);
			assertEquals(
					"this is a fake text done for testing. this is a fake text done for testing. this is a fake text done for testing.\n"
					+ "\nthis is a fake text done for testing. this is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing.\n\nthis is a fake text done for testing. "
					+ "I'm included.this is a fake text done for testing. this is a fake text done for testing. this is a "
					+ "fake text done for testing. this is a fake text done\nfor testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done\nfor testing. this is a "
					+ "fake text done for testing. this is a fake text done for testing.",
					value);
		}
	
		@Test
		public void saver01() throws Exception {
			File file = file("saverpredefinition.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake text done "
					+ "for testing.\n\nthis is a fake text done for testing. this is a fake text done for testing. "
					+ "this is a fake text done for testing. this is a fake text done for testing.\n\nthis is a fake text done "
					+ "for testing. `this is`this is a fake text done for testing. this is a fake text done for testing. "
					+ "this is a fake text done for testing. this is a fake text done \nfor testing. this is a fake text done "
					+ "for testing. this is a fake text done for testing. this is a fake text done\nfor testing. this is a "
					+ "fake text done for testing. this is a fake text done for testing.",
					value);
		}
	
		@Test
		public void saver02() throws Exception {
			File file = file("saverpostdefinition.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake text "
					+ "done for testing.\n\nthis is a fake text done for testing. `this is a fake text`this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing.\n\nthis "
					+ "is a fake text done for testing. this is a fake text done for testing. this is a "
					+ "fake text done for testing. this is a fake text done for testing. this is a fake text done \nfor "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing. this is a "
					+ "fake text done\nfor testing. this is a fake text done for testing. this is a fake text done "
					+ "for testing.",
					value);
		}
	
		@Test(expected = ParsingException.class)
		public void saver03() throws Exception {
			File file = file("savernodefinition.txt");
			this.parser.transform(file);
		}
	
		@Test
		public void success01() throws Exception {
			File file = file("success.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing.\n\n"
					+ "\n\nthis is a fake text done for testing. this is a fake text done for testing. this "
					+ "is a fake text done for testing. this is a fake text done for testing. this is a "
					+ "fake text done \nfor testing. this is a fake text done for testing. this is a fake text "
					+ "done for testing. this is a fake text done\nfor testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing.",
					value);
		}
	
		@Test
		public void failure01() throws Exception {
			File file = file("failure.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing.\n\n"
					+ "\n\nthis is a fake text done for testing. this is a fake text done for testing. this "
					+ "is a fake text done for testing. this is a fake text done for testing. this is a "
					+ "fake text done \nfor testing. this is a fake text done for testing. this is a fake text "
					+ "done for testing. this is a fake text done\nfor testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing.",
					value);
		}
	
		@Test
		public void outline01() throws Exception {
			File file = file("outline.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing.\n\n[::Outline::]"
					+ "\n\nthis is a fake text done for testing. this is a fake text done for testing. this "
					+ "is a fake text done for testing. this is a fake text done for testing. this is a "
					+ "fake text done \nfor testing. this is a fake text done for testing. this is a fake text "
					+ "done for testing. this is a fake text done\nfor testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing.",
					value);
		}
	
		@Test
		public void parameterDelimiters01() throws Exception {
			File file = file("parameterdelimiters.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake text "
					+ "done for testing.\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text\ndone for testing. this is a fake text "
					+ "done for testing.\n\n`this`\n`testing`\n`is`\n`done`",
					value);
		}
	
		@Test
		public void saveInSave01() throws Exception {
			File file = file("saveinsave.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing.\n\n"
					+ "`this is a fake text`\n`fake`",
					value);
		}
	
		@Test
		public void saveInSuccess01() throws Exception {
			File file = file("saveinsuccess.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.\n\n\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing.\n\n"
					+ "`hidden`",
					value);
		}
	
		@Test
		public void fact01() throws Exception {
			File file = file("fact.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing.\n\n\n\nthis "
					+ "is a fake text done for testing. this is a fake text done for testing. this is a fake text done "
					+ "for testing. this is a fake text done for testing. this is a fake text done \nfor testing. this "
					+ "is a fake text done for testing. this is a fake text done for testing. this is a fake text "
					+ "done\nfor testing. this is a fake text done for testing. this is a fake text done for testing.",
					value);
		}

		@Test
		public void onOff01() throws Exception {
			File file = file("onOffInSuccess.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing.\n\n"
					+ "\tof code\n\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing. this is "
					+ "a fake text done \nfor testing. this is a fake text done for testing. this is a fake text "
					+ "done for testing. this is a fake text done\nfor testing. this is a fake text done for testing. "
					+ "this is a fake text done for testing.",
					value);
		}

		@Test
		public void onOff02() throws Exception {
			File file = file("onOffInFailure.txt");
			String value = this.parser.transform(file);
			assertEquals("this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing.\n\n"
					+ "\tof code\n\n\nthis is a fake text done for testing. this is a fake text done for "
					+ "testing. this is a fake text done for testing. this is a fake text done for testing. this is "
					+ "a fake text done \nfor testing. this is a fake text done for testing. this is a fake text "
					+ "done for testing. this is a fake text done\nfor testing. this is a fake text done for testing. "
					+ "this is a fake text done for testing.",
					value);
		}

	}

	public static class ValidationTest {

		private SarlDocumentationParser parser;

		@Before
		public void setUp() {
			Injector injector = SARLStandaloneSetup.doSetup();
			this.parser = injector.getInstance(SarlDocumentationParser.class);
			this.parser.setBlockCodeTemplate(SarlDocumentationParser.getBasicCodeBlockFormatter());
		}

		@Test
		public void success01() {
			File file = file("success2.txt");
			this.parser.extractValidationComponents(file, (components) -> {
				assertEquals(1, components.size());
				Tag key = components.keySet().iterator().next();
				assertEquals(Tag.SUCCESS, key);
				List<MutableTriple<File, Integer, String>> values = components.get(key);
				assertNotNull(values);
				assertEquals(1, values.size());
				assertEquals("package io.sarl.docs.tests\n"
						+ "		import io.sarl.core.Initialize\n"
						+ "		import io.sarl.core.Logging\n"
						+ "		agent MyAgent {\n"
						+ "			uses Logging\n"
						+ "			on Initialize {\n"
						+ "				info(\"Hello\")\n"
						+ "			}\n"
						+ "		}",
						values.get(0).getRight());
			});
		}

		@Test
		public void failure01() {
			File file = file("failure2.txt");
			this.parser.extractValidationComponents(file, (components) -> {
				assertEquals(1, components.size());
				Tag key = components.keySet().iterator().next();
				assertEquals(Tag.FAILURE, key);
				List<MutableTriple<File, Integer, String>> values = components.get(key);
				assertNotNull(values);
				assertEquals(1, values.size());
				assertEquals("package io.sarl.docs.tests\n"
						+ "		agent MyAgent {\n"
						+ "			uses Logging\n"
						+ "			on Initialize {\n"
						+ "				info(\"Hello\")\n"
						+ "			}\n"
						+ "		}",
						values.get(0).getRight());
			});
		}

		@Test
		public void fact01() {
			File file = file("fact.txt");
			this.parser.extractValidationComponents(file, (components) -> {
				assertEquals(1, components.size());
				Tag key = components.keySet().iterator().next();
				assertEquals(Tag.FACT, key);
				List<MutableTriple<File, Integer, String>> values = components.get(key);
				assertNotNull(values);
				assertEquals(1, values.size());
				assertEquals("typeof(Integer)",
						values.get(0).getRight());
			});
		}

		@Test
		public void successFailureFact01() {
			File file = file("multipleblocks.txt");
			this.parser.extractValidationComponents(file, (components) -> {
				assertEquals(3, components.size());
				List<MutableTriple<File, Integer, String>> values = components.get(Tag.FACT);
				assertNotNull(values);
				assertEquals(1, values.size());
				assertEquals("true",values.get(0).getRight());

				values = components.get(Tag.FAILURE);
				assertNotNull(values);
				assertEquals(1, values.size());
				assertEquals("package io.sarl.docs.tests\n"
						+ "		agent MyAgent {\n"
						+ "			uses Logging\n"
						+ "			on Initialize {\n"
						+ "				info(\"Hello\")\n"
						+ "			}\n"
						+ "		}",
						values.get(0).getRight());


				values = components.get(Tag.SUCCESS);
				assertNotNull(values);
				assertEquals(2, values.size());
				assertEquals("package io.sarl.docs.tests\n"
						+ "		import io.sarl.core.Initialize\n"
						+ "		import io.sarl.core.Logging\n"
						+ "		agent MyAgent {\n"
						+ "			uses Logging\n"
						+ "			on Initialize {\n"
						+ "				info(\"Hello\")\n"
						+ "			}\n"
						+ "		}",
						values.get(0).getRight());
				assertEquals("agent MyAgent {}",
						values.get(1).getRight());
			});
		}

	}

}

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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.maven.docs.generator.tests.markdown;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.File;
import java.net.URL;

import com.google.common.io.Resources;
import com.google.inject.Injector;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.maven.docs.markdown.MarkdownParser;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings("all")
@DisplayName("MarkdownParser")
@org.junit.jupiter.api.Tag("maven")
public class MarkdownParserTest {

	private static File file(String basename) {
		URL url = Resources.getResource(MarkdownParserTest.class, basename);
		assumeTrue(url != null);
		File file = FileSystem.convertURLToFile(url);
		assumeTrue(file != null);
		return file;
	}

	@Nested
	@org.junit.jupiter.api.Tag("unit")
	public class TransformTest {

		private MarkdownParser parser;

		@BeforeEach
		public void setUp() {
			Injector injector = SARLStandaloneSetup.doSetup();
			this.parser = injector.getInstance(MarkdownParser.class);
			this.parser.setOutlineStyleId(null);
		}

		@Test
		public void outline01_level1() throws Exception {
			File file = file("outline.txt");
			this.parser.setOutlineDepthRange(new IntegerRange(1, Integer.MAX_VALUE));
			this.parser.setAutoSectionNumbering(true);
			String value = this.parser.transform(file);
			assertEquals("# 1. Title\n\n## 1.1. Title 0\n\nthis is a fake text done for testing. this is a fake text done "
					+ "for testing. this is a fake text done for testing.\n\n\n"
					+ "> * [1. Title](#1-title)\n"
					+ "> \t* [1.1. Title 0](#1-1-title-0)\n"
					+ "> * [2. Title 1](#2-title-1)\n"
					+ "> \t* [2.1. Title 2](#2-1-title-2)\n"
					+ "> \t\t* [2.1.1. Title 3](#2-1-1-title-3)\n"
					+ "> \t* [2.2. Title 4](#2-2-title-4)\n"
					+ "> \t* [2.3. Title 5](#2-3-title-5)\n"
					+ "> * [3. Title 6](#3-title-6)\n"
					+ "\n\n\nthis is a fake text done for testing.\n\n# 2. Title 1\n\n"
					+ "this is a fake text done for testing. this is a fake text done for testing. this is a\n"
					+ "\n## 2.1. Title 2\n\nfake text done for testing. this is a fake text\n\n### 2.1.1. Title 3\n\n"
					+ "## 2.2. Title 4\n\n## 2.3. Title 5\n\ndone for testing. this is a fake text done\nfor testing. "
					+ "this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done\n\n# 3. Title 6\n\nfor testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.",
					value);
		}

		@Test
		public void outline01_level2() throws Exception {
			File file = file("outline.txt");
			this.parser.setOutlineDepthRange(new IntegerRange(2, Integer.MAX_VALUE));
			this.parser.setAutoSectionNumbering(true);
			String value = this.parser.transform(file);
			assertEquals("# Title\n\n## 1. Title 0\n\nthis is a fake text done for testing. this is a fake text done "
					+ "for testing. this is a fake text done for testing.\n\n\n"
					+ "> * [1. Title 0](#1-title-0)\n"
					+ "> * [2. Title 2](#2-title-2)\n"
					+ "> \t* [2.1. Title 3](#2-1-title-3)\n"
					+ "> * [3. Title 4](#3-title-4)\n"
					+ "> * [4. Title 5](#4-title-5)\n"
					+ "\n\n\nthis is a fake text done for testing.\n\n# Title 1\n\n"
					+ "this is a fake text done for testing. this is a fake text done for testing. this is a\n"
					+ "\n## 2. Title 2\n\nfake text done for testing. this is a fake text\n\n### 2.1. Title 3\n\n"
					+ "## 3. Title 4\n\n## 4. Title 5\n\ndone for testing. this is a fake text done\nfor testing. "
					+ "this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done\n\n# Title 6\nfor testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.",
					value);
		}

		@Test
		public void outline01_level3() throws Exception {
			File file = file("outline.txt");
			this.parser.setOutlineDepthRange(new IntegerRange(3, Integer.MAX_VALUE));
			this.parser.setAutoSectionNumbering(true);
			String value = this.parser.transform(file);
			assertEquals("# Title\n\n## Title 0\n\nthis is a fake text done for testing. this is a fake text done "
					+ "for testing. this is a fake text done for testing.\n\n\n"
					+ "> * [1. Title 3](#1-title-3)\n"
					+ "\n\n\nthis is a fake text done for testing.\n\n# Title 1\n\n"
					+ "this is a fake text done for testing. this is a fake text done for testing. this is a\n"
					+ "\n## Title 2\n\nfake text done for testing. this is a fake text\n\n### 1. Title 3\n\n"
					+ "## Title 4\n\n## Title 5\n\ndone for testing. this is a fake text done\nfor testing. "
					+ "this is a fake text done for testing. this is a fake text done for testing. this is a fake "
					+ "text done\n\n# Title 6\nfor testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.",
					value);
		}

		@Test
		public void outline02_level1() throws Exception {
			File file = file("outline.txt");
			this.parser.setOutlineDepthRange(new IntegerRange(1, Integer.MAX_VALUE));
			this.parser.setAutoSectionNumbering(false);
			String value = this.parser.transform(file);
			assertEquals("# Title\n\n## Title 0\n\nthis is a fake text done for testing. this is a fake text "
					+ "done for testing. this is a fake text done for testing.\n\n\n"
					+ "> * [Title](#title)\n"
					+ "> \t* [Title 0](#title-0)\n"
					+ "> * [Title 1](#title-1)\n"
					+ "> \t* [Title 2](#title-2)\n"
					+ "> \t\t* [Title 3](#title-3)\n"
					+ "> \t* [Title 4](#title-4)\n"
					+ "> \t* [Title 5](#title-5)\n"
					+ "> * [Title 6](#title-6)\n"
					+ "\n\n\nthis is a fake text done for testing.\n\n# Title 1\n\nthis is a fake "
					+ "text done for testing. this is a fake text done for testing. this is a\n\n## Title 2\n"
					+ "\nfake text done for testing. this is a fake text\n\n### Title 3\n\n## Title 4\n"
					+ "\n## Title 5\n\ndone for testing. this is a fake text done\nfor testing. this is a "
					+ "fake text done for testing. this is a fake text done for testing. this is a fake text "
					+ "done\n\n# Title 6\nfor testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.",
					value);
		}

		@Test
		public void outline02_level2() throws Exception {
			File file = file("outline.txt");
			this.parser.setOutlineDepthRange(new IntegerRange(2, Integer.MAX_VALUE));
			this.parser.setAutoSectionNumbering(false);
			String value = this.parser.transform(file);
			assertEquals("# Title\n\n## Title 0\n\nthis is a fake text done for testing. this is a fake text "
					+ "done for testing. this is a fake text done for testing.\n\n\n"
					+ "> * [Title 0](#title-0)\n"
					+ "> * [Title 2](#title-2)\n"
					+ "> \t* [Title 3](#title-3)\n"
					+ "> * [Title 4](#title-4)\n"
					+ "> * [Title 5](#title-5)\n"
					+ "\n\n\nthis is a fake text done for testing.\n\n# Title 1\n\nthis is a fake "
					+ "text done for testing. this is a fake text done for testing. this is a\n\n## Title 2\n"
					+ "\nfake text done for testing. this is a fake text\n\n### Title 3\n\n## Title 4\n"
					+ "\n## Title 5\n\ndone for testing. this is a fake text done\nfor testing. this is a "
					+ "fake text done for testing. this is a fake text done for testing. this is a fake text "
					+ "done\n\n# Title 6\nfor testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.",
					value);
		}

		@Test
		public void outline02_level3() throws Exception {
			File file = file("outline.txt");
			this.parser.setOutlineDepthRange(new IntegerRange(3, Integer.MAX_VALUE));
			this.parser.setAutoSectionNumbering(false);
			String value = this.parser.transform(file);
			assertEquals("# Title\n\n## Title 0\n\nthis is a fake text done for testing. this is a fake text "
					+ "done for testing. this is a fake text done for testing.\n\n\n"
					+ "> * [Title 3](#title-3)\n"
					+ "\n\n\nthis is a fake text done for testing.\n\n# Title 1\n\nthis is a fake "
					+ "text done for testing. this is a fake text done for testing. this is a\n\n## Title 2\n"
					+ "\nfake text done for testing. this is a fake text\n\n### Title 3\n\n## Title 4\n"
					+ "\n## Title 5\n\ndone for testing. this is a fake text done\nfor testing. this is a "
					+ "fake text done for testing. this is a fake text done for testing. this is a fake text "
					+ "done\n\n# Title 6\nfor testing. this is a fake text done for testing. this is a fake "
					+ "text done for testing.",
					value);
		}

		@Test
		public void hrefMapping01() throws Exception {
			File file = file("hrefmapping.txt");
			String value = this.parser.transform(file);
			assertEquals("My link to [local MD file](./outline.html)\n\nMy link to [local file](./outline.txt)\n\n"
					+ "My link to [remote file](http://www.sarl.io)",
					value);
		}

		@Test
		public void image01() throws Exception {
			File file = file("image.txt");
			String value = this.parser.transform(file);
			assertEquals("My link to ![local MD file](./outline.md)", value);
		}

	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.apputils.eclipseextensions.tests.buildpath;

import static org.mockito.Mockito.*;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IAccessRule;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.apputils.eclipseextensions.buildpath.SARLClasspathEntryComparator;
import io.sarl.lang.SARLConfig;
import io.sarl.tests.api.tools.TestAssertions;

/** Tests for {@code SARLClasspathEntryComparator}.
 *
 * @author $Author: sgalland$
 * @version eclipseextensions 0.15.0 20250909-115749
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid eclipseextensions
 * @since 0.15
 */
@SuppressWarnings("all")
@DisplayName("SARLClasspathEntryComparator")
@Tag("unit")
public class SARLClasspathEntryComparatorTest {

	private static List<IClasspathEntry> entries;

	private static String[] expected;

	private SARLClasspathEntryComparator comparator;
	
	@BeforeAll
	public static void globalSetUp() throws Exception {
		final var basenames = Arrays.asList(
				"src/main/java",
				"src/main/resources",
				"src/test/java",
				"src/test/resources",
				"src/it/java",
				"src/it/resources",
				"src/main/sarl",
				"src/main/generated-sources/sarl",
				"src/test/generated-sources/sarl",
				"src/it/generated-sources/sarl",
				"src/test/sarl",
				"src/it/sarl",
				"src/main/bspl",
				"src/test/bspl",
				"src/it/bspl",
				"src/main/generated-sources/bspl",
				"src/test/generated-sources/bspl",
				"src/it/generated-sources/bspl",
				"src/main/bdi",
				"src/test/bdi",
				"src/it/bdi",
				"src/main/generated-sources/bdi",
				"src/test/generated-sources/bdi",
				"src/it/generated-sources/bdi",
				"src/x/y/z",
				"x/y/z");
		expected = Arrays.asList(
				"src/main/sarl",
				"src/main/bdi",
				"src/main/bspl",
				"src/main/java",
				"src/x/y/z",
				"x/y/z",
				"src/main/resources",

				"src/test/sarl",
				"src/test/bdi",
				"src/test/bspl",
				"src/test/java",
				"src/test/resources",

				"src/it/sarl",
				"src/it/bdi",
				"src/it/bspl",
				"src/it/java",
				"src/it/resources",

				"src/it/generated-sources/bdi",
				"src/it/generated-sources/bspl",
				"src/it/generated-sources/sarl",
				"src/main/generated-sources/bdi",
				"src/main/generated-sources/bspl",
				"src/main/generated-sources/sarl",
				"src/test/generated-sources/bdi",
				"src/test/generated-sources/bspl",
				"src/test/generated-sources/sarl").toArray(size -> new String[size]);
		entries = basenames.stream()
				.map(it -> mockEntry(it))
				.collect(Collectors.toList());
	}
	
	private static IClasspathEntry mockEntry(String name) {
		final var m = mock(IClasspathEntry.class);
		when(m.getPath()).thenAnswer(it -> Path.fromPortableString(name));
		when(m.toString()).thenReturn(name);
		return m;
	}

	@BeforeEach
	public void setUp() throws Exception {
		this.comparator = new SARLClasspathEntryComparator();
	}
	
	@Test
	@DisplayName("Sort list")
	public void listSort() {
		final var actual = entries.toArray(size -> new IClasspathEntry[size]);
		Arrays.sort(actual, this.comparator);
		final var act = Arrays.asList(actual).stream()
				.map(it -> it.getPath().toPortableString())
				.toArray(size -> new String[size]);
		Assertions.assertArrayEquals(expected, act);
	}

}

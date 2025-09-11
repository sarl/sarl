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

import static io.sarl.tests.api.tools.TestAssertions.assertStrictlyPositive;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.stream.Collectors;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.apputils.eclipseextensions.buildpath.SARLPathComparator;

/** Tests for {@code SARLPathComparator}.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@SuppressWarnings("all")
@DisplayName("SARLPathComparator")
@Tag("unit")
public class SARLPathEntryComparatorTest {

	private SARLPathComparator comparator;
	
	private static IPath mockEntry(String name) {
		return IPath.fromPortableString(name);
	}

	@BeforeEach
	public void setUp() throws Exception {
		this.comparator = new SARLPathComparator();
	}

	@Test
	@DisplayName("categorizes(\"src/main/java\")")
	public void categorizes1() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/main/java"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/main/resources\")")
	public void categorizes2() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/main/resources"));
		assertEquals(40, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/test/java\")")
	public void categorizes3() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/test/java"));
		assertEquals(220, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/test/resources\")")
	public void categorizes4() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/test/resources"));
		assertEquals(240, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/it/java\")")
	public void categorizes5() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/it/java"));
		assertEquals(420, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/it/resources\")")
	public void categorizes6() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/it/resources"));
		assertEquals(440, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/main/sarl\")")
	public void categorizes7() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/main/sarl"));
		assertEquals(0, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/main/generated-sources/sarl\")")
	public void categorizes8() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/main/generated-sources/sarl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/test/generated-sources/sarl\")")
	public void categorizes9() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/test/generated-sources/sarl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/it/generated-sources/sarl\")")
	public void categorizes10() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/it/generated-sources/sarl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/test/sarl\")")
	public void categorizes11() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/test/sarl"));
		assertEquals(200, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/it/sarl\")")
	public void categorizes12() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/it/sarl"));
		assertEquals(400, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/main/bspl\")")
	public void categorizes13() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/main/bspl"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/test/bspl\")")
	public void categorizes14() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/test/bspl"));
		assertEquals(220, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/it/bspl\")")
	public void categorizes15() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/it/bspl"));
		assertEquals(420, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/main/generated-sources/bspl\")")
	public void categorizes16() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/main/generated-sources/bspl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/test/generated-sources/bspl\")")
	public void categorizes17() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/test/generated-sources/bspl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/it/generated-sources/bspl\")")
	public void categorizes18() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/it/generated-sources/bspl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/main/bdi\")")
	public void categorizes19() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/main/bdi"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/test/bdi\")")
	public void categorizes20() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/test/bdi"));
		assertEquals(220, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/it/bdi\")")
	public void categorizes21() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/it/bdi"));
		assertEquals(420, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/main/generated-sources/bdi\")")
	public void categorizes22() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/main/generated-sources/bdi"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/test/generated-sources/bdi\")")
	public void categorizes23() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/test/generated-sources/bdi"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/it/generated-sources/bdi\")")
	public void categorizes24() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/it/generated-sources/bdi"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"src/x/y/z\")")
	public void categorizes25() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("src/x/y/z"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"x/y/z\")")
	public void categorizes26() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("x/y/z"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/main/java\")")
	public void categorizes27() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/main/java"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/main/resources\")")
	public void categorizes28() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/main/resources"));
		assertEquals(40, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/test/java\")")
	public void categorizes29() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/test/java"));
		assertEquals(220, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/test/resources\")")
	public void categorizes30() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/test/resources"));
		assertEquals(240, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/it/java\")")
	public void categorizes31() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/it/java"));
		assertEquals(420, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/it/resources\")")
	public void categorizes32() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/it/resources"));
		assertEquals(440, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/main/sarl\")")
	public void categorizes33() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/main/sarl"));
		assertEquals(0, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/main/generated-sources/sarl\")")
	public void categorizes34() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/main/generated-sources/sarl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/test/generated-sources/sarl\")")
	public void categorizes35() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/test/generated-sources/sarl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/it/generated-sources/sarl\")")
	public void categorizes36() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/it/generated-sources/sarl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/test/sarl\")")
	public void categorizes37() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/test/sarl"));
		assertEquals(200, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/it/sarl\")")
	public void categorizes38() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/it/sarl"));
		assertEquals(400, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/main/bspl\")")
	public void categorizes39() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/main/bspl"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/test/bspl\")")
	public void categorizes40() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/test/bspl"));
		assertEquals(220, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/it/bspl\")")
	public void categorizes41() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/it/bspl"));
		assertEquals(420, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/main/generated-sources/bspl\")")
	public void categorizes42() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/main/generated-sources/bspl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/test/generated-sources/bspl\")")
	public void categorizes43() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/test/generated-sources/bspl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/it/generated-sources/bspl\")")
	public void categorizes44() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/it/generated-sources/bspl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/main/bdi\")")
	public void categorizes45() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/main/bdi"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/test/bdi\")")
	public void categorizes46() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/test/bdi"));
		assertEquals(220, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/it/bdi\")")
	public void categorizes47() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/it/bdi"));
		assertEquals(420, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/main/generated-sources/bdi\")")
	public void categorizes48() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/main/generated-sources/bdi"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/test/generated-sources/bdi\")")
	public void categorizes49() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/test/generated-sources/bdi"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/it/generated-sources/bdi\")")
	public void categorizes50() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/it/generated-sources/bdi"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/src/x/y/z\")")
	public void categorizes51() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/src/x/y/z"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"/x/y/z\")")
	public void categorizes52() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/x/y/z"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/main/java\")")
	public void categorizes53() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/main/java"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/main/resources\")")
	public void categorizes54() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/main/resources"));
		assertEquals(40, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/test/java\")")
	public void categorizes55() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/test/java"));
		assertEquals(220, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/test/resources\")")
	public void categorizes56() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/test/resources"));
		assertEquals(240, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/it/java\")")
	public void categorizes57() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/it/java"));
		assertEquals(420, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/it/resources\")")
	public void categorizes58() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/it/resources"));
		assertEquals(440, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/main/sarl\")")
	public void categorizes59() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/main/sarl"));
		assertEquals(0, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/main/generated-sources/sarl\")")
	public void categorizes60() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/main/generated-sources/sarl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/test/generated-sources/sarl\")")
	public void categorizes61() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/test/generated-sources/sarl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/it/generated-sources/sarl\")")
	public void categorizes62() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/it/generated-sources/sarl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/test/sarl\")")
	public void categorizes63() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/test/sarl"));
		assertEquals(200, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/it/sarl\")")
	public void categorizes64() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/it/sarl"));
		assertEquals(400, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/main/bspl\")")
	public void categorizes65() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/main/bspl"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/test/bspl\")")
	public void categorizes66() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/test/bspl"));
		assertEquals(220, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/it/bspl\")")
	public void categorizes67() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/it/bspl"));
		assertEquals(420, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/main/generated-sources/bspl\")")
	public void categorizes68() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/main/generated-sources/bspl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/test/generated-sources/bspl\")")
	public void categorizes69() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/test/generated-sources/bspl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/it/generated-sources/bspl\")")
	public void categorizes70() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/it/generated-sources/bspl"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/main/bdi\")")
	public void categorizes71() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/main/bdi"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/test/bdi\")")
	public void categorizes72() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/test/bdi"));
		assertEquals(220, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/it/bdi\")")
	public void categorizes73() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/it/bdi"));
		assertEquals(420, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/main/generated-sources/bdi\")")
	public void categorizes74() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/main/generated-sources/bdi"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/test/generated-sources/bdi\")")
	public void categorizes75() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/test/generated-sources/bdi"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/it/generated-sources/bdi\")")
	public void categorizes76() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/it/generated-sources/bdi"));
		assertEquals(600, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/src/x/y/z\")")
	public void categorizes77() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/src/x/y/z"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("categorizes(\"/prj/x/y/z\")")
	public void categorizes78() {
		var actual = SARLPathComparator.categorizes(Path.fromPortableString("/prj/x/y/z"));
		assertEquals(20, actual);
	}

	@Test
	@DisplayName("Single #1")
	public void single1() {
		final var path1 = Path.fromPortableString("src/main/resources");
		final var path2 = Path.fromPortableString("src/main/sarl");
		final var actual = this.comparator.compare(path1, path2);
		assertStrictlyPositive(actual);
	}

	@Test
	@DisplayName("Single #2")
	public void single2() {
		final var path1 = Path.fromPortableString("/src/main/resources");
		final var path2 = Path.fromPortableString("/src/main/sarl");
		final var actual = this.comparator.compare(path1, path2);
		assertStrictlyPositive(actual);
	}

	@Test
	@DisplayName("Single #3")
	public void single3() {
		final var path1 = Path.fromPortableString("myprj/src/main/resources");
		final var path2 = Path.fromPortableString("myprj/src/main/sarl");
		final var actual = this.comparator.compare(path1, path2);
		assertStrictlyPositive(actual);
	}

	@Test
	@DisplayName("Single #4")
	public void single4() {
		final var path1 = Path.fromPortableString("/myprj/src/main/resources");
		final var path2 = Path.fromPortableString("/myprj/src/main/sarl");
		final var actual = this.comparator.compare(path1, path2);
		assertStrictlyPositive(actual);
	}

	@Test
	@DisplayName("Sort list")
	public void listSort() {
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
		final var expected = Arrays.asList(
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
		final var entries = basenames.stream()
				.map(it -> mockEntry(it))
				.collect(Collectors.toList());

		final var actual = entries.toArray(size -> new IPath[size]);
		Arrays.sort(actual, this.comparator);
		final var act = Arrays.asList(actual).stream()
				.map(it -> it.toPortableString())
				.toArray(size -> new String[size]);
		Assertions.assertArrayEquals(expected, act);
	}

}

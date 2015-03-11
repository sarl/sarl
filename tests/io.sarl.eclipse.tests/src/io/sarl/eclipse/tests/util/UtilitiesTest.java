/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.tests.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import io.sarl.eclipse.util.Utilities;
import io.sarl.tests.api.AbstractSarlTest;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.osgi.framework.Version;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	UtilitiesTest.ComparisonTests.class,
})
@SuppressWarnings("all")
public final class UtilitiesTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ComparisonTests extends AbstractSarlTest {

		@Test
		public void compareTo_null_null() {
			int actual = Utilities.compareTo(null, null);
			assertZero(actual);
		}

		@Test
		public void compareTo_null_1() {
			assertStrictlyNegative(Utilities.compareTo(null, 1));
		}

		@Test
		public void compareTo_1_null() {
			assertStrictlyPositive(Utilities.compareTo(1, null));
		}

		@Test
		public void compareTo_1_1() {
			assertZero(Utilities.compareTo(1, 1));
		}

		@Test
		public void compareTo_0_1() {
			assertStrictlyNegative(Utilities.compareTo(0, 1));
		}

		@Test
		public void compareTo_1_0() {
			assertStrictlyPositive(Utilities.compareTo(1, 0));
		}

		@Test
		public void parseVersion() {
			assertNull(Utilities.parseVersion(null));
			assertNull(Utilities.parseVersion(""));
			assertEquals(new Version(1, 2, 3), Utilities.parseVersion("1.2.3"));
			assertEquals(new Version(1, 2, 3, "something"), Utilities.parseVersion("1.2.3.something"));
			assertNull(Utilities.parseVersion("1.2.3-something"));
		}

		@Test
		public void compareVersionToRange_1_null_null() {
			Version v1 = new Version(1, 0, 0);
			assertZero(Utilities.compareVersionToRange(v1, null, null));
		}

		@Test
		public void compareVersionToRange_1_null_1() {
			Version v1 = new Version(1, 0, 0);
			assertStrictlyPositive(Utilities.compareVersionToRange(v1, null, v1));
		}

		@Test
		public void compareVersionToRange_1_null_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertZero(Utilities.compareVersionToRange(v1, null, v2));
		}

		@Test
		public void compareVersionToRange_1_null_3() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(Utilities.compareVersionToRange(v1, null, v3));
		}

		@Test
		public void compareVersionToRange_1_1_null() {
			Version v1 = new Version(1, 0, 0);
			assertZero(Utilities.compareVersionToRange(v1, v1, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_1_1() {
			Version v1 = new Version(1, 0, 0);
			Utilities.compareVersionToRange(v1, v1, v1);
		}

		@Test
		public void compareVersionToRange_1_1_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertZero(Utilities.compareVersionToRange(v1, v1, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_1_3() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v1, v1, v3);
		}

		@Test
		public void compareVersionToRange_1_2_null() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertStrictlyNegative(Utilities.compareVersionToRange(v1, v2, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_2_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Utilities.compareVersionToRange(v1, v2, v1);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_2_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Utilities.compareVersionToRange(v1, v2, v2);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_2_3() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v1, v2, v3);
		}

		@Test
		public void compareVersionToRange_1_3_null() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(Utilities.compareVersionToRange(v1, v3, null));
		}

		@Test
		public void compareVersionToRange_1_3_1() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(Utilities.compareVersionToRange(v1, v3, v1));
		}

		@Test
		public void compareVersionToRange_1_3_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(Utilities.compareVersionToRange(v1, v3, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_1_3_3() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v1, v3, v3);
		}

		@Test
		public void compareVersionToRange_2_null_null() {
			Version v2 = new Version(2, 0, 0);
			assertZero(Utilities.compareVersionToRange(v2, null, null));
		}

		@Test
		public void compareVersionToRange_2_null_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertStrictlyPositive(Utilities.compareVersionToRange(v2, null, v1));
		}

		@Test
		public void compareVersionToRange_2_null_2() {
			Version v2 = new Version(2, 0, 0);
			assertStrictlyPositive(Utilities.compareVersionToRange(v2, null, v2));
		}

		@Test
		public void compareVersionToRange_2_null_3() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(Utilities.compareVersionToRange(v2, null, v3));
		}

		@Test
		public void compareVersionToRange_2_1_null() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertZero(Utilities.compareVersionToRange(v2, v1, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_1_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Utilities.compareVersionToRange(v2, v1, v1);
		}

		@Test
		public void compareVersionToRange_2_1_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			assertStrictlyPositive(Utilities.compareVersionToRange(v2, v1, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_1_3() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v2, v1, v3);
		}

		@Test
		public void compareVersionToRange_2_2_null() {
			Version v2 = new Version(2, 0, 0);
			assertZero(Utilities.compareVersionToRange(v2, v2, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_2_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Utilities.compareVersionToRange(v2, v2, v1);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_2_2() {
			Version v2 = new Version(2, 0, 0);
			Utilities.compareVersionToRange(v2, v2, v2);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_2_3() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v2, v2, v3);
		}

		@Test
		public void compareVersionToRange_2_3_null() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(Utilities.compareVersionToRange(v2, v3, null));
		}

		@Test
		public void compareVersionToRange_2_3_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(Utilities.compareVersionToRange(v2, v3, v1));
		}

		@Test
		public void compareVersionToRange_2_3_2() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(Utilities.compareVersionToRange(v2, v3, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_2_3_3() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v2, v3, v3);
		}

		@Test
		public void compareVersionToRange_3_null_null() {
			Version v3 = new Version(0, 10, 0);
			assertZero(Utilities.compareVersionToRange(v3, null, null));
		}

		@Test
		public void compareVersionToRange_3_null_1() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(Utilities.compareVersionToRange(v3, null, v1));
		}

		@Test
		public void compareVersionToRange_3_null_2() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(Utilities.compareVersionToRange(v3, null, v2));
		}

		@Test
		public void compareVersionToRange_3_null_3() {
			Version v3 = new Version(0, 10, 0);
			assertStrictlyPositive(Utilities.compareVersionToRange(v3, null, v3));
		}

		@Test
		public void compareVersionToRange_3_1_null() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyNegative(Utilities.compareVersionToRange(v3, v1, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_1_1() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v3, v1, v1);
		}

		@Test
		public void compareVersionToRange_3_1_2() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyNegative(Utilities.compareVersionToRange(v3, v1, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_1_3() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v3, v1, v3);
		}

		@Test
		public void compareVersionToRange_3_2_null() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertStrictlyNegative(Utilities.compareVersionToRange(v3, v2, null));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_2_1() {
			Version v1 = new Version(1, 0, 0);
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v3, v2, v1);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_2_2() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v3, v2, v2);
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_2_3() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v3, v2, v3);
		}

		@Test
		public void compareVersionToRange_3_3_null() {
			Version v3 = new Version(0, 10, 0);
			assertZero(Utilities.compareVersionToRange(v3, v3, null));
		}

		@Test
		public void compareVersionToRange_3_3_1() {
			Version v1 = new Version(1, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(Utilities.compareVersionToRange(v3, v3, v1));
		}

		@Test
		public void compareVersionToRange_3_3_2() {
			Version v2 = new Version(2, 0, 0);
			Version v3 = new Version(0, 10, 0);
			assertZero(Utilities.compareVersionToRange(v3, v3, v2));
		}

		@Test(expected = AssertionError.class)
		public void compareVersionToRange_3_3_3() {
			Version v3 = new Version(0, 10, 0);
			Utilities.compareVersionToRange(v3, v3, v3);
		}

	}

}

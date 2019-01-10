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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.util.tests.sarlspecification;

import static io.sarl.tests.api.AbstractSarlTest.assertNaN;
import static io.sarl.tests.api.AbstractSarlTest.assertStrictlyNegative;
import static io.sarl.tests.api.AbstractSarlTest.assertStrictlyPositive;
import static io.sarl.tests.api.AbstractSarlTest.assertZero;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.sarlspecification.SarlSpecificationChecker;
import io.sarl.sarlspecification.StandardSarlSpecificationChecker;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class StandardSarlSpecificationCheckerTest {

	private SarlSpecificationChecker checker;
	
	@Before
	public void setUp() {
		this.checker = new StandardSarlSpecificationChecker();
	}
	
	@After
	public void tearDown() {
		this.checker = null;
	}

	@Test
	public void getSarlSpecificationVersion() {
		assertNaN(this.checker.getSarlSpecificationVersion(Type1.class));
		assertNaN(this.checker.getSarlSpecificationVersion(Type2.class));
		assertEquals(0.1f, this.checker.getSarlSpecificationVersion(Type3.class), 0f);
		assertEquals(SARLVersion.SPECIFICATION_RELEASE_VERSION, this.checker.getSarlSpecificationVersion(Type4.class), 0f);
		assertEquals(10000000f, this.checker.getSarlSpecificationVersion(Type5.class), 0f);
	}

	@Test
	public void compareToSarlSpecificationVersion() {
		assertStrictlyPositive(this.checker.compareToSarlSpecificationVersion(Type1.class));
		assertStrictlyPositive(this.checker.compareToSarlSpecificationVersion(Type2.class));
		assertStrictlyNegative(this.checker.compareToSarlSpecificationVersion(Type3.class));
		assertZero(this.checker.compareToSarlSpecificationVersion(Type4.class));
		assertStrictlyPositive(this.checker.compareToSarlSpecificationVersion(Type5.class));
	}

	@Test
	public void isValidSarlElement() {
		assertFalse(this.checker.isValidSarlElement(Type1.class));
		assertFalse(this.checker.isValidSarlElement(Type2.class));
		assertFalse(this.checker.isValidSarlElement(Type3.class));
		assertTrue(this.checker.isValidSarlElement(Type4.class));
		assertFalse(this.checker.isValidSarlElement(Type5.class));
	}

	public static class Type1 {
		// No SARL specification marker
	}

	@SarlSpecification("")
	public static class Type2 {
		// Invalid SARL specification marker
	}

	@SarlSpecification("0.1")
	public static class Type3 {
		// Lower SARL specification marker
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class Type4 {
		// Equal SARL specification marker
	}

	@SarlSpecification("10000000.0")
	public static class Type5 {
		// Greater SARL specification marker
	}

}

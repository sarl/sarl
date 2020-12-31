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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.util.tests.sarlspecification;

import static io.sarl.tests.api.tools.TestAssertions.assertStrictlyNegative;
import static io.sarl.tests.api.tools.TestAssertions.assertStrictlyPositive;
import static io.sarl.tests.api.tools.TestAssertions.assertZero;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.osgi.framework.Version;

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
@DisplayName("StandardSarlSpecificationChecker")
@Tag("api")
@Tag("unit")
public class StandardSarlSpecificationCheckerTest {

	private SarlSpecificationChecker checker;
	
	@BeforeEach
	public void setUp() {
		this.checker = new StandardSarlSpecificationChecker();
	}
	
	@AfterEach
	public void tearDown() {
		this.checker = null;
	}

	@Test
	public void getSarlSpecificationVersionObject() {
		assertNull(this.checker.getSarlSpecificationVersionObject(Type1.class));
		assertNull(this.checker.getSarlSpecificationVersionObject(Type2.class));
		assertEquals(Version.parseVersion("0.1"), this.checker.getSarlSpecificationVersionObject(Type3.class));
		assertEquals(Version.parseVersion("0.1.0"), this.checker.getSarlSpecificationVersionObject(Type3.class));
		assertEquals(Version.parseVersion(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING), this.checker.getSarlSpecificationVersionObject(Type4.class));
		assertEquals(Version.parseVersion("10000000"), this.checker.getSarlSpecificationVersionObject(Type5.class));
		assertEquals(Version.parseVersion("10000000.0"), this.checker.getSarlSpecificationVersionObject(Type5.class));
		assertEquals(Version.parseVersion("10000000.0.0"), this.checker.getSarlSpecificationVersionObject(Type5.class));
		assertEquals(Version.parseVersion("0.10"), this.checker.getSarlSpecificationVersionObject(Type6.class));
		assertEquals(Version.parseVersion("0.10.0"), this.checker.getSarlSpecificationVersionObject(Type6.class));
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

	@SarlSpecification("0.10")
	public static class Type6 {
		// Leading zero
	}

}

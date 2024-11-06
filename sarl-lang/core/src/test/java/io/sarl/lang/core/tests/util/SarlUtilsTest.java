/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
package io.sarl.lang.core.tests.util;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.util.SarlUtils;

/** This class tests the {@link SarlUtils} for SARL.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.10
 */
@SuppressWarnings("all")
@DisplayName("Utils")
@Tag("core")
@Tag("unit")
public class SarlUtilsTest {

	@Test
	@DisplayName("isHiddenMember")
	public void isHiddenMember() {
		assertFalse(SarlUtils.isHiddenMember(""));
		assertFalse(SarlUtils.isHiddenMember("abcde"));
		assertTrue(SarlUtils.isHiddenMember("ab$cd$e"));
	}

}

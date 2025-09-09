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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.core.tests.util;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.util.SarlUtils;

/** This class tests the {@link SarlUtils} for SARL.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.10
 */
@SuppressWarnings("all")
@DisplayName("Utils")
@Tag("core")
@Tag("unit")
public class SarlUtilsTest {
	
	private String[] extensions;

	@BeforeEach
	public void setUp() {
		this.extensions = new String[] { "sarl", "SARL", "Sarl" };
	}

	@Test
	@DisplayName("isHiddenMember")
	public void isHiddenMember() {
		assertFalse(SarlUtils.isHiddenMember(""));
		assertFalse(SarlUtils.isHiddenMember("abcde"));
		assertTrue(SarlUtils.isHiddenMember("ab$cd$e"));
	}

	@Test
	@DisplayName("getFileExtensions(String)")
	public void getFileExtensionsStringArray() {
		String[] expected = { "sarl", "SARL", "Sarl" };
		assertArrayEquals(expected, SarlUtils.getFileExtensions("sarl,SARL,Sarl"));
		assertArrayEquals(expected, SarlUtils.getFileExtensions("sarl,,SARL,Sarl,"));
		assertArrayEquals(expected, SarlUtils.getFileExtensions("sarl,  SARL , Sarl"));
	}

	@Test
	@DisplayName("getMajorFileExtension(String[])")
	public void getMajorFileExtensionStringArray() {
		assertEquals("sarl", SarlUtils.getMajorFileExtension(new String[] {"sarl", "SARL", "Sarl"}));
	}

	@Test
	@DisplayName("getMajorFileExtension(String)")
	public void getMajorFileExtensionString() {
		assertEquals("sarl", SarlUtils.getMajorFileExtension("sarl,SARL,Sarl"));
		assertEquals("sarl", SarlUtils.getMajorFileExtension("sarl,,SARL,Sarl,"));
		assertEquals("sarl", SarlUtils.getMajorFileExtension("sarl,  SARL , Sarl"));
	}

	@Test
	@DisplayName("hasFileExtension(null)")
	public void hasFileExtension_null() {
		assertFalse(SarlUtils.hasFileExtension(null, this.extensions));
	}

	@Test
	@DisplayName("hasFileExtension(\"\")")
	public void hasFileExtension_empty() {
		assertFalse(SarlUtils.hasFileExtension("", this.extensions));
	}

	@Test
	@DisplayName("hasFileExtension(\"abc\")")
	public void hasFileExtension_noextension() {
		assertFalse(SarlUtils.hasFileExtension("abc", this.extensions));		
	}

	@Test
	@DisplayName("hasFileExtension(\"abc.doc\")")
	public void hasFileExtension_doc() {
		assertFalse(SarlUtils.hasFileExtension("abc.doc", this.extensions));		
	}

	@Test
	@DisplayName("hasFileExtension(\"abc.sarl\")")
	public void hasFileExtension_sarl() {
		assertTrue(SarlUtils.hasFileExtension("abc.sarl", this.extensions));		
	}

	@Test
	@DisplayName("hasFileExtension(\"abc.SARL\")")
	public void hasFileExtension_SARL() {
		assertTrue(SarlUtils.hasFileExtension("abc.SARL", this.extensions));		
	}

	@Test
	@DisplayName("hasFileExtension(\"abc.Sarl\")")
	public void hasFileExtension_Sarl() {
		assertTrue(SarlUtils.hasFileExtension("abc.Sarl", this.extensions));		
	}

	@Test
	@DisplayName("hasFileExtension(\"abc.sArl\")")
	public void hasFileExtension_sArl() {
		assertFalse(SarlUtils.hasFileExtension("abc.sArl", this.extensions));		
	}

	@Test
	@DisplayName("hasFileExtension(\"abc.sar\")")
	public void hasFileExtension_sar() {
		assertFalse(SarlUtils.hasFileExtension("abc.sar", this.extensions));		
	}

	@Test
	@DisplayName("hasFileExtension(\"abc.sarlx\")")
	public void hasFileExtension_sarlx() {
		assertFalse(SarlUtils.hasFileExtension("abc.sarlx", this.extensions));		
	}

	@Test
	@DisplayName("hasFileExtension(\"abc.xsarl\")")
	public void hasFileExtension_xsarl() {
		assertFalse(SarlUtils.hasFileExtension("abc.xsarl", this.extensions));		
	}

	@Test
	@DisplayName("hasFileExtension(\"abcsarl\")")
	public void hasFileExtension_abcsarl() {
		assertFalse(SarlUtils.hasFileExtension("abcsarl", this.extensions));		
	}

}

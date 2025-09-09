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

package io.sarl.apputils.uiextensions.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.ITypeParameter;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.apputils.uiextensions.Utilities;

/** Tests for {@link Utilities}.
 *
 * @author $Author: sgalland$
 * @version uiextensions 0.15.0 20250909-115749
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid uiextensions
 * @since 0.15
 */
@SuppressWarnings("all")
@DisplayName("Utilities")
@Tag("unit")
public class UtilitiesTest {

	private static final List<IPath> FOLDERS = Arrays.asList(
			IPath.fromPortableString("src/main/java"),
			IPath.fromPortableString("src/main/resources"),
			IPath.fromPortableString("src/main/sarl"));
	
	@Test
	@DisplayName("isNested(abc/def)")
	public void isNested_abcdef() {
		assertFalse(Utilities.isNested(IPath.fromPortableString("abc/def"), FOLDERS.iterator()));
	}

	@Test
	@DisplayName("isNested(src/abc/def)")
	public void isNested_srcabcdef() {
		assertFalse(Utilities.isNested(IPath.fromPortableString("src/abc/def"), FOLDERS.iterator()));
	}

	@Test
	@DisplayName("isNested(src/main/abc/def)")
	public void isNested_srcmainabcdef() {
		assertFalse(Utilities.isNested(IPath.fromPortableString("src/main/abc/def"), FOLDERS.iterator()));
	}

	@Test
	@DisplayName("isNested(src/main/sarl/abc/def)")
	public void isNested_srcmainsarlabcdef() {
		assertTrue(Utilities.isNested(IPath.fromPortableString("src/main/sarl/abc/def"), FOLDERS.iterator()));
	}

	@Test
	@DisplayName("isNested(src/main/java/abc/def)")
	public void isNested_srcmainjavaabcdef() {
		assertTrue(Utilities.isNested(IPath.fromPortableString("src/main/java/abc/def"), FOLDERS.iterator()));
	}

	@Test
	@DisplayName("isNested(src/main/resources/abc/def)")
	public void isNested_srcmainresourcesabcdef() {
		assertTrue(Utilities.isNested(IPath.fromPortableString("src/main/resources/abc/def"), FOLDERS.iterator()));
	}

	@Test
	@DisplayName("isNested(src/main/generated-sources/sarl/abc/def)")
	public void isNested_srcmaingeneratedsourcessarlabcdef() {
		assertFalse(Utilities.isNested(IPath.fromPortableString("src/main/generated-sources/sarl/abc/def"), FOLDERS.iterator()));
	}

	@Test
	@DisplayName("parseVersion(null)")
	public void parseVersion_null() {
		assertNull(Utilities.parseVersion(null));
	}

	@Test
	@DisplayName("parseVersion(\"\")")
	public void parseVersion_empty() {
		assertNull(Utilities.parseVersion(""));
	}

	@Test
	@DisplayName("parseVersion(\"1\")")
	public void parseVersion_1() {
		final var v = Utilities.parseVersion("1");
		assertNotNull(v);
		assertEquals(1, v.getMajor());
		assertEquals(0, v.getMinor());
		assertEquals(0, v.getMicro());
		assertEquals("", v.getQualifier());
	}

	@Test
	@DisplayName("parseVersion(\"1.2\")")
	public void parseVersion_12() {
		final var v = Utilities.parseVersion("1.2");
		assertNotNull(v);
		assertEquals(1, v.getMajor());
		assertEquals(2, v.getMinor());
		assertEquals(0, v.getMicro());
		assertEquals("", v.getQualifier());
	}

	@Test
	@DisplayName("parseVersion(\"1.2.3\")")
	public void parseVersion_123() {
		final var v = Utilities.parseVersion("1.2.3");
		assertNotNull(v);
		assertEquals(1, v.getMajor());
		assertEquals(2, v.getMinor());
		assertEquals(3, v.getMicro());
		assertEquals("", v.getQualifier());
	}

	@Test
	@DisplayName("parseVersion(\"1.2.3.4\")")
	public void parseVersion_1234() {
		final var v = Utilities.parseVersion("1.2.3.4");
		assertNotNull(v);
		assertEquals(1, v.getMajor());
		assertEquals(2, v.getMinor());
		assertEquals(3, v.getMicro());
		assertEquals("4", v.getQualifier());
	}

	@Test
	@DisplayName("parseVersion(\"1.2.3.4qua\")")
	public void parseVersion_1234qua() {
		final var v = Utilities.parseVersion("1.2.3.4qua");
		assertNotNull(v);
		assertEquals(1, v.getMajor());
		assertEquals(2, v.getMinor());
		assertEquals(3, v.getMicro());
		assertEquals("4qua", v.getQualifier());
	}

	@Test
	@DisplayName("parseVersion(\"4qua\")")
	public void parseVersion_4qua() {
		final var v = Utilities.parseVersion("4qua");
		assertNull(v);
	}

	@Test
	@DisplayName("compareVersionToRange(0, 1, 3)")
	public void compareVersionToRange_0_1_3() {
		final var v = Utilities.parseVersion("0");
		final var min = Utilities.parseVersion("1");
		final var max = Utilities.parseVersion("3");
		assertEquals(-1, Utilities.compareVersionToRange(v, min, max));
	}

	@Test
	@DisplayName("compareVersionToRange(1, 1, 3)")
	public void compareVersionToRange_1_1_3() {
		final var v = Utilities.parseVersion("1");
		final var min = Utilities.parseVersion("1");
		final var max = Utilities.parseVersion("3");
		assertEquals(0, Utilities.compareVersionToRange(v, min, max));
	}

	@Test
	@DisplayName("compareVersionToRange(2, 1, 3)")
	public void compareVersionToRange_2_1_3() {
		final var v = Utilities.parseVersion("2");
		final var min = Utilities.parseVersion("1");
		final var max = Utilities.parseVersion("3");
		assertEquals(0, Utilities.compareVersionToRange(v, min, max));
	}

	@Test
	@DisplayName("compareVersionToRange(3, 1, 3)")
	public void compareVersionToRange_3_1_3() {
		final var v = Utilities.parseVersion("3");
		final var min = Utilities.parseVersion("1");
		final var max = Utilities.parseVersion("3");
		assertEquals(1, Utilities.compareVersionToRange(v, min, max));
	}

	@Test
	@DisplayName("compareVersionToRange(4, 1, 3)")
	public void compareVersionToRange_4_1_3() {
		final var v = Utilities.parseVersion("4");
		final var min = Utilities.parseVersion("1");
		final var max = Utilities.parseVersion("3");
		assertEquals(1, Utilities.compareVersionToRange(v, min, max));
	}

	@Test
	@DisplayName("compareVersionToRange(1.0, 1.1, 1.3)")
	public void compareVersionToRange_10_11_13() {
		final var v = Utilities.parseVersion("1.0");
		final var min = Utilities.parseVersion("1.1");
		final var max = Utilities.parseVersion("1.3");
		assertEquals(-1, Utilities.compareVersionToRange(v, min, max));
	}

	@Test
	@DisplayName("compareVersionToRange(1.1, 1.1, 1.3)")
	public void compareVersionToRange_11_11_13() {
		final var v = Utilities.parseVersion("1.1");
		final var min = Utilities.parseVersion("1.1");
		final var max = Utilities.parseVersion("1.3");
		assertEquals(0, Utilities.compareVersionToRange(v, min, max));
	}

	@Test
	@DisplayName("compareVersionToRange(1.2, 1.1, 1.3)")
	public void compareVersionToRange_12_11_13() {
		final var v = Utilities.parseVersion("1.2");
		final var min = Utilities.parseVersion("1.1");
		final var max = Utilities.parseVersion("1.3");
		assertEquals(0, Utilities.compareVersionToRange(v, min, max));
	}

	@Test
	@DisplayName("compareVersionToRange(1.3, 1.1, 1.3)")
	public void compareVersionToRange_13_11_13() {
		final var v = Utilities.parseVersion("1.3");
		final var min = Utilities.parseVersion("1.1");
		final var max = Utilities.parseVersion("1.3");
		assertEquals(1, Utilities.compareVersionToRange(v, min, max));
	}

	@Test
	@DisplayName("compareVersionToRange(1.4, 1.1, 1.3)")
	public void compareVersionToRange_14_11_13() {
		final var v = Utilities.parseVersion("1.4");
		final var min = Utilities.parseVersion("1.1");
		final var max = Utilities.parseVersion("1.3");
		assertEquals(1, Utilities.compareVersionToRange(v, min, max));
	}

	@Test
	@DisplayName("getNameWithTypeParameters(null)")
	public void getNameWithTypeParameters_null() {
		Assertions.assertThrows(AssertionError.class, () -> {
			Utilities.getNameWithTypeParameters(null);
		});
	}

	@Test
	@DisplayName("getNameWithTypeParameters(abc.def<X,Y,Z>)")
	public void getNameWithTypeParameters() throws Exception {
		final var param0 = mock(ITypeParameter.class);
		when(param0.getElementName()).thenReturn("X");
		final var param1 = mock(ITypeParameter.class);
		when(param1.getElementName()).thenReturn("Y");
		final var param2 = mock(ITypeParameter.class);
		when(param2.getElementName()).thenReturn("Z");
		final var t = mock(IType.class);
		when(t.getFullyQualifiedName(anyChar())).thenReturn("abc.def");
		final ITypeParameter[] params =  {	
				param0, param1, param2
		};
		when(t.getTypeParameters()).thenReturn(params);
		assertEquals("abc.def<X, Y, Z>", Utilities.getNameWithTypeParameters(t));
	}

	@Test
	@DisplayName("isGeneratedFolder((String) null)")
	public void isGeneratedFolder_String_null() {
		assertFalse(Utilities.isGeneratedFolder((String) null));
	}

	@Test
	@DisplayName("isGeneratedFolder(\"\")")
	public void isGeneratedFolder_String_empty() {
		assertFalse(Utilities.isGeneratedFolder(""));
	}

	@Test
	@DisplayName("isGeneratedFolder(\"abc/def\")")
	public void isGeneratedFolder_String_abcdef() {
		assertFalse(Utilities.isGeneratedFolder("abc/def"));
	}

	@Test
	@DisplayName("isGeneratedFolder(\"abc/src-gen/def\")")
	public void isGeneratedFolder_String_srcgen() {
		assertTrue(Utilities.isGeneratedFolder("abc/src-gen/def"));
	}

	@Test
	@DisplayName("isGeneratedFolder(\"abc/generated-sources/def\")")
	public void isGeneratedFolder_String_generatedsources() {
		assertTrue(Utilities.isGeneratedFolder("abc/generated-sources/def"));
	}

	@Test
	@DisplayName("isGeneratedFolder((IPath) null)")
	public void isGeneratedFolder_IPath_null() {
		assertFalse(Utilities.isGeneratedFolder((IPath) null));
	}

	@Test
	@DisplayName("isGeneratedFolder(abc/def)")
	public void isGeneratedFolder_IPath_abcdef() {
		assertFalse(Utilities.isGeneratedFolder(IPath.fromPortableString("abc/def")));
	}

	@Test
	@DisplayName("isGeneratedFolder(abc/src-gen/def)")
	public void isGeneratedFolder_IPath_srcgen() {
		assertTrue(Utilities.isGeneratedFolder(IPath.fromPortableString("abc/src-gen/def")));
	}

	@Test
	@DisplayName("isGeneratedFolder(abc/generated-sources/def)")
	public void isGeneratedFolder_IPath_generatedsources() {
		assertTrue(Utilities.isGeneratedFolder(IPath.fromPortableString("abc/generated-sources/def")));
	}

}
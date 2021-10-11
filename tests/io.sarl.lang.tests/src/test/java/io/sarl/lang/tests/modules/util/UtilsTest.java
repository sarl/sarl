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
package io.sarl.lang.tests.modules.util;

import static io.sarl.tests.api.tools.TestAssertions.assertStrictlyNegative;
import static io.sarl.tests.api.tools.TestAssertions.assertStrictlyPositive;
import static io.sarl.tests.api.tools.TestAssertions.assertZero;
import static io.sarl.tests.api.tools.TestMockito.mock;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyChar;
import static org.mockito.Mockito.when;

import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Collections;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.osgi.framework.Version;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.DefaultValueUse;
import io.sarl.lang.annotation.EarlyExit;
import io.sarl.lang.annotation.PrivateAPI;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.util.OutParameter;
import io.sarl.lang.util.SarlUtils;
import io.sarl.lang.util.Utils;
import io.sarl.lang.util.Utils.SarlLibraryErrorCode;
import io.sarl.tests.api.AbstractSarlTest;

/** This class tests the {@link SarlUtils} for SARL.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
@SuppressWarnings("all")
@DisplayName("Utils")
@Tag("core")
@Tag("unit")
public class UtilsTest extends AbstractSarlTest {

	@Test
	public void fixHiddenMember() {
		assertEquals("", Utils.fixHiddenMember(""));
		assertEquals("abcde", Utils.fixHiddenMember("abcde"));
		assertEquals("ab_cd_e", Utils.fixHiddenMember("ab$cd$e"));
	}

	@Test
	public void isNameForHiddenCapacityImplementationCallingMethod() {
		assertFalse(Utils.isNameForHiddenCapacityImplementationCallingMethod(null));
		assertFalse(Utils.isNameForHiddenCapacityImplementationCallingMethod(""));
		assertFalse(Utils.isNameForHiddenCapacityImplementationCallingMethod("x"));
		assertFalse(Utils.isNameForHiddenCapacityImplementationCallingMethod("$CAPACITY_USE$x"));
		assertFalse(Utils.isNameForHiddenCapacityImplementationCallingMethod("x$CALLER"));
		assertTrue(Utils.isNameForHiddenCapacityImplementationCallingMethod("$CAPACITY_USE$x$CALLER"));
	}

	@Test
	public void isClass_LightweightTypeReference() {
		assertTrue(Utils.isClass(mockLightweightTypeReference(NotFinalClassT.class)));
		assertFalse(Utils.isClass(mockLightweightTypeReference(InterfaceT.class)));
	}

	@Test
	public void isClass_Class() {
		assertTrue(Utils.isClass(NotFinalClassT.class));
		assertFalse(Utils.isClass(InterfaceT.class));
	}

	@Test
	public void isFinal_LightweightTypeReference() {
		assertFalse(Utils.isFinal(mockLightweightTypeReference(NotFinalClassT.class)));
		assertTrue(Utils.isFinal(mockLightweightTypeReference(FinalClassT.class)));
	}
	
	@Test
	public void isFinal_Class() {
		assertFalse(Utils.isFinal(NotFinalClassT.class));
		assertTrue(Utils.isFinal(FinalClassT.class));
	}

	@Test
	public void isInterface_LightweightTypeReference() {
		assertFalse(Utils.isInterface(mockLightweightTypeReference(NotFinalClassT.class)));
		assertTrue(Utils.isInterface(mockLightweightTypeReference(InterfaceT.class)));
	}

	@Test
	public void compareVersions() {
		assertStrictlyPositive(Utils.compareVersions("1.0", "0.1"));
		assertStrictlyPositive(Utils.compareVersions("1.1", "1.0"));
		assertStrictlyPositive(Utils.compareVersions("1.1", "1.0.9"));
		assertStrictlyPositive(Utils.compareVersions("1.0.9", "1.0.8"));
		assertStrictlyNegative(Utils.compareVersions("0.1", "1.0"));
		assertStrictlyNegative(Utils.compareVersions("1.0", "1.1"));
		assertStrictlyNegative(Utils.compareVersions("1.0.9", "1.1"));
		assertStrictlyNegative(Utils.compareVersions("1.0.8", "1.0.9"));
		assertZero(Utils.compareVersions("1.0.9", "1.0.9"));
	}

	@Test
	public void singletonList() {
		assertEquals(Collections.emptyList(), Utils.singletonList(null));
		assertEquals(Collections.singletonList(1), Utils.singletonList(1));
	}

	@Test
	public void getQualifiedName() {
		final JvmIdentifiableElement element = mock(JvmIdentifiableElement.class);
		when(element.getQualifiedName(anyChar())).thenReturn("a.cd.e.f");
		final QualifiedName name = Utils.getQualifiedName(element);
		assertNotNull(name);
		assertEquals(Arrays.asList("a", "cd", "e", "f"), name.getSegments());
	}

	@Test
	public void isCompatibleSARLLibraryVersion() {
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.1"));
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.2"));
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.3"));
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.4"));
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.5"));
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.6"));
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.7"));
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.8"));
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.9"));
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.10"));
		assertFalse(Utils.isCompatibleSARLLibraryVersion("0.10.1"));

		assertTrue(Utils.isCompatibleSARLLibraryVersion(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING));
		assertTrue(Utils.isCompatibleSARLLibraryVersion(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + ".0"));
		assertTrue(Utils.isCompatibleSARLLibraryVersion(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + ".1"));

		Version nextVersion = Version.parseVersion(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING);
		assertFalse(Utils.isCompatibleSARLLibraryVersion(nextVersion.getMajor() + "." + (nextVersion.getMinor() + 1)));
	}

	@Test
	public void getSARLLibraryVersionOnClasspath_TypeReferencesNotifierOutParameter() {
		final TypeReferences references = mock(TypeReferences.class);
		JvmType type = mock(JvmType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);
		final Notifier context = mock(Notifier.class);
		final OutParameter<String> version = new OutParameter<>();
		
		SarlLibraryErrorCode code = Utils.getSARLLibraryVersionOnClasspath(references, context, version);
		assertEquals(SarlLibraryErrorCode.NO_SARL_VERSION_DECLARED_TYPE, code);

		JvmDeclaredType dtype = mock(JvmDeclaredType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(dtype);
		JvmField field = mock(JvmField.class);
		when(field.getSimpleName()).thenReturn("X");
		when(dtype.getDeclaredFields()).thenReturn(Arrays.asList(field));
		code = Utils.getSARLLibraryVersionOnClasspath(references, context, version);
		assertEquals(SarlLibraryErrorCode.NO_SARL_VERSION_FIELD, code);

		when(field.getSimpleName()).thenReturn("SPECIFICATION_RELEASE_VERSION_STRING");
		when(field.getConstantValueAsString()).thenReturn("");
		code = Utils.getSARLLibraryVersionOnClasspath(references, context, version);
		assertEquals(SarlLibraryErrorCode.NO_SARL_VERSION_VALUE, code);

		when(field.getConstantValueAsString()).thenReturn("2.3.4");
		code = Utils.getSARLLibraryVersionOnClasspath(references, context, version);
		assertEquals(SarlLibraryErrorCode.SARL_FOUND, code);
		assertEquals("2.3.4", version.get());
	}

	@Test
	public void isCompatibleSARLLibraryOnClasspath() {
		final TypeReferences references = mock(TypeReferences.class);
		JvmType type = mock(JvmType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);
		final Notifier context = mock(Notifier.class);
		
		assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context));

		JvmDeclaredType dtype = mock(JvmDeclaredType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(dtype);
		JvmField field = mock(JvmField.class);
		when(field.getSimpleName()).thenReturn("X");
		when(dtype.getDeclaredFields()).thenReturn(Arrays.asList(field));
		assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context));

		when(field.getSimpleName()).thenReturn("SPECIFICATION_RELEASE_VERSION_STRING");
		when(field.getConstantValueAsString()).thenReturn("");
		assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context));

		when(field.getConstantValueAsString()).thenReturn("2.3.4");
		assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context));

		when(field.getConstantValueAsString()).thenReturn("0.9");
		assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context));

		when(field.getConstantValueAsString()).thenReturn("0.10");
		assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context));

		when(field.getConstantValueAsString()).thenReturn("0.10.1");
		assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context));

		when(field.getConstantValueAsString()).thenReturn(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING);
		assertTrue(Utils.isCompatibleSARLLibraryOnClasspath(references, context));
		
		when(field.getConstantValueAsString()).thenReturn(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + ".0");
		assertTrue(Utils.isCompatibleSARLLibraryOnClasspath(references, context));

		when(field.getConstantValueAsString()).thenReturn(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + ".1");
		assertTrue(Utils.isCompatibleSARLLibraryOnClasspath(references, context));

		Version nextVersion = Version.parseVersion(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING);
		when(field.getConstantValueAsString()).thenReturn(nextVersion.getMajor() + "." + (nextVersion.getMinor() + 1));
		assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context));
	}

	@Test
	public void isCompatibleJDKVersionWithSARLCompilationEnvironment_String() {
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("1.4"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("1.5"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("1.6"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("1.7"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("1.8"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("1.9"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("9"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("10"));
		assertTrue(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("11"));
		assertTrue(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("11.1"));
		assertTrue(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("11.9"));
		assertTrue(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("12"));
		assertTrue(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("13"));
		assertTrue(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("14"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("15"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("16"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("17"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("18"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("19"));
		assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("20"));
	}

	@Test
	public void isCompatibleJDKVersionWhenInSARLProjectClasspath_String() {
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("1.4"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("1.5"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("1.6"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("1.7"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("1.8"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("1.9"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("9"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("10"));
		assertTrue(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("11"));
		assertTrue(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("11.1"));
		assertTrue(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment("11.9"));
		assertTrue(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("12"));
		assertTrue(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("13"));
		assertTrue(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("14"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("15"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("16"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("17"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("18"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("19"));
		assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath("20"));
	}

	@Test
	public void isCompatibleXtextVersion_String() {
		assertFalse(Utils.isCompatibleXtextVersion("2.14"));
		assertFalse(Utils.isCompatibleXtextVersion("2.15"));
		assertFalse(Utils.isCompatibleXtextVersion("2.16"));
		assertFalse(Utils.isCompatibleXtextVersion("2.17"));
		assertFalse(Utils.isCompatibleXtextVersion("2.18"));
		assertFalse(Utils.isCompatibleXtextVersion("2.19"));
		assertFalse(Utils.isCompatibleXtextVersion("2.20"));
		assertFalse(Utils.isCompatibleXtextVersion("2.21"));
		assertFalse(Utils.isCompatibleXtextVersion("2.22"));
		assertFalse(Utils.isCompatibleXtextVersion("2.23"));
		assertFalse(Utils.isCompatibleXtextVersion("2.24"));
		assertTrue(Utils.isCompatibleXtextVersion("2.25"));
		assertTrue(Utils.isCompatibleXtextVersion("2.25.1"));
		assertTrue(Utils.isCompatibleXtextVersion("2.26"));
		assertTrue(Utils.isCompatibleXtextVersion("2.27"));
		assertTrue(Utils.isCompatibleXtextVersion("2.28"));
		assertTrue(Utils.isCompatibleXtextVersion("2.29"));
		assertTrue(Utils.isCompatibleXtextVersion("2.30"));
		assertTrue(Utils.isCompatibleXtextVersion("2.31"));
	}

	@Test
	public void isSARLAnnotation_Class() {
		assertFalse(Utils.isSARLAnnotation(Test.class));
		assertTrue(Utils.isSARLAnnotation(SarlSpecification.class));
		assertTrue(Utils.isSARLAnnotation(DefaultValue.class));
		assertTrue(Utils.isSARLAnnotation(DefaultValueSource.class));
		assertTrue(Utils.isSARLAnnotation(DefaultValueUse.class));
		assertTrue(Utils.isSARLAnnotation(EarlyExit.class));
		assertTrue(Utils.isSARLAnnotation(PrivateAPI.class));
	}

	@Test
	public void isSARLAnnotation_String() {
		assertFalse(Utils.isSARLAnnotation(Test.class.getName()));
		assertTrue(Utils.isSARLAnnotation(SarlSpecification.class.getName()));
		assertTrue(Utils.isSARLAnnotation(DefaultValue.class.getName()));
		assertTrue(Utils.isSARLAnnotation(DefaultValueSource.class.getName()));
		assertTrue(Utils.isSARLAnnotation(DefaultValueUse.class.getName()));
		assertTrue(Utils.isSARLAnnotation(EarlyExit.class.getName()));
		assertTrue(Utils.isSARLAnnotation(PrivateAPI.class.getName()));
	}

	private static LightweightTypeReference mockLightweightTypeReference(Class<?> type) {
		final JvmGenericType jvmtype = mock(JvmGenericType.class);
		when(jvmtype.isInterface()).thenReturn(type.isInterface());
		when(jvmtype.isAbstract()).thenReturn(Modifier.isAbstract(type.getModifiers()));
		when(jvmtype.isFinal()).thenReturn(Modifier.isFinal(type.getModifiers()));
		final LightweightTypeReference reference = mock(LightweightTypeReference.class);
		when(reference.getType()).thenReturn(jvmtype);
		return reference;
	}
	
	private static class NotFinalClassT {
		//
	}

	private static final class FinalClassT {
		//
	}

	private static interface InterfaceT {
		//
	}

}

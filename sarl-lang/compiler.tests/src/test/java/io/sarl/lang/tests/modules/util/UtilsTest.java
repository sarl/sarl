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
package io.sarl.lang.tests.modules.util;

import static io.sarl.tests.api.tools.TestAssertions.assertStrictlyNegative;
import static io.sarl.tests.api.tools.TestAssertions.assertStrictlyPositive;
import static io.sarl.tests.api.tools.TestAssertions.assertZero;
import static io.sarl.tests.api.tools.TestMockito.mock;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyChar;
import static org.mockito.Mockito.when;

import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.typesystem.references.ITypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.WildcardTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.function.Executable;
import org.osgi.framework.Version;

import com.google.inject.Inject;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.core.annotation.DefaultValue;
import io.sarl.lang.core.annotation.DefaultValueSource;
import io.sarl.lang.core.annotation.DefaultValueUse;
import io.sarl.lang.core.annotation.EarlyExit;
import io.sarl.lang.core.annotation.PrivateAPI;
import io.sarl.lang.core.annotation.SarlSpecification;
import io.sarl.lang.core.util.OutParameter;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.util.Utils;
import io.sarl.lang.util.Utils.SarlLibraryErrorCode;

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

	@Inject
	private CommonTypeComputationServices services;

	private static JvmTypeReference mockJvmTypeReference(Class<?> type) {
		final JvmGenericType jvmtype = mock(JvmGenericType.class);
		when(jvmtype.isInterface()).thenReturn(type.isInterface());
		when(jvmtype.isAbstract()).thenReturn(Modifier.isAbstract(type.getModifiers()));
		when(jvmtype.isFinal()).thenReturn(Modifier.isFinal(type.getModifiers()));
		final JvmTypeReference reference = mock(JvmTypeReference.class);
		when(reference.getType()).thenReturn(jvmtype);
		return reference;
	}

	private static JvmTypeReference mockJvmTypeReference(Class<?> type, ITypeReferenceOwner owner) {
		return owner.getServices().getTypeReferences().getTypeForName(type, mock(Notifier.class));
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

	private static List<LightweightTypeReference> mockLightweightTypeReferences(Class<?> type) {
		return Collections.singletonList(mockLightweightTypeReference(type));
	}

	private static LightweightTypeReference mockGenericTypeReference(String genericTypeName, Class<?> type) {
		final JvmGenericType jvmtype = mock(JvmGenericType.class);
		when(jvmtype.isInterface()).thenReturn(type.isInterface());
		when(jvmtype.isAbstract()).thenReturn(Modifier.isAbstract(type.getModifiers()));
		when(jvmtype.isFinal()).thenReturn(Modifier.isFinal(type.getModifiers()));
		final LightweightTypeReference reference = mock(LightweightTypeReference.class);
		when(reference.getType()).thenReturn(jvmtype);
		when(reference.getIdentifier()).thenReturn(genericTypeName);
		return reference;
	}

	private static WildcardTypeReference mockWildcardTypeReference(Class<?> type) {
		final JvmGenericType jvmtype = mock(JvmGenericType.class);
		when(jvmtype.isInterface()).thenReturn(type.isInterface());
		when(jvmtype.isAbstract()).thenReturn(Modifier.isAbstract(type.getModifiers()));
		when(jvmtype.isFinal()).thenReturn(Modifier.isFinal(type.getModifiers()));
		final WildcardTypeReference reference = mock(WildcardTypeReference.class);
		when(reference.getType()).thenReturn(jvmtype);
		return reference;
	}

	private static List<LightweightTypeReference> mockWildcardTypeReferences(Class<?> type) {
		return Collections.singletonList(mockWildcardTypeReference(type));
	}

	private static List<DynamicTest> buildDynamicTestsOnVersions(String referenceVersion,
			Function2<String, String, Executable> currentVersionTest,
			Function2<String, String, Executable> lowerMinorVersionTest,
			Function2<String, String, Executable> upperMinorVersionTest,
			Function2<String, String, Executable> lowerMajorVersionTest,
			Function2<String, String, Executable> upperMajorVersionTest) {
		final var tests = new ArrayList<DynamicTest>(50);
		final var current = Version.parseVersion(referenceVersion);

		// Previous major versions
		for (var i = current.getMajor() - 1; i >= 0; --i) {
			final var versionStr = new StringBuilder().append(i);
			final var vers = versionStr.toString();
			tests.add(dynamicTest(vers, lowerMajorVersionTest.apply("Previous major+0: " + vers, vers)));
			versionStr.append(".");
			for (var j = 0; j < 20; ++j) {
				final var versionStr1 = new StringBuilder(versionStr).append(j).toString();
				tests.add(dynamicTest(versionStr1, lowerMajorVersionTest.apply("Previous major+minor: " + versionStr1, versionStr1)));
			}
		}
		
		// Current major version
		final var currentVersionStr = new StringBuilder().append(current.getMajor());
		if (current.getMinor() == 0) {
			final var vers = currentVersionStr.toString();
			tests.add(dynamicTest(vers, currentVersionTest.apply("Current major+0: " + vers, vers)));
		} else {
			final var vers = currentVersionStr.toString();
			tests.add(dynamicTest(vers, lowerMinorVersionTest.apply("Previous major+minor: " + vers, vers)));
		}
		currentVersionStr.append(".");
		for (var j = 0; j < current.getMinor(); ++j) {
			final var currentVersionStr1 = new StringBuilder(currentVersionStr).append(j).toString();
			tests.add(dynamicTest(currentVersionStr1, lowerMinorVersionTest.apply("Previous minor: " + currentVersionStr1, currentVersionStr1)));
		}
		final var currentVersionStr2 = new StringBuilder(currentVersionStr).append(current.getMinor()).toString();
		tests.add(dynamicTest(currentVersionStr2, currentVersionTest.apply("Current major+minor: " + currentVersionStr2, currentVersionStr2)));
		for (var j = current.getMinor() + 1; j < current.getMinor() + 5; ++j) {
			final var currentVersionStr1 = new StringBuilder(currentVersionStr).append(j).toString();
			tests.add(dynamicTest(currentVersionStr1, upperMinorVersionTest.apply("Next minor: " + currentVersionStr1, currentVersionStr1)));
		}

		// Future major versions
		for (var i = current.getMajor() + 1; i <= current.getMajor() + 5; ++i) {
			final var versionStr = new StringBuilder().append(i);
			final var vers = versionStr.toString();
			tests.add(dynamicTest(vers, upperMajorVersionTest.apply("Next major+0: " + vers, vers)));
			versionStr.append(".");
			for (var j = 0; j < 5; ++j) {
				final var versionStr1 = new StringBuilder(versionStr).append(j).toString();
				tests.add(dynamicTest(versionStr1, upperMajorVersionTest.apply("Next major+minor: " + versionStr1, versionStr1)));
			}
		}

		return tests;
	}
	
	@Test
	@DisplayName("fixHiddenMember(\"\")")
	public void fixHiddenMember_0() {
		assertEquals("", Utils.fixHiddenMember(""));
	}

	@Test
	@DisplayName("fixHiddenMember(\"abcde\")")
	public void fixHiddenMember_1() {
		assertEquals("abcde", Utils.fixHiddenMember("abcde"));
	}

	@Test
	@DisplayName("fixHiddenMember(\"ab_cd_e\")")
	public void fixHiddenMember_2() {
		assertEquals("ab_cd_e", Utils.fixHiddenMember("ab$cd$e"));
	}

	@Test
	@DisplayName("isNameForHiddenCapacityImplementationCallingMethod(null)")
	public void isNameForHiddenCapacityImplementationCallingMethod_0() {
		assertFalse(Utils.isNameForHiddenCapacityImplementationCallingMethod(null));
	}

	@Test
	@DisplayName("isNameForHiddenCapacityImplementationCallingMethod(\"\")")
	public void isNameForHiddenCapacityImplementationCallingMethod_1() {
		assertFalse(Utils.isNameForHiddenCapacityImplementationCallingMethod(""));
	}

	@Test
	@DisplayName("isNameForHiddenCapacityImplementationCallingMethod(\"x\")")
	public void isNameForHiddenCapacityImplementationCallingMethod_2() {
		assertFalse(Utils.isNameForHiddenCapacityImplementationCallingMethod("x"));
	}

	@Test
	@DisplayName("isNameForHiddenCapacityImplementationCallingMethod(\"$CAPACITY_USE$x\")")
	public void isNameForHiddenCapacityImplementationCallingMethod_3() {
		assertFalse(Utils.isNameForHiddenCapacityImplementationCallingMethod("$CAPACITY_USE$x"));
	}

	@Test
	@DisplayName("isNameForHiddenCapacityImplementationCallingMethod(\"x$CALLER\")")
	public void isNameForHiddenCapacityImplementationCallingMethod_4() {
		assertFalse(Utils.isNameForHiddenCapacityImplementationCallingMethod("x$CALLER"));
	}

	@Test
	@DisplayName("isNameForHiddenCapacityImplementationCallingMethod(\"$CAPACITY_USE$x$CALLER\")")
	public void isNameForHiddenCapacityImplementationCallingMethod() {
		assertTrue(Utils.isNameForHiddenCapacityImplementationCallingMethod("$CAPACITY_USE$x$CALLER"));
	}

	@Test
	@DisplayName("isClass(ref to not-final class)")
	public void isClass_LightweightTypeReference_0() {
		assertTrue(Utils.isClass(mockLightweightTypeReference(NotFinalClassT.class)));
	}

	@Test
	@DisplayName("isClass(ref to interface)")
	public void isClass_LightweightTypeReference_1() {
		assertFalse(Utils.isClass(mockLightweightTypeReference(InterfaceT.class)));
	}

	@Test
	@DisplayName("isClass(ref to final class)")
	public void isClass_LightweightTypeReference_2() {
		assertTrue(Utils.isClass(mockLightweightTypeReference(FinalClassT.class)));
	}

	@Test
	@DisplayName("isClass(not-final class)")
	public void isClass_Class_0() {
		assertTrue(Utils.isClass(NotFinalClassT.class));
	}

	@Test
	@DisplayName("isClass(final class)")
	public void isClass_Class_1() {
		assertTrue(Utils.isClass(FinalClassT.class));
	}

	@Test
	@DisplayName("isClass(interface)")
	public void isClass_Interface() {
		assertFalse(Utils.isClass(InterfaceT.class));
	}

	@Test
	@DisplayName("isFinal(ref to not-final class)")
	public void isFinal_LightweightTypeReference_0() {
		assertFalse(Utils.isFinal(mockLightweightTypeReference(NotFinalClassT.class)));
	}
	
	@Test
	@DisplayName("isFinal(ref to final class)")
	public void isFinal_LightweightTypeReference_1() {
		assertTrue(Utils.isFinal(mockLightweightTypeReference(FinalClassT.class)));
	}

	@Test
	@DisplayName("isFinal(ref to interface )")
	public void isFinal_LightweightTypeReference_2() {
		assertFalse(Utils.isFinal(mockLightweightTypeReference(InterfaceT.class)));
	}

	@Test
	@DisplayName("isFinal(not-final class)")
	public void isFinal_NotFinalClass() {
		assertFalse(Utils.isFinal(NotFinalClassT.class));
	}

	@Test
	@DisplayName("isFinal(final class)")
	public void isFinal_FinalClass() {
		assertTrue(Utils.isFinal(FinalClassT.class));
	}

	@Test
	@DisplayName("isFinal(interface)")
	public void isFinal_Interface() {
		assertFalse(Utils.isFinal(InterfaceT.class));
	}

	@Test
	@DisplayName("isInterface(ref to not-final class)")
	public void isInterface_LightweightTypeReference_0() {
		assertFalse(Utils.isInterface(mockLightweightTypeReference(NotFinalClassT.class)));
	}

	@Test
	@DisplayName("isInterface(ref to final class)")
	public void isInterface_LightweightTypeReference_1() {
		assertFalse(Utils.isInterface(mockLightweightTypeReference(FinalClassT.class)));
	}

	@Test
	@DisplayName("isInterface(ref to interface)")
	public void isInterface_LightweightTypeReference_2() {
		assertTrue(Utils.isInterface(mockLightweightTypeReference(InterfaceT.class)));
	}

	@Test
	@DisplayName("compareMajorMinorVersions(\"1.0\", \"0.1\")")
	public void compareMajorMinorVersions_0() {
		assertStrictlyPositive(Utils.compareMajorMinorVersions("1.0", "0.1"));
	}

	@Test
	@DisplayName("compareMajorMinorVersions(\"1.1\", \"1.0\")")
	public void compareMajorMinorVersions_1() {
		assertStrictlyPositive(Utils.compareMajorMinorVersions("1.1", "1.0"));
	}

	@Test
	@DisplayName("compareMajorMinorVersions(\"1.1\", \"1.0.9\")")
	public void compareMajorMinorVersions_2() {
		assertStrictlyPositive(Utils.compareMajorMinorVersions("1.1", "1.0.9"));
	}

	@Test
	@DisplayName("compareMajorMinorVersions(\"1.0.9\", \"1.0.8\")")
	public void compareMajorMinorVersions_3() {
		assertZero(Utils.compareMajorMinorVersions("1.0.9", "1.0.8"));
	}

	@Test
	@DisplayName("compareMajorMinorVersions(\"0.1\", \"1.0\")")
	public void compareMajorMinorVersions_4() {
		assertStrictlyNegative(Utils.compareMajorMinorVersions("0.1", "1.0"));
	}

	@Test
	@DisplayName("compareMajorMinorVersions(\"1.0\", \"1.1\")")
	public void compareMajorMinorVersions_5() {
		assertStrictlyNegative(Utils.compareMajorMinorVersions("1.0", "1.1"));
	}

	@Test
	@DisplayName("compareMajorMinorVersions(\"1.0.9\", \"1.1\")")
	public void compareMajorMinorVersions_6() {
		assertStrictlyNegative(Utils.compareMajorMinorVersions("1.0.9", "1.1"));
	}

	@Test
	@DisplayName("compareMajorMinorVersions(\"1.0.8\", \"1.0.9\")")
	public void compareMajorMinorVersions_7() {
		assertZero(Utils.compareMajorMinorVersions("1.0.8", "1.0.9"));
	}

	@Test
	@DisplayName("compareMajorMinorVersions(\"1.0.9\", \"1.0.9\")")
	public void compareMajorMinorVersions_8() {
		assertZero(Utils.compareMajorMinorVersions("1.0.9", "1.0.9"));
	}

	@Test
	@DisplayName("singletonList(null)")
	public void singletonList_0() {
		assertEquals(Collections.emptyList(), Utils.singletonList(null));
	}

	@Test
	@DisplayName("singletonList(127)")
	public void singletonList_1() {
		assertEquals(Collections.singletonList(127), Utils.singletonList(127));
	}

	@Test
	@DisplayName("getQualifiedName(null)")
	public void getQualifiedName_0() {
		final QualifiedName name = Utils.getQualifiedName(null);
		assertNull(name);
	}

	@Test
	@DisplayName("getQualifiedName(\"a.cd.e.f\")")
	public void getQualifiedName_1() {
		final JvmIdentifiableElement element = mock(JvmIdentifiableElement.class);
		when(element.getQualifiedName(anyChar())).thenReturn("a.cd.e.f");
		final QualifiedName name = Utils.getQualifiedName(element);
		assertNotNull(name);
		assertEquals(Arrays.asList("a", "cd", "e", "f"), name.getSegments());
	}

	/** Create the dynamic tests for {@code isCompatibleSARLLibraryVersion}.
	 *
	 * @return the dynamic tests.
	 */
	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("isCompatibleSARLLibraryVersion")
	public List<DynamicTest> isCompatibleSARLLibraryVersion() {
		final Function2<String, String, Executable> trueTest = (label, version) -> {
			return () -> assertTrue(Utils.isCompatibleSARLLibraryVersion(version.toString()), label);
		};
		
		final Function2<String, String, Executable> falseTest = (label, version) -> {
			return () -> assertFalse(Utils.isCompatibleSARLLibraryVersion(version.toString()), label);
		};

		return buildDynamicTestsOnVersions(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING, 
				trueTest,
				falseTest, falseTest,
				falseTest, falseTest);
	}

	@Test
	@DisplayName("getSARLLibraryVersionOnClasspath - no type")
	public void getSARLLibraryVersionOnClasspath_TypeReferencesNotifierOutParameter_0() {
		final TypeReferences references = mock(TypeReferences.class);
		final Notifier context = mock(Notifier.class);
		final OutParameter<String> version = new OutParameter<>();

		JvmType type = mock(JvmType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);

		SarlLibraryErrorCode code = Utils.getSARLLibraryVersionOnClasspath(references, context, version);
		assertEquals(SarlLibraryErrorCode.NO_SARL_VERSION_DECLARED_TYPE, code);
	}

	@Test
	@DisplayName("getSARLLibraryVersionOnClasspath - no field")
	public void getSARLLibraryVersionOnClasspath_TypeReferencesNotifierOutParameter_1() {
		final TypeReferences references = mock(TypeReferences.class);
		final Notifier context = mock(Notifier.class);
		final OutParameter<String> version = new OutParameter<>();

		JvmDeclaredType type = mock(JvmDeclaredType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);
		
		JvmField field = mock(JvmField.class);
		when(field.getSimpleName()).thenReturn("X");
		when(type.getDeclaredFields()).thenReturn(Arrays.asList(field));
		
		SarlLibraryErrorCode code = Utils.getSARLLibraryVersionOnClasspath(references, context, version);
		assertEquals(SarlLibraryErrorCode.NO_SARL_VERSION_FIELD, code);
	}

	@Test
	@DisplayName("getSARLLibraryVersionOnClasspath - no version value")
	public void getSARLLibraryVersionOnClasspath_TypeReferencesNotifierOutParameter_2() {
		final TypeReferences references = mock(TypeReferences.class);
		final Notifier context = mock(Notifier.class);
		final OutParameter<String> version = new OutParameter<>();
		
		JvmDeclaredType type = mock(JvmDeclaredType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);

		JvmField field = mock(JvmField.class);
		when(field.getSimpleName()).thenReturn("SPECIFICATION_RELEASE_VERSION_STRING");
		when(field.getConstantValueAsString()).thenReturn("");
		when(type.getDeclaredFields()).thenReturn(Arrays.asList(field));

		SarlLibraryErrorCode code = Utils.getSARLLibraryVersionOnClasspath(references, context, version);
		assertEquals(SarlLibraryErrorCode.NO_SARL_VERSION_VALUE, code);
	}

	@Test
	@DisplayName("getSARLLibraryVersionOnClasspath - valid")
	public void getSARLLibraryVersionOnClasspath_TypeReferencesNotifierOutParameter_3() {
		final TypeReferences references = mock(TypeReferences.class);
		final Notifier context = mock(Notifier.class);
		final OutParameter<String> version = new OutParameter<>();
		
		JvmDeclaredType type = mock(JvmDeclaredType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);

		JvmField field = mock(JvmField.class);
		when(field.getSimpleName()).thenReturn("SPECIFICATION_RELEASE_VERSION_STRING");
		when(field.getConstantValueAsString()).thenReturn("2.3.4");
		when(type.getDeclaredFields()).thenReturn(Arrays.asList(field));

		SarlLibraryErrorCode code = Utils.getSARLLibraryVersionOnClasspath(references, context, version);
		assertEquals(SarlLibraryErrorCode.SARL_FOUND, code);
		assertEquals("2.3.4", version.get());
	}

	/** Create the dynamic tests for {@code isCompatibleSARLLibraryOnClasspath} without defined {@code SARLVersion} type.
	 *
	 * @return the dynamic tests.
	 */
	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("isCompatibleSARLLibraryOnClasspath - no type")
	public List<DynamicTest> isCompatibleSARLLibraryOnClasspath_0() {
		final TypeReferences references = mock(TypeReferences.class);
		final Notifier context = mock(Notifier.class);

		JvmType type = mock(JvmType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);

		final Function2<String, String, Executable> falseTest = (label, version) -> {
			return () -> assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context), label);
		};

		return buildDynamicTestsOnVersions(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING,
				falseTest,
				falseTest, falseTest,
				falseTest, falseTest);
	}

	/** Create the dynamic tests for {@code isCompatibleSARLLibraryOnClasspath} without defined field in {@code SARLVersion} type.
	 *
	 * @return the dynamic tests.
	 */
	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("isCompatibleSARLLibraryOnClasspath - no field")
	public List<DynamicTest> isCompatibleSARLLibraryOnClasspath_1() {
		final TypeReferences references = mock(TypeReferences.class);
		final Notifier context = mock(Notifier.class);

		JvmDeclaredType type = mock(JvmDeclaredType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);
		
		JvmField field = mock(JvmField.class);
		when(field.getSimpleName()).thenReturn("X");
		when(type.getDeclaredFields()).thenReturn(Arrays.asList(field));

		final Function2<String, String, Executable> falseTest = (label, version) -> {
			return () -> assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context), label);
		};

		return buildDynamicTestsOnVersions(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING,
				falseTest,
				falseTest, falseTest,
				falseTest, falseTest);
	}

	/** Create the dynamic tests for {@code isCompatibleSARLLibraryOnClasspath} with invalid field value in {@code SARLVersion} type.
	 *
	 * @return the dynamic tests.
	 */
	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("isCompatibleSARLLibraryOnClasspath - no version value")
	public List<DynamicTest> isCompatibleSARLLibraryOnClasspath_2() {
		final TypeReferences references = mock(TypeReferences.class);
		final Notifier context = mock(Notifier.class);

		JvmDeclaredType type = mock(JvmDeclaredType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);

		JvmField field = mock(JvmField.class);
		when(field.getSimpleName()).thenReturn("SPECIFICATION_RELEASE_VERSION_STRING");
		when(field.getConstantValueAsString()).thenReturn("");
		when(type.getDeclaredFields()).thenReturn(Arrays.asList(field));
		
		final Function2<String, String, Executable> falseTest = (label, version) -> {
			return () -> assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context), label);
		};

		return buildDynamicTestsOnVersions(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING,
				falseTest,
				falseTest, falseTest,
				falseTest, falseTest);
	}

	/** Create the dynamic tests for {@code isCompatibleSARLLibraryOnClasspath} with {@code SARLVersion} type for the current version.
	 *
	 * @return the dynamic tests.
	 */
	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("isCompatibleSARLLibraryOnClasspath - current version")
	public List<DynamicTest> isCompatibleSARLLibraryOnClasspath_3() {
		final TypeReferences references = mock(TypeReferences.class);
		final Notifier context = mock(Notifier.class);

		JvmDeclaredType type = mock(JvmDeclaredType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);

		JvmField field = mock(JvmField.class);
		when(field.getSimpleName()).thenReturn("SPECIFICATION_RELEASE_VERSION_STRING");
		when(field.getConstantValueAsString()).thenReturn(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING);
		when(type.getDeclaredFields()).thenReturn(Arrays.asList(field));

		final Function2<String, String, Executable> trueTest = (label, version) -> {
			return () -> assertTrue(Utils.isCompatibleSARLLibraryOnClasspath(references, context), label);
		};
		
		return buildDynamicTestsOnVersions(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING,
				trueTest,
				trueTest, trueTest,
				trueTest, trueTest);
	}

	/** Create the dynamic tests for {@code isCompatibleSARLLibraryOnClasspath} with {@code SARLVersion} type for the old version.
	 *
	 * @return the dynamic tests.
	 */
	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("isCompatibleSARLLibraryOnClasspath - old version")
	public List<DynamicTest> isCompatibleSARLLibraryOnClasspath_4() {
		final TypeReferences references = mock(TypeReferences.class);
		final Notifier context = mock(Notifier.class);

		JvmDeclaredType type = mock(JvmDeclaredType.class);
		when(references.findDeclaredType(any(Class.class), any(Notifier.class))).thenReturn(type);

		JvmField field = mock(JvmField.class);
		when(field.getSimpleName()).thenReturn("SPECIFICATION_RELEASE_VERSION_STRING");
		when(field.getConstantValueAsString()).thenReturn("0.9.10");
		when(type.getDeclaredFields()).thenReturn(Arrays.asList(field));

		final Function2<String, String, Executable> falseTest = (label, version) -> {
			return () -> assertFalse(Utils.isCompatibleSARLLibraryOnClasspath(references, context), label);
		};
		
		return buildDynamicTestsOnVersions(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING,
				falseTest,
				falseTest, falseTest,
				falseTest, falseTest);
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("isCompatibleJDKVersionWithSARLCompilationEnvironment")
	public List<DynamicTest> isCompatibleJDKVersionWithSARLCompilationEnvironment() {
		final var maxVersion = Utils.parseVersion(SARLVersion.INCOMPATIBLE_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);

		final Function2<String, String, Executable> trueTest = (label, version) -> {
			return () -> assertTrue(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment(version.toString()), label);
		};

		final Function2<String, String, Executable> falseTest = (label, version) -> {
			return () -> assertFalse(Utils.isCompatibleJDKVersionWithSARLCompilationEnvironment(version.toString()), label);
		};

		final Function2<String, String, Executable> maxTest = (label, version) -> {
			final var cversion = Utils.parseVersion(version.toString());
			if (cversion == null || cversion.compareTo(maxVersion) >= 0) {
				return falseTest.apply(label, version);
			}
			return trueTest.apply(label, version);
		};

		return buildDynamicTestsOnVersions(SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH,
				trueTest,
				falseTest, trueTest,
				falseTest, maxTest);
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("isCompatibleJDKVersionWhenInSARLProjectClasspath")
	public List<DynamicTest> isCompatibleJDKVersionWhenInSARLProjectClasspath() {
		final var maxVersion = Utils.parseVersion(SARLVersion.INCOMPATIBLE_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH);

		final Function2<String, String, Executable> trueTest = (label, version) -> {
			return () -> assertTrue(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath(version.toString()), label);
		};

		final Function2<String, String, Executable> falseTest = (label, version) -> {
			return () -> assertFalse(Utils.isCompatibleJDKVersionWhenInSARLProjectClasspath(version.toString()), label);
		};

		final Function2<String, String, Executable> maxTest = (label, version) -> {
			final var cversion = Utils.parseVersion(version.toString());
			if (cversion == null || cversion.compareTo(maxVersion) >= 0) {
				return falseTest.apply(label, version);
			}
			return trueTest.apply(label, version);
		};

		return buildDynamicTestsOnVersions(SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH,
				trueTest,
				falseTest, trueTest,
				falseTest, maxTest);
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("isCompatibleXtextVersion")
	public List<DynamicTest> isCompatibleXtextVersion() {
		final Function2<String, String, Executable> trueTest = (label, version) -> {
			return () -> assertTrue(Utils.isCompatibleXtextVersion(version.toString()), label);
		};

		final Function2<String, String, Executable> falseTest = (label, version) -> {
			return () -> assertFalse(Utils.isCompatibleXtextVersion(version.toString()), label);
		};

		return buildDynamicTestsOnVersions(SARLVersion.MINIMAL_XTEXT_VERSION,
				trueTest,
				falseTest, trueTest,
				falseTest, trueTest);
	}

	@Test
	@DisplayName("isSARLAnnotation(Test.class)")
	public void isSARLAnnotation_Class_0() {
		assertFalse(Utils.isSARLAnnotation(Test.class));
	}

	@Test
	@DisplayName("isSARLAnnotation(SarlSpecification.class)")
	public void isSARLAnnotation_Class_1() {
		assertTrue(Utils.isSARLAnnotation(SarlSpecification.class));
	}

	@Test
	@DisplayName("isSARLAnnotation(DefaultValue.class)")
	public void isSARLAnnotation_Class_2() {
		assertTrue(Utils.isSARLAnnotation(DefaultValue.class));
	}

	@Test
	@DisplayName("isSARLAnnotation(DefaultValueSource.class)")
	public void isSARLAnnotation_Class_3() {
		assertTrue(Utils.isSARLAnnotation(DefaultValueSource.class));
	}

	@Test
	@DisplayName("isSARLAnnotation(DefaultValueUse.class)")
	public void isSARLAnnotation_Class_4() {
		assertTrue(Utils.isSARLAnnotation(DefaultValueUse.class));
	}

	@Test
	@DisplayName("isSARLAnnotation(EarlyExit.class)")
	public void isSARLAnnotation_Class_5() {
		assertTrue(Utils.isSARLAnnotation(EarlyExit.class));
	}

	@Test
	@DisplayName("isSARLAnnotation(PrivateAPI.class)")
	public void isSARLAnnotation_Class_6() {
		assertTrue(Utils.isSARLAnnotation(PrivateAPI.class));
	}

	@Test
	@DisplayName("isSARLAnnotation(\"Test\")")
	public void isSARLAnnotation_String_0() {
		assertFalse(Utils.isSARLAnnotation(Test.class.getName()));
	}

	@Test
	@DisplayName("isSARLAnnotation(\"SarlSpecification\")")
	public void isSARLAnnotation_String_1() {
		assertTrue(Utils.isSARLAnnotation(SarlSpecification.class.getName()));
	}

	@Test
	@DisplayName("isSARLAnnotation(\"DefaultValue\")")
	public void isSARLAnnotation_String_2() {
		assertTrue(Utils.isSARLAnnotation(DefaultValue.class.getName()));
	}

	@Test
	@DisplayName("isSARLAnnotation(\"DefaultValueSource\")")
	public void isSARLAnnotation_String_3() {
		assertTrue(Utils.isSARLAnnotation(DefaultValueSource.class.getName()));
	}

	@Test
	@DisplayName("isSARLAnnotation(\"DefaultValueUse\")")
	public void isSARLAnnotation_String_4() {
		assertTrue(Utils.isSARLAnnotation(DefaultValueUse.class.getName()));
	}

	@Test
	@DisplayName("isSARLAnnotation(\"EarlyExit\")")
	public void isSARLAnnotation_String_5() {
		assertTrue(Utils.isSARLAnnotation(EarlyExit.class.getName()));
	}

	@Test
	@DisplayName("isSARLAnnotation(\"PrivateAPI\")")
	public void isSARLAnnotation_String_6() {
		assertTrue(Utils.isSARLAnnotation(PrivateAPI.class.getName()));
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class NotFinalClassT {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static final class FinalClassT {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static interface InterfaceT {
		//
	}

}

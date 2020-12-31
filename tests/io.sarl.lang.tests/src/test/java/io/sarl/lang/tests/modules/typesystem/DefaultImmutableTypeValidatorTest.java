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
package io.sarl.lang.tests.modules.typesystem;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import javax.inject.Inject;

import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.Procedures;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.typesystem.DefaultImmutableTypeValidator;
import io.sarl.lang.util.Utils;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

/** This class tests the {@link DefaultImmutableTypeValidator} for SARL.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@SuppressWarnings("all")
@DisplayName("DefaultImmutableTypeValidator")
@Tag("core")
@Tag("unit")
public class DefaultImmutableTypeValidatorTest extends AbstractSarlTest {

	@Nullable
	private DefaultImmutableTypeValidator validator;

	@Nullable
	private Notifier context;

	@Inject
	private AnnotationLookup annotationFinder;

	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private TypeReferences typeReferences;

	private boolean isImmutable(Class<?> type) throws Exception {
		final JvmType jvmtype = this.typeReferences.findDeclaredType(type, this.context);
		assertNotNull(jvmtype);
		final LightweightTypeReference ref = Utils.toLightweightTypeReference(jvmtype, this.services);
		assertNotNull(ref);
		return this.validator.isImmutable(ref);
	}

	@BeforeEach
	public void setUp() throws Exception {
		final SarlScript script = getParseHelper().parse("agent XXX { }");
		assertNotNull(script);
		this.context = script;
		this.validator = new DefaultImmutableTypeValidator();
		this.validator.setAnnotationLookup(this.annotationFinder);
	}
	
	@ParameterizedTest
	@MethodSource
	@DisplayName("Declared immutable types")
	public void isImmutable_declaredImmutableTypes(Class<?> type) throws Exception {
		assertTrue(isImmutable(type));
	}

	private static List<Class<?>> isImmutable_declaredImmutableTypes() {
		return Arrays.asList(DefaultImmutableTypeValidator.IMMUTABLE_TYPES);
	}

	@ParameterizedTest
	@ValueSource(classes = {Object.class, BigInteger.class, AtomicInteger.class})
	@DisplayName("Mutable types")
	public void isImmutable_Object(Class<?> type) throws Exception {
		assertFalse(isImmutable(type));
	}

	@ParameterizedTest
	@ValueSource(classes = {byte.class, short.class, int.class, long.class, float.class, double.class, boolean.class, char.class})
	@DisplayName("Primitive types")
	public void isImmutable_primitive(Class<?> type) throws Exception {
		assertTrue(isImmutable(type));
	}

	@ParameterizedTest
	@ValueSource(classes = {void.class, Void.class})
	@DisplayName("Void types")
	public void isImmutable_void(Class<?> type) throws Exception {
		assertTrue(isImmutable(type));
	}

	@ParameterizedTest
	@MethodSource
	@DisplayName("Procedures")
	public void isImmutable_procedure(Class<?> type) throws Exception {
		assertFalse(isImmutable(type));
	}

	private static List<Class<?>> isImmutable_procedure() {
		return Arrays.asList(Procedures.class.getDeclaredClasses());
	}

	@ParameterizedTest
	@MethodSource
	@DisplayName("Functions")
	public void isImmutable_function(Class<?> type) throws Exception {
		assertFalse(isImmutable(type));
	}

	private static List<Class<?>> isImmutable_function() {
		return Arrays.asList(Functions.class.getDeclaredClasses());
	}

	@Test
	@DisplayName("Mutable user type")
	public void isImmutable_MutableUserType() throws Exception {
		assertFalse(isImmutable(MutableUserType.class));
	}

	@Test
	@DisplayName("Immutable user type")
	public void isImmutable_ImmutableUserType() throws Exception {
		assertTrue(isImmutable(ImmutableUserType.class));
	}

	private class MutableUserType {
		//
	}

	@Data
	private class ImmutableUserType {
		//
	}

}

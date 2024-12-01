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
package io.sarl.lang.tests.modules.typesystem;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmUpperBound;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.typesystem.conformance.RawTypeConformanceComputer;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.TestFactory;
import org.opentest4j.TestAbortedException;

import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.typesystem.DefaultSARLTypeChecker;
import io.sarl.lang.util.Utils;

/** This class tests the {@link SarlUtils} for SARL.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
@SuppressWarnings("all")
@DisplayName("DefaultSARLTypeChecker")
@Tag("core")
@Tag("unit")
public class DefaultSARLTypeCheckerTest extends AbstractSarlTest {

	private static final boolean MANUAL_DEBUG = true;
	
	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private DefaultSARLTypeChecker test;

	private enum TypeArgumentConformanceSourceType {
		/** {@code Object}
		 */
		OBJECT {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return Object.class;
			}
		},
		/** {@code Number}
		 */
		NUMBER {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return Number.class;
			}
		},
		/** {@code Double}
		 */
		DOUBLE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return Double.class;
			}
		},
		/** {@code String}
		 */
		STRING {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return String.class;
			}
		},
		/** {@code GenericType}
		 */
		GENERICTYPE_RAW {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return GenericType.class;
			}
		},
		/** {@code GenericType<?>}
		 */
		GENERICTYPE_ANY {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class);
			}
		},
		/** {@code GenericType<Object>}
		 */
		GENERICTYPE_OBJECT {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, Object.class);
			}
		},
		/** {@code GenericType<Number>}
		 */
		GENERICTYPE_NUMBER {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, Number.class);
			}
		},
		/** {@code GenericType<Double>}
		 */
		GENERICTYPE_DOUBLE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, Double.class);
			}
		},
		/** {@code GenericType<String>}
		 */
		GENERICTYPE_STRING {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, String.class);
			}
		},
		/** {@code ? extends GenericType}
		 */
		ANY_GENERICTYPE_RAW {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockWildcardTypeReference(context, GenericType.class);
			}
		},
		/** {@code ? extends GenericType<Object>}
		 */
		ANY_GENERICTYPE_OBJECT {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockWildcardTypeReference(context, GenericType.class, Object.class);
			}
		},
		/** {@code ? extends GenericType<Number>}
		 */
		ANY_GENERICTYPE_NUMBER {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockWildcardTypeReference(context, GenericType.class, Number.class);
			}
		},
		/** {@code ? extends GenericType<Double>}
		 */
		ANY_GENERICTYPE_DOUBLE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockWildcardTypeReference(context, GenericType.class, Double.class);
			}
		},
		/** {@code ? extends GenericType<String>}
		 */
		ANY_GENERICTYPE_STRING {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockWildcardTypeReference(context, GenericType.class, String.class);
			}
		},
		/** {@code GenericType<GenericType>}
		 */
		GENERICTYPE_GENERICTYPE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockWildcardTypeReference(context, GenericType.class, GenericType.class);
			}
		},
		/** {@code GenericType<? extends GenericType>}
		 */
		GENERICTYPE_ANY_GENERICTYPE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, caller.mockWildcardTypeReference(context, GenericType.class));
			}
		},
		/** {@code GenericType<GenericType<Object>>}
		 */
		GENERICTYPE_GENERICTYPE_OBJECT {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, caller.mockParameterizedTypeReference(context, GenericType.class, Object.class));
			}
		},
		/** {@code GenericType<GenericType<Number>>}
		 */
		GENERICTYPE_GENERICTYPE_NUMBER {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, caller.mockParameterizedTypeReference(context, GenericType.class, Number.class));
			}
		},
		/** {@code GenericType<GenericType<Double>>}
		 */
		GENERICTYPE_GENERICTYPE_DOUBLE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, caller.mockParameterizedTypeReference(context, GenericType.class, Double.class));
			}
		},
		/** {@code GenericType<GenericType<String>>}
		 */
		GENERICTYPE_GENERICTYPE_STRING {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, caller.mockParameterizedTypeReference(context, GenericType.class, String.class));
			}
		},
		/** {@code GenericType<GenericType<? extends Object>>}
		 */
		GENERICTYPE_GENERICTYPE_ANY_OBJECT {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, caller.mockWildcardTypeReference(context, GenericType.class, Object.class));
			}
		},
		/** {@code GenericType<GenericType<? extends Number>>}
		 */
		GENERICTYPE_GENERICTYPE_ANY_NUMBER {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, caller.mockWildcardTypeReference(context, GenericType.class, Number.class));
			}
		},
		/** {@code GenericType<GenericType<? extends Double>>}
		 */
		GENERICTYPE_GENERICTYPE_ANY_DOUBLE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, caller.mockWildcardTypeReference(context, GenericType.class, Double.class));
			}
		},
		/** {@code GenericType<GenericType<? extends String>>}
		 */
		GENERICTYPE_GENERICTYPE_ANY_STRING {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, GenericType.class, caller.mockWildcardTypeReference(context, GenericType.class, String.class));
			}
		},
		/** {@code LoopType}
		 */
		LOOPTYPE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class);
			}
		},
		/** {@code SubLoopType}
		 */
		SUBLOOPTYPE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubLoopType.class);
			}
		},
		/** {@code LoopType<SubLoopType>}
		 */
		LOOPTYPE_SUBLOOPTYPE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, SubLoopType.class);
			}
		},
		/** {@code LoopType<? extends SubLoopType>}
		 */
		LOOPTYPE_ANY_SUBLOOPTYPE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, caller.mockWildcardTypeReference(context, SubLoopType.class));
			}
		},
		/** {@code SubSubLoopType0}
		 */
		SUBSUBLOOPTYPE0 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubSubLoopType0.class);
			}
		},
		/** {@code SubSubLoopType1}
		 */
		SUBSUBLOOPTYPE1 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubSubLoopType1.class);
			}
		},
		/** {@code LoopType<SubType>}
		 */
		LOOTYPE_SUBTYPE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, SubType.class);
			}
		},
		/** {@code LoopType<? extends SubType>>}
		 */
		LOOPTYPE_ANY_SUBTYPE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, caller.mockWildcardTypeReference(context, SubType.class));
			}
		},
		/** {@code LoopType<SubType<?>>}
		 */
		LOOTYPE_SUBTYPE_ANY {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, caller.mockParameterizedTypeReference(context, SubType.class));
			}
		},
		/** {@code LoopType<? extends SubType<?>>}
		 */
		LOOPTYPE_ANY_SUBTYPE_ANY {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, caller.mockWildcardTypeReference(context, SubType.class));
			}
		},
		/** {@code LoopType<SubSubType0>}
		 */
		LOOTYPE_SUBSUBTYPE0 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, SubSubType0.class);
			}
		},
		/** {@code LoopType<? extends SubSubType0>>}
		 */
		LOOPTYPE_ANY_SUBSUBTYPE0 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, caller.mockWildcardTypeReference(context, SubSubType0.class));
			}
		},
		/** {@code LoopType<SubSubType1>}
		 */
		LOOTYPE_SUBSUBTYPE1 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, SubSubType1.class);
			}
		},
		/** {@code LoopType<? extends SubSubType1>>}
		 */
		LOOPTYPE_ANY_SUBSUBTYPE1 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, caller.mockWildcardTypeReference(context, SubSubType1.class));
			}
		},
		/** {@code SubType}
		 */
		SUBTYPE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubType.class);
			}
		},
		/** {@code SubType<SubType>>}
		 */
		SUBTYPE_SUBTYPE {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubType.class, SubType.class);
			}
		},
		/** {@code SubType<SubType<?>>}
		 */
		SUBTYPE_SUBTYPE_ANY {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubType.class, caller.mockParameterizedTypeReference(context, SubType.class));
			}
		},
		/** {@code SubType<? extends SubType<?>>}
		 */
		SUBTYPE_ANY_SUBTYPE_ANY {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubType.class, caller.mockWildcardTypeReference(context, SubType.class));
			}
		},
		/** {@code SubType<SubSubType0>}
		 */
		SUBTYPE_SUBSUBTYPE0 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubType.class, SubSubType0.class);
			}
		},
		/** {@code SubType<? extends SubSubType0>>}
		 */
		SUBTYPE_ANY_SUBSUBTYPE0 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubType.class, caller.mockWildcardTypeReference(context, SubSubType0.class));
			}
		},
		/** {@code SubType<SubSubType1>}
		 */
		SUBTYPE_SUBSUBTYPE1 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubType.class, SubSubType1.class);
			}
		},
		/** {@code SubType<? extends SubSubType1>>}
		 */
		SUBTYPE_ANY_SUBSUBTYPE1 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, LoopType.class, caller.mockWildcardTypeReference(context, SubSubType1.class));
			}
		},
		/** {@code SubSubType0}
		 */
		SUBSUBTYPE0 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubSubType0.class);
			}
		},
		/** {@code SubSubType1}
		 */
		SUBSUBTYPE1 {
			@Override
			public Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context) {
				return caller.mockParameterizedTypeReference(context, SubSubType1.class);
			}
		};

		public abstract Object getTypeReference(DefaultSARLTypeCheckerTest caller, EObject context);
		
	}

	private JvmTypeParameter mockTypeParameter(EObject context, String name, Class<?> type) {
		final var typeRef = this.services.getTypeReferences().getTypeForName(type, context);

		final JvmTypeParameter typeParameter = this.services.getTypesFactory().createJvmTypeParameter();
		typeParameter.setName(name);
		
		final var constraint = this.services.getTypesFactory().createJvmUpperBound();		
		constraint.setTypeReference(typeRef);
		typeParameter.getConstraints().add(constraint);

		return typeParameter;
	}

	private JvmTypeParameter mockTypeParameter(EObject context, String name, LightweightTypeReference type) {
		final JvmTypeParameter typeParameter = this.services.getTypesFactory().createJvmTypeParameter();
		typeParameter.setName(name);
		
		final var constraint = this.services.getTypesFactory().createJvmUpperBound();		
		constraint.setTypeReference(type.toJavaCompliantTypeReference());
		typeParameter.getConstraints().add(constraint);

		return typeParameter;
	}

	private LightweightTypeReference mockParameterizedTypeReference(EObject context, Class<?> type, LightweightTypeReference typeParameter) {
		final var typeRef = this.services.getTypeReferences().getTypeForName(type, context, typeParameter.toJavaCompliantTypeReference());
		return Utils.toLightweightTypeReference(typeRef, this.services, context.eResource().getResourceSet());
	}

	private LightweightTypeReference mockParameterizedTypeReference(EObject context, Class<?> type, Class<?>... typeParameter) {
		final var references = this.services.getTypeReferences();
		final var tab = new JvmTypeReference[typeParameter.length];
		for (var i = 0; i < typeParameter.length; ++i) {
			tab[i] = references.getTypeForName(typeParameter[i], context);
		}
		final var typeRef = references.getTypeForName(type, context, tab);
		return Utils.toLightweightTypeReference(typeRef, this.services, context.eResource().getResourceSet());
	}

	private LightweightTypeReference mockWildcardTypeReference(EObject context, Class<?> type, Class<?>... typeParameter) {
		final var references = this.services.getTypeReferences();
		final var tab = new JvmTypeReference[typeParameter.length];
		for (var i = 0; i < typeParameter.length; ++i) {
			tab[i] = references.getTypeForName(typeParameter[i], context);
		}
		final var typeRef = references.getTypeForName(type, context, tab);
		final var factory = this.services.getTypesFactory();
		final var ub = factory.createJvmUpperBound();
		ub.setTypeReference(typeRef);
		final var wc = factory.createJvmWildcardTypeReference();
		wc.getConstraints().add(ub);
		return Utils.toLightweightTypeReference(wc, this.services, context.eResource().getResourceSet());
	}
	
	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("isTypeArgumentConformant")
	private List<DynamicTest> buildTypeArgumentConformanceTests(EObject context, JvmTypeParameter parameter, Function1<TypeArgumentConformanceSourceType, Boolean> argumentsAndResults) {
		final List<DynamicTest> tests = new ArrayList<>();
		final var postfixLabel = new StringBuilder("> to <").append(parameter.getName());
		for (final var constraint : parameter.getConstraints()) {
			if (constraint instanceof JvmUpperBound) {
				postfixLabel.append(" extends ");
			} else {
				postfixLabel.append(" super ");
			}
			postfixLabel.append(Utils.toLightweightTypeReference(constraint.getTypeReference(), this.services, context.eResource().getResourceSet()).getHumanReadableName());
		}
		postfixLabel.append(">");
		for (final var argType0 : TypeArgumentConformanceSourceType.values()) {
			final var expectedResult = argumentsAndResults.apply(argType0);
			buildTypeArgumentConformantTests0(tests, context, parameter, postfixLabel, expectedResult, false, false, argType0);
//TODO			buildTypeArgumentConformantTests0(tests, context, parameter, postfixLabel, expectedResult, true, false, argType0);
//			buildTypeArgumentConformantTests0(tests, context, parameter, postfixLabel, expectedResult, false, true, argType0);
		}
		return tests;
	}

	private void buildTypeArgumentConformantTests0(List<DynamicTest> tests, EObject context, JvmTypeParameter parameter,
			CharSequence postfixLabel, Boolean expectedSuccessObject, boolean appendWildcard, boolean appendGenericName, TypeArgumentConformanceSourceType argType) {
		final var rs = context.eResource().getResourceSet();
		LightweightTypeReference argument;
		final var argType0 = argType.getTypeReference(this, context);
		if (argType0 instanceof Class ctype) {
			argument = Utils.toLightweightTypeReference(this.services.getTypeReferences().getTypeForName(ctype, context), this.services, rs);
		} else if (argType0 instanceof LightweightTypeReference creference) {
			argument = creference;
		} else {
			throw new IllegalArgumentException(argType0 == null ? "null" : argType0.getClass().toString());
		}
		if (appendWildcard && !argument.isType(Object.class)) {
			assert !appendGenericName;
			try {
				final var factory = this.services.getTypesFactory();
				final var ub = factory.createJvmUpperBound();
				ub.setTypeReference(argument.toJavaCompliantTypeReference());
				final var wc = factory.createJvmWildcardTypeReference();
				wc.getConstraints().add(ub);
				argument = Utils.toLightweightTypeReference(wc, this.services, rs);
			} catch (Throwable ex) {
				return;
			}
		}
		if (appendGenericName) {
			assert !appendWildcard;
			try {
				final var factory = this.services.getTypesFactory();
				final var ub = factory.createJvmUpperBound();
				ub.setTypeReference(argument.toJavaCompliantTypeReference());
				final var tp = factory.createJvmTypeParameter();
				tp.setName("S");
				tp.getConstraints().add(ub);
				final var rf = this.services.getTypeReferences().createTypeRef(tp);
				argument = Utils.toLightweightTypeReference(rf, this.services, rs);
			} catch (Throwable ex) {
				return;
			}
		}
		final List<LightweightTypeReference> arguments = Collections.singletonList(argument);
		final var label = new StringBuilder("<").append(argument.getHumanReadableName()).append(postfixLabel).toString();
		final var parameters = Collections.singletonList(parameter);
		final var owner = new StandardTypeReferenceOwner(this.services, context);
		final DynamicTest test;
		if (expectedSuccessObject == null) {
			test = DynamicTest.dynamicTest(label, () -> {
				if (MANUAL_DEBUG) {
					throw new TestAbortedException("Unimplemented argtype: " + argType);
				}
				throw new IllegalStateException("Unimplemented argtype: " + argType);
			});
		} else {
			final var expectedSuccess = expectedSuccessObject.booleanValue();
			test = DynamicTest.dynamicTest(label, () -> {
				final var conf = this.test.getTypeArgumentConformance(arguments, parameters, owner);
				final var actualSuccess = (conf & RawTypeConformanceComputer.SUCCESS) != 0;
				if (expectedSuccess != actualSuccess) {
					fail(generateMessageWithConformance(label, conf).append(" - ").append(argType.name()).toString());
				}
			});
		}
		tests.add(test);
	}
	
	private static StringBuilder generateMessageWithConformance(String label, int conformance) {
		final var buffer = new StringBuilder();
		buffer.append(label).append("; Conformance flags: ");
		if ((conformance & RawTypeConformanceComputer.SUCCESS) != 0) {
			buffer.append(" SUCCESS");
		}
		if ((conformance & RawTypeConformanceComputer.AS_TYPE_ARGUMENT) != 0) {
			buffer.append(" AS_TYPE_ARGUMENT");
		}
		if ((conformance & RawTypeConformanceComputer.AS_NESTED_TYPE_ARGUMENT) != 0) {
			buffer.append(" AS_NESTED_TYPE_ARGUMENT");
		}
		if ((conformance & RawTypeConformanceComputer.INCOMPATIBLE) != 0) {
			buffer.append(" INCOMPATIBLE");
		}
		if ((conformance & RawTypeConformanceComputer.DEMAND_CONVERSION) != 0) {
			buffer.append(" DEMAND_CONVERSION");
		}
		if ((conformance & RawTypeConformanceComputer.RAW_TYPE_CONVERSION) != 0) {
			buffer.append(" RAW_TYPE_CONVERSION");
		}
		if ((conformance & RawTypeConformanceComputer.RAW_TYPE) != 0) {
			buffer.append(" RAW_TYPE");
		}
		if ((conformance & RawTypeConformanceComputer.SUBTYPE) != 0) {
			buffer.append(" SUBTYPE");
		}
		if ((conformance & RawTypeConformanceComputer.UNBOXING) != 0) {
			buffer.append(" UNBOXING");
		}
		if ((conformance & RawTypeConformanceComputer.BOXING) != 0) {
			buffer.append(" BOXING");
		}
		return buffer;
	}

	private EObject createTypeArgumentConformanceCodeOwner() throws Exception {
		SarlScript mas = file(getParseHelper(), null, "package io.sarl.lang.utilstest; class XTestX {}");
		return mas.getXtendTypes().get(0);
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends Object")
	public List<DynamicTest> getTypeArgumentConformance_0() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends Object
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", Object.class), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return true;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends Number")
	public List<DynamicTest> getTypeArgumentConformance_1() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends Number
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", Number.class), it -> {
			switch (it) {
			case OBJECT:
				return false;
			case NUMBER:
			case DOUBLE:
				return true;
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends Double")
	public List<DynamicTest> getTypeArgumentConformance_2() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends Double
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", Double.class), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
				return false;
			case DOUBLE:
				return true;
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends String")
	public List<DynamicTest> getTypeArgumentConformance_3() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends String
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", String.class), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
				return false;
			case STRING:
				return true;
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends GenericType")
	public List<DynamicTest> getTypeArgumentConformance_4() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends GenericType
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", GenericType.class), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
				return false;
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
				return true;
			case GENERICTYPE_OBJECT:
				return false;
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
				return true;
			case GENERICTYPE_STRING:
				return false;
			case ANY_GENERICTYPE_RAW:
				return true;
			case ANY_GENERICTYPE_OBJECT:
				return false;
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
				return true;
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends GenericType<?>")
	public List<DynamicTest> getTypeArgumentConformance_5() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends GenericType<?>
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", mockParameterizedTypeReference(context, GenericType.class)), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
				return false;
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
				return true;
			case GENERICTYPE_OBJECT:
				return false;
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
				return true;
			case GENERICTYPE_STRING:
				return false;
			case ANY_GENERICTYPE_RAW:
				return true;
			case ANY_GENERICTYPE_OBJECT:
				return false;
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
				return true;
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends GenericType<Number>")
	public List<DynamicTest> getTypeArgumentConformance_6() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends GenericType<Number>
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", mockParameterizedTypeReference(context, GenericType.class, Number.class)), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
				return false;
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
				return true;
			case GENERICTYPE_OBJECT:
				return false;
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
				return true;
			case GENERICTYPE_STRING:
				return false;
			case ANY_GENERICTYPE_RAW:
				return true;
			case ANY_GENERICTYPE_OBJECT:
				return false;
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
				return true;
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends GenericType<Double>")
	public List<DynamicTest> getTypeArgumentConformance_7() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends GenericType<Double>
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", mockParameterizedTypeReference(context, GenericType.class, Double.class)), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
				return false;
			case GENERICTYPE_DOUBLE:
				return true;
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
				return false;
			case ANY_GENERICTYPE_DOUBLE:
				return true;
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends GenericType<String>")
	public List<DynamicTest> getTypeArgumentConformance_8() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends GenericType<String>
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", mockParameterizedTypeReference(context, GenericType.class, String.class)), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
				return false;
			case GENERICTYPE_STRING:
				return true;
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
				return false;
			case ANY_GENERICTYPE_STRING:
				return true;
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends LoopType")
	public List<DynamicTest> getTypeArgumentConformance_9() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends LoopType
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", LoopType.class), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
				return false;
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return true;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends LoopType<?>")
	public List<DynamicTest> getTypeArgumentConformance_10() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends LoopType<?>
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", mockParameterizedTypeReference(context, LoopType.class)), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
				return false;
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return true;
			default:
			}
			return null;
		});
	}
	
	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends LoopType<? extends LoopType>")
	public List<DynamicTest> getTypeArgumentConformance_11() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends LoopType<? extends LoopType>
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", 
				mockParameterizedTypeReference(context, LoopType.class, mockWildcardTypeReference(context, LoopType.class))),
				it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
				return false;
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return true;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends LoopType<SubLoopType>")
	public List<DynamicTest> getTypeArgumentConformance_12() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends LoopType<SubLoopType>
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", mockParameterizedTypeReference(context, LoopType.class, SubLoopType.class)), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
				return false;
			case SUBLOOPTYPE:
				return true;
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
				return true;
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends SubLoopType")
	public List<DynamicTest> getTypeArgumentConformance_13() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends SubLoopType
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", mockParameterizedTypeReference(context, SubLoopType.class)), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
				return false;
			case SUBLOOPTYPE:
				return true;
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
				return false;
			case SUBSUBLOOPTYPE0:
			case SUBSUBLOOPTYPE1:
				return true;
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends SubSubLoopType0")
	public List<DynamicTest> getTypeArgumentConformance_14() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends SubSubLoopType0
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", mockParameterizedTypeReference(context, SubSubLoopType0.class)), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
				return false;
			case SUBSUBLOOPTYPE0:
				return true;
			case SUBSUBLOOPTYPE1:
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	@SuppressWarnings("static-method")
	@TestFactory
	@DisplayName("getTypeArgumentConformance - TS extends SubSubLoopType1")
	public List<DynamicTest> getTypeArgumentConformance_15() throws Exception {
		final var context = createTypeArgumentConformanceCodeOwner();
		// TS extends SubSubLoopType1
		return buildTypeArgumentConformanceTests(context, mockTypeParameter(context, "TS", mockParameterizedTypeReference(context, SubSubLoopType1.class)), it -> {
			switch (it) {
			case OBJECT:
			case NUMBER:
			case DOUBLE:
			case STRING:
			case GENERICTYPE_RAW:
			case GENERICTYPE_ANY:
			case GENERICTYPE_OBJECT:
			case GENERICTYPE_NUMBER:
			case GENERICTYPE_DOUBLE:
			case GENERICTYPE_STRING:
			case ANY_GENERICTYPE_RAW:
			case ANY_GENERICTYPE_OBJECT:
			case ANY_GENERICTYPE_NUMBER:
			case ANY_GENERICTYPE_DOUBLE:
			case ANY_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE:
			case GENERICTYPE_ANY_GENERICTYPE:
			case GENERICTYPE_GENERICTYPE_OBJECT:
			case GENERICTYPE_GENERICTYPE_NUMBER:
			case GENERICTYPE_GENERICTYPE_DOUBLE:
			case GENERICTYPE_GENERICTYPE_STRING:
			case GENERICTYPE_GENERICTYPE_ANY_OBJECT:
			case GENERICTYPE_GENERICTYPE_ANY_NUMBER:
			case GENERICTYPE_GENERICTYPE_ANY_DOUBLE:
			case GENERICTYPE_GENERICTYPE_ANY_STRING:
			case LOOPTYPE:
			case SUBLOOPTYPE:
			case LOOPTYPE_SUBLOOPTYPE:
			case LOOPTYPE_ANY_SUBLOOPTYPE:
			case SUBSUBLOOPTYPE0:
				return false;
			case SUBSUBLOOPTYPE1:
				return true;
			case LOOTYPE_SUBTYPE:
			case LOOPTYPE_ANY_SUBTYPE:
			case LOOTYPE_SUBTYPE_ANY:
			case LOOPTYPE_ANY_SUBTYPE_ANY:
			case LOOTYPE_SUBSUBTYPE0:
			case LOOPTYPE_ANY_SUBSUBTYPE0:
			case LOOTYPE_SUBSUBTYPE1:
			case LOOPTYPE_ANY_SUBSUBTYPE1:
			case SUBTYPE:
			case SUBTYPE_SUBTYPE:
			case SUBTYPE_SUBTYPE_ANY:
			case SUBTYPE_ANY_SUBTYPE_ANY:
			case SUBTYPE_SUBSUBTYPE0:
			case SUBTYPE_ANY_SUBSUBTYPE0:
			case SUBTYPE_SUBSUBTYPE1:
			case SUBTYPE_ANY_SUBSUBTYPE1:
			case SUBSUBTYPE0:
			case SUBSUBTYPE1:
				return false;
			default:
			}
			return null;
		});
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private static class GenericType<X extends Number> {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private static class LoopType<X extends LoopType<X>> {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private static class SubLoopType extends LoopType<SubLoopType> {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private static class SubType<T extends SubType<T>> extends LoopType<T> {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private static class SubSubLoopType0 extends SubLoopType {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private static class SubSubLoopType1 extends SubLoopType {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private static class SubSubType0 extends SubType<SubSubType0> {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	private static class SubSubType1 extends SubType<SubSubType1> {
		//
	}

}

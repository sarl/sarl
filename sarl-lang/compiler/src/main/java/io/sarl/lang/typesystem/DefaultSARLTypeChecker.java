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

package io.sarl.lang.typesystem;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.google.inject.Singleton;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeParameterDeclarator;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.typesystem.conformance.RawTypeConformanceComputer;
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputationArgument;
import org.eclipse.xtext.xbase.typesystem.conformance.TypeConformanceComputer;
import org.eclipse.xtext.xbase.typesystem.references.ITypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.WildcardTypeReference;

/**
 * Default implementation of the SARL type checker that provides extended methods for checking
 * types compared to the standard Xtext type checker. 
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.15
 */
@Singleton
public class DefaultSARLTypeChecker implements ISARLTypeChecker {

	private static final TypeConformanceComputationArgument CONFORMANCE_OPTIONS = new TypeConformanceComputationArgument(false, false, true, true, false, true);

	private static final TypeConformanceComputationArgument RAW_CONFORMANCE_OPTIONS = new TypeConformanceComputationArgument(true, false, true, true, false, true);

	@Override
	public int getTypeArgumentConformance(List<LightweightTypeReference> typeArguments,
			List<JvmTypeParameter> typeParameters, ITypeReferenceOwner referenceOwner) {
		final var maxArgumentSize = Math.min(typeArguments.size(), typeParameters.size());
		// Special case where there is nothing to test
		if (maxArgumentSize == 0) {
			return typeArguments.size() == typeParameters.size() ? RawTypeConformanceComputer.SUCCESS : RawTypeConformanceComputer.INCOMPATIBLE;
		}
		final var conformanceComputer = referenceOwner.getServices().getTypeConformanceComputer();
		return getTypeArgumentConformance(
				typeParameters, it -> referenceOwner.newParameterizedTypeReference(it),
				typeArguments,
				referenceOwner, conformanceComputer, new TreeSet<>((a, b) -> {
					if (a == b) {
						return 0;
					}
					if (a == null) {
						return -1;
					}
					if (b == null) {
						return 1;
					}
					var cmp = a.getKey().compareTo(b.getKey());
					if (cmp != 0) {
						return cmp;
					}
					return a.getValue().compareTo(b.getValue());
				}));
	}

	private static <T> int getTypeArgumentConformance(
			List<T> leftTypes, Function1<T, LightweightTypeReference> mapper,
			List<LightweightTypeReference> rightTypes,
			ITypeReferenceOwner referenceOwner,
			TypeConformanceComputer conformanceComputer,
			Set<Pair<String, String>> analyzedTypes) {
		final var maxTypesSize = Math.min(leftTypes.size(), rightTypes.size());
		// Check type conformance of types
		var conformance = RawTypeConformanceComputer.SUCCESS;
		for (var i = 0; i < maxTypesSize; ++i) {
			final var left = mapper.apply(leftTypes.get(i));
			final var right = rightTypes.get(i);
			if (left.getType() != right.getType()) {
				conformance = getTypeArgumentConformance(left, right, referenceOwner, conformanceComputer, analyzedTypes);
				if ((conformance & RawTypeConformanceComputer.SUCCESS) == 0) {
					return conformance;
				}
			}
		}
		return conformance;
	}

	private static int getTypeArgumentConformance(LightweightTypeReference left, LightweightTypeReference right,
			ITypeReferenceOwner referenceOwner, TypeConformanceComputer conformanceComputer,
			Set<Pair<String, String>> analyzedTypes) {
		final var left1 = substitute(left, referenceOwner);
		final var right1 = substitute(right, referenceOwner);
		if (isParameterizedType(left1)) {
			// The left type is parameterized, so that, we have to change the type parameters and type arguments conformance
			final var rawLeft = left1.getRawTypeReference();
			final var rawRight = right1.getRawTypeReference();
			var conformance = conformanceComputer.isConformant(rawLeft, rawRight, RAW_CONFORMANCE_OPTIONS);
			if ((conformance & RawTypeConformanceComputer.SUCCESS) != 0) {
				final var key = Pair.of(rawLeft.getIdentifier(), rawRight.getIdentifier());
				if (!analyzedTypes.add(key)) {
					// The left and right types were already encountered during the conformance computation process.
					// This is denoting a possible infinite loop in the process that should be broken.
					return conformance;
				}
				if ((conformance & RawTypeConformanceComputer.SUBTYPE) != 0) {
					final var id = rawLeft.getIdentifier();
					final var iter = right1.getAllSuperTypes().stream()
							.filter(it -> id.equals(it.getRawTypeReference().getIdentifier()))
							.iterator();
					while (iter.hasNext()) {
						final var candidate = iter.next();
						conformance = getTypeArgumentConformance(
								left1, candidate,
								referenceOwner, conformanceComputer, analyzedTypes);
						if ((conformance & RawTypeConformanceComputer.SUCCESS) == 0) {
							return conformance;
						}
					}
				} else {
					final var leftTypes = left1.getTypeArguments();
					final var rightTypes = right1.getTypeArguments();
					conformance = getTypeArgumentConformance(
							leftTypes, it -> it,
							rightTypes,
							referenceOwner, conformanceComputer, analyzedTypes);
				}
			}
			return conformance;
		}
		// The left type is not parameterized, so that, we don't need to test the type parameters.
		return conformanceComputer.isConformant(
				left1, right1.getRawTypeReference(), CONFORMANCE_OPTIONS);
	}

	private static LightweightTypeReference substitute(LightweightTypeReference reference, ITypeReferenceOwner referenceOwner) {
		var ref = reference;
		while (ref instanceof WildcardTypeReference cvalue) {
			final var bound = cvalue.getUpperBoundSubstitute();
			ref = bound.copyInto(referenceOwner);
		}
		if (ref.getType() instanceof JvmTypeParameter) {
			return ref.getConstraintSubstitute();
		}
		return ref;
	}

	private static boolean isParameterizedType(LightweightTypeReference type) {
		final var jvmType = type.getType();
		if (jvmType instanceof JvmTypeParameterDeclarator gtype) {
			return !gtype.getTypeParameters().isEmpty();
		}
		return false;
	}

	private static LightweightTypeReference substitute(LightweightTypeReference argument, JvmTypeParameter parameter,
			ITypeReferenceOwner referenceOwner) {
		if (argument instanceof WildcardTypeReference wild) {
			var upper = wild.getUpperBoundSubstitute();
			if (upper.isType(Object.class)) {
				upper = getTypeParameterSubstitute(parameter, referenceOwner);
			}
			return upper;
		}
		return argument;
	}

	private static LightweightTypeReference getTypeParameterSubstitute(JvmTypeParameter parameter, ITypeReferenceOwner referenceOwner) {
		final var parameterReference = referenceOwner.toLightweightTypeReference(parameter);
		return parameterReference.getUpperBoundSubstitute();
	}
	
	@Override
	public List<LightweightTypeReference> substituteRootWildcard(List<LightweightTypeReference> arguments,
			List<JvmTypeParameter> parameters, ITypeReferenceOwner referenceOwner) {
		final var result = new ArrayList<LightweightTypeReference>(parameters.size());
		var i = 0;
		for (var parameter : parameters) {
			final LightweightTypeReference arg;
			if (i < arguments.size()) {
				final var argument = arguments.get(i);
				arg = substitute(argument, parameter, referenceOwner);
			} else {
				arg = getTypeParameterSubstitute(parameter, referenceOwner);
			}
			result.add(arg);
			++i;
		}
		return result;
	}
	
}

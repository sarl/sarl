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

import java.util.List;

import com.google.inject.ImplementedBy;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.conformance.RawTypeConformanceComputer;
import org.eclipse.xtext.xbase.typesystem.references.ITypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/**
 * SARL type checker that provides extended methods for checking types compared to the standard Xtext type checker. 
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@ImplementedBy(DefaultSARLTypeChecker.class)
public interface ISARLTypeChecker {

	/** Replies if the given type arguments are conform to the type parameters.
	 *
	 * <p>The code of this function is adapted from the code of Xtext library.
	 *
	 * @param typeArguments the list of type parameters that are passed as arguments.
	 * @param typeParameters the list of types parameters that have been declared.
	 * @param referenceOwner the owner of the type reference.
	 * @return {@code true} if the type arguments and type parameters are conform.
	 */
	@Pure
	default boolean isTypeArgumentConformant(List<LightweightTypeReference> typeArguments,
			List<JvmTypeParameter> typeParameters, ITypeReferenceOwner referenceOwner) {
		final var conformance = getTypeArgumentConformance(typeArguments, typeParameters, referenceOwner);
		return (conformance & RawTypeConformanceComputer.SUCCESS) != 0;
	}

	/** Replies the conformance of the to type parameters.
	 *
	 * <p>The code of this function is adapted from the code of Xtext library.
	 * The Xtext library seems not treating the bounding constraint in the expected way: the original conformance flags are
	 * immediately returned when tested the bounds of a second-level type parameter. This leads always to incompatibility.
	 *
	 * @param typeArguments the list of type parameters that are passed as arguments.
	 * @param typeParameters the list of types parameters that have been declared.
	 * @param referenceOwner the owner of the type reference.
	 * @return the conformance.
	 * @since 0.15
	 * @see RawTypeConformanceComputer
	 */
	@Pure
	int getTypeArgumentConformance(List<LightweightTypeReference> typeArguments,
			List<JvmTypeParameter> typeParameters, ITypeReferenceOwner referenceOwner);

	/** Substitute the root wildcards {@code ? extends X} by {@code X}.
	 * If there is no root wildcard, the argument is replied as-is.
	 * If the wildcard is {@code ?} then it is substituted by the upper bound from the
	 * parameter declaration at the same index in the list of parameters; otherwise,
	 * {@code Object} is used as replacement.
	 * The wildcards that are defined as type arguments inside {@code X} are not substituted.
	 *
	 * @param arguments the arguments to change.
	 * @param parameters the parameters' declarations that are corresponding to the given arguments.
	 * @param referenceOwner the owner of the references that are created during the substitution. 
	 * @return the result of the substitution.
	 */
	List<LightweightTypeReference> substituteRootWildcard(
			List<LightweightTypeReference> arguments, List<JvmTypeParameter> parameters, ITypeReferenceOwner referenceOwner);

}

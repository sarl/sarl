/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.jvmmodel.fragments;

import org.eclipse.emf.ecore.EObject;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;

/** Fragment for inferred to the JVM model based on a two stage process.
 * The two stage process in based on a transformation preparation and
 * the final transformation.
 *
 * @param <S> the type of the SARL's Ecore element.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public interface IBiStageInferrerFragment<S extends EObject> {

	/** Transform the source Ecore element to its equivalent JVM elements during the second step of
	 * the creation process of multiple output files.
	 *
	 * @param source the source Ecore element.
	 * @param inferredJvmTypes the provider of the generated JVM types.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @see #prepareTransform(EObject, JvmGenericTypeFactory, IBaseJvmModelInferrer)
	 */
	void transform(S source, JvmGenericTypeProvider inferredJvmTypes, IBaseJvmModelInferrer baseInferrer);

	/** Transform the source Ecore element to its equivalent JVM elements during the first step of
	 * the creation process of multiple output files.
	 *
	 * @param source the source Ecore element.
	 * @param inferredJvmTypes the provider of the generated JVM types.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @see #transform(EObject, JvmGenericTypeProvider, IBaseJvmModelInferrer)
	 */
	void prepareTransform(S source, JvmGenericTypeFactory inferredJvmTypes, IBaseJvmModelInferrer baseInferrer);

	/** Replies the type of the source Ecore element that is supported by this fragment.
	 *
	 * @return the type.
	 */
	Class<S> getSupportedType();

}

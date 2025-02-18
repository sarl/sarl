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

package io.sarl.lang.jvmmodel.fragments;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmDeclaredType;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;

/** Fragment for inferred to the JVM model based on a single stage process.
 *
 * @param <S> the type of the SARL's Ecore element.
 * @param <T> the type of the JVM's Ecore element.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public interface ISingleStageInferrerFragment<S extends EObject, T extends JvmDeclaredType> {

	/** Transform the source element to its equivalent JVM elements.
	 *
	 * @param source the source element.
	 * @param inferredJvmType the generated JVM types.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 */
	void transform(S source, T inferredJvmType, IBaseJvmModelInferrer baseInferrer);

	/** Replies the type of the source Ecore element that is supported by this fragment.
	 *
	 * @return the type.
	 */
	Class<S> getSupportedType();

}

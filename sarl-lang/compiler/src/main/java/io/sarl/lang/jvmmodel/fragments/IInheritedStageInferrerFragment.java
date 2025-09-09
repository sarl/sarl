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

import java.util.function.BiConsumer;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmDeclaredType;

import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;

/** Fragment for inferred to the JVM model based on a single stage process and with a callback
 * to the transformation function that is provided by Xbase or Xtend.
 *
 * @param <S> the type of the SARL's Ecore element.
 * @param <T> the type of the JVM's Ecore element.
 * @author $Author: sgalland$
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.15
 */
public interface IInheritedStageInferrerFragment<S extends EObject, T extends JvmDeclaredType> {

	/** Transform the source Ecore element to its equivalent JVM elements.
	 *
	 * @param source the source Ecore element.
	 * @param inferredJvmType the receiver of the JVM elements.
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param inheritedTransformer the transformer that is inherited from the Xtext/Xtend API.
	 */
	void transform(S source, T inferredJvmType, IBaseJvmModelInferrer baseInferrer,
			BiConsumer<S, T> inheritedTransformer);

	/** Replies the type of the source Ecore element that is supported by this fragment.
	 *
	 * @return the type.
	 */
	Class<S> getSupportedType();

}

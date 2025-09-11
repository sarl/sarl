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

import java.util.Collection;

import org.eclipse.emf.ecore.EObject;

import com.google.inject.ImplementedBy;

/** Tool for obtaining all the fragment contributions to the JVM model inferrer.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.15
 */
@ImplementedBy(JavaPluginInferrerFragmentContributions.class)
@SuppressWarnings("rawtypes")
public interface IInferrerFragmentContributions {

	/** Replies all the registered contributions for a single stage process without inherited transformer.
	 *
	 * @param type the type of the source element.
	 * @return the collection of the contributions.
	 */
	Collection<ISingleStageInferrerFragment> getSingleStageFragmentContributions(Class<? extends EObject> type);

	/** Replies all the registered contributions for a bi-stage process that are supported the given type
	 * of the Ecore source element.
	 *
	 * @param type the type of the source element.
	 * @return the collection of the contributions.
	 */
	Collection<IBiStageInferrerFragment> getBiStageFragmentContributions(Class<? extends EObject> type);

}

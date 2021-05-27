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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.pythongenerator.generator.configuration;

import com.google.inject.ImplementedBy;
import org.eclipse.emf.ecore.resource.Resource;


/** Provider of a configuration for the SARL-to-Python generator.
 *
 * @author <a href="http://www.ciad-lab.fr/author-10836/">St&eacute;phane Galland</a>
 * @version io.sarl.pythongenerator.generator 0.12.0 20210527-171007
 * @mavengroupid io.sarl.pythongenerator
 * @mavenartifactid io.sarl.pythongenerator.generator
 * @since 0.8
 */
@FunctionalInterface
@ImplementedBy(PyGeneratorConfigurationProvider.class)
public interface IPyGeneratorConfigurationProvider {

	/** Replies the configuration for the given context. If the configuration does not exist, it may be automatically
	 * installed within the context.
	 *
	 * @param context the context.
	 * @param installedIfNew {@code true} to install the configuration if it is new, {@code false} to not install it.
	 * @return the configuration, never {@code null}.
	 */
	PyGeneratorConfiguration get(Resource context, boolean installedIfNew);

}

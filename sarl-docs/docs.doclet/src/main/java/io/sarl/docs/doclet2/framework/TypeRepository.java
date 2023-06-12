/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.doclet2.framework;

import java.util.Set;
import java.util.SortedSet;

import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;

/** Store the types for all the types.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface TypeRepository {

	/** Build the type repository.
	 *
	 * @param typeElements the initial set of types for which documentation must be generated.
	 * @param environment the generation environment.
	 */
	void buildRepository(Iterable<? extends TypeElement> typeElements, SarlDocletEnvironment environment);

	/** Replies all the registered types.
	 *
	 * @param moduleName the module.
	 * @return the types in the module.
	 */
	SortedSet<TypeElement> getTypes();

	/** Replies the given in the given module.
	 *
	 * @param moduleName the module.
	 * @return the types in the module.
	 */
	SortedSet<TypeElement> getTypesInModule(ModuleElement moduleName);

	/** Replies the given in the given package.
	 *
	 * @param packageName the package.
	 * @return the types in the package.
	 */
	SortedSet<TypeElement> getTypesInPackage(PackageElement packageName);

	/** Replies all the modules.
	 *
	 * @return the registered modules.
	 */
	Set<ModuleElement> getModules();

	/** Replies all the packages.
	 *
	 * @return the registered packages.
	 */
	Set<PackageElement> getPackages();

	/** Replies the packages of the given module.
	 *
	 * @param moduleElement the module.
	 * @return the packages.
	 */
	Iterable<PackageElement> getPackagesFor(ModuleElement moduleElement);

	/** Replies if a deprecated feature was discovered.
	 *
	 * @return {@code true} if a discovered feature is registered.
	 */
	boolean hasDeprecatedFeatures();

	/** Replies all the packages.
	 *
	 * @return the registered packages.
	 */
	SortedSet<TypeElement> getDeprecatedTypes();

	/** Replies all the packages.
	 *
	 * @return the registered packages.
	 */
	SortedSet<VariableElement> getDeprecatedFields();

	/** Replies all the packages.
	 *
	 * @return the registered packages.
	 */
	SortedSet<ExecutableElement> getDeprecatedExecutables();

}

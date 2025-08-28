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

package io.sarl.bspl.lang.compiler.generic;

import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.generator.IGeneratorContext;

import com.google.inject.ImplementedBy;

/** Factory of context for the generation of SARL files.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@ImplementedBy(DefaultSarlTargetGeneratorContextFactory.class)
public interface ISarlTargetGeneratorContextFactory {

	/** Create a context that is specific to the SARL target generation.
	 *
	 * @param <T> the type of the name provider.
	 * @param fsa the file system manager.
	 * @param context the generator context.
	 * @param resourceSet the resource set in which the generated resources are located.
	 * @return the created context.
	 */
	<T> ISarlTargetGeneratorContext<T> createContext(IFileSystemAccess2 fsa, IGeneratorContext context, ResourceSet resourceSet);
	
	/** Create a context that is specific to the SARL target generation.
	 *
	 * @param <T> the type of the name provider.
	 * @param fsa the file system manager.
	 * @param resourceSet the resource set in which the generated resources are located.
	 * @return the created context.
	 */
	<T> ISarlTargetGeneratorContext<T> createContext(IFileSystemAccess fsa, ResourceSet resourceSet);

}

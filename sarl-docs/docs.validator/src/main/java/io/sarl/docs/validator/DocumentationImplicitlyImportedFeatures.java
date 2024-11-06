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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.docs.validator;

import java.util.List;

import io.sarl.lang.scoping.SARLImplicitlyImportedFeatures;

/** Implicitly imported extensions for the testing of the documentation.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.validator 0.14.0 20241106-161409
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.validator
 * @since 0.6
 */
public class DocumentationImplicitlyImportedFeatures extends SARLImplicitlyImportedFeatures {

	/** Construct the provider.
	 */
	public DocumentationImplicitlyImportedFeatures() {
		super();
	}

	@Override
	protected List<Class<?>> getStaticImportClasses() {
		final var xtextList = super.getStaticImportClasses();
		// Insert at the beginning for ensuring the SARL extension is selected before any Xtext extension.
		xtextList.add(0, IssueDatabaseExtensions.class);
		xtextList.add(0, MarkdownExtensions.class);
		xtextList.add(0, FactExtensions.class);
		xtextList.add(0, ShouldExtensions.class);
		xtextList.add(0, OperatorExtensions.class);
		xtextList.add(0, ReflectExtensions.class);
		xtextList.add(0, ShellExtensions.class);
		xtextList.add(0, CollectionExtensions.class);
		return xtextList;
	}

	@Override
	protected List<Class<?>> getExtensionClasses() {
		final var xtextList = super.getExtensionClasses();
		// Insert at the beginning for ensuring the SARL extension is selected before any Xtext extension.
		xtextList.add(0, IssueDatabaseExtensions.class);
		xtextList.add(0, MarkdownExtensions.class);
		xtextList.add(0, FactExtensions.class);
		xtextList.add(0, ShouldExtensions.class);
		xtextList.add(0, OperatorExtensions.class);
		xtextList.add(0, ReflectExtensions.class);
		xtextList.add(0, ShellExtensions.class);
		xtextList.add(0, CollectionExtensions.class);
		return xtextList;
	}

}

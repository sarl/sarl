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

package io.sarl.bspl.lang.compiler;

import java.util.Collections;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.AbstractGenerator;
import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.generator.IGenerator;
import org.eclipse.xtext.generator.IGeneratorContext;
import org.eclipse.xtext.util.PolymorphicDispatcher;

/** Abstract implementation of a generator that is generating SARL code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSarlTargetGenerator extends AbstractGenerator implements IGenerator {

	private final PolymorphicDispatcher<Void> generateDispatcher;
	
	/** Constructor.
	 */
	public AbstractSarlTargetGenerator() {
		this.generateDispatcher = new PolymorphicDispatcher<>(
				"_generate", 2, 2, //$NON-NLS-1$
				Collections.singletonList(this));
	}

	@Override
	public void doGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		doGenerate(input, fsa, null);
	}

	@Override
	public void doGenerate(Resource input, IFileSystemAccess fsa) {
		for (final var obj : input.getContents()) {
			doGenerate(obj, fsa);
		}
	}

	/** Generate the SARL code from the given BSPL Ecore element.
	 *
	 * @param object the Ecore element of SARL BSPL
	 * @param fsa the file system manager.
	 */
	protected void doGenerate(EObject object, IFileSystemAccess fsa) {
		this.generateDispatcher.invoke(object, fsa);
	}

}

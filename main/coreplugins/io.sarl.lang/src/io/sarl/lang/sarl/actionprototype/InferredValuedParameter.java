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

package io.sarl.lang.sarl.actionprototype;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/**
 * This class permits to wrap a default value when building the function signatures.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class InferredValuedParameter extends InferredStandardParameter {

	/** Constructor.
	 * @param source the original parameter.
	 * @param name the name of the formal parameter.
	 * @param type the type of the formal parameter.
	 * @param argument the value of the calling argument for the formal parameter.
	 */
	public InferredValuedParameter(EObject source, String name, LightweightTypeReference type, DynamicArgumentName argument) {
		super(source, name, type, argument);
	}

	/** Replies the value of the calling argument.
	 *
	 * @return the value of the calling argument.
	 */
	public String getCallingArgument() {
		return this.dynamicArgument.getArgument();
	}

	@Override
	public String toString() {
		return super.toString() + " {" + getDynamicCallingArgument() + "}"; //$NON-NLS-1$//$NON-NLS-2$
	}

}

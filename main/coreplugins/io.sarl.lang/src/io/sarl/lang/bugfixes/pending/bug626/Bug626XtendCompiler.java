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

package io.sarl.lang.bugfixes.pending.bug626;

import org.eclipse.xtend.core.compiler.XtendCompiler;
import org.eclipse.xtext.xbase.compiler.Later;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/** Contribute to the fixing of SARL issue 996: Invalid Java code for a cast of the null value.
 *
 * <p>Issue is associated to PR 626 (https://github.com/eclipse/xtext-extras/pull/626)
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/996"
 * @see "https://github.com/eclipse/xtext-extras/pull/626"
 */
public class Bug626XtendCompiler extends XtendCompiler {

	/** Generate a cast conversion into the given buffer.
	 *
	 * @param castTo is the expected type for the expression.
	 * @param buffer is the buffer to fill out with the cast conversion.
	 * @param expression represents the expression to cast.
	 */
	protected void doCastConversion(final LightweightTypeReference castTo, final ITreeAppendable buffer, final Later expression) {
		buffer.append("((");
		buffer.append(castTo);
		buffer.append(")");
		expression.exec(buffer);
		buffer.append(")");
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.signature;

import io.sarl.lang.sarl.SarlFormalParameter;

import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.xbase.XExpression;

/**
 * This class permits to wrap a default value when building the function signatures.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class InferredValuedParameter extends InferredStandardParameter {

	/**
	 * @param parameter - the wrapper parameter.
	 */
	protected InferredValuedParameter(XtendParameter parameter) {
		super(parameter);
	}

	/** Replies the default value.
	 *
	 * @return the default value.
	 */
	public XExpression getExpr() {
		if (this.parameter instanceof SarlFormalParameter) {
			return ((SarlFormalParameter) this.parameter).getDefaultValue();
		}
		return null;
	}

	@Override
	public String toString() {
		XExpression expr = getExpr();
		if (expr == null) {
			return super.toString();
		}
		return expr.toString();
	}

}

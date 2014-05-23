/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.signature;

import io.sarl.lang.sarl.FormalParameter;

import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * This class permits to wrap the formal parameters.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class InferredStandardParameter {
	/** Parameter.
	 */
	protected final FormalParameter parameter;
	
	/**
	 * @param parameter
	 */
	protected InferredStandardParameter(FormalParameter parameter) {
		this.parameter = parameter;
	}
	
	
	/** Replies the parameter.
	 * 
	 * @return the parameter.
	 */
	public FormalParameter getParameter() {
		return this.parameter;
	}

	/** Replies the type of the parameter.
	 * 
	 * @return the type.
	 */
	public JvmTypeReference getType() {
		return this.parameter.getParameterType();
	}

	@Override
	public String toString() {
		return this.parameter.toString();
	}
}

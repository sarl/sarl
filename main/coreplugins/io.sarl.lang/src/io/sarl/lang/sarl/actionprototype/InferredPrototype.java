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

import java.util.List;
import java.util.Map;

/**
 * Provides additional function signatures according the semantic
 * associated to the parameter's default values.
 *
 * <p>The parameter types are classified into two categories:<ol>
 * <li>the parameter types of the action that are defined by the SARL programmer:
 *     the <strong>original parameter types</strong>; and</li>
 * <li>the parameter types that are inferred and computed by the SARL compiler according to
 *     the default values of the original parameter types:
 *     the <strong>inferred parameter types</strong>.</li>
 * </ol>
 *
 * <p>Example: <pre><code>
 * def myfct(a : int, b : float = 0, c : char, d : int = 0)
 * </code></pre>
 * The parameter types are:<ol>
 * <li>Original parameter types: <code>int,float,char,int</code></li>
 * <li>Inferred parameter types: <ul>
 * 		<li><code>int,float,char,int</code></li>
 * 		<li><code>int,char,int</code></li>
 * 		<li><code>int,float,char</code></li>
 * 		<li><code>int,char</code></li>
 * 		</ul></li>
 * </ol>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface InferredPrototype {

	/** Replies the qualified name of the action related to this prototype.
	 * This qualified name is used for identifying the prototype in a {@link IActionPrototypeProvider}.
	 *
	 * @return the key.
	 */
	QualifiedActionName getActionName();

	/** Replies the prototypes that were inferred according to the semantic
	 * of the parameter's default value.
	 *
	 * <p>This function replies the inferred parameters types (see the documentation of the class for details).
	 *
	 * @return the inferred prototypes.
	 * @see #getOriginalParameterTypes()
	 * @see #getParameterTypeAlternatives()
	 */
	Map<ActionParameterTypes, List<InferredStandardParameter>> getInferredParameterTypes();

	/** Replies an iterator on the original and inferred parameter types.
	 *
	 * @return all the signatures.
	 * @see #getInferredParameterTypes()
	 * @see #getParameterTypeAlternatives()
	 */
	List<InferredStandardParameter> getOriginalParameterTypes();

	/** Replies an iterator on the original and inferred parameter types.
	 *
	 * @return all the signatures.
	 * @see #getOriginalParameterTypes()
	 * @see #getInferredParameterTypes()
	 */
	Iterable<ActionParameterTypes> getParameterTypeAlternatives();

	/** Replies the parameters that are NOT inferred.
	 *
	 * @return the parameters.
	 */
	FormalParameterProvider getFormalParameters();

	/** Replies the full list of parameter types associated to this prototype.
	 * The replies list does not hide any formal parameter with a default value.
	 *
	 * @return the parameter types.
	 */
	ActionParameterTypes getFormalParameterTypes();

	/** Replies if the prototype has a vararg.
	 *
	 * @return <code>true</code> if the last parameter is a vararg, <code>false</code>
	 *     otherwise.
	 */
	boolean isVarargs();

	/** Replies the formatted list of parameters.
	 *
	 * @return the parameter list.
	 */
	@Override
	String toString();

	/** Replies the formatted list of parameters with an associated function name.
	 *
	 * @param functionName the name of the function to be put in the replied string.
	 * @return the parameter list.
	 */
	String toString(String functionName);

}

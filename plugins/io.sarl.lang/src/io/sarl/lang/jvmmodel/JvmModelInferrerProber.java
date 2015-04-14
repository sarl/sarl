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
package io.sarl.lang.jvmmodel;

import java.util.Map;
import java.util.TreeMap;

import com.google.inject.Singleton;


/** Interface that defines a probing object for the {@link SARLJvmModelInferrer JVM model inferrer}.
 *
 * This object is used by the {@link SARLJvmModelInferrer JVM model inferrer} for notifying
 * the prober about any internal change.
 *
 * This object may be used for debugging or unit tests.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class JvmModelInferrerProber {

	private final Map<Step, Map<String, Object>> data = new TreeMap<>();

	/** Clear registered data.
	 */
	public void clear() {
		this.data.clear();
	}

	/** Clear registered data for the given step.
	 *
	 * @param step the step for which the data must be cleared.
	 */
	void clear(Step step) {
		this.data.remove(step);
	}

	/** Register the given value.
	 *
	 * @param step the step for which the data must be registered.
	 * @param key the key of the value.
	 * @param value the value.
	 */
	void register(Step step, String key, Object value) {
		Map<String, Object> values = this.data.get(step);
		if (values == null) {
			values = new TreeMap<>();
			this.data.put(step, values);
		}
		values.put(key, value);
	}

	/** Replies the registered data.
	 *
	 * @param <T> the type of the data.
	 * @param step the step for which the data must be retreived.
	 * @param key the key of the value.
	 * @param type the type of the data.
	 * @return the registered data, or <code>null</code>.
	 */
	public <T> T get(Step step, String key, Class<T> type) {
		Map<String, Object> values = this.data.get(step);
		if (values != null) {
			Object value = values.get(key);
			if (type.isInstance(value)) {
				return type.cast(value);
			}
		}
		return null;
	}

	/** The lists of the steps that are supported by the {@link SARLJvmModelInferrer}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public enum Step {

		/** Step 0 of {@link SARLJvmModelInferrer#appendSarlMembers(org.eclipse.xtext.common.types.JvmGenericType,
		 * org.eclipse.xtend.core.xtend.XtendTypeDeclaration, io.sarl.lang.jvmmodel.SARLJvmModelInferrer.GenerationContext)}.
		 */
		GENERATE_CODE_FOR_FEATURES_0,

		/** Step 1 of {@link SARLJvmModelInferrer#appendSarlMembers(org.eclipse.xtext.common.types.JvmGenericType,
		 * org.eclipse.xtend.core.xtend.XtendTypeDeclaration, io.sarl.lang.jvmmodel.SARLJvmModelInferrer.GenerationContext)}.
		 */
		GENERATE_CODE_FOR_FEATURES_1,

		/** Step 2 of {@link SARLJvmModelInferrer#appendSarlMembers(org.eclipse.xtext.common.types.JvmGenericType,
		 * org.eclipse.xtend.core.xtend.XtendTypeDeclaration, io.sarl.lang.jvmmodel.SARLJvmModelInferrer.GenerationContext)}.
		 */
		GENERATE_CODE_FOR_FEATURES_2,

		/** Step 3 of {@link SARLJvmModelInferrer#appendSarlMembers(org.eclipse.xtext.common.types.JvmGenericType,
		 * org.eclipse.xtend.core.xtend.XtendTypeDeclaration, io.sarl.lang.jvmmodel.SARLJvmModelInferrer.GenerationContext)}.
		 */
		GENERATE_CODE_FOR_FEATURES_3;

	}

}

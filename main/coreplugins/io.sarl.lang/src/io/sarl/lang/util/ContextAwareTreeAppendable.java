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

package io.sarl.lang.util;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;

/**
 * A delegating tree appendable which is aware of th current compilation context.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8.6
 */
public class ContextAwareTreeAppendable extends DelegateTreeAppendable {

	private final Map<String, Object> values;

	/** Constructor.
	 *
	 * @param delegate the appendable to delegate to.
	 */
	public ContextAwareTreeAppendable(ITreeAppendable delegate) {
		super(delegate);
		this.values = new HashMap<>();
	}

	/** Constructor.
	 *
	 * @param contextualValues the contextual values.
	 * @param delegate the appendable to delegate to.
	 */
	protected ContextAwareTreeAppendable(Map<String, Object> contextualValues, ITreeAppendable delegate) {
		super(delegate);
		this.values = contextualValues;
	}

	@Override
	protected ITreeAppendable createDelegateToChild(ITreeAppendable child) {
		return new ContextAwareTreeAppendable(this.values, child);
	}

	/** Define a contextual value.
	 *
	 * @param <T> the type of the value.
	 * @param key the name of the value.
	 * @param value the value itself.
	 * @return the previous value associated to the key, or {@code null} if none.
	 */
	@SuppressWarnings("unchecked")
	public <T> T defineContextualValue(String key, Object value) {
		return (T) this.values.put(key,  value);
	}

	/** Define a contextual value.
	 *
	 * @param <T> the type of the value.
	 * @param key the name of the value.
	 * @return the previous value associated to the key, or {@code null} if none.
	 */
	@SuppressWarnings("unchecked")
	public <T> T deleteContextualValue(String key) {
		return (T) this.values.remove(key);
	}

	/** Replies the contextual value associated to the given key.
	 *
	 * @param <T> the type of the value.
	 * @param key the name of the value.
	 * @return the value associated to the key, or {@code null} if none.
	 */
	@SuppressWarnings("unchecked")
	public <T> T getContextualValue(String key) {
		return (T) this.values.get(key);
	}

	/** Replies if a contextual value is associated to the given key.
	 *
	 * @param key the name of the value.
	 * @return {@code true} if a value is associated to the key, or {@code false} if none.
	 */
	public boolean hasContextualValue(String key) {
		return this.values.containsKey(key);
	}

}

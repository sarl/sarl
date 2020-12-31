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

package io.sarl.lang.mwe2.binding;

import java.text.MessageFormat;
import java.util.Objects;

import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * An injected element.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BindingElement {

	private static final int HASH_VALUE = 31;

	private String functionName;

	private boolean singleton;

	private boolean eagerSingleton;

	private boolean instance;

	private String injectedType;

	private String concreteType;

	private String annotatedWith;

	private String annotatedWithName;

	private boolean overridePreviousDefinition;

	private boolean provider;

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof BindingElement) {
			return Objects.equals(getBind(), ((BindingElement) obj).getBind())
					&& Objects.equals(getAnnotatedWith(), ((BindingElement) obj).getAnnotatedWith())
					&& Objects.equals(getAnnotatedWithName(), ((BindingElement) obj).getAnnotatedWithName());
		}
		return false;
	}

	@Override
	public int hashCode() {
		int bits = 1;
		bits = bits * HASH_VALUE + Objects.hashCode(getBind());
		bits = bits * HASH_VALUE + Objects.hashCode(getAnnotatedWith());
		bits = bits * HASH_VALUE + Objects.hashCode(getAnnotatedWithName());
		return bits ^ (bits >> 31);
	}

	@Override
	public String toString() {
		if (!Strings.isEmpty(getAnnotatedWith())) {
			return MessageFormat.format("@{2} {0} => {1}", getBind(), getTo(), getAnnotatedWith()); //$NON-NLS-1$
		}
		if (!Strings.isEmpty(getAnnotatedWithName())) {
			return MessageFormat.format("@Named({2}) {0} => {1}", getBind(), getTo(), getAnnotatedWithName()); //$NON-NLS-1$
		}
		return MessageFormat.format("{0} => {1}", getBind(), getTo()); //$NON-NLS-1$
	}

	/** Replies the string representation of the binding key.
	 *
	 * @return the string representation of the binding key.
	 * @since 0.8
	 */
	public String getKeyString() {
		if (!Strings.isEmpty(getAnnotatedWith())) {
			return MessageFormat.format("@{1} {0}", getBind(), getAnnotatedWith()); //$NON-NLS-1$
		}
		if (!Strings.isEmpty(getAnnotatedWithName())) {
			return MessageFormat.format("@Named({1}) {0}", getBind(), getAnnotatedWithName()); //$NON-NLS-1$
		}
		return MessageFormat.format("{0}", getBind()); //$NON-NLS-1$
	}

	/** Set the element could override a previously defined element.
	 *
	 * @param override <code>true</code> for overriding.
	 */
	public void setOverride(boolean override) {
		this.overridePreviousDefinition = override;
	}

	/** Replies if the element could override a previously defined element.
	 *
	 * @return <code>true</code> for overriding.
	 */
	@Pure
	public boolean isOverride() {
		return this.overridePreviousDefinition;
	}

	/** Set the element is a provider.
	 *
	 * @param provider <code>true</code> if a provider.
	 */
	public void setProvider(boolean provider) {
		this.provider = provider;
	}

	/** Replies if the element is a provider.
	 *
	 * @return <code>true</code> if it is a provider.
	 */
	@Pure
	public boolean isProvider() {
		return this.provider;
	}

	/** Set the function name.
	 *
	 * @param name the name of the binding function.
	 */
	public void setFunctionName(String name) {
		if (!Strings.isEmpty(name)) {
			this.functionName = name;
		}
	}

	/** Replies the function name.
	 *
	 * @return the name of the binding function, or {@code null}.
	 */
	@Pure
	public String getFunctionName() {
		return this.functionName;
	}

	/** Set the annotation.
	 *
	 * @param annotation the annotation.
	 */
	public void setAnnotatedWith(String annotation) {
		if (!Strings.isEmpty(annotation)) {
			this.annotatedWith = annotation;
		}
	}

	/** Replies the annotation.
	 *
	 * @return the annotation.
	 */
	@Pure
	public String getAnnotatedWith() {
		return this.annotatedWith;
	}

	/** Set the <code>@</code><code>Named</code> property.
	 *
	 * @param name the name.
	 */
	public void setAnnotatedWithName(String name) {
		if (!Strings.isEmpty(name)) {
			this.annotatedWithName = name;
		}
	}

	/** Replies the <code>@</code><code>Named</code> property..
	 *
	 * @return the name.
	 */
	@Pure
	public String getAnnotatedWithName() {
		return this.annotatedWithName;
	}

	/** Set as singleton.
	 *
	 * @param singleton <code>true</code> for singleton binding.
	 */
	public void setSingleton(boolean singleton) {
		this.singleton = singleton;
	}

	/** Replies if it is a singleton binding.
	 *
	 * @return <code>true</code> for singleton binding.
	 */
	@Pure
	public boolean isSingleton() {
		return this.singleton;
	}

	/** Set as eager singleton.
	 *
	 * @param eager <code>true</code> for eager singleton binding.
	 */
	public void setEager(boolean eager) {
		this.eagerSingleton = eager;
	}

	/** Replies if it is an eager singleton binding.
	 *
	 * @return <code>true</code> for eager singleton binding.
	 */
	@Pure
	public boolean isEager() {
		return this.eagerSingleton;
	}

	/** Set as instane binding.
	 *
	 * @param instance <code>true</code> for instance binding.
	 */
	public void setInstance(boolean instance) {
		this.instance = instance;
	}

	/** Replies if it is an instance binding.
	 *
	 * @return <code>true</code> for instance binding.
	 */
	@Pure
	public boolean isInstance() {
		return this.instance;
	}

	/** Change the binded type.
	 *
	 * @param injectedType the binded type.
	 */
	public void setBind(String injectedType) {
		if (!Strings.isEmpty(injectedType)) {
			this.injectedType = injectedType;
		}
	}

	/** Replies the binded type.
	 *
	 * @return the binded type.
	 */
	@Pure
	public String getBind() {
		return this.injectedType;
	}

	/** Change the concrete type.
	 *
	 * @param concreteType the concrete type.
	 */
	public void setTo(String concreteType) {
		if (!Strings.isEmpty(concreteType)) {
			this.concreteType = concreteType;
		}
	}

	/** Replies the concrete type.
	 *
	 * @return the concrete type.
	 */
	@Pure
	public String getTo() {
		return this.concreteType;
	}

}

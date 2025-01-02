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

package io.sarl.lang.jvmmodel.fragments;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.TypesFactory;

import com.google.common.base.Strings;

/** Factory of JVM type for the 1-to-many generation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class DefaultJvmGenericTypeProvider implements JvmGenericTypeFactory, JvmGenericTypeProvider {

	private final Map<Integer, JvmGenericType> createdTypes = new HashMap<>();
	
	private final TypesFactory jvmTypesFactory;

	/** Constructor.
	 *
	 * @param jvmTypesFactory the facotry of JVM types.
	 */
	public DefaultJvmGenericTypeProvider(TypesFactory jvmTypesFactory) {
		this.jvmTypesFactory = jvmTypesFactory;
	}
	
	@Override
	public JvmGenericTypeFactory createReceiver(int index, String name) {
		assert index >= 0 && !Strings.isNullOrEmpty(name);
		this.createdTypes.computeIfAbsent(Integer.valueOf(index), it -> {
			final var type = this.jvmTypesFactory.createJvmGenericType();
			type.setSimpleName(name);
			return type;
		});
		return this;
	}
	
	@Override
	public JvmGenericType getGenericType(int index) {
		assert index >= 0;
		final var type = this.createdTypes.get(Integer.valueOf(index));
		assert type != null;
		return type;
	}

	/** Replies the stream on the created JVM types.
	 *
	 * @return the stream, never {@code null}.
	 */
	public Stream<JvmGenericType> stream() {
		return this.createdTypes.values().stream();
	}

	/** Replies if the provider contains generated types.
	 *
	 * @return {@code true} if at least one type is generated.
	 */
	public boolean hasGeneratedType() {
		return !this.createdTypes.isEmpty();
	}
	
}

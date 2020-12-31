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

package io.sarl.lang.typesystem;

import com.google.inject.ImplementedBy;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/**
 * Tool for validating the types against their immutability.
 *
 * <p>An immutable type is a type those state cannot be changed after it is constructed.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@ImplementedBy(DefaultImmutableTypeValidator.class)
public interface IImmutableTypeValidator {

	/** Replies if the given type is associated to an immutable type.
	 *
	 * <p>An unmodifiable type is a primitive type or an object type those state cannot
	 * be changed after is it created in memory, e.g. {@link String}.
	 * @param type the type to test.
	 * @return {@code true} if the given type is known as unmodifiable. Otherwise {@code false}.
	 */
	boolean isImmutable(LightweightTypeReference type);

}

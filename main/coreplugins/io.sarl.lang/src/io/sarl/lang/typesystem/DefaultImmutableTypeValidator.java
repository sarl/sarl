/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import java.io.File;
import java.net.InetAddress;
import java.net.URI;
import java.net.URL;
import java.util.Date;
import java.util.Locale;
import java.util.UUID;

import com.google.inject.Singleton;
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
@Singleton
public class DefaultImmutableTypeValidator implements IImmutableTypeValidator {

	private static final Class<?>[] IMMUTABLE_TYPES = {
		String.class,
		UUID.class,
		URL.class,
		URI.class,
		Enum.class,
		Number.class,
		Date.class,
		File.class,
		Locale.class,
		InetAddress.class,
		StackTraceElement.class,
	};

	@Override
	public boolean isImmutable(LightweightTypeReference type) {
		assert type != null;
		final LightweightTypeReference ref = type.getPrimitiveIfWrapperType();
		if (ref.isArray()) {
			return false;
		}
		if (ref.isPrimitive() || ref.isPrimitiveVoid()) {
			return true;
		}
		for (final Class<?> jvmType : IMMUTABLE_TYPES) {
			if (type.isSubtypeOf(jvmType)) {
				return true;
			}
		}
		return false;
	}

}

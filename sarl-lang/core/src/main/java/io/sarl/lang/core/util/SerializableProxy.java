/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.core.util;

import java.io.InvalidClassException;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.io.StreamCorruptedException;
import java.io.WriteAbortedException;
import java.lang.reflect.Constructor;

/**
 * An object that could be used as a proxy for serializing an object.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.8.6
 */
public class SerializableProxy implements Serializable {

	private static final long serialVersionUID = -2601677585237973795L;

	/** Type of the serialized type.
	 */
	private final Class<?> proxyType;

	/** The values that are serialized.
	 */
	private final Object[] values;

	/** Construct a proxy.
	 *
	 * @param type the inner or anonymous class of the object to create when deserializing.
	 * @param values the values to serialize.
	 */
	public SerializableProxy(Class<?> type, Object... values) {
		assert type.isLocalClass() || type.isAnonymousClass();
		this.proxyType = type;
		this.values = values;
	}

	/** Replies the value at the given index.
	 *
	 * @param <T> the expected type of the data.
	 * @param index the index.
	 * @return the value.
	 */
	@SuppressWarnings("unchecked")
	public <T> T getValue(int index) {
		return (T) this.values[index];
	}

	/** This function enables to deserialize an instance of this proxy.
	 *
	 * @return the deserialized object.
	 * @throws ObjectStreamException if the deserialization fails
	 */
	private Object readResolve() throws ObjectStreamException {
		Constructor<?> compatible = null;
		for (final var candidate : this.proxyType.getDeclaredConstructors()) {
			if (candidate != null && isCompatible(candidate)) {
				if (compatible != null) {
					throw new IllegalStateException();
				}
				compatible = candidate;
			}
		}
		if (compatible != null) {
			// TODO Is this compatible with Java 11?
			compatible.setAccessible(true);
			try {
				final var arguments = new Object[this.values.length + 1];
				System.arraycopy(this.values, 0, arguments, 1, this.values.length);
				return compatible.newInstance(arguments);
			} catch (Exception exception) {
				throw new WriteAbortedException(exception.getLocalizedMessage(), exception);
			}
		}
		throw new InvalidClassException("compatible constructor not found"); //$NON-NLS-1$
	}

	private boolean isCompatible(Constructor<?> candidate) throws ObjectStreamException {
		final var parameterTypes = candidate.getParameterTypes();
		if (parameterTypes.length != this.values.length + 1) {
			return false;
		}
		final var enclosingType = this.proxyType.getEnclosingClass();
		if (!parameterTypes[0].isAssignableFrom(enclosingType)) {
			return false;
		}
		for (int i = 0, j = 1; i < this.values.length; ++i, ++j) {
			final Object param = this.values[i];
			assert j < parameterTypes.length;
			var clazz = parameterTypes[j];
			if (clazz.isPrimitive()) {
				clazz = wrapperTypeFor(clazz);
			}
			if (param != null && !clazz.isInstance(param)) {
				return false;
			}
		}
		return true;
	}

	private static Class<?> wrapperTypeFor(Class<?> primitive) throws ObjectStreamException {
        if (primitive == Boolean.TYPE) {
        	return Boolean.class;
        }
        if (primitive == Byte.TYPE) {
        	return Byte.class;
        }
        if (primitive == Character.TYPE) {
        	return Character.class;
        }
        if (primitive == Short.TYPE) {
        	return Short.class;
        }
        if (primitive == Integer.TYPE) {
        	return Integer.class;
        }
        if (primitive == Long.TYPE) {
        	return Long.class;
        }
        if (primitive == Float.TYPE) {
        	return Float.class;
        }
        if (primitive == Double.TYPE) {
        	return Double.class;
        }
        if (primitive == Void.TYPE) {
        	return Void.class;
        }
        throw new StreamCorruptedException();
    }

}

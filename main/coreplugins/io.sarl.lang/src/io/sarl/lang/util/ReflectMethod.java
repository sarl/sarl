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

import java.lang.reflect.Method;

/**
 * Uitlity class for accessing to a method by reflection.
 *
 * @param <RT> the type of the receiver.
 * @param <T> the type of the returned value.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public final class ReflectMethod<RT, T> {

	private final Class<RT> receiverType;

	private final Class<T> returnType;

	private final String methodName;

	private Method method;

	/** Constructor.
	 *
	 * @param receiverType the type of the receiver.
	 * @param returnType the type of the returned values.
	 * @param methodName the name of the method.
	 */
	protected ReflectMethod(Class<RT> receiverType, Class<T> returnType, String methodName) {
		this.receiverType = receiverType;
		this.returnType = returnType;
		this.methodName = methodName;
	}

	/** Static constructor.
	 *
	 * @param <RT> the type of the receiver.
	 * @param <T> the type of the returned value.
	 * @param receiverType the type of the receiver.
	 * @param returnType the type of the returned values.
	 * @param methodName the name of the method.
	 * @return the instance.
	 */
	public static <RT, T> ReflectMethod<RT, T> of(Class<RT> receiverType, Class<T> returnType, String methodName) {
		return new ReflectMethod<>(receiverType, returnType, methodName);
	}

	/** Invoke the method.
	 *
	 * @param receiver the receiver, never {@code null}.
	 * @param arguments the arguments.
	 * @return the result of the invocation.
	 */
	public T invoke(RT receiver, Object... arguments) {
		assert receiver != null;
		final Method method;
		synchronized (this) {
			if (this.method == null) {
				this.method = getMethod(receiver, arguments);
				method = this.method;
			} else {
				method = this.method;
			}
		}
		try {
			return this.returnType.cast(method.invoke(receiver, arguments));
		} catch (RuntimeException | Error exception) {
			throw exception;
		} catch (Throwable exception) {
			throw new RuntimeException(exception);
		}
	}

	private Method getMethod(RT receiver, Object[] args) {
		try {
			final Object[] arguments = args == null ? new Object[] {null} : args;

			Class<? extends Object> clazz = this.receiverType;
			Method compatible = null;
			do {
				for (final Method candidate : clazz.getDeclaredMethods()) {
					if (candidate != null && !candidate.isBridge() && isCompatible(candidate, this.methodName, arguments)) {
						if (compatible != null) {
							throw new IllegalStateException("Ambiguous methods to invoke. Both " //$NON-NLS-1$
									+ compatible + " and " + candidate + " would be compatible choices.");  //$NON-NLS-1$//$NON-NLS-2$
						}
						compatible = candidate;
					}
				}
				clazz = clazz.getSuperclass();
			} while (compatible == null && clazz != null);
			if (compatible != null) {
				compatible.setAccessible(true);
				return compatible;
			}
			// not found provoke method not found exception
			final Class<?>[] paramTypes = new Class<?>[arguments.length];
			for (int i = 0; i < arguments.length; ++i) {
				paramTypes[i] = arguments[i] == null ? Object.class : arguments[i].getClass();
			}
			return receiver.getClass().getMethod(this.methodName, paramTypes);
		} catch (Throwable exception) {
			throw new Error(exception);
		}
	}

	private static boolean isCompatible(Method candidate, String featureName, Object... args) {
		if (!candidate.getName().equals(featureName)) {
			return false;
		}
		if (candidate.getParameterTypes().length != args.length) {
			return false;
		}
		for (int i = 0; i < candidate.getParameterTypes().length; ++i) {
			final Object param = args[i];
			Class<?> class1 = candidate.getParameterTypes()[i];
			if (class1.isPrimitive()) {
				class1 = wrapperTypeFor(class1);
			}
			if (param != null && !class1.isInstance(param)) {
				return false;
			}
		}
		return true;
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
	private static Class<?> wrapperTypeFor(Class<?> primitive) {
		assert primitive != null;
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
		throw new IllegalArgumentException(primitive + " is not a primitive"); //$NON-NLS-1$
	}

}

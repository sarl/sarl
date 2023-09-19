/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
package io.sarl.tests.api.tools;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import com.google.common.base.Objects;
import com.google.common.base.Throwables;
import org.mockito.internal.util.Primitives;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import io.sarl.tests.api.TestPluginActivator;

/** Utilities that are related to Java reflection.
 *
 * @author $Author: sgalland$
 * @version tests.api 0.13.0 20230919-093055
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid tests.api
 * @since 0.11
 */
public class TestReflections {

	private TestReflections() {
		//
	}

	/**
	 * Retrieves the value of the given accessible static field of the given type.
	 * 
	 * @param receiverType the type of the container of the field, not {@code null}
	 * @param fieldName the field's name, not {@code null}
	 * @return the value of the field
	 * 
	 * @throws NoSuchFieldException see {@link Class#getField(String)}
	 * @throws SecurityException see {@link Class#getField(String)}
	 * @throws IllegalAccessException see {@link Field#get(Object)}
	 * @throws IllegalArgumentException see {@link Field#get(Object)}
	 */
	@SuppressWarnings("unchecked")
	public static <T> T getStatic(Class<?> receiverType, String fieldName) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
		Field f = getDeclaredField(receiverType, fieldName);
		f.setAccessible(true);
		return (T) f.get(null);
	}

	/**
	 * Set the value of the given accessible static field of the given type.
	 * 
	 * @param receiverType the type of the container of the field, not {@code null}
	 * @param fieldName the field's name, not {@code null}
	 * @param value the value of the field
	 * @throws NoSuchFieldException see {@link Class#getField(String)}
	 * @throws SecurityException see {@link Class#getField(String)}
	 * @throws IllegalAccessException see {@link Field#get(Object)}
	 * @throws IllegalArgumentException see {@link Field#get(Object)}
	 */
	public static <T> void setStatic(Class<?> receiverType, String fieldName, Object value) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
		Field f = getDeclaredField(receiverType, fieldName);
		f.setAccessible(true);
		f.set(null, value);
	}

	/**
	 * Set the value of the given accessible field of the given instance.
	 * 
	 * @param instance the container of the field, not {@code null}
	 * @param fieldName the field's name, not {@code null}
	 * @param value the value of the field
	 * @throws NoSuchFieldException see {@link Class#getField(String)}
	 * @throws SecurityException see {@link Class#getField(String)}
	 * @throws IllegalAccessException see {@link Field#get(Object)}
	 * @throws IllegalArgumentException see {@link Field#get(Object)}
	 */
	public static <T> void set(Object instance, String fieldName, Object value) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
		Class<?> type = instance.getClass();
		while (type != null) {
			try {
				Field f = getDeclaredField(type, fieldName);
				f.setAccessible(true);
				f.set(instance, value);
				return;
			} catch (NoSuchFieldException exception) {
				//
			}
			type = type.getSuperclass();
		}
		throw new NoSuchFieldException(fieldName);
	}

	/**
	 * Replies the value of the given accessible field of the given instance.
	 * 
	 * @param instance the container of the field, not {@code null}
	 * @param fieldName the field's name, not {@code null}
	 * @return the value of the field
	 * @throws NoSuchFieldException see {@link Class#getField(String)}
	 * @throws SecurityException see {@link Class#getField(String)}
	 * @throws IllegalAccessException see {@link Field#get(Object)}
	 * @throws IllegalArgumentException see {@link Field#get(Object)}
	 */
	@SuppressWarnings("unchecked")
	public static <T> T get(Object instance, String fieldName) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
		Class<?> type = instance.getClass();
		while (type != null) {
			try {
				Field f = getDeclaredField(type, fieldName);
				f.setAccessible(true);
				return (T) f.get(instance);
			} catch (NoSuchFieldException exception) {
				//
			}
			type = type.getSuperclass();
		}
		throw new NoSuchFieldException(fieldName);
	}

	/**
	 * Invokes the first accessible constructor defined on the receiver's class with
	 * a parameter list compatible to the given arguments.
	 *
	 * @param <T> the type of the object to create.
	 * @param type type of the object to create.
	 * @param args the arguments for the method invocation
	 * @return the result of the constructor invocation.
	 * @throws InvocationTargetException 
	 * @throws IllegalArgumentException 
	 * @throws IllegalAccessException 
	 * @throws InstantiationException 
	 * @throws SecurityException 
	 * @throws NoSuchMethodException 
	 */
	public static <T> T newInstance(Class<T> type, Object... args) throws InstantiationException, IllegalAccessException,
	IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
		final Object[] arguments = args == null ? new Object[]{null} : args;
		Constructor<?> compatible = null;
		for (Constructor<?> candidate : type.getDeclaredConstructors()) {
			if (candidate != null && isCompatible(candidate, arguments)) {
				if (compatible != null) {
					throw new IllegalStateException(
							"Ambiguous constructor to invoke. Both " //$NON-NLS-1$
							+ compatible + " and  " + candidate + " would be compatible choices."); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-2
				}
				compatible = candidate;
			}
		}
		if (compatible != null) {
			compatible.setAccessible(true);
			return type.cast(compatible.newInstance(arguments));
		}
		// not found provoke constructor not found exception
		Class<?>[] paramTypes = new Class<?>[arguments.length];
		for (int i = 0; i< arguments.length ; i++) {
			paramTypes[i] = arguments[i] == null ? Object.class : arguments[i].getClass();
		}
		Constructor<T> cons = type.getConstructor(paramTypes);
		return cons.newInstance(args);
	}

	/** Replies if the givan constructor is compatible with the arguments.
	 *
	 * @param candidate the constructor to test.
	 * @param args the values of the arguments.
	 * @return {@code true} if the arguments could be passed to the constructor.
	 */
	protected static boolean isCompatible(Constructor<?> candidate, Object... args) {
		if (candidate.getParameterTypes().length != args.length)
			return false;
		for (int i = 0; i< candidate.getParameterTypes().length; i++) {
			Object param = args[i];
			Class<?> class1 = candidate.getParameterTypes()[i];
			if (class1.isPrimitive()) {
				class1 = wrapperTypeFor(class1);
			}
			if (param != null && !class1.isInstance(param))
				return false;
		}
		return true;
	}

	/** Replies the Object version of the native types if application.
	 * Otherwise, reply the type itself.
	 *
	 * @param primitive the input type.
	 * @return the output type.
	 */
	protected static Class<?> wrapperTypeFor(Class<?> primitive) {
		assert primitive != null;
		if (primitive == Boolean.TYPE) return Boolean.class;
		if (primitive == Byte.TYPE) return Byte.class;
		if (primitive == Character.TYPE) return Character.class;
		if (primitive == Short.TYPE) return Short.class;
		if (primitive == Integer.TYPE) return Integer.class;
		if (primitive == Long.TYPE) return Long.class;
		if (primitive == Float.TYPE) return Float.class;
		if (primitive == Double.TYPE) return Double.class;
		if (primitive == Void.TYPE) return Void.class;
		throw new IllegalArgumentException(primitive+ " is not a primitive"); //$NON-NLS-1$
	}

	/** Replies the declared field in the given class or one of its super classes.
	 *
	 * @param clazz the class.
	 * @param name the name of the field.
	 * @return the field.
	 * @throws NoSuchFieldException if the field cannot be found.
	 */
	protected static Field getDeclaredField(Class<?> clazz, String name) throws NoSuchFieldException {
		Class<?> type = clazz;
		NoSuchFieldException initialException = null;
		do {
			try {
				Field f = type.getDeclaredField(name);
				return f;
			} catch(NoSuchFieldException noSuchField) {
				if (initialException == null) {
					initialException = noSuchField;
				}
			}
		} while((type = type.getSuperclass()) != null);
		throw initialException;
	}

	/**
	 * Invokes the first accessible constructor defined on the receiver's class with
	 * a parameter list compatible to the given arguments.
	 *
	 * @param <T> the type of the object to create.
	 * @param type type of the object to create.
	 * @param args the arguments for the method invocation
	 * @return the result of the constructor invocation.
	 * @throws InvocationTargetException 
	 * @throws IllegalArgumentException 
	 * @throws IllegalAccessException 
	 * @throws InstantiationException 
	 * @throws SecurityException 
	 * @throws NoSuchMethodException 
	 * @throws ClassNotFoundException 
	 */
	@SuppressWarnings("unchecked")
	public static <T> T newInstance(String type, Object... args)
			throws InstantiationException, IllegalAccessException, IllegalArgumentException,
			InvocationTargetException, NoSuchMethodException, SecurityException, ClassNotFoundException {
		final Class<?> t = forName(type);
		return (T) newInstance(t, args);
	}

	/**
	 * Find the given type.
	 *
	 * @param name the name of the type. 
	 * @return the class.
	 * @throws ClassNotFoundException 
	 */
	public static Class<?> forName(String name) throws ClassNotFoundException {
		try {
			return Class.forName(name);
		} catch (Exception exception) {
			//
		}
		BundleContext context = TestPluginActivator.context;
		if (context != null) {
			for (Bundle b : context.getBundles()) {
				try {
					return b.loadClass(name);
				} catch (ClassNotFoundException e) {
					// No problem, this bundle doesn't have the class
				}
			}
		}
		throw new ClassNotFoundException(name);
	}

	/**
	 * Invokes the first accessible method defined on the receiver'c class with the given name and
	 * without parameter.
	 * 
	 * @param receiver the method call receiver, not {@code null}
	 * @param methodName the method name, not {@code null}
	 * @return the result of the method invocation. {@code null} if the method was of type void.
	 * @throws SecurityException 
	 * @throws IllegalArgumentException 
	 * @throws IllegalAccessException 
	 * @throws InvocationTargetException 
	 * @throws NoSuchMethodException 
	 */
	public static Object invoke(Object receiver, String methodName) throws SecurityException, IllegalArgumentException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		assert receiver != null;
		assert methodName != null;

		Class<? extends Object> clazz = receiver.getClass();
		Method compatible = null;
		do {
			for (Method candidate : clazz.getDeclaredMethods()) {
				if (candidate != null && !candidate.isBridge() && Objects.equal(methodName, candidate.getName())
						&& candidate.getParameterCount() == 0) {
					if (compatible != null) 
						throw new IllegalStateException("Ambiguous methods to invoke. Both " + compatible + " and " + candidate + " would be compatible choices."); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					compatible = candidate;
				}
			}
		} while(compatible == null && (clazz = clazz.getSuperclass()) != null);
		if (compatible != null) {
			compatible.setAccessible(true);
			return compatible.invoke(receiver);
		}
		// not found provoke method not found exception
		Method method = receiver.getClass().getMethod(methodName);
		return method.invoke(receiver);
	}

	/**
	 * Invokes the first accessible method defined on the receiver'c class with the given name and
	 * a parameter list compatible to the given arguments.
	 * 
	 * @param receiver the method call receiver, not {@code null}
	 * @param methodName the method name, not {@code null}
	 * @param args the arguments.
	 * @return the result of the method invocation. {@code null} if the method was of type void.
	 * @throws Exception 
	 * @throws IllegalArgumentException 
	 * @throws IllegalAccessException 
	 * @throws InvocationTargetException 
	 * @throws NoSuchMethodException 
	 */
	public static Object invoke(Object receiver, String methodName, Object... args) throws Exception, IllegalArgumentException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		assert receiver != null;
		return invoke(receiver, receiver.getClass(), methodName, args);
	}

	/**
	 * Invokes the first accessible method defined on the receiver'c class with the given name and
	 * a parameter list compatible to the given arguments.
	 * 
	 * @param receiver the method call receiver, may be {@code null}
	 * @param receiverType the type of the receiver, not {@code null}
	 * @param methodName the method name, not {@code null}
	 * @param args the arguments.
	 * @return the result of the method invocation. {@code null} if the method was of type void.
	 * @throws Exception 
	 * @throws IllegalArgumentException 
	 * @throws IllegalAccessException 
	 * @throws InvocationTargetException 
	 * @throws NoSuchMethodException 
	 * @since 0.11
	 */
	public static Object invoke(Object receiver, Class<?> receiverType, String methodName, Object... args) throws Exception, IllegalArgumentException, IllegalAccessException, InvocationTargetException, NoSuchMethodException {
		assert receiverType != null;
		assert receiver == null || receiverType.isInstance(receiver);
		assert methodName != null;

		final Object[] arguments;
		if (args == null) {
			arguments = new Object[] {null};
		} else {
			arguments = args;
		}

		Class<? extends Object> clazz = receiverType;
		Method compatible = null;
		do {
			for (Method candidate : clazz.getDeclaredMethods()) {
				if (candidate != null && !candidate.isBridge() && Objects.equal(methodName, candidate.getName())
						&& isValidArgs(candidate.isVarArgs(), arguments, candidate.getParameterTypes())) {
					if (compatible != null) 
						throw new IllegalStateException("Ambiguous methods to invoke. Both "+compatible+" and  "+candidate+" would be compatible choices."); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					compatible = candidate;
				}
			}
		} while(compatible == null && (clazz = clazz.getSuperclass()) != null);
		if (compatible != null) {
			compatible.setAccessible(true);
			if (compatible.isVarArgs()) {
				Object[] newArgs = new Object[compatible.getParameterCount()];
				for (int i = 0; i < compatible.getParameterCount() - 1; ++i) {
					newArgs[i] = arguments[i];
				}
				Class<?> componentType = compatible.getParameterTypes()[compatible.getParameterCount() - 1].getComponentType();
				int varArgsLength = arguments.length - compatible.getParameterCount() + 1;
				Object varArgs = Array.newInstance(componentType, varArgsLength);
				for (int i = 0; i < varArgsLength; ++i) {
					Array.set(varArgs, i, arguments[i + compatible.getParameterCount() - 1]);
				}
				newArgs[compatible.getParameterCount() - 1] = varArgs;
				return compatible.invoke(compatible.getDeclaringClass().cast(receiver), newArgs);
			}
			return compatible.invoke(compatible.getDeclaringClass().cast(receiver), arguments);
		}
		// not found provoke method not found exception
		Method method = receiverType.getMethod(methodName);
		return method.invoke(receiver);
	}

	/**
	 * Invokes the first accessible method defined on the receiver'c class with the given name and
	 * the given parameter list.
	 * 
	 * @param receiverType the type of the receiver.
	 * @param receiver the instance on which the method should be called.
	 * @param returnType the type of the return value.
	 * @param methodName the name of the method to invoke.
	 * @param parameterTypes the types of the parameters.
	 * @param arguments the arguments' values.
	 * @return the value replied by the invoked function.
	 * @throws Exception 
	 * @since 0.11
	 */
	public static <R> R invokeFunc(Class<?> receiverType, Object receiver, Class<R> returnType,
			String methodName, Class<?>[] parameterTypes, Object... arguments) throws Exception {
		Class<?> type = receiverType;
		Method method = null;
		while (type != null && method == null) {
			try {
				method = type.getDeclaredMethod(methodName, parameterTypes);
			} catch (Throwable exception) {
				method = null;
			}
			type = type.getSuperclass();
		}
		if (method != null) {
			method.setAccessible(true);
			try {
				final Object result = method.invoke(receiver, arguments);
				if (result == null) {
					return null;
				}
				return returnType.cast(result);
			} catch (InvocationTargetException internalException) {
				Throwable ex = Throwables.getRootCause(internalException);
				if (ex instanceof RuntimeException) {
					throw (RuntimeException) ex;
				} else if (ex instanceof Error) {
					throw (Error) ex;
				} else if (ex != null) {
					throw new Error(ex);
				}
				throw internalException;
			}
		}
		throw new NoSuchMethodError(methodName);
	}

	/**
	 * Invokes the first accessible method defined on the receiver'c class with the given name and
	 * without parameter.
	 *
	 * @param <R> the type of the return type.
	 * @param receiverType the type of the receiver.
	 * @param receiver the instance on which the method should be called.
	 * @param returnType the type of the return value.
	 * @param methodName the name of the method to invoke.
	 * @return the value replied by the invoked function.
	 * @throws Exception 
	 * @since 0.11
	 */
	public static <R> R invokeFunc(Class<?> receiverType, Object receiver, Class<R> returnType,
			String methodName) throws Exception {
		Class<?> type = receiverType;
		Method method = null;
		while (type != null && method == null) {
			try {
				method = type.getDeclaredMethod(methodName);
			} catch (Throwable exception) {
				method = null;
			}
			type = type.getSuperclass();
		}
		if (method != null) {
			method.setAccessible(true);
			try {
				final Object result = method.invoke(receiver);
				if (result == null) {
					return null;
				}
				return returnType.cast(result);
			} catch (InvocationTargetException internalException) {
				Throwable ex = Throwables.getRootCause(internalException);
				if (ex instanceof RuntimeException) {
					throw (RuntimeException) ex;
				} else if (ex instanceof Error) {
					throw (Error) ex;
				} else if (ex != null) {
					throw new Error(ex);
				}
				throw internalException;
			}
		}
		throw new NoSuchMethodError(methodName);
	}

	/**
	 * Invokes the first accessible method defined on the receiver'c class with the given name and
	 * the given parameter list.
	 * 
	 * @param receiverType the type of the receiver.
	 * @param receiver the instance on which the method should be called.
	 * @param methodName the name of the method to invoke.
	 * @param parameterTypes the types of the parameters.
	 * @param arguments the arguments' values.
	 * @throws Exception 
	 * @since 0.11
	 */
	public static void invokeProc(Class<?> receiverType, Object receiver,
			String methodName, Class<?>[] parameterTypes, Object... arguments) throws Exception {
		Class<?> type = receiverType;
		Method method = null;
		while (type != null && method == null) {
			try {
				method = type.getDeclaredMethod(methodName, parameterTypes);
			} catch (Throwable exception) {
				method = null;
			}
			type = type.getSuperclass();
		}
		if (method != null) {
			method.setAccessible(true);
			try {
				method.invoke(receiver, arguments);
			} catch (InvocationTargetException internalException) {
				Throwable ex = Throwables.getRootCause(internalException);
				if (ex instanceof RuntimeException) {
					throw (RuntimeException) ex;
				} else if (ex instanceof Error) {
					throw (Error) ex;
				} else if (ex != null) {
					throw new Error(ex);
				}
				throw internalException;
			}
		} else {
			throw new NoSuchMethodError(methodName);
		}
	}

	/**
	 * Invokes the first accessible method defined on the receiver'c class with the given name and
	 * without parameter list.
	 * 
	 * @param receiverType the type of the receiver.
	 * @param receiver the instance on which the method should be called.
	 * @param methodName the name of the method to invoke.
	 * @throws Exception 
	 * @since 0.11
	 */
	public static void invokeProc(Class<?> receiverType, Object receiver,
			String methodName) throws Exception {
		Class<?> type = receiverType;
		Method method = null;
		while (type != null && method == null) {
			try {
				method = type.getDeclaredMethod(methodName);
			} catch (Throwable exception) {
				method = null;
			}
			type = type.getSuperclass();
		}
		if (method != null) {
			method.setAccessible(true);
			try {
				method.invoke(receiver);
			} catch (InvocationTargetException internalException) {
				Throwable ex = Throwables.getRootCause(internalException);
				if (ex instanceof RuntimeException) {
					throw (RuntimeException) ex;
				} else if (ex instanceof Error) {
					throw (Error) ex;
				} else if (ex != null) {
					throw new Error(ex);
				}
				throw internalException;
			}
		} else {
			throw new NoSuchMethodError(methodName);
		}
	}

	private static boolean isValidArgs(boolean varargs, Object[] args, Class<?>[] params) {
		for (int i = 0; i < args.length; ++i) {
			if (i >= params.length) {
				return false;
			}
			if (args[i] == null) {
				if (params[i].isPrimitive()) {
					return false;
				}
			} else if ((!(params[i].isInstance(args[i]))) && varargs && i == params.length - 1) {
				Class<?> componentType = params[i].getComponentType();

				Class<?>[] newParams = new Class[args.length - params.length + 1];
				for (int j = 0; j < newParams.length; ++j) {
					newParams[j] = componentType;
				}

				Object[] newArgs = new Object[newParams.length];
				for (int j = 0; j < newArgs.length; ++j, ++i) {
					newArgs[j] = args[i];
				}

				return isValidArgs(false, newArgs, newParams);
			} else if (!(params[i].isInstance(args[i]))) {
				if (Primitives.isPrimitiveOrWrapper(params[i])) {
					if (!Objects.equal(
							Primitives.primitiveTypeOf(params[i]),
							Primitives.primitiveTypeOf(args[i].getClass()))) {
						return false;
					}
				} else {
					return false;
				}
			}
		}
		return true;
	}

	/** Run a command-line program with a call to the {@code main} Java function.
	 *
	 * @param className the name of the class to launch. This class must contain the {@code main} function.
	 * @param data the data to pass to the program as the content of the standard input stream. It may be {@code null}.
	 * @param arguments the command-line arguments to pass to program.
	 * @return the standard output stream.
	 * @throws Exception if there is some issue when running the program.
	 * @see #runRun(String, String, String...)
	 * @see #runInternal(String, String, String, String...)
	 * @since 0.13
	 */
	public static String runMain(String className, String data, String... arguments) throws Exception {
		return runInternal("main", className, data, arguments); //$NON-NLS-1$
	}

	/** Run a command-line program with a call to the {@code run} Java function that has the same
	 * prototype as the {@code main} standard Java. The {@code run} function is assumed to never
	 * invoked the {@link System#exit(int)} function.
	 *
	 * @param className the name of the class to launch. This class must contain the {@code main} function.
	 * @param data the data to pass to the program as the content of the standard input stream. It may be {@code null}.
	 * @param arguments the command-line arguments to pass to program.
	 * @return the standard output stream.
	 * @throws Exception if there is some issue when running the program.
	 * @see #runMain(String, String, String...)
	 * @see #runInternal(String, String, String, String...)
	 * @since 0.13
	 */
	public static String runRun(String className, String data, String... arguments) throws Exception {
		return runInternal("run", className, data, arguments); //$NON-NLS-1$
	}

	/** Run a command-line program with a call to the Java function with the given name that has the same
	 * prototype as the {@code main} standard Java.
	 *
	 * @param functionName the name of the static function to be invoked.
	 * @param className the name of the class to launch. This class must contain the {@code main} function.
	 * @param data the data to pass to the program as the content of the standard input stream. It may be {@code null}.
	 * @param arguments the command-line arguments to pass to program.
	 * @return the standard output stream.
	 * @throws Exception if there is some issue when running the program.
	 * @see #runMain(String, String, String...)
	 * @see #runRun(String, String, String...)
     * @since 0.13
	 */
	@SuppressWarnings("resource")
	public static String runInternal(String functionName, String className, String data, String... arguments) throws Exception {
		String result = null;
		try (final ByteArrayOutputStream stdout = new ByteArrayOutputStream()) {
			try (final ByteArrayOutputStream stderr = new ByteArrayOutputStream()) {
				final byte[] inputBytes = data == null ? new byte[0] : data.getBytes("UTF-8"); //$NON-NLS-1$
				try (final InputStream input = new ByteArrayInputStream(inputBytes)) {
					final PrintStream console = System.out;
					final PrintStream errConsole = System.err;
					final InputStream old = System.in;
					final PrintStream ps0 = new PrintStream(stdout);
					final PrintStream ps1 = new PrintStream(stderr);
					try {
						System.setOut(ps0);
						System.setErr(ps1);
						System.setIn(input);
						final Class<?> cls = Class.forName(className);
						final Method meth = cls.getDeclaredMethod(functionName, String[].class);
						meth.invoke(null, (Object) arguments);
					} finally {
						ps0.flush();
						ps0.close();
						ps1.flush();
						ps1.close();
						result = stdout.toString("UTF-8") + stderr.toString("UTF-8"); //$NON-NLS-1$ //$NON-NLS-2$
						System.setOut(console);
						System.setErr(errConsole);
						System.setIn(old);
					}
				}
			}
		}
		return result;
	}

}

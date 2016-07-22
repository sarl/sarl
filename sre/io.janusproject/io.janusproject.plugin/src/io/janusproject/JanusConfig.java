/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject;

import java.util.Properties;

import io.janusproject.modules.StandardJanusPlatformModule;
import io.janusproject.modules.hazelcast.HazelcastKernelLoggerFactory;

/**
 * Constants for the Janus configuration.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class JanusConfig {

	/**
	 * Name of the property that contains the logger factory for hazelcast.
	 * 
	 * @see #HAZELCAST_LOGGER_FACTORY_VALUE
	 */
	public static final String HAZELCAST_LOGGER_FACTORY_NAME = "hazelcast.logging.class"; //$NON-NLS-1$

	/**
	 * The default name of the hazelcast logger factory of Janus.
	 *
	 * @see #HAZELCAST_LOGGER_FACTORY_NAME
	 */

	public static final String HAZELCAST_LOGGER_FACTORY_VALUE = HazelcastKernelLoggerFactory.class.getName();

	/**
	 * Name of the property that contains the verbosity level of Janus.
	 *
	 * @see #VERBOSE_LEVEL_VALUE
	 */
	public static final String VERBOSE_LEVEL_NAME = "janus.verbose.level"; //$NON-NLS-1$

	/**
	 * The default verbosity level of Janus.
	 *
	 * @see #VERBOSE_LEVEL_NAME
	 */
	public static final String VERBOSE_LEVEL_VALUE = "info"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the flag for showing the Janus logo.
	 *
	 * @see #JANUS_LOGO_SHOW
	 */
	public static final String JANUS_LOGO_SHOW_NAME = "janus.logo.show"; //$NON-NLS-1$

	/**
	 * The default value of the flag for showing the Janus logo.
	 *
	 * @see #JANUS_LOGO_SHOW_NAME
	 */
	public static final Boolean JANUS_LOGO_SHOW = Boolean.TRUE;

	/**
	 * Name of the property that contains the verbosity level of Janus.
	 *
	 * @see #LOGGING_PROPERTY_FILE_VALUE
	 */
	public static final String LOGGING_PROPERTY_FILE_NAME = "java.util.logging.config.file"; //$NON-NLS-1$

	/**
	 * The default name of the logging property file of Janus.
	 *
	 * @see #LOGGING_PROPERTY_FILE_NAME
	 */
	public static final String LOGGING_PROPERTY_FILE_VALUE = "resource:io/janusproject/logging.properties"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the identifier of the Janus context.
	 *
	 * @see #DEFAULT_CONTEXT_ID_VALUE
	 */
	public static final String DEFAULT_CONTEXT_ID_NAME = "janus.context.id"; //$NON-NLS-1$

	/**
	 * The default value for the Janus context identifier.
	 *
	 * @see #DEFAULT_CONTEXT_ID_NAME
	 */
	public static final String DEFAULT_CONTEXT_ID_VALUE = "2c38fb7f-f363-4f6e-877b-110b1f07cc77"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the identifier for the default space of the Janus context.
	 *
	 * @see #DEFAULT_SPACE_ID_VALUE
	 */
	public static final String DEFAULT_SPACE_ID_NAME = "janus.context.space.id"; //$NON-NLS-1$

	/**
	 * The default value for the Janus space identifier.
	 *
	 * @see #DEFAULT_SPACE_ID_NAME
	 */
	public static final String DEFAULT_SPACE_ID_VALUE = "7ba8885d-545b-445a-a0e9-b655bc15ebe0"; //$NON-NLS-1$

	/**
	 * Name of the property that indicates if the ID of the default context must be randomly computed at boot time, or not.
	 *
	 * @see #DEFAULT_CONTEXT_ID_NAME
	 * @see #RANDOM_DEFAULT_CONTEXT_ID_VALUE
	 */
	public static final String RANDOM_DEFAULT_CONTEXT_ID_NAME = "janus.context.id.random"; //$NON-NLS-1$

	/**
	 * Indicates if the default context id has a random value or not at each boot time.
	 *
	 * @see #RANDOM_DEFAULT_CONTEXT_ID_NAME
	 */
	public static final Boolean RANDOM_DEFAULT_CONTEXT_ID_VALUE = Boolean.FALSE;

	/**
	 * Name of the property that indicates if the ID of the default context must be computed from the boot agent type, or not.
	 *
	 * @see #DEFAULT_CONTEXT_ID_NAME
	 * @see #DEFAULT_CONTEXT_ID_VALUE
	 */
	public static final String BOOT_DEFAULT_CONTEXT_ID_NAME = "janus.context.id.boot"; //$NON-NLS-1$

	/**
	 * Indicates if the default context id has a value computed from the boot agent type.
	 *
	 * @see #BOOT_DEFAULT_CONTEXT_ID_NAME
	 */
	public static final Boolean BOOT_DEFAULT_CONTEXT_ID_VALUE = Boolean.FALSE;

	/**
	 * Name of the property that contains the boolean value for offline/online.
	 */
	public static final String OFFLINE = "janus.network.offline"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the classname of the boot agent.
	 */
	public static final String BOOT_AGENT = "janus.boot.agent"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the identifier of the boot agent.
	 */
	public static final String BOOT_AGENT_ID = "janus.boot.agent.id"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the public network URI.
	 */
	public static final String PUB_URI = "network.pub.uri"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the maximal number of threads in the pool.
	 *
	 * @see #MAX_NUMBER_OF_THREADS_IN_EXECUTOR_VALUE
	 */
	public static final String MAX_NUMBER_OF_THREADS_IN_EXECUTOR_NAME = "janus.executors.threads.max"; //$NON-NLS-1$

	/**
	 * Indicates the maximal number of threads to keep in the pool, even if they are idle, unless {@code allowCoreThreadTimeOut}
	 * is set.
	 *
	 * @see #MAX_NUMBER_OF_THREADS_IN_EXECUTOR_NAME
	 */
	public static final int MAX_NUMBER_OF_THREADS_IN_EXECUTOR_VALUE = 50;

	/**
	 * Name of the property that contains the numbers of seconds that the kernel is waiting for thread terminations before
	 * timeout.
	 *
	 * @see #KERNEL_THREAD_TIMEOUT_VALUE
	 */
	public static final String KERNEL_THREAD_TIMEOUT_NAME = "janus.executors.timeout"; //$NON-NLS-1$

	/**
	 * Indicates the numbers of seconds that the kernel is waiting for thread terminations before timeout.
	 *
	 * @see #KERNEL_THREAD_TIMEOUT_NAME
	 */
	public static final int KERNEL_THREAD_TIMEOUT_VALUE = 30;

	/**
	 * Name of the property that contains the injection module.
	 *
	 * @see #INJECTION_MODULE_NAME_VALUE
	 */
	public static final String INJECTION_MODULE_NAME = "janus.injection.module"; //$NON-NLS-1$

	/**
	 * The default name of the injection module.
	 *
	 * @see #INJECTION_MODULE_NAME
	 */
	public static final String INJECTION_MODULE_NAME_VALUE = StandardJanusPlatformModule.class.getName();

	/**
	 * Name of the property that contains the numbers of seconds between two purges of the terminated threads by the kernel.
	 *
	 * @see #KERNEL_THREAD_PURGE_DELAY_VALUE
	 */
	public static final String KERNEL_THREAD_PURGE_DELAY_NAME = "janus.executors.purgeDelay"; //$NON-NLS-1$

	/**
	 * Indicates the numbers of seconds between two purges of the terminated threads by the kernel.
	 *
	 * @see #KERNEL_THREAD_PURGE_DELAY_NAME
	 */
	public static final int KERNEL_THREAD_PURGE_DELAY_VALUE = 30;

	private JanusConfig() {
		//
	}

	/**
	 * Replies the default values for the properties supported by Janus config.
	 *
	 * @param defaultValues - filled with the default values supported by the Janus platform.
	 */
	public static void getDefaultValues(Properties defaultValues) {
		defaultValues.put(BOOT_AGENT, ""); //$NON-NLS-1$
		defaultValues.put(BOOT_AGENT_ID, ""); //$NON-NLS-1$
		defaultValues.put(BOOT_DEFAULT_CONTEXT_ID_NAME, BOOT_DEFAULT_CONTEXT_ID_VALUE.toString());
		defaultValues.put(DEFAULT_CONTEXT_ID_NAME, DEFAULT_CONTEXT_ID_VALUE);
		defaultValues.put(DEFAULT_SPACE_ID_NAME, DEFAULT_SPACE_ID_VALUE);
		defaultValues.put(OFFLINE, Boolean.FALSE.toString());
		defaultValues.put(PUB_URI, ""); //$NON-NLS-1$
		defaultValues.put(RANDOM_DEFAULT_CONTEXT_ID_NAME, RANDOM_DEFAULT_CONTEXT_ID_VALUE.toString());
		defaultValues.put(VERBOSE_LEVEL_NAME, VERBOSE_LEVEL_VALUE);
		defaultValues.put(LOGGING_PROPERTY_FILE_NAME, LOGGING_PROPERTY_FILE_VALUE);
		defaultValues.put(HAZELCAST_LOGGER_FACTORY_NAME, HAZELCAST_LOGGER_FACTORY_VALUE);
		defaultValues.put(MAX_NUMBER_OF_THREADS_IN_EXECUTOR_NAME, Integer.toString(MAX_NUMBER_OF_THREADS_IN_EXECUTOR_VALUE));
		defaultValues.put(KERNEL_THREAD_TIMEOUT_NAME, Integer.toString(KERNEL_THREAD_TIMEOUT_VALUE));
		defaultValues.put(KERNEL_THREAD_PURGE_DELAY_NAME, Integer.toString(KERNEL_THREAD_PURGE_DELAY_VALUE));
		defaultValues.put(INJECTION_MODULE_NAME, INJECTION_MODULE_NAME_VALUE);
		defaultValues.put(JANUS_LOGO_SHOW_NAME, JANUS_LOGO_SHOW.toString());
	}

	/**
	 * Replies the value of the system property.
	 *
	 * @param name - name of the property.
	 * @return the value, or <code>null</code> if no property found.
	 */
	public static String getSystemProperty(String name) {
		return getSystemProperty(name, null);
	}

	/**
	 * Replies the value of the system property.
	 *
	 * @param name - name of the property.
	 * @param defaultValue - value to reply if the these is no property found
	 * @return the value, or defaultValue.
	 */
	public static String getSystemProperty(String name, String defaultValue) {
		String value;
		value = System.getProperty(name, null);
		if (value != null) {
			return value;
		}
		value = System.getenv(name);
		if (value != null) {
			return value;
		}
		return defaultValue;
	}

	/**
	 * Replies the value of the boolean system property.
	 *
	 * @param name - name of the property.
	 * @return the value, or <code>false</code> if no property found.
	 */
	public static boolean getSystemPropertyAsBoolean(String name) {
		return getSystemPropertyAsBoolean(name, false);
	}

	/**
	 * Replies the value of the boolean system property.
	 *
	 * @param name - name of the property.
	 * @param defaultValue - value to reply if the these is no property found
	 * @return the value, or defaultValue.
	 */
	public static boolean getSystemPropertyAsBoolean(String name, boolean defaultValue) {
		String value = getSystemProperty(name, null);
		if (value != null) {
			try {
				return Boolean.parseBoolean(value);
			} catch (Throwable exception) {
				//
			}
		}
		return defaultValue;
	}

	/**
	 * Replies the value of the integer system property.
	 *
	 * @param name - name of the property.
	 * @return the value, or <code>0</code> if no property found.
	 */
	public static int getSystemPropertyAsInteger(String name) {
		return getSystemPropertyAsInteger(name, 0);
	}

	/**
	 * Replies the value of the integer system property.
	 *
	 * @param name - name of the property.
	 * @param defaultValue - value to reply if the these is no property found
	 * @return the value, or defaultValue.
	 */
	public static int getSystemPropertyAsInteger(String name, int defaultValue) {
		String value = getSystemProperty(name, null);
		if (value != null) {
			try {
				return Integer.parseInt(value);
			} catch (Throwable exception) {
				//
			}
		}
		return defaultValue;
	}

	/**
	 * Replies the value of the single precision floating point value system property.
	 *
	 * @param name - name of the property.
	 * @return the value, or <code>0</code> if no property found.
	 */
	public static float getSystemPropertyAsFloat(String name) {
		return getSystemPropertyAsFloat(name, 0f);
	}

	/**
	 * Replies the value of the single precision floating point value system property.
	 *
	 * @param name - name of the property.
	 * @param defaultValue - value to reply if the these is no property found
	 * @return the value, or defaultValue.
	 */
	public static float getSystemPropertyAsFloat(String name, float defaultValue) {
		String value = getSystemProperty(name, null);
		if (value != null) {
			try {
				return Float.parseFloat(value);
			} catch (Throwable exception) {
				//
			}
		}
		return defaultValue;
	}

	/**
	 * Replies the value of the enumeration system property.
	 *
	 * @param <S> - type of the enumeration to read.
	 * @param type - type of the enumeration.
	 * @param name - name of the property.
	 * @return the value, or <code>null</code> if no property found.
	 */
	public static <S extends Enum<S>> S getSystemPropertyAsEnum(Class<S> type, String name) {
		return getSystemPropertyAsEnum(type, name, null);
	}

	/**
	 * Replies the value of the integer system property.
	 *
	 * @param <S> - type of the enumeration to read.
	 * @param type - type of the enumeration.
	 * @param name - name of the property.
	 * @param defaultValue - value to reply if the these is no property found
	 * @return the value, or defaultValue.
	 */
	public static <S extends Enum<S>> S getSystemPropertyAsEnum(Class<S> type, String name, S defaultValue) {
		String value = getSystemProperty(name, null);
		if (value != null) {
			try {
				S enumeration = Enum.valueOf(type, value);
				if (enumeration != null) {
					return enumeration;
				}
			} catch (Throwable exception) {
				//
			}
			try {
				int ordinal = Integer.parseInt(value);
				S enumeration = type.getEnumConstants()[ordinal];
				if (enumeration != null) {
					return enumeration;
				}
			} catch (Throwable exception) {
				//
			}
		}
		return defaultValue;
	}

	/**
	 * Replies the value of the type system property.
	 *
	 * @param <S> - type to reply.
	 * @param type - type to reply.
	 * @param name - name of the property.
	 * @return the type, or <code>null</code> if no property found.
	 */
	public static <S> Class<? extends S> getSystemPropertyAsClass(Class<S> type, String name) {
		return getSystemPropertyAsClass(type, name, (Class<S>) null);
	}

	/**
	 * Replies the value of the type system property.
	 *
	 * @param <S> - type to reply.
	 * @param type - type to reply.
	 * @param name - name of the property.
	 * @param defaultValue - value to reply if the these is no property found
	 * @return the value, or defaultValue.
	 */
	public static <S> Class<? extends S> getSystemPropertyAsClass(Class<S> type, String name, Class<S> defaultValue) {
		String value = getSystemProperty(name, null);
		if (value != null) {
			try {
				Class<?> typeInstance = Class.forName(value);
				if (typeInstance != null) {
					return typeInstance.asSubclass(type);
				}
			} catch (Throwable exception) {
				//
			}
		}
		return defaultValue;
	}

	/**
	 * Replies the value of the type system property.
	 *
	 * @param name - name of the property.
	 * @return the type, or <code>null</code> if no property found.
	 */
	public static Class<?> getSystemPropertyAsClass(String name) {
		return getSystemPropertyAsClass(name, (Class<?>) null);
	}

	/**
	 * Replies the value of the type system property.
	 *
	 * @param name - name of the property.
	 * @param defaultValue - value to reply if the these is no property found
	 * @return the value, or defaultValue.
	 */
	public static Class<?> getSystemPropertyAsClass(String name, Class<?> defaultValue) {
		String value = getSystemProperty(name, null);
		if (value != null) {
			try {
				Class<?> typeInstance = Class.forName(value);
				if (typeInstance != null) {
					return typeInstance;
				}
			} catch (Throwable exception) {
				//
			}
		}
		return defaultValue;
	}

	/**
	 * Replies the value of the type system property.
	 *
	 * @param <S> - type to reply.
	 * @param type - type to reply.
	 * @param name - name of the property.
	 * @param defaultValue - value to reply if the these is no property found
	 * @return the value, or defaultValue.
	 */
	public static <S> Class<? extends S> getSystemPropertyAsClass(Class<S> type, String name, String defaultValue) {
		String value = getSystemProperty(name, null);
		if (value != null) {
			try {
				Class<?> typeInstance = Class.forName(value);
				if (typeInstance != null) {
					return typeInstance.asSubclass(type);
				}
			} catch (Throwable exception) {
				//
			}
		}
		if (defaultValue != null) {
			try {
				Class<?> typeInstance = Class.forName(defaultValue);
				if (typeInstance != null) {
					return typeInstance.asSubclass(type);
				}
			} catch (Throwable exception) {
				//
			}
		}
		return null;
	}

	/**
	 * Replies the value of the type system property.
	 *
	 * @param name - name of the property.
	 * @param defaultValue - value to reply if the these is no property found
	 * @return the value, or defaultValue.
	 */
	public static Class<?> getSystemPropertyAsClass(String name, String defaultValue) {
		String value = getSystemProperty(name, null);
		if (value != null) {
			try {
				Class<?> typeInstance = Class.forName(value);
				if (typeInstance != null) {
					return typeInstance;
				}
			} catch (Throwable exception) {
				//
			}
		}
		if (defaultValue != null) {
			try {
				Class<?> typeInstance = Class.forName(defaultValue);
				if (typeInstance != null) {
					return typeInstance;
				}
			} catch (Throwable exception) {
				//
			}
		}
		return null;
	}

}

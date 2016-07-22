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

package io.janusproject.services.network;

import java.nio.charset.Charset;
import java.util.Properties;

import com.google.common.base.Charsets;
import com.google.inject.name.Named;
import io.janusproject.JanusConfig;
import io.janusproject.modules.eventserial.NetworkEventModule;

/**
 * Public configuration properties for the network modules. Define the properties as required in your application module as a
 * {@link Named} annotation.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class NetworkConfig {

	/**
	 * Name of the property for the AES key.
	 */
	public static final String AES_KEY = "network.encrypter.aes.key"; //$NON-NLS-1$

	/**
	 * Name of the property for the classname of the serializer to use.
	 */
	public static final String SERIALIZER_CLASSNAME = "network.serializer.class"; //$NON-NLS-1$

	/**
	 * Name of the property for the classname of the encrypter to use.
	 */
	public static final String ENCRYPTER_CLASSNAME = "network.encrypter.class"; //$NON-NLS-1$

	/**
	 * Name of the property for charset that must be used for string encoding.
	 *
	 * @see #BYTE_ARRAY_STRING_CHARSET_VALUE
	 */
	public static final String BYTE_ARRAY_STRING_CHARSET_NAME = "network.serializer.charset"; //$NON-NLS-1$

	/**
	 * Charset that should be used for converting String to byte array or byte array to String.
	 *
	 * <p>
	 * This constant was introduced to enforce the values on different platforms.
	 *
	 * @see #BYTE_ARRAY_STRING_CHARSET_NAME
	 */
	public static final Charset BYTE_ARRAY_STRING_CHARSET_VALUE = Charsets.UTF_8;

	private static Charset currentStringEncoding;

	private NetworkConfig() {
		//
	}

	/**
	 * Replies the default values for the properties supported by Janus config.
	 *
	 * @param defaultValues - filled with the default values supported by the Janus platform.
	 */
	public static void getDefaultValues(Properties defaultValues) {
		NetworkEventModule.getDefaultValues(defaultValues);
		defaultValues.put(AES_KEY, ""); //$NON-NLS-1$
		defaultValues.put(BYTE_ARRAY_STRING_CHARSET_NAME, BYTE_ARRAY_STRING_CHARSET_VALUE.name());
	}

	/**
	 * Replies the charset that must be used for encoding the strings.
	 *
	 * @return the encoding charset.
	 */
	public static Charset getStringEncodingCharset() {
		if (currentStringEncoding == null) {
			String value = JanusConfig.getSystemProperty(BYTE_ARRAY_STRING_CHARSET_NAME, null);
			if (value != null) {
				try {
					currentStringEncoding = Charset.forName(value);
					if (currentStringEncoding == null) {
						currentStringEncoding = BYTE_ARRAY_STRING_CHARSET_VALUE;
					}
				} catch (Throwable exception) {
					currentStringEncoding = BYTE_ARRAY_STRING_CHARSET_VALUE;
				}
			} else {
				currentStringEncoding = BYTE_ARRAY_STRING_CHARSET_VALUE;
			}
		}
		return currentStringEncoding;
	}

}

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

package io.janusproject.modules;

import java.io.IOError;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.net.InetAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import com.google.common.base.Strings;
import com.google.inject.AbstractModule;
import com.google.inject.Key;
import com.google.inject.MembersInjector;
import com.google.inject.Provider;
import com.google.inject.Provides;
import com.google.inject.TypeLiteral;
import com.google.inject.matcher.Matchers;
import com.google.inject.name.Named;
import com.google.inject.name.Names;
import com.google.inject.spi.TypeEncounter;
import com.google.inject.spi.TypeListener;
import io.janusproject.JanusConfig;
import io.janusproject.services.network.NetworkUtil;
import io.janusproject.util.LoggerCreator;
import org.arakhne.afc.vmutil.FileSystem;

/**
 * The module configures the minimum requirements for the system variables.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BootModule extends AbstractModule {

	@Override
	protected void configure() {
		// Custom logger
		LoggerCreator.useJanusMessageFormat();
		bindListener(Matchers.any(), new LoggerMemberListener());

		// Bind the system properties.
		boolean foundPubUri = false;
		String name;
		for (Entry<Object, Object> entry : System.getProperties().entrySet()) {
			name = entry.getKey().toString();
			bind(Key.get(String.class, Names.named(name))).toInstance(entry.getValue().toString());
			if (JanusConfig.PUB_URI.equals(name)) {
				foundPubUri = true;
			}
		}

		// If the PUB_URI is already given as system property,
		// then it was already binded (with a property-based binder).
		// Otherwise, the PUB_URI should be binded here with a provider.
		if (!foundPubUri) {
			bind(Key.get(String.class, Names.named(JanusConfig.PUB_URI))).toProvider(PublicURIProvider.class);
		}
	}

	/**
	 * Create a context identifier.
	 *
	 * @return the contextID
	 */
	@Provides
	@Named(JanusConfig.DEFAULT_CONTEXT_ID_NAME)
	private static UUID getContextID() {
		String str = JanusConfig.getSystemProperty(JanusConfig.DEFAULT_CONTEXT_ID_NAME);
		if (Strings.isNullOrEmpty(str)) {
			Boolean v;

			// From boot agent type
			str = JanusConfig.getSystemProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME);
			if (Strings.isNullOrEmpty(str)) {
				v = JanusConfig.BOOT_DEFAULT_CONTEXT_ID_VALUE;
			} else {
				v = Boolean.valueOf(Boolean.parseBoolean(str));
			}
			if (v.booleanValue()) {
				String bootClassname = JanusConfig.getSystemProperty(JanusConfig.BOOT_AGENT);
				str = UUID.nameUUIDFromBytes(bootClassname.getBytes()).toString();
			} else {
				// Random
				str = JanusConfig.getSystemProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME);
				if (Strings.isNullOrEmpty(str)) {
					v = JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_VALUE;
				} else {
					v = Boolean.valueOf(Boolean.parseBoolean(str));
				}
				if (v.booleanValue()) {
					str = UUID.randomUUID().toString();
				} else {
					str = JanusConfig.DEFAULT_CONTEXT_ID_VALUE;
				}
			}

			// Force the global value of the property to prevent to re-generate the UUID at the next call.
			System.setProperty(JanusConfig.DEFAULT_CONTEXT_ID_NAME, str);
		}

		assert (!Strings.isNullOrEmpty(str));
		return UUID.fromString(str);
	}

	/**
	 * Construct a space identifier.
	 *
	 * @return the spaceID
	 */
	@Provides
	@Named(JanusConfig.DEFAULT_SPACE_ID_NAME)
	private static UUID getSpaceID() {
		String v = JanusConfig.getSystemProperty(JanusConfig.DEFAULT_SPACE_ID_NAME, JanusConfig.DEFAULT_SPACE_ID_VALUE);
		return UUID.fromString(v);
	}

	/**
	 * Inject the PUB_URI as a real {@link URI}.
	 * 
	 * @return the PUB_URI
	 */
	@Provides
	@Named(JanusConfig.PUB_URI)
	private static URI getPubURIAsURI() {
		String v = getPUBURIAsString();
		try {
			return NetworkUtil.toURI(v);
		} catch (URISyntaxException e) {
			throw new IOError(e);
		}
	}

	/**
	 * Extract the current value of the PUB_URI from the system's property or form the platform default value.
	 *
	 * @return the current PUB_URI
	 */
	private static String getPUBURIAsString() {
		String pubUri = JanusConfig.getSystemProperty(JanusConfig.PUB_URI);
		if (pubUri == null || pubUri.isEmpty()) {
			InetAddress a = NetworkUtil.getPrimaryAddress();
			if (a == null) {
				a = NetworkUtil.getLoopbackAddress();
			}
			if (a != null) {
				pubUri = NetworkUtil.toURI(a, -1).toString();
				System.setProperty(JanusConfig.PUB_URI, pubUri);
			}
		}
		return pubUri;
	}

	/**
	 * Provider of public URI for the network layer.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class PublicURIProvider implements Provider<String> {

		@SuppressWarnings("synthetic-access")
		@Override
		public String get() {
			return getPUBURIAsString();
		}

	}

	/**
	 * Provider of logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static final class LoggerMemberListener implements TypeListener {

		private final AtomicBoolean isInit = new AtomicBoolean(false);

		/**
		 * Construct.
		 */
		LoggerMemberListener() {
			//
		}

		private static void init() {
			String propertyFileName = JanusConfig.getSystemProperty(JanusConfig.LOGGING_PROPERTY_FILE_NAME,
					JanusConfig.LOGGING_PROPERTY_FILE_VALUE);
			if (propertyFileName != null && !propertyFileName.isEmpty()) {
				URL url = FileSystem.convertStringToURL(propertyFileName, true);
				if (url != null) {
					try (InputStream is = url.openStream()) {
						LogManager.getLogManager().readConfiguration(is);
					} catch (IOException e) {
						throw new IOError(e);
					}
				}
			}
		}

		@Override
		public <I> void hear(TypeLiteral<I> type, TypeEncounter<I> encounter) {
			for (Field field : type.getRawType().getDeclaredFields()) {
				if (field.getType() == Logger.class) {
					if (!this.isInit.getAndSet(true)) {
						init();
					}
					encounter.register(new LoggerMemberInjector<I>(field));
				}
			}
		}

	}

	/**
	 * Provider of logger.
	 *
	 * @param <T> the type of the type of the field.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static final class LoggerMemberInjector<T> implements MembersInjector<T> {

		private final Field field;

		/**
		 * Construct.
		 *
		 * @param field the field to inject.
		 */
		LoggerMemberInjector(Field field) {
			this.field = field;
		}

		@Override
		public void injectMembers(T instance) {
			Logger logger = LoggerCreator.createLogger(this.field.getDeclaringClass().getName());

			boolean accessible = this.field.isAccessible();
			try {
				this.field.setAccessible(true);
				this.field.set(instance, logger);
			} catch (IllegalArgumentException | IllegalAccessException e) {
				throw new RuntimeException(e);
			} finally {
				this.field.setAccessible(accessible);
			}
		}

	}

}

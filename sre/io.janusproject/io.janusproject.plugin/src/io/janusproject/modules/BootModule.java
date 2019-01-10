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

package io.janusproject.modules;

import java.io.IOError;
import java.net.InetAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Map.Entry;
import java.util.UUID;

import com.google.common.base.Strings;
import com.google.inject.AbstractModule;
import com.google.inject.Key;
import com.google.inject.Provider;
import com.google.inject.Provides;
import com.google.inject.name.Named;
import com.google.inject.name.Names;

import io.janusproject.JanusConfig;
import io.janusproject.services.network.NetworkUtil;

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
        // Bind the system properties.
        boolean foundPubUri = false;
        String name;
        for (final Entry<Object, Object> entry : System.getProperties().entrySet()) {
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
    public static UUID getContextID() {
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
                final String bootClassname = JanusConfig.getSystemProperty(JanusConfig.BOOT_AGENT);
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

        assert !Strings.isNullOrEmpty(str);
        return UUID.fromString(str);
    }

    /**
     * Construct a space identifier.
     *
     * @return the spaceID
     */
    @Provides
    @Named(JanusConfig.DEFAULT_SPACE_ID_NAME)
    public static UUID getSpaceID() {
        final String v = JanusConfig.getSystemProperty(JanusConfig.DEFAULT_SPACE_ID_NAME, JanusConfig.DEFAULT_SPACE_ID_VALUE);
        return UUID.fromString(v);
    }

    /**
     * Inject the PUB_URI as a real {@link URI}.
     *
     * @return the PUB_URI
     */
    @Provides
    @Named(JanusConfig.PUB_URI)
    public static URI getPubURIAsURI() {
        final String v = getPUBURIAsString();
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

}

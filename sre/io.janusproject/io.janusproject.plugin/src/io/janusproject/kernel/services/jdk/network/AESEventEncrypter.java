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

package io.janusproject.kernel.services.jdk.network;

import java.security.GeneralSecurityException;
import java.util.UUID;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import javax.inject.Named;

import com.google.inject.Inject;
import io.janusproject.services.network.AbstractEventEncrypter;
import io.janusproject.services.network.EventEnvelope;
import io.janusproject.services.network.NetworkConfig;
import org.arakhne.afc.vmutil.locale.Locale;

/**
 * Encrypts the {@link EventEnvelope} content using the AES algorithm.
 *
 * <p>
 * To define the key you need to specify the binding {@link NetworkConfig}.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:magicnumber")
public class AESEventEncrypter extends AbstractEventEncrypter {

	private static final String ALGORITHM = "AES/CBC/PKCS5Padding"; //$NON-NLS-1$

	private SecretKeySpec skeySpec;

	/**
	 * Change the encryption key.
	 *
	 * @param key - injected encryption key.
	 * @throws Exception - when the given key is invalid.
	 */
	@Inject
	public void setKey(@Named(NetworkConfig.AES_KEY) String key) throws Exception {
		byte[] raw = key.getBytes(NetworkConfig.getStringEncodingCharset());
		int keySize = raw.length;
		if ((keySize % 16) == 0 || (keySize % 24) == 0 || (keySize % 32) == 0) {
			this.skeySpec = new SecretKeySpec(raw, "AES"); //$NON-NLS-1$
			// this.cipher = Cipher.getInstance(ALGORITHM);
		} else {
			throw new IllegalArgumentException(Locale.getString("INVALID_KEY_SIZE")); //$NON-NLS-1$
		}

	}

	@Override
	public void encrypt(EventEnvelope envelope) throws Exception {
		assert (envelope != null) : "Parameter 'envelope' must not be null"; //$NON-NLS-1$

		Cipher cipher = Cipher.getInstance(ALGORITHM);
		cipher.init(Cipher.ENCRYPT_MODE, this.skeySpec, new IvParameterSpec(new byte[16]));

		envelope.setContextId(cipher.doFinal(envelope.getContextId()));
		envelope.setSpaceId(cipher.doFinal(envelope.getSpaceId()));
		envelope.setScope(cipher.doFinal(envelope.getScope()));
		envelope.setCustomHeaders(cipher.doFinal(envelope.getCustomHeaders()));
		envelope.setBody(cipher.doFinal(envelope.getBody()));
	}

	@Override
	public void decrypt(EventEnvelope envelope) throws Exception {
		assert (envelope != null) : "Parameter 'envelope' must not be null"; //$NON-NLS-1$

		Cipher cipher = Cipher.getInstance(ALGORITHM);
		cipher.init(Cipher.DECRYPT_MODE, this.skeySpec, new IvParameterSpec(new byte[16]));

		envelope.setContextId(cipher.doFinal(envelope.getContextId()));
		envelope.setSpaceId(cipher.doFinal(envelope.getSpaceId()));
		envelope.setScope(cipher.doFinal(envelope.getScope()));
		envelope.setCustomHeaders(cipher.doFinal(envelope.getCustomHeaders()));
		envelope.setBody(cipher.doFinal(envelope.getBody()));
	}

	@Override
	public byte[] encryptUUID(UUID uuid) {
		try {
			Cipher cipher = Cipher.getInstance(ALGORITHM);
			cipher.init(Cipher.ENCRYPT_MODE, this.skeySpec, new IvParameterSpec(new byte[16]));
			return cipher.doFinal(super.encryptUUID(uuid));
		} catch (GeneralSecurityException e) {
			throw new RuntimeException(e);
		}
	}

}

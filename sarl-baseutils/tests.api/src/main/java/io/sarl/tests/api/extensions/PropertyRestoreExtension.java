/**
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
package io.sarl.tests.api.extensions;

import java.util.Properties;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.XbaseGenerated;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

/**
 * JUnit 5 extension that is saving and restoring system properties.
 * 
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version tests.api 0.15.0 20250909-115746
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid tests.api
 * @since 0.11
 */
@SuppressWarnings("all")
public class PropertyRestoreExtension implements BeforeEachCallback, AfterEachCallback {

	private Properties savedProperties;

	/** Constructor.
	 */
	public PropertyRestoreExtension() {
		//
	}

	@Override
	public void beforeEach(final ExtensionContext context) throws Exception {
		final var props = System.getProperties();
		final var clone = props.clone();
		this.savedProperties = (Properties) clone;
	}

	@Override
	public void afterEach(final ExtensionContext context) throws Exception {
		final var sp = this.savedProperties;
		this.savedProperties = null;
		if ((sp != null)) {
			final var props = System.getProperties();
			props.clear();
			props.putAll(sp);
		}
	}

	@Override
	@Pure
	public boolean equals(final Object obj) {
		return super.equals(obj);
	}

	@Override
	@Pure
	public int hashCode() {
		final var result = super.hashCode();
		return result;
	}

}

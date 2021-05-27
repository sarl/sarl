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

package io.sarl.pythongenerator.generator.generator;

import org.eclipse.osgi.util.NLS;

/** Localized Messages.
 *
 * @author <a href="http://www.ciad-lab.fr/author-10836/">St&eacute;phane Galland</a>
 * @version io.sarl.pythongenerator.generator 0.12.0 20210527-171007
 * @mavengroupid io.sarl.pythongenerator
 * @mavenartifactid io.sarl.pythongenerator.generator
 * @ExcludeFromApidoc
 */
@SuppressWarnings("all")
public final class Messages extends NLS {

	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages";

	public static String PyOutputConfigurationProvider_0;

	public static String PyExpressionGenerator_0;

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.pythongenerator.generator;

import io.sarl.lang.compilation.generator.extra.ExtraLanguageAppendable;
import io.sarl.lang.compilation.generator.extra.ExtraLanguageImportManager;
import io.sarl.lang.compilation.generator.extra.ExtraLanguageTypeConverter;

/** Appendable dedicated to Python.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class PyAppendable extends ExtraLanguageAppendable {

	private static final String INDENTATION = "\t"; //$NON-NLS-1$

	private static final String LINE_SEPARATOR = "\n"; //$NON-NLS-1$

	private static final char INNER_TYPE_SEPARATOR = '.';

	/** Constructor.
	 *
	 * @param converter the type converter.
	 */
	public PyAppendable(ExtraLanguageTypeConverter converter) {
		super(INDENTATION, LINE_SEPARATOR,
				new ExtraLanguageImportManager(converter, INNER_TYPE_SEPARATOR));
	}

}

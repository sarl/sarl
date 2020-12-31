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

import org.eclipse.xtext.common.types.JvmDeclaredType;

import io.sarl.lang.extralanguage.compiler.ExtraLanguageAppendable;
import io.sarl.lang.extralanguage.compiler.ExtraLanguageImportManager;
import io.sarl.lang.extralanguage.compiler.ExtraLanguageTypeConverter;

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
	 * @param thisType the current type for which the appendable should be created. If it is {@code null}, the import manager
	 *     of the appendable is not associated to a "this" type.
	 * @param converter the type converter.
	 */
	public PyAppendable(JvmDeclaredType thisType, ExtraLanguageTypeConverter converter) {
		super(INDENTATION, LINE_SEPARATOR,
				new ExtraLanguageImportManager(converter, thisType, INNER_TYPE_SEPARATOR));
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.extralanguage.compiler;

import org.eclipse.xtext.util.Strings;

/** Describes the result of a replacement.
 *
 * @param text the text.
 * @param conversion is the description of the conversion.
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.6
 */
public record ConversionResult(String text, Object[] conversion) {

	/** Replies if the replacement result is a simple feature renaming.
	 *
	 * @return {@code true} if the feature should be renamed, {@code false} for a complex replacement.
	 */
	public boolean isFeatureRenaming() {
		return !Strings.isEmpty(this.text);
	}

	/** Replies the complex conversion.
	 *
	 * <p>The replied value is an array of {@link CharSequence}, {@link org.eclipse.xtext.common.types.JvmType},
	 * {@link org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference}, or
	 * {@link org.eclipse.xtext.xbase.XExpression}.
	 *
	 * @return the complex conversion.
	 */
	public Object[] toComplexConversion() {
		return this.conversion;
	}

	@Override
	public String toString() {
		return this.text;
	}

}

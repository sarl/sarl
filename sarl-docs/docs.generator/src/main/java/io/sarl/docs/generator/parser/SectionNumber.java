/*
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

package io.sarl.docs.generator.parser;

import java.text.MessageFormat;
import java.util.LinkedList;

/** A number of section.
 *
 * @author $Author: sgalland$
 * @version docs.generator 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.generator
 * @since 0.6
 */
public class SectionNumber {

	/** The default format, compatible with {@link MessageFormat} for the section numbers.
	 */
	public static final String DEFAULT_SECTION_NUMBER_FORMAT = "{0}.{1}"; //$NON-NLS-1$

	private final LinkedList<Integer> numbers = new LinkedList<>();

	/** Constructor.
	 */
	public SectionNumber() {
		this.numbers.add(Integer.valueOf(0));
	}

	/** Change this version number from the given string representation.
	 *
	 * <p>The string representation should be integer numbers, separated by dot characters.
	 *
	 * @param sectionNumber the string representation of the version number.
	 * @param level is the level at which the section number is visible (1 for the top level, 2 for subsections...)
	 */
	public void setFromString(String sectionNumber, int level) {
		assert level >= 1;
		final var numbers = sectionNumber.split("[^0-9]+"); //$NON-NLS-1$
		final var len = Math.max(0, this.numbers.size() - numbers.length);
		for (var i = 0; i < len; ++i) {
			this.numbers.removeLast();
		}
		for (var i = 0; i < numbers.length && i < level; ++i) {
			this.numbers.addLast(Integer.valueOf(numbers[i]));
		}
	}

	/** Change this version number by incrementing the number for the given level.
	 *
	 * @param level is the level at which the section number is visible (1 for the top level, 2 for subsections...)
	 */
	public void increment(int level) {
		assert level >= 1;
		if (this.numbers.size() < level) {
			do {
				this.numbers.addLast(Integer.valueOf(0));
			} while (this.numbers.size() < level);
		} else if (this.numbers.size() > level) {
			do {
				this.numbers.removeLast();
			} while (this.numbers.size() > level);
		}
		assert this.numbers.size() == level;
		final var previousSection = this.numbers.removeLast().intValue();
		this.numbers.addLast(Integer.valueOf(previousSection + 1));
	}

	/** Format the section numbers.
	 *
	 * @return the formatted section number.
	 */
	@Override
	public String toString() {
		return toString(DEFAULT_SECTION_NUMBER_FORMAT);
	}

	/** Format the section numbers.
	 *
	 * @param format the format. It must be compatible with {@link MessageFormat}, with
	 *     the first parameter {@code {0}} equals to the first part of the full section number, and
	 *     the second parameter {@code {1}} equals to a single section number.
	 * @return the formatted section number.
	 */
	public String toString(String format) {
		String str = null;
		for (final var nb : this.numbers) {
			if (str == null) {
				str = nb.toString();
			} else {
				str = MessageFormat.format(format, str, nb.toString());
			}
		}
		return str;
	}

}

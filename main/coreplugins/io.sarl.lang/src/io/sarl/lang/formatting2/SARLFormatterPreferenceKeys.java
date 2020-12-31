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

package io.sarl.lang.formatting2;

import org.eclipse.xtend.core.formatting2.XtendFormatterPreferenceKeys;
import org.eclipse.xtext.preferences.BooleanKey;
import org.eclipse.xtext.xbase.formatting2.BlankLineKey;

/**
 * Preference keys for the SARL formatter.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */

public class SARLFormatterPreferenceKeys extends XtendFormatterPreferenceKeys {

	/** Blank line between inner class declarations.
	 */
	public static final BlankLineKey BLANK_LINES_BETWEEN_INNER_TYPES = new BlankLineKey(
			"blank.lines.between.inner.types", Integer.valueOf(1)); //$NON-NLS-1$

	/** Blank line between behavior units.
	 */
	public static final BlankLineKey BLANK_LINES_BETWEEN_BEHAVIOR_UNITS = new BlankLineKey(
			"blank.lines.between.behavior.units", Integer.valueOf(1)); //$NON-NLS-1$

	/** Blank line between capacity uses.
	 */
	public static final BlankLineKey BLANK_LINES_BETWEEN_CAPACITY_USES = new BlankLineKey(
			"blank.lines.between.capacity.uses", Integer.valueOf(0)); //$NON-NLS-1$

	/** Blank line between capacity requirements.
	 */
	public static final BlankLineKey BLANK_LINES_BETWEEN_CAPACITY_REQUIREMENTS = new BlankLineKey(
			"blank.lines.between.capacity.requirements", Integer.valueOf(0)); //$NON-NLS-1$

	/** Blank line between two types of members.
	 */
	public static final BlankLineKey BLANK_LINES_BETWEEN_MEMBER_CATEGORIES = new BlankLineKey(
			"blank.lines.between.member.categories", Integer.valueOf(1)); //$NON-NLS-1$

	/** Allow single-line expressions.
	 */
	public static final BooleanKey ENABLE_SINGLELINE_EXPRESSION = new BooleanKey(
			"enable.single.line.expressions", Boolean.valueOf(false)); //$NON-NLS-1$

}

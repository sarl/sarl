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

package io.sarl.lang.ui.extralanguage.properties;

import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;

/** Control wrapper that may be automatically considered in the option dialog.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public interface IExtraControlController {

	/** Get a preference value.
	 *
	 * @param key the preference key.
	 * @return the value.
	 */
	String getValue(String key);

	/** Notify the controller that a widget content changed.
	 *
	 * @param widget the changed widget.
	 */
	void controlChanged(Widget widget);

	/** Notify the controller that a preference has changed.
	 *
	 * @param preferenceKey the key of the preference.
	 * @param preferenceValue the new value for the preference entry.
	 */
	void controlChanged(String preferenceKey, String preferenceValue);

	/** Notify the controller that a text widget has changed.
	 *
	 * @param textControl the changed widget.
	 */
	void textChanged(Text textControl);

	/** Register the given preference key.
	 *
	 * @param key the key to be registered.
	 */
	void registerKey(String key);

}

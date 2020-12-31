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

package io.sarl.lang.ui.preferences;

import javax.inject.Inject;

import com.google.inject.Injector;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;

import io.sarl.lang.ui.internal.LangActivator;

/** Abstract implementation of an accessor to preferences.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public abstract class AbstractPreferenceAccess {

	private IPreferenceStoreAccess preferenceStoreAccess;

	private IPreferenceStore writePreferenceStore;

	/** Change the preference accessor.
	 *
	 * @param preferenceStoreAccess the accessor.
	 */
	@Inject
	public void setPreferenceStoreAccess(IPreferenceStoreAccess preferenceStoreAccess) {
		this.preferenceStoreAccess = preferenceStoreAccess;
	}

	/** Replies the preference accessor.
	 *
	 * @return the accessor.
	 */
	protected IPreferenceStoreAccess getPreferenceStoreAccess() {
		if (this.preferenceStoreAccess == null) {
			final Injector injector = LangActivator.getInstance().getInjector(LangActivator.IO_SARL_LANG_SARL);
			this.preferenceStoreAccess = injector.getInstance(IPreferenceStoreAccess.class);
		}
		return this.preferenceStoreAccess;
	}

	/** Replies the writable preference store to be used for the SARL editor.
	 *
	 * @param context the context (project, etc.) or {@code null} if none (global preferences).
	 * @return the modifiable preference store.
	 */
	public IPreferenceStore getWritablePreferenceStore(Object context) {
		if (this.writePreferenceStore == null) {
			this.writePreferenceStore = getPreferenceStoreAccess().getWritablePreferenceStore(context);
		}
		return this.writePreferenceStore;
	}

	/** Change the writable preference store to be used for the SARL editor.
	 *
	 * @param store the new store.
	 */
	public void setWritablePreferenceStore(IPreferenceStore store) {
		this.writePreferenceStore = store;
	}

	/** Set the values of the preferences to the default values.
	 *
	 * @param context the context (project, etc.) or {@code null} if none (global preferences).
	 */
	public abstract void setToDefault(Object context);

}

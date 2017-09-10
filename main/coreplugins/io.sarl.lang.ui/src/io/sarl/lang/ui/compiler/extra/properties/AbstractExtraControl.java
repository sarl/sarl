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

package io.sarl.lang.ui.compiler.extra.properties;

import java.lang.ref.WeakReference;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.Image;

/** Abstract implementation for the control wrappers that may be automatically considered in the optiona dialog.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public abstract class AbstractExtraControl implements IExtraControl {

	private final WeakReference<IExtraControlController> controller;

	private final String pluginID;

	private final Image languageImage;

	private final IPreferenceStore preferenceStore;

	/** Constructor.
	 *
	 * @param controller the controller.
	 * @param pluginID the identifier of the plugin.
	 * @param languageImage for the target language (16x16).
	 * @param preferenceStore the preference store to be used.
	 */
	public AbstractExtraControl(IExtraControlController controller, String pluginID, Image languageImage,
			IPreferenceStore preferenceStore) {
		this.controller = new WeakReference<>(controller);
		this.pluginID = pluginID;
		this.languageImage = languageImage;
		this.preferenceStore = preferenceStore;
	}

	/** Replies the controller of this widget.
	 *
	 * @return the controller.
	 */
	protected IExtraControlController getController() {
		return this.controller.get();
	}

	/** Register the given key.
	 *
	 * @param key the key to be registrered.
	 */
	protected void registerKey(String key) {
		getController().registerKey(key);
	}

	/** Replies the image associated to the extra language.
	 *
	 * @return the image.
	 */
	protected Image getLanguageImage() {
		return this.languageImage;
	}

	/** Replies the identifier of the plugin.
	 *
	 * @return the identifier.
	 */
	protected String getPluginID() {
		return this.pluginID;
	}

	/** Replies the preference store.
	 *
	 * @return the preference store.
	 */
	protected IPreferenceStore getPreferenceStore() {
		return this.preferenceStore;
	}

}

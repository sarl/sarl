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

package io.sarl.docs.doclet.utils;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import com.sun.tools.doclets.internal.toolkit.util.MessageRetriever;

import io.sarl.docs.doclet.SarlConfiguration;

/** Message retreiver for SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlMessageRetreiver extends MessageRetriever {

	/** Name of the bundle's property for the label of an agent type.
	 */
	public static final String DOCLET_AGENT = "doclet.Agent"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a behavior type.
	 */
	public static final String DOCLET_BEHAVIOR = "doclet.Behavior"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a event type.
	 */
	public static final String DOCLET_EVENT = "doclet.Event"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a capacity type.
	 */
	public static final String DOCLET_CAPACITY = "doclet.Capacity"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a skill type.
	 */
	public static final String DOCLET_SKILL = "doclet.Skill"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of a space type.
	 */
	public static final String DOCLET_SPACE = "doclet.Space"; //$NON-NLS-1$

	/** Name of the bundle's property for the label of an artifact type.
	 */
	public static final String DOCLET_ARTIFACT = "doclet.Artifact"; //$NON-NLS-1$

	private static final String STANDARD_RESOURCE = "com.sun.tools.doclets.formats.html.resources.standard"; //$NON-NLS-1$

	private static final String SARL_RESOURCE = SarlMessageRetreiver.class.getPackage()
			.getName().replaceAll("\\.", "/") + "/messages"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

	private ResourceBundle messages;

	/** Constructor.
	 *
	 * @param configuration the configuration.
	 */
	public SarlMessageRetreiver(SarlConfiguration configuration) {
		super(configuration, STANDARD_RESOURCE);
	}

	@Override
    public String getText(String key, Object... args) throws MissingResourceException {
    	if (this.messages == null) {
            try {
                this.messages = ResourceBundle.getBundle(SARL_RESOURCE);
            } catch (MissingResourceException e) {
                throw new Error("Fatal: Resource (" + SARL_RESOURCE + ") for javadoc doclets is missing."); //$NON-NLS-1$ //$NON-NLS-2$
            }
        }
    	if (this.messages.containsKey(key)) {
            final String message = this.messages.getString(key);
            return MessageFormat.format(message, args);
    	}
    	return super.getText(key, args);
    }

}

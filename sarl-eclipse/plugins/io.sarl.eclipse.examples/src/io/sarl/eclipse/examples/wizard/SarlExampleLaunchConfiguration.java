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

package io.sarl.eclipse.examples.wizard;

import static io.sarl.eclipse.examples.wizard.XmlUtils.readXmlAttribute;

import java.io.File;

import com.google.common.base.Strings;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure5;
import org.w3c.dom.Document;

/** Utilities related to the specification of launch configurations for examples.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public final class SarlExampleLaunchConfiguration {

	/** Name of the file that describes the launch configurations.
	 */
	public static final String LAUNCH_PROPERTY_FILE = "launch.xml"; //$NON-NLS-1$

	/** Name of the root tag into the launch configuration file.
	 */
	public static final String LAUNCH_PROPERTY_ROOT_TAG = "launchConfigurations"; //$NON-NLS-1$

	/** Name of the tag for agent launch configuration.
	 */
	public static final String LAUNCH_PROPERTY_AGENT_TAG = "agent"; //$NON-NLS-1$

	/** Name of the tag for application launch configuration.
	 */
	public static final String LAUNCH_PROPERTY_APPLICATION_TAG = "application"; //$NON-NLS-1$

	/** Name of the attribute for specify the type to be launched.
	 */
	public static final String LAUNCH_PROPERTY_TYPE_FIELD = "class"; //$NON-NLS-1$

	/** Name of the attribute for specify the name of the launch configuration.
	 */
	public static final String LAUNCH_PROPERTY_NAME_FIELD = "name"; //$NON-NLS-1$

	/** Name of the attribute for specify the log level of the launch configuration.
	 */
	public static final String LAUNCH_PROPERTY_LOGLEVEL_FIELD = "log"; //$NON-NLS-1$

	private SarlExampleLaunchConfiguration() {
		//
	}

	/** Parse the XML content for extracted information on launch configurations.
	 *
	 * @param document the document to parse.
	 * @param rootFolder the root folder to pass to the callback.
	 * @param callback the lambda that is called for each discovered launch configuration.
	 *     The first argument is the type to be launch. The second argument is the
	 *     name of the launch configuration. The third argument indicates
	 *     if it is a configuration for launching an agent ({@code true}) or an
	 *     application ({@code false}). And, the fourth argument is {@code rootFolder}.
	 */
	public static void readLaunchConfigurationFromXml(Document document, File rootFolder, Procedure5<String, String, Boolean, File, String> callback) {
		var nodes = document.getChildNodes();
		final var len = nodes.getLength();
		for (var i = 0; i < len; ++i) {
			var node = nodes.item(i);
			if (node != null) {
				if (LAUNCH_PROPERTY_ROOT_TAG.equalsIgnoreCase(node.getNodeName())) {
					final var logLevel = readXmlAttribute(node, LAUNCH_PROPERTY_LOGLEVEL_FIELD);
					nodes = node.getChildNodes();
					final var len2 = nodes.getLength();
					for (var j = 0; j < len2; ++j) {
						node = nodes.item(j);
						final var nodeName = node.getNodeName();
						final boolean isAgent;
						if (LAUNCH_PROPERTY_AGENT_TAG.equalsIgnoreCase(nodeName)) {
							isAgent = true;
						} else if (LAUNCH_PROPERTY_APPLICATION_TAG.equalsIgnoreCase(nodeName)) {
							isAgent = false;
						} else {
							continue;
						}
						final var type = readXmlAttribute(node, LAUNCH_PROPERTY_TYPE_FIELD);
						if (!Strings.isNullOrEmpty(type)) {
							callback.apply(type,
									readXmlAttribute(node, LAUNCH_PROPERTY_NAME_FIELD),
									Boolean.valueOf(isAgent),
									rootFolder,
									logLevel);
						}
					}
					// Break the loop
					return;
				}
			}
		}
	}

}

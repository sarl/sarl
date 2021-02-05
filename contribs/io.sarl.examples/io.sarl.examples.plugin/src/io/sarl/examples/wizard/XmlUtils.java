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

package io.sarl.examples.wizard;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.core.runtime.CoreException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import io.sarl.examples.SARLExamplePlugin;

/** Utilities related XML configurations.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public final class XmlUtils {

	private XmlUtils() {
		//
	}

	/** Read the XML content from the given file.
	 *
	 * @param jFile the file to read.
	 * @return the XML content.
	 * @throws CoreException if some error occurs.
	 */
	public static Document readXmlContent(File jFile) throws CoreException {
		try (InputStream stream = new FileInputStream(jFile)) {
			final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			final DocumentBuilder builder = factory.newDocumentBuilder();
			return builder.parse(stream);
		} catch (ParserConfigurationException | IOException | SAXException exception) {
			throw new CoreException(SARLExamplePlugin.createStatus(exception));
		}
	}

	/** Read the value from an XML attribute.
	 *
	 * @param xmlNode the XML node to read.
	 * @param attributeName the name of the attribute.
	 * @return the value of the attribute.
	 */
	public static String readXmlAttribute(Node xmlNode, String attributeName) {
		final NamedNodeMap attributes = xmlNode.getAttributes();
		final Node field = attributes.getNamedItem(attributeName);
		if (field != null) {
			return field.getTextContent();
		}
		return null;
	}

}

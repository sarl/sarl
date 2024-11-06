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

package io.sarl.eclipse.examples.wizard;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.core.runtime.CoreException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import io.sarl.eclipse.examples.SARLExamplePlugin;

/** Utilities related XML configurations.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.examples 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.examples
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
		try (var stream = new FileInputStream(jFile)) {
			final var factory = DocumentBuilderFactory.newInstance();
			final var builder = factory.newDocumentBuilder();
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
		final var attributes = xmlNode.getAttributes();
		final var field = attributes.getNamedItem(attributeName);
		if (field != null) {
			return field.getTextContent();
		}
		return null;
	}

}

/**
 * 
 */
package io.janusproject;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.jdt.core.IAccessRule;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.internal.compiler.util.GenericXMLWriter;
import org.eclipse.jdt.internal.core.ClasspathAccessRule;
import org.eclipse.jdt.internal.core.ClasspathAttribute;
import org.eclipse.jdt.internal.core.ClasspathEntry;
import org.eclipse.jdt.internal.core.ClasspathEntry.AssertionFailedException;
import org.eclipse.jdt.internal.core.JavaProject;
import org.eclipse.jdt.internal.core.util.Messages;
import org.eclipse.jdt.internal.core.util.Util;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * @author ngaud
 *
 */
public class JavaClasspathParser {

	static class UnknownXmlElements {
		String[] attributes;
		ArrayList children;
	}

	public static IClasspathEntry[][] readFileEntriesWithException(String projectName, URL projectRootAbsoluteFullPath) throws CoreException, IOException, ClasspathEntry.AssertionFailedException, URISyntaxException {
		return readFileEntriesWithException(projectName, projectRootAbsoluteFullPath,null);
	}

	public static IClasspathEntry[][] readFileEntriesWithException(String projectName, URL projectRootAbsoluteFullPath,
			Map unknownElements) throws CoreException, IOException, ClasspathEntry.AssertionFailedException, URISyntaxException {

		URL rscFile = new URL(projectRootAbsoluteFullPath.toExternalForm().concat(JavaProject.CLASSPATH_FILENAME));
		byte[] bytes;

		// when a project is imported, we get a first delta for the addition of the .project, but the .classpath is not accessible
		// so default to using java.io.File
		// see https://bugs.eclipse.org/bugs/show_bug.cgi?id=96258
		URI location;
		try {
			location = rscFile.toURI();
		} catch (URISyntaxException e) {
			throw e;			
		}
		if (location == null)
			throw new IOException("Cannot obtain a location URI for " + rscFile); //$NON-NLS-1$
		File file = Util.toLocalFile(location, null/* no progress monitor available */);
		if (file == null)
			throw new IOException("Unable to fetch file from " + location); //$NON-NLS-1$
		try {
			bytes = org.eclipse.jdt.internal.compiler.util.Util.getFileByteContent(file);
		} catch (IOException e) {
			throw e;
		}

		if (hasUTF8BOM(bytes)) { // see https://bugs.eclipse.org/bugs/show_bug.cgi?id=240034
			int length = bytes.length - IContentDescription.BOM_UTF_8.length;
			System.arraycopy(bytes, IContentDescription.BOM_UTF_8.length, bytes = new byte[length], 0, length);
		}
		String xmlClasspath;
		try {
			xmlClasspath = new String(bytes, org.eclipse.jdt.internal.compiler.util.Util.UTF_8); // .classpath always encoded with UTF-8
		} catch (UnsupportedEncodingException e) {
			Util.log(e, "Could not read .classpath with UTF-8 encoding"); //$NON-NLS-1$
			// fallback to default
			xmlClasspath = new String(bytes);
		}
		return decodeClasspath(projectName, Path.fromPortableString(projectRootAbsoluteFullPath.getPath()),xmlClasspath, unknownElements);
	}
	
	private static boolean hasUTF8BOM(byte[] bytes) {
		if (bytes.length > IContentDescription.BOM_UTF_8.length) {
			for (int i = 0, length = IContentDescription.BOM_UTF_8.length; i < length; i++) {
				if (IContentDescription.BOM_UTF_8[i] != bytes[i])
					return false;
			}
			return true;
		}
		return false;
	}

	/**
	 * Reads and decode an XML classpath string. Returns a two-dimensional array, where the number of elements in the row is fixed to 2. The first element is an array of raw classpath entries and the second element is an array of referenced entries that may have been stored by the client earlier. See {@link IJavaProject#getReferencedClasspathEntries()} for more details.
	 * 
	 */
	public static IClasspathEntry[][] decodeClasspath(String projectName, IPath projectRootAbsoluteFullPath, String xmlClasspath,
			Map unknownElements) throws IOException, ClasspathEntry.AssertionFailedException {

		ArrayList paths = new ArrayList();
		IClasspathEntry defaultOutput = null;
		StringReader reader = new StringReader(xmlClasspath);
		Element cpElement;
		try {
			DocumentBuilder parser = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			cpElement = parser.parse(new InputSource(reader)).getDocumentElement();
		} catch (SAXException e) {
			throw new IOException(Messages.file_badFormat);
		} catch (ParserConfigurationException e) {
			throw new IOException(Messages.file_badFormat);
		} finally {
			reader.close();
		}

		if (!cpElement.getNodeName().equalsIgnoreCase("classpath")) { //$NON-NLS-1$
			throw new IOException(Messages.file_badFormat);
		}
		NodeList list = cpElement.getElementsByTagName(ClasspathEntry.TAG_CLASSPATHENTRY);
		int length = list.getLength();

		for (int i = 0; i < length; ++i) {
			Node node = list.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE) {
				IClasspathEntry entry = elementDecode((Element) node, projectName, projectRootAbsoluteFullPath, unknownElements);
				if (entry != null) {
					if (entry.getContentKind() == ClasspathEntry.K_OUTPUT) {
						defaultOutput = entry; // separate output
					} else {
						paths.add(entry);
					}
				}
			}
		}
		int pathSize = paths.size();
		IClasspathEntry[][] entries = new IClasspathEntry[2][];
		entries[0] = new IClasspathEntry[pathSize + (defaultOutput == null ? 0 : 1)];
		paths.toArray(entries[0]);
		if (defaultOutput != null)
			entries[0][pathSize] = defaultOutput; // ensure output is last item

		paths.clear();
		list = cpElement.getElementsByTagName(ClasspathEntry.TAG_REFERENCED_ENTRY);
		length = list.getLength();

		for (int i = 0; i < length; ++i) {
			Node node = list.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE) {
				IClasspathEntry entry = elementDecode((Element) node, projectName, projectRootAbsoluteFullPath, unknownElements);
				if (entry != null) {
					paths.add(entry);
				}
			}
		}
		entries[1] = new IClasspathEntry[paths.size()];
		paths.toArray(entries[1]);

		return entries;
	}

	// public IClasspathEntry decodeClasspathEntry(String projectName, IPath projectRootAbsoluteFullPath,String encodedEntry) {
	//
	// try {
	// if (encodedEntry == null) return null;
	// StringReader reader = new StringReader(encodedEntry);
	// Element node;
	//
	// try {
	// DocumentBuilder parser =
	// DocumentBuilderFactory.newInstance().newDocumentBuilder();
	// node = parser.parse(new InputSource(reader)).getDocumentElement();
	// } catch (SAXException e) {
	// return null;
	// } catch (ParserConfigurationException e) {
	// return null;
	// } finally {
	// reader.close();
	// }
	//
	// if (!node.getNodeName().equalsIgnoreCase(ClasspathEntry.TAG_CLASSPATHENTRY)
	// || node.getNodeType() != Node.ELEMENT_NODE) {
	// return null;
	// }
	// return elementDecode(node, projectName,projectRootAbsoluteFullPath, null/*not interested in unknown elements*/);
	// } catch (IOException e) {
	// // bad format
	// return null;
	// }
	// }

	public static IClasspathEntry elementDecode(Element element, String projectName, IPath projectRootAbsoluteFullPath,
			Map unknownElements) {

		IPath projectPath = projectRootAbsoluteFullPath;
		NamedNodeMap attributes = element.getAttributes();
		NodeList children = element.getChildNodes();
		boolean[] foundChildren = new boolean[children.getLength()];
		String kindAttr = removeAttribute(ClasspathEntry.TAG_KIND, attributes);
		String pathAttr = removeAttribute(ClasspathEntry.TAG_PATH, attributes);

		// ensure path is absolute
		IPath path = new Path(pathAttr);
		int kind = kindFromString(kindAttr);
		if (kind != IClasspathEntry.CPE_VARIABLE && kind != IClasspathEntry.CPE_CONTAINER && !path.isAbsolute()) {
			if (!(path.segmentCount() > 0 && path.segment(0).equals(ClasspathEntry.DOT_DOT))) {
				path = projectPath.append(path);
			}
		}
		// source attachment info (optional)
		IPath sourceAttachmentPath = element.hasAttribute(ClasspathEntry.TAG_SOURCEPATH)
				? new Path(removeAttribute(ClasspathEntry.TAG_SOURCEPATH, attributes)) : null;
		if (kind != IClasspathEntry.CPE_VARIABLE && sourceAttachmentPath != null && !sourceAttachmentPath.isAbsolute()) {
			sourceAttachmentPath = projectPath.append(sourceAttachmentPath);
		}
		IPath sourceAttachmentRootPath = element.hasAttribute(ClasspathEntry.TAG_ROOTPATH)
				? new Path(removeAttribute(ClasspathEntry.TAG_ROOTPATH, attributes)) : null;

		// exported flag (optional)
		boolean isExported = removeAttribute(ClasspathEntry.TAG_EXPORTED, attributes).equals("true"); //$NON-NLS-1$

		// inclusion patterns (optional)
		IPath[] inclusionPatterns = decodePatterns(attributes, ClasspathEntry.TAG_INCLUDING);
		if (inclusionPatterns == null)
			inclusionPatterns = ClasspathEntry.INCLUDE_ALL;

		// exclusion patterns (optional)
		IPath[] exclusionPatterns = decodePatterns(attributes, ClasspathEntry.TAG_EXCLUDING);
		if (exclusionPatterns == null)
			exclusionPatterns = ClasspathEntry.EXCLUDE_NONE;

		// access rules (optional)
		NodeList attributeList = getChildAttributes(ClasspathEntry.TAG_ACCESS_RULES, children, foundChildren);
		IAccessRule[] accessRules = decodeAccessRules(attributeList);

		// backward compatibility
		if (accessRules == null) {
			accessRules = getAccessRules(inclusionPatterns, exclusionPatterns);
		}

		// combine access rules (optional)
		boolean combineAccessRestrictions = !removeAttribute(ClasspathEntry.TAG_COMBINE_ACCESS_RULES, attributes).equals("false"); //$NON-NLS-1$

		// extra attributes (optional)
		attributeList = getChildAttributes(ClasspathEntry.TAG_ATTRIBUTES, children, foundChildren);
		IClasspathAttribute[] extraAttributes = decodeExtraAttributes(attributeList);

		// custom output location
		IPath outputLocation = element.hasAttribute(ClasspathEntry.TAG_OUTPUT)
				? projectPath.append(removeAttribute(ClasspathEntry.TAG_OUTPUT, attributes)) : null;

		String[] unknownAttributes = null;
		ArrayList unknownChildren = null;

		if (unknownElements != null) {
			// unknown attributes
			int unknownAttributeLength = attributes.getLength();
			if (unknownAttributeLength != 0) {
				unknownAttributes = new String[unknownAttributeLength * 2];
				for (int i = 0; i < unknownAttributeLength; i++) {
					Node attribute = attributes.item(i);
					unknownAttributes[i * 2] = attribute.getNodeName();
					unknownAttributes[i * 2 + 1] = attribute.getNodeValue();
				}
			}

			// unknown children
			for (int i = 0, length = foundChildren.length; i < length; i++) {
				if (!foundChildren[i]) {
					Node node = children.item(i);
					if (node.getNodeType() != Node.ELEMENT_NODE)
						continue;
					if (unknownChildren == null)
						unknownChildren = new ArrayList();
					StringBuffer buffer = new StringBuffer();
					decodeUnknownNode(node, buffer);
					unknownChildren.add(buffer.toString());
				}
			}
		}

		// recreate the CP entry
		IClasspathEntry entry = null;
		switch (kind) {

		case IClasspathEntry.CPE_PROJECT:
			entry = new ClasspathEntry(IPackageFragmentRoot.K_SOURCE, IClasspathEntry.CPE_PROJECT, path,
					ClasspathEntry.INCLUDE_ALL, // inclusion patterns
					ClasspathEntry.EXCLUDE_NONE, // exclusion patterns
					null, // source attachment
					null, // source attachment root
					null, // specific output folder
					isExported, accessRules, combineAccessRestrictions, extraAttributes);
			break;
		case IClasspathEntry.CPE_LIBRARY:
			entry = JavaCore.newLibraryEntry(path, sourceAttachmentPath, sourceAttachmentRootPath, accessRules, extraAttributes,
					isExported);
			break;
		case IClasspathEntry.CPE_SOURCE:
			// must be an entry in this project or specify another project
			String projSegment = path.segment(0);
			if (projSegment != null && projSegment.equals(projectName)) { // this project
				entry = JavaCore.newSourceEntry(path, inclusionPatterns, exclusionPatterns, outputLocation, extraAttributes);
			} else {
				if (path.segmentCount() == 1) {
					// another project
					entry = JavaCore.newProjectEntry(path, accessRules, combineAccessRestrictions, extraAttributes, isExported);
				} else {
					// an invalid source folder
					entry = JavaCore.newSourceEntry(path, inclusionPatterns, exclusionPatterns, outputLocation, extraAttributes);
				}
			}
			break;
		case IClasspathEntry.CPE_VARIABLE:
			entry = JavaCore.newVariableEntry(path, sourceAttachmentPath, sourceAttachmentRootPath, accessRules, extraAttributes,
					isExported);
			break;
		case IClasspathEntry.CPE_CONTAINER:
			entry = JavaCore.newContainerEntry(path, accessRules, extraAttributes, isExported);
			break;
		case ClasspathEntry.K_OUTPUT:
			if (!path.isAbsolute())
				return null;
			entry = new ClasspathEntry(ClasspathEntry.K_OUTPUT, IClasspathEntry.CPE_LIBRARY, path, ClasspathEntry.INCLUDE_ALL,
					ClasspathEntry.EXCLUDE_NONE, null, // source attachment
					null, // source attachment root
					null, // custom output location
					false, null, // no access rules
					false, // no accessible files to combine
					ClasspathEntry.NO_EXTRA_ATTRIBUTES);
			break;
		default:
			throw new AssertionFailedException(Messages.bind(Messages.classpath_unknownKind, kindAttr));
		}

		if (unknownAttributes != null || unknownChildren != null) {
			UnknownXmlElements unknownXmlElements = new UnknownXmlElements();
			unknownXmlElements.attributes = unknownAttributes;
			unknownXmlElements.children = unknownChildren;
			unknownElements.put(path, unknownXmlElements);
		}

		return entry;
	}

	public static NodeList getChildAttributes(String childName, NodeList children, boolean[] foundChildren) {
		for (int i = 0, length = foundChildren.length; i < length; i++) {
			Node node = children.item(i);
			if (childName.equals(node.getNodeName())) {
				foundChildren[i] = true;
				return node.getChildNodes();
			}
		}
		return null;
	}

	private static String removeAttribute(String nodeName, NamedNodeMap nodeMap) {
		Node node = removeNode(nodeName, nodeMap);
		if (node == null)
			return ""; // //$NON-NLS-1$
		return node.getNodeValue();
	}

	private static Node removeNode(String nodeName, NamedNodeMap nodeMap) {
		try {
			return nodeMap.removeNamedItem(nodeName);
		} catch (DOMException e) {
			if (e.code != DOMException.NOT_FOUND_ERR)
				throw e;
			return null;
		}
	}

	/**
	 * Decode some element tag containing a sequence of patterns into IPath[]
	 */
	private static IPath[] decodePatterns(NamedNodeMap nodeMap, String tag) {
		String sequence = removeAttribute(tag, nodeMap);
		if (!sequence.equals("")) { //$NON-NLS-1$
			char[][] patterns = CharOperation.splitOn('|', sequence.toCharArray());
			int patternCount;
			if ((patternCount = patterns.length) > 0) {
				IPath[] paths = new IPath[patternCount];
				int index = 0;
				for (int j = 0; j < patternCount; j++) {
					char[] pattern = patterns[j];
					if (pattern.length == 0)
						continue; // see https://bugs.eclipse.org/bugs/show_bug.cgi?id=105581
					paths[index++] = new Path(new String(pattern));
				}
				if (index < patternCount)
					System.arraycopy(paths, 0, paths = new IPath[index], 0, index);
				return paths;
			}
		}
		return null;
	}

	static IAccessRule[] decodeAccessRules(NodeList list) {
		if (list == null)
			return null;
		int length = list.getLength();
		if (length == 0)
			return null;
		IAccessRule[] result = new IAccessRule[length];
		int index = 0;
		for (int i = 0; i < length; i++) {
			Node accessRule = list.item(i);
			if (accessRule.getNodeType() == Node.ELEMENT_NODE) {
				Element elementAccessRule = (Element) accessRule;
				String pattern = elementAccessRule.getAttribute(ClasspathEntry.TAG_PATTERN);
				if (pattern == null)
					continue;
				String tagKind = elementAccessRule.getAttribute(ClasspathEntry.TAG_KIND);
				int kind;
				if (ClasspathEntry.TAG_ACCESSIBLE.equals(tagKind))
					kind = IAccessRule.K_ACCESSIBLE;
				else if (ClasspathEntry.TAG_NON_ACCESSIBLE.equals(tagKind))
					kind = IAccessRule.K_NON_ACCESSIBLE;
				else if (ClasspathEntry.TAG_DISCOURAGED.equals(tagKind))
					kind = IAccessRule.K_DISCOURAGED;
				else
					continue;
				boolean ignoreIfBetter = "true".equals(elementAccessRule.getAttribute(ClasspathEntry.TAG_IGNORE_IF_BETTER)); //$NON-NLS-1$
				result[index++] = new ClasspathAccessRule(new Path(pattern),
						ignoreIfBetter ? kind | IAccessRule.IGNORE_IF_BETTER : kind);
			}
		}
		if (index != length)
			System.arraycopy(result, 0, result = new IAccessRule[index], 0, index);
		return result;
	}

	static IClasspathAttribute[] decodeExtraAttributes(NodeList attributes) {
		if (attributes == null)
			return ClasspathEntry.NO_EXTRA_ATTRIBUTES;
		int length = attributes.getLength();
		if (length == 0)
			return ClasspathEntry.NO_EXTRA_ATTRIBUTES;
		IClasspathAttribute[] result = new IClasspathAttribute[length];
		int index = 0;
		for (int i = 0; i < length; ++i) {
			Node node = attributes.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE) {
				Element attribute = (Element) node;
				String name = attribute.getAttribute(ClasspathEntry.TAG_ATTRIBUTE_NAME);
				if (name == null)
					continue;
				String value = attribute.getAttribute(ClasspathEntry.TAG_ATTRIBUTE_VALUE);
				if (value == null)
					continue;
				result[index++] = new ClasspathAttribute(name, value);
			}
		}
		if (index != length)
			System.arraycopy(result, 0, result = new IClasspathAttribute[index], 0, index);
		return result;
	}

	private static void decodeUnknownNode(Node node, StringBuffer buffer) {
		ByteArrayOutputStream s = new ByteArrayOutputStream();
		OutputStreamWriter writer;
		try {
			writer = new OutputStreamWriter(s, "UTF8"); //$NON-NLS-1$
			GenericXMLWriter xmlWriter = new GenericXMLWriter(writer, System.getProperty("line.separator"),
					false/* don't print XML version */);
			decodeUnknownNode(node, xmlWriter, true/* insert new line */);
			xmlWriter.flush();
			xmlWriter.close();
			buffer.append(s.toString("UTF8")); //$NON-NLS-1$
		} catch (UnsupportedEncodingException e) {
			// ignore (UTF8 is always supported)
		}
	}

	private static void decodeUnknownNode(Node node, GenericXMLWriter xmlWriter, boolean insertNewLine) {
		switch (node.getNodeType()) {
		case Node.ELEMENT_NODE:
			NamedNodeMap attributes;
			HashMap parameters = null;
			if ((attributes = node.getAttributes()) != null) {
				int length = attributes.getLength();
				if (length > 0) {
					parameters = new HashMap();
					for (int i = 0; i < length; i++) {
						Node attribute = attributes.item(i);
						parameters.put(attribute.getNodeName(), attribute.getNodeValue());
					}
				}
			}
			NodeList children = node.getChildNodes();
			int childrenLength = children.getLength();
			String nodeName = node.getNodeName();
			xmlWriter.printTag(nodeName, parameters, false/* don't insert tab */, false/* don't insert new line */,
					childrenLength == 0/* close tag if no children */);
			if (childrenLength > 0) {
				for (int i = 0; i < childrenLength; i++) {
					decodeUnknownNode(children.item(i), xmlWriter, false/* don't insert new line */);
				}
				xmlWriter.endTag(nodeName, false/* don't insert tab */, insertNewLine);
			}
			break;
		case Node.TEXT_NODE:
			String data = ((Text) node).getData();
			xmlWriter.printString(data, false/* don't insert tab */, false/* don't insert new line */);
			break;
		}
	}

	/**
	 * Returns the kind of a <code>PackageFragmentRoot</code> from its <code>String</code> form.
	 */
	static int kindFromString(String kindStr) {

		if (kindStr.equalsIgnoreCase("prj")) //$NON-NLS-1$
			return IClasspathEntry.CPE_PROJECT;
		if (kindStr.equalsIgnoreCase("var")) //$NON-NLS-1$
			return IClasspathEntry.CPE_VARIABLE;
		if (kindStr.equalsIgnoreCase("con")) //$NON-NLS-1$
			return IClasspathEntry.CPE_CONTAINER;
		if (kindStr.equalsIgnoreCase("src")) //$NON-NLS-1$
			return IClasspathEntry.CPE_SOURCE;
		if (kindStr.equalsIgnoreCase("lib")) //$NON-NLS-1$
			return IClasspathEntry.CPE_LIBRARY;
		if (kindStr.equalsIgnoreCase("output")) //$NON-NLS-1$
			return ClasspathEntry.K_OUTPUT;
		return -1;
	}

	/*
	 * Backward compatibility: only accessible and non-accessible files are suported.
	 */
	public static IAccessRule[] getAccessRules(IPath[] accessibleFiles, IPath[] nonAccessibleFiles) {
		int accessibleFilesLength = accessibleFiles == null ? 0 : accessibleFiles.length;
		int nonAccessibleFilesLength = nonAccessibleFiles == null ? 0 : nonAccessibleFiles.length;
		int length = accessibleFilesLength + nonAccessibleFilesLength;
		if (length == 0)
			return null;
		IAccessRule[] accessRules = new IAccessRule[length];
		for (int i = 0; i < accessibleFilesLength; i++) {
			accessRules[i] = JavaCore.newAccessRule(accessibleFiles[i], IAccessRule.K_ACCESSIBLE);
		}
		for (int i = 0; i < nonAccessibleFilesLength; i++) {
			accessRules[accessibleFilesLength + i] = JavaCore.newAccessRule(nonAccessibleFiles[i], IAccessRule.K_NON_ACCESSIBLE);
		}
		return accessRules;
	}
}

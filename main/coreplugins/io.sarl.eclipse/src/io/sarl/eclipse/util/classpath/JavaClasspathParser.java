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

package io.sarl.eclipse.util.classpath;

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
import java.util.List;
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
 * Class mostly inspired from jdt See {@link JavaProject#decodeClasspath} and {@code ClasspathEntry}.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:classdataabstractioncoupling")
public final class JavaClasspathParser {

    /**
     * UnknownXmlElements.
     *
     * @author $Author: ngaud$
     * @version $FullVersion$
     * @mavengroupid $GroupId$
     * @mavenartifactid $ArtifactId$
     */
    static class UnknownXmlElements {
        /**
         * Unknown attributes.
         */
        String[] attributes;

        /**
         * Unknown children.
         */
        List<String> children;
    }

    /**
     * Private constructor.
     */
    private JavaClasspathParser() {
        //
    }

    /**
     * Reads entry of a .classpath file.
     *
     * @param projectName
     *            - the name of project containing the .classpath file
     * @param projectRootAbsoluteFullPath
     *            - the path to project containing the .classpath file
     * @return the set of CLasspath ENtries extracted from the .classpath
     * @throws CoreException
     *             - exception during parsing of .classpath
     * @throws IOException
     *             - exception during parsing of .classpath
     * @throws ClasspathEntry.AssertionFailedException
     *             - exception during parsing of .classpath
     * @throws URISyntaxException
     *             - exception during parsing of .classpath
     */
    public static IClasspathEntry[][] readFileEntriesWithException(String projectName, URL projectRootAbsoluteFullPath)
            throws CoreException, IOException, ClasspathEntry.AssertionFailedException, URISyntaxException {
        return readFileEntriesWithException(projectName, projectRootAbsoluteFullPath, null);
    }

    /**
     * Reads entry of a .classpath file.
     *
     * @param projectName
     *            - the name of project containing the .classpath file
     * @param projectRootAbsoluteFullPath
     *            - the path to project containing the .classpath file
     * @param unknownElements
     *            - map of unknow elements
     * @return the set of CLasspath Entries extracted from the .classpath
     * @throws CoreException
     *             - exception during parsing of .classpath
     * @throws IOException
     *             - exception during parsing of .classpath
     * @throws ClasspathEntry.AssertionFailedException
     *             - exception during parsing of .classpath
     * @throws URISyntaxException
     *             - exception during parsing of .classpath
     */
    @SuppressWarnings("checkstyle:innerassignment")
    public static IClasspathEntry[][] readFileEntriesWithException(String projectName, URL projectRootAbsoluteFullPath,
            Map<IPath, UnknownXmlElements> unknownElements)
            throws CoreException, IOException, ClasspathEntry.AssertionFailedException, URISyntaxException {

        final URL rscFile = new URL(
        		projectRootAbsoluteFullPath.toExternalForm().concat(JavaProject.CLASSPATH_FILENAME).replace(" ", "%20")); //$NON-NLS-1$ //$NON-NLS-2$
        byte[] bytes;

        // when a project is imported, we get a first delta for the addition of the .project, but the .classpath is not accessible
        // so default to using java.io.File
        // see https://bugs.eclipse.org/bugs/show_bug.cgi?id=96258
        final URI location;
        try {
            location = rscFile.toURI();
        } catch (URISyntaxException e) {
            throw e;
        }
        if (location == null) {
            throw new IOException("Cannot obtain a location URI for " + rscFile); //$NON-NLS-1$
        }
        final File file = Util.toLocalFile(location, null/* no progress monitor available */);
        if (file == null) {
            throw new IOException("Unable to fetch file from " + location); //$NON-NLS-1$
        }

        try {
            bytes = org.eclipse.jdt.internal.compiler.util.Util.getFileByteContent(file);
        } catch (IOException e) {
            throw e;
        }

        if (hasUTF8BOM(bytes)) {
            // see https://bugs.eclipse.org/bugs/show_bug.cgi?id=240034
            final int length = bytes.length - IContentDescription.BOM_UTF_8.length;
            System.arraycopy(bytes, IContentDescription.BOM_UTF_8.length, bytes = new byte[length], 0, length);
        }
        String xmlClasspath;
        try {
            // .classpath always encoded with UTF-8
            xmlClasspath = new String(bytes, org.eclipse.jdt.internal.compiler.util.Util.UTF_8);
        } catch (UnsupportedEncodingException e) {
            Util.log(e, "Could not read .classpath with UTF-8 encoding"); //$NON-NLS-1$
            // fallback to default
            xmlClasspath = new String(bytes);
        }
        return decodeClasspath(projectName, Path.fromPortableString(projectRootAbsoluteFullPath.getPath()), xmlClasspath, unknownElements);
    }

    private static boolean hasUTF8BOM(byte[] bytes) {
        if (bytes.length > IContentDescription.BOM_UTF_8.length) {
            for (int i = 0, length = IContentDescription.BOM_UTF_8.length; i < length; i++) {
                if (IContentDescription.BOM_UTF_8[i] != bytes[i]) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    /**
     * Reads and decode an XML classpath string. Returns a two-dimensional array, where the number of elements in the row is fixed to 2. The first
     * element is an array of raw classpath entries and the second element is an array of referenced entries that may have been stored by the client
     * earlier. See {@link IJavaProject#getReferencedClasspathEntries()} for more details.
     *
     * @param projectName
     *            - the name of project containing the .classpath file
     * @param projectRootAbsoluteFullPath
     *            - the path to project containing the .classpath file
     * @param xmlClasspath
     *            - path to the XML
     * @param unknownElements
     *            - map of unknow elements
     * @return the set of CLasspath ENtries extracted from the .classpath
     * @throws IOException
     *             - exception during parsing of .classpath
     * @throws ClasspathEntry.AssertionFailedException
     *             - exception during parsing of .classpath
     */
    @SuppressWarnings("checkstyle:npathcomplexity")
    public static IClasspathEntry[][] decodeClasspath(String projectName, IPath projectRootAbsoluteFullPath, String xmlClasspath,
            Map<IPath, UnknownXmlElements> unknownElements) throws IOException, ClasspathEntry.AssertionFailedException {

        final List<IClasspathEntry> paths = new ArrayList<>();
        IClasspathEntry defaultOutput = null;
        final Element cpElement;

        try (StringReader reader = new StringReader(xmlClasspath);) {
            final DocumentBuilder parser = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            cpElement = parser.parse(new InputSource(reader)).getDocumentElement();
        } catch (SAXException e) {
            throw new IOException(Messages.file_badFormat);
        } catch (ParserConfigurationException e) {
            throw new IOException(Messages.file_badFormat);
        }

        if (!cpElement.getNodeName().equalsIgnoreCase("classpath")) { //$NON-NLS-1$
            throw new IOException(Messages.file_badFormat);
        }
        NodeList list = cpElement.getElementsByTagName(ClasspathEntry.TAG_CLASSPATHENTRY);
        int length = list.getLength();

        for (int i = 0; i < length; ++i) {
            final Node node = list.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE) {
                final IClasspathEntry entry = elementDecode((Element) node, projectName, projectRootAbsoluteFullPath, unknownElements);
                if (entry != null) {
                    if (entry.getContentKind() == ClasspathEntry.K_OUTPUT) {
                        // separate output
                        defaultOutput = entry;
                    } else {
                        paths.add(entry);
                    }
                }
            }
        }
        final int pathSize = paths.size();
        final IClasspathEntry[][] entries = new IClasspathEntry[2][];
        entries[0] = new IClasspathEntry[pathSize + (defaultOutput == null ? 0 : 1)];
        paths.toArray(entries[0]);
        if (defaultOutput != null) {
            // ensure output is last item
            entries[0][pathSize] = defaultOutput;
        }
        paths.clear();
        list = cpElement.getElementsByTagName(ClasspathEntry.TAG_REFERENCED_ENTRY);
        length = list.getLength();

        for (int i = 0; i < length; ++i) {
            final Node node = list.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE) {
                final IClasspathEntry entry = elementDecode((Element) node, projectName, projectRootAbsoluteFullPath, unknownElements);
                if (entry != null) {
                    paths.add(entry);
                }
            }
        }
        entries[1] = new IClasspathEntry[paths.size()];
        paths.toArray(entries[1]);

        return entries;
    }

    /**
     * Decodes one XML element with the XML stream.
     *
     * @param element
     *            - the considered element
     * @param projectName
     *            - the name of project containing the .classpath file
     * @param projectRootAbsoluteFullPath
     *            - he path to project containing the .classpath file
     * @param unknownElements
     *            - map of unknown elements
     * @return the set of CLasspath ENtries extracted from the considered element
     */
    @SuppressWarnings({ "checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity" })
    public static IClasspathEntry elementDecode(Element element, String projectName, IPath projectRootAbsoluteFullPath,
            Map<IPath, UnknownXmlElements> unknownElements) {
        final IPath projectPath = projectRootAbsoluteFullPath;
        final NamedNodeMap attributes = element.getAttributes();
        final NodeList children = element.getChildNodes();
        final boolean[] foundChildren = new boolean[children.getLength()];
        final String kindAttr = removeAttribute(ClasspathEntry.TAG_KIND, attributes);
        final String pathAttr = removeAttribute(ClasspathEntry.TAG_PATH, attributes);

        // ensure path is absolute
        IPath path = new Path(pathAttr);
        final int kind = kindFromString(kindAttr);
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
        final IPath sourceAttachmentRootPath = element.hasAttribute(ClasspathEntry.TAG_ROOTPATH)
                ? new Path(removeAttribute(ClasspathEntry.TAG_ROOTPATH, attributes)) : null;

        // exported flag (optional)
        final boolean isExported = removeAttribute(ClasspathEntry.TAG_EXPORTED, attributes).equals("true"); //$NON-NLS-1$

        // inclusion patterns (optional)
        IPath[] inclusionPatterns = decodePatterns(attributes, ClasspathEntry.TAG_INCLUDING);
        if (inclusionPatterns == null) {
            inclusionPatterns = ClasspathEntry.INCLUDE_ALL;
        }

        // exclusion patterns (optional)
        IPath[] exclusionPatterns = decodePatterns(attributes, ClasspathEntry.TAG_EXCLUDING);
        if (exclusionPatterns == null) {
            exclusionPatterns = ClasspathEntry.EXCLUDE_NONE;
        }

        // access rules (optional)
        NodeList attributeList = getChildAttributes(ClasspathEntry.TAG_ACCESS_RULES, children, foundChildren);
        IAccessRule[] accessRules = decodeAccessRules(attributeList);

        // backward compatibility
        if (accessRules == null) {
            accessRules = getAccessRules(inclusionPatterns, exclusionPatterns);
        }

        // combine access rules (optional)
        final boolean combineAccessRestrictions = !removeAttribute(ClasspathEntry.TAG_COMBINE_ACCESS_RULES, attributes).equals("false"); //$NON-NLS-1$

        // extra attributes (optional)
        attributeList = getChildAttributes(ClasspathEntry.TAG_ATTRIBUTES, children, foundChildren);
        final IClasspathAttribute[] extraAttributes = decodeExtraAttributes(attributeList);

        // custom output location
        final IPath outputLocation = element.hasAttribute(ClasspathEntry.TAG_OUTPUT)
                ? projectPath.append(removeAttribute(ClasspathEntry.TAG_OUTPUT, attributes)) : null;

        String[] unknownAttributes = null;
        ArrayList<String> unknownChildren = null;

        if (unknownElements != null) {
            // unknown attributes
            final int unknownAttributeLength = attributes.getLength();
            if (unknownAttributeLength != 0) {
                unknownAttributes = new String[unknownAttributeLength * 2];
                for (int i = 0; i < unknownAttributeLength; i++) {
                    final Node attribute = attributes.item(i);
                    unknownAttributes[i * 2] = attribute.getNodeName();
                    unknownAttributes[i * 2 + 1] = attribute.getNodeValue();
                }
            }

            // unknown children
            for (int i = 0, length = foundChildren.length; i < length; i++) {
                if (!foundChildren[i]) {
                    final Node node = children.item(i);
                    if (node.getNodeType() != Node.ELEMENT_NODE) {
                        continue;
                    }
                    if (unknownChildren == null) {
                        unknownChildren = new ArrayList<>();
                    }
                    final StringBuffer buffer = new StringBuffer();
                    decodeUnknownNode(node, buffer);
                    unknownChildren.add(buffer.toString());
                }
            }
        }

        // recreate the CP entry
        IClasspathEntry entry = null;
        switch (kind) {

        case IClasspathEntry.CPE_PROJECT:
            /*
             * IPackageFragmentRoot.K_SOURCE, IClasspathEntry.CPE_PROJECT, path, ClasspathEntry.INCLUDE_ALL, // inclusion patterns
             * ClasspathEntry.EXCLUDE_NONE, // exclusion patterns null, // source attachment null, // source attachment root null, // specific output
             * folder
             */
            entry = new ClasspathEntry(IPackageFragmentRoot.K_SOURCE, IClasspathEntry.CPE_PROJECT, path, ClasspathEntry.INCLUDE_ALL,
                    ClasspathEntry.EXCLUDE_NONE, null, null, null, isExported, accessRules, combineAccessRestrictions, extraAttributes);
            break;
        case IClasspathEntry.CPE_LIBRARY:
            entry = JavaCore.newLibraryEntry(path, sourceAttachmentPath, sourceAttachmentRootPath, accessRules, extraAttributes, isExported);
            break;
        case IClasspathEntry.CPE_SOURCE:
            // must be an entry in this project or specify another project
            final String projSegment = path.segment(0);
            if (projSegment != null && projSegment.equals(projectName)) {
                // this project
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
            entry = JavaCore.newVariableEntry(path, sourceAttachmentPath, sourceAttachmentRootPath, accessRules, extraAttributes, isExported);
            break;
        case IClasspathEntry.CPE_CONTAINER:
            entry = JavaCore.newContainerEntry(path, accessRules, extraAttributes, isExported);
            break;
        case ClasspathEntry.K_OUTPUT:
            if (!path.isAbsolute()) {
                return null;
            }
            /*
             * ClasspathEntry.EXCLUDE_NONE, null, // source attachment null, // source attachment root null, // custom output location false, null, //
             * no access rules false, // no accessible files to combine
             */
            entry = new ClasspathEntry(ClasspathEntry.K_OUTPUT, IClasspathEntry.CPE_LIBRARY, path, ClasspathEntry.INCLUDE_ALL,
                    ClasspathEntry.EXCLUDE_NONE, null, null, null, false, null, false, ClasspathEntry.NO_EXTRA_ATTRIBUTES);
            break;
        default:
            throw new AssertionFailedException(Messages.bind(Messages.classpath_unknownKind, kindAttr));
        }

        if (unknownAttributes != null || unknownChildren != null) {
            final UnknownXmlElements unknownXmlElements = new UnknownXmlElements();
            unknownXmlElements.attributes = unknownAttributes;
            unknownXmlElements.children = unknownChildren;
            if (unknownElements != null) {
            	unknownElements.put(path, unknownXmlElements);
            }
        }

        return entry;
    }

    private static NodeList getChildAttributes(String childName, NodeList children, boolean[] foundChildren) {
        for (int i = 0, length = foundChildren.length; i < length; i++) {
            final Node node = children.item(i);
            if (childName.equals(node.getNodeName())) {
                foundChildren[i] = true;
                return node.getChildNodes();
            }
        }
        return null;
    }

    private static String removeAttribute(String nodeName, NamedNodeMap nodeMap) {
        final Node node = removeNode(nodeName, nodeMap);
        if (node == null) {
            return ""; // //$NON-NLS-1$
        }
        return node.getNodeValue();
    }

    private static Node removeNode(String nodeName, NamedNodeMap nodeMap) {
        try {
            return nodeMap.removeNamedItem(nodeName);
        } catch (DOMException e) {
            if (e.code != DOMException.NOT_FOUND_ERR) {
                throw e;
            }
            return null;
        }
    }

    /**
     * Decode some element tag containing a sequence of patterns into IPath[].
     *
     * @param nodeMap
     *            - map
     * @param tag
     *            - tag
     * @return aarray of IPATH
     */
    @SuppressWarnings("checkstyle:innerassignment")
    private static IPath[] decodePatterns(NamedNodeMap nodeMap, String tag) {
        final String sequence = removeAttribute(tag, nodeMap);
        if (!"".equals(sequence)) { //$NON-NLS-1$
            final char[][] patterns = CharOperation.splitOn('|', sequence.toCharArray());
            final int patternCount;
            if ((patternCount = patterns.length) > 0) {
                IPath[] paths = new IPath[patternCount];
                int index = 0;
                for (int j = 0; j < patternCount; j++) {
                    final char[] pattern = patterns[j];
                    if (pattern.length == 0) {
                        // see https://bugs.eclipse.org/bugs/show_bug.cgi?id=105581
                        continue;
                    }
                    paths[index++] = new Path(new String(pattern));
                }
                if (index < patternCount) {
                    System.arraycopy(paths, 0, paths = new IPath[index], 0, index);
                }
                return paths;
            }
        }
        return null;
    }

    @SuppressWarnings({ "checkstyle:npathcomplexity", "checkstyle:innerassignment" })
    private static IAccessRule[] decodeAccessRules(NodeList list) {
        if (list == null) {
            return null;
        }
        final int length = list.getLength();
        if (length == 0) {
            return null;
        }
        IAccessRule[] result = new IAccessRule[length];
        int index = 0;
        for (int i = 0; i < length; i++) {
            final Node accessRule = list.item(i);
            if (accessRule.getNodeType() == Node.ELEMENT_NODE) {
                final Element elementAccessRule = (Element) accessRule;
                final String pattern = elementAccessRule.getAttribute(ClasspathEntry.TAG_PATTERN);
                if (pattern == null) {
                    continue;
                }
                final String tagKind = elementAccessRule.getAttribute(ClasspathEntry.TAG_KIND);
                final int kind;
                if (ClasspathEntry.TAG_ACCESSIBLE.equals(tagKind)) {
                    kind = IAccessRule.K_ACCESSIBLE;
                } else if (ClasspathEntry.TAG_NON_ACCESSIBLE.equals(tagKind)) {
                    kind = IAccessRule.K_NON_ACCESSIBLE;
                } else if (ClasspathEntry.TAG_DISCOURAGED.equals(tagKind)) {
                    kind = IAccessRule.K_DISCOURAGED;
                } else {
                    continue;
                }
                final boolean ignoreIfBetter = "true".equals(elementAccessRule.getAttribute(ClasspathEntry.TAG_IGNORE_IF_BETTER)); //$NON-NLS-1$
                result[index++] = new ClasspathAccessRule(new Path(pattern), ignoreIfBetter ? kind | IAccessRule.IGNORE_IF_BETTER : kind);
            }
        }
        if (index != length) {
            System.arraycopy(result, 0, result = new IAccessRule[index], 0, index);
        }
        return result;
    }

    @SuppressWarnings("checkstyle:innerassignment")
    private static IClasspathAttribute[] decodeExtraAttributes(NodeList attributes) {
        if (attributes == null) {
            return ClasspathEntry.NO_EXTRA_ATTRIBUTES;
        }
        final int length = attributes.getLength();
        if (length == 0) {
            return ClasspathEntry.NO_EXTRA_ATTRIBUTES;
        }
        IClasspathAttribute[] result = new IClasspathAttribute[length];
        int index = 0;
        for (int i = 0; i < length; ++i) {
            final Node node = attributes.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE) {
                final Element attribute = (Element) node;
                final String name = attribute.getAttribute(ClasspathEntry.TAG_ATTRIBUTE_NAME);
                if (name == null) {
                    continue;
                }
                final String value = attribute.getAttribute(ClasspathEntry.TAG_ATTRIBUTE_VALUE);
                if (value == null) {
                    continue;
                }
                result[index++] = new ClasspathAttribute(name, value);
            }
        }
        if (index != length) {
            System.arraycopy(result, 0, result = new IClasspathAttribute[index], 0, index);
        }
        return result;
    }

    private static void decodeUnknownNode(Node node, StringBuffer buffer) {
        final ByteArrayOutputStream s = new ByteArrayOutputStream();
        final OutputStreamWriter writer;
        try {
            writer = new OutputStreamWriter(s, "UTF8"); //$NON-NLS-1$
            try (GenericXMLWriter xmlWriter = new GenericXMLWriter(writer, System.getProperty("line.separator"), //$NON-NLS-1$
                    false/* don't print XML version */)) {
                decodeUnknownNode(node, xmlWriter, true/* insert new line */);
                xmlWriter.flush();
            }
            buffer.append(s.toString("UTF8")); //$NON-NLS-1$
        } catch (UnsupportedEncodingException e) {
            // ignore (UTF8 is always supported)
        }
    }

    @SuppressWarnings({"checkstyle:innerassignment", "checkstyle:illegaltype"})
    private static void decodeUnknownNode(Node node, GenericXMLWriter xmlWriter, boolean insertNewLine) {
        switch (node.getNodeType()) {
        case Node.ELEMENT_NODE:
            final NamedNodeMap attributes;
            HashMap<String, String> parameters = null;
            if ((attributes = node.getAttributes()) != null) {
                final int length = attributes.getLength();
                if (length > 0) {
                    parameters = new HashMap<>();
                    for (int i = 0; i < length; i++) {
                        final Node attribute = attributes.item(i);
                        parameters.put(attribute.getNodeName(), attribute.getNodeValue());
                    }
                }
            }
            final NodeList children = node.getChildNodes();
            final int childrenLength = children.getLength();
            final String nodeName = node.getNodeName();
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
            final String data = ((Text) node).getData();
            xmlWriter.printString(data, false/* don't insert tab */, false/* don't insert new line */);
            break;
        default:
            break;
        }
    }

    /**
     * Returns the kind of a <code>PackageFragmentRoot</code> from its <code>String</code> form.
     *
     * @param kindStr
     *            - string to test
     * @return the integer identifier of the type of the specified string: CPE_PROJECT, CPE_VARIABLE, CPE_CONTAINER, etc.
     */
    @SuppressWarnings("checkstyle:equalsavoidnull")
    private static int kindFromString(String kindStr) {

        if (kindStr.equalsIgnoreCase("prj")) { //$NON-NLS-1$
            return IClasspathEntry.CPE_PROJECT;
        }
        if (kindStr.equalsIgnoreCase("var")) { //$NON-NLS-1$
            return IClasspathEntry.CPE_VARIABLE;
        }
        if (kindStr.equalsIgnoreCase("con")) { //$NON-NLS-1$
            return IClasspathEntry.CPE_CONTAINER;
        }
        if (kindStr.equalsIgnoreCase("src")) { //$NON-NLS-1$
            return IClasspathEntry.CPE_SOURCE;
        }
        if (kindStr.equalsIgnoreCase("lib")) { //$NON-NLS-1$
            return IClasspathEntry.CPE_LIBRARY;
        }
        if (kindStr.equalsIgnoreCase("output")) { //$NON-NLS-1$
            return ClasspathEntry.K_OUTPUT;
        }
        return -1;
    }

    /*
     * Backward compatibility: only accessible and non-accessible files are suported.
     */
    private static IAccessRule[] getAccessRules(IPath[] accessibleFiles, IPath[] nonAccessibleFiles) {
        final int accessibleFilesLength = accessibleFiles == null ? 0 : accessibleFiles.length;
        final int nonAccessibleFilesLength = nonAccessibleFiles == null ? 0 : nonAccessibleFiles.length;
        final int length = accessibleFilesLength + nonAccessibleFilesLength;
        if (length == 0) {
            return null;
        }
        final IAccessRule[] accessRules = new IAccessRule[length];
        if (accessibleFiles != null) {
	        for (int i = 0; i < accessibleFilesLength; i++) {
	            accessRules[i] = JavaCore.newAccessRule(accessibleFiles[i], IAccessRule.K_ACCESSIBLE);
	        }
        }
        if (nonAccessibleFiles != null) {
	        for (int i = 0; i < nonAccessibleFilesLength; i++) {
	            accessRules[accessibleFilesLength + i] = JavaCore.newAccessRule(nonAccessibleFiles[i], IAccessRule.K_NON_ACCESSIBLE);
	        }
        }
        return accessRules;
    }
}

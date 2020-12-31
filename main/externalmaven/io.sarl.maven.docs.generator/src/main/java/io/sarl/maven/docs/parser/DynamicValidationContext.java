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

package io.sarl.maven.docs.parser;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.io.Files;
import org.apache.commons.lang3.StringUtils;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.junit.jupiter.api.Assertions;

/** Context for building a dynamic validation component.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class DynamicValidationContext {

	private List<String> sources;

	private List<String> resources;

	private List<String> destinations;

	private List<String> tmpResources;

	@Override
	public String toString() {
		final StringBuilder buffer = new StringBuilder();
		buffer.append("sources = ").append(getSourceRoots()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		buffer.append("resources = ").append(getResourceRoots()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		buffer.append("tmp = ").append(getTempResourceRoots()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		buffer.append("destinations = ").append(getDestinationRoots()).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
		return buffer.toString();
	}

	private static File canon(File file) {
		try {
			return file.getCanonicalFile();
		} catch (IOException e) {
			return file;
		}
	}

	/** Replies the root folders for sources.
	 *
	 * @return the root folders.
	 */
	public List<String> getSourceRoots() {
		return this.sources == null ? Collections.emptyList() : this.sources;
	}

	/** Change the root folders for sources.
	 *
	 * @param roots the root folders.
	 */
	public void setSourceRoots(List<String> roots) {
		this.sources = roots;
	}

	/** Replies the root folders for destinations.
	 *
	 * @return the root folders.
	 */
	public List<String> getDestinationRoots() {
		return this.destinations == null ? Collections.emptyList() : this.destinations;
	}

	/** Change the root folders for destinations.
	 *
	 * @param roots the root folders.
	 */
	public void setDestinationRoots(List<String> roots) {
		this.destinations = roots;
	}

	/** Replies the root folders for resources.
	 *
	 * @return the root folders.
	 */
	public List<String> getResourceRoots() {
		return this.resources == null ? Collections.emptyList() : this.resources;
	}

	/** Change the root folders for resources.
	 *
	 * @param roots the root folders.
	 */
	public void setResourceRoots(List<String> roots) {
		this.resources = roots;
	}

	/** Replies the temprary root folders for resources.
	 *
	 * <p>The temporary resources are forgiven as soon as this function is called.
	 *
	 * @return the root folders.
	 */
	public List<String> getTempResourceRoots() {
		final List<String> tmp = this.tmpResources == null ? Collections.emptyList() : this.tmpResources;
		this.tmpResources = null;
		return tmp;
	}

	/** Change the root folders for resources.
	 *
	 * <p>The temporary resources are forgiven as soon as the function {@link #getTempResourceRoots()} is called.
	 *
	 * @param roots the root folders.
	 */
	public void setTempResourceRoots(List<String> roots) {
		this.tmpResources = roots;
	}

	/** Append to the given receiver the code for testing the existence of a file.
	 *
	 * @param receiver the receiver.
	 * @param relativeFile the filename to test.
	 * @param errorLabel the label to be output when the file was not found.
	 */
	public void appendFileExistenceTest(ITreeAppendable receiver, File relativeFile, String errorLabel) {
		if (relativeFile.isAbsolute()) {
			receiver.newLine();
			receiver.append("{"); //$NON-NLS-1$
			receiver.increaseIndentation().newLine();
			receiver.append(File.class).append(" file = new ").append(File.class); //$NON-NLS-1$
			receiver.append("(\"").append(Strings.convertToJavaString(relativeFile.toString())).append("\");"); //$NON-NLS-1$ //$NON-NLS-2$
			receiver.newLine();
			receiver.append(Assertions.class).append(".assertTrue(file.exists(), () -> \"" + Strings.convertToJavaString(errorLabel) //$NON-NLS-1$
					+ ": \" + file);"); //$NON-NLS-1$
			receiver.decreaseIndentation().newLine();
			receiver.append("}"); //$NON-NLS-1$
		}
		for (final String resource : getResourceRoots()) {
			final File fileInResource = canon(FileSystem.makeAbsolute(relativeFile, new File(resource)));
			appendSafeFileExistenceTest(receiver, fileInResource);
		}
		for (final String resource : getTempResourceRoots()) {
			final File fileInResource = canon(FileSystem.makeAbsolute(relativeFile, new File(resource)));
			appendSafeFileExistenceTest(receiver, fileInResource);
		}
		receiver.newLine();
		receiver.append(Assertions.class).append(".fail(\"" + Strings.convertToJavaString(errorLabel) //$NON-NLS-1$
			+ ": " + Strings.convertToJavaString(relativeFile.toString()) + "\");"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Append to the given receiver the code for testing the existence of a file.
	 *
	 * @param receiver the receiver.
	 * @param relativeFile the filename to test.
	 * @param errorLabel the label to be output when the file was not found.
	 * @param extensions the file extensions to be considered as equivalent.
	 */
	public void appendFileExistenceTest(ITreeAppendable receiver, File relativeFile, String errorLabel, Iterable<String> extensions) {
		if (!hasExtension(relativeFile, extensions)) {
			appendFileExistenceTest(receiver, relativeFile, errorLabel);
			return;
		}

		final File fileWithoutExtension = FileSystem.removeExtension(relativeFile);

		for (final String newExtension : extensions) {
			final File fileWithNewExtension = FileSystem.addExtension(fileWithoutExtension, newExtension);
			if (relativeFile.isAbsolute()) {
				appendSafeFileExistenceTest(receiver, fileWithNewExtension);
			} else {
				for (final String resource : getResourceRoots()) {
					final File fileInResource = canon(FileSystem.makeAbsolute(fileWithNewExtension, new File(resource)));
					appendSafeFileExistenceTest(receiver, fileInResource);
				}
				for (final String resource : getTempResourceRoots()) {
					final File fileInResource = canon(FileSystem.makeAbsolute(fileWithNewExtension, new File(resource)));
					appendSafeFileExistenceTest(receiver, fileInResource);
				}
			}
		}
		receiver.newLine();
		receiver.append(Assertions.class).append(".fail(\""); //$NON-NLS-1$
		receiver.append(Strings.convertToJavaString(errorLabel));
		receiver.append(": ").append(Strings.convertToJavaString(fileWithoutExtension.toString())); //$NON-NLS-1$
		receiver.append("["); //$NON-NLS-1$
		boolean first = true;
		for (final String ext : extensions) {
			if (first) {
				first = false;
			} else {
				receiver.append("|"); //$NON-NLS-1$
			}
			receiver.append(Strings.convertToJavaString(ext));
		}
		receiver.append("]\");"); //$NON-NLS-1$
	}

	private static void appendSafeFileExistenceTest(ITreeAppendable receiver, File fileInResource) {
		receiver.newLine();
		receiver.append("{"); //$NON-NLS-1$
		receiver.increaseIndentation().newLine();
		receiver.append(File.class).append(" file = new ").append(File.class); //$NON-NLS-1$
		receiver.append("(\"").append(Strings.convertToJavaString(fileInResource.toString())).append("\");"); //$NON-NLS-1$ //$NON-NLS-2$
		receiver.newLine();
		receiver.append("if (file.exists()) {"); //$NON-NLS-1$
		receiver.increaseIndentation().newLine();
		receiver.append("return;"); //$NON-NLS-1$
		receiver.decreaseIndentation().newLine();
		receiver.append("}"); //$NON-NLS-1$
		receiver.decreaseIndentation().newLine();
		receiver.append("}"); //$NON-NLS-1$
	}

	private static boolean hasExtension(File file, Iterable<String> extensions) {
		final String extension = FileSystem.extension(file);
		for (final String ext : extensions) {
			if (Strings.equal(ext, extension)) {
				return true;
			}
		}
		return false;
	}

	/** Append to the given receiver the code for testing the existence of an title anchor.
	 *
	 * @param receiver the receiver.
	 * @param relativeFile the filename to test.
	 * @param anchor the name of the title reference.
	 * @param sectionPatternSpecification the regular expression for extracting a title from the file content.
	 * @param extensions the file extensions to be considered as equivalent.
	 */
	public void appendTitleAnchorExistenceTest(ITreeAppendable receiver, File relativeFile, String anchor,
			String sectionPatternSpecification, Iterable<String> extensions) {
		if (!hasExtension(relativeFile, extensions)) {
			return;
		}

		final File fileWithoutExtension = FileSystem.removeExtension(relativeFile);

		receiver.append(Pattern.class).append(" sectionPattern = "); //$NON-NLS-1$
		receiver.append(Pattern.class).append(".compile(\""); //$NON-NLS-1$
		receiver.append(Strings.convertToJavaString(sectionPatternSpecification));
		receiver.append("\", ").append(Pattern.class).append(".MULTILINE);").newLine(); //$NON-NLS-1$ //$NON-NLS-2$

		receiver.append(Set.class).append(" alternatives = new "); //$NON-NLS-1$
		receiver.append(TreeSet.class).append("<>();").newLine(); //$NON-NLS-1$

		for (final String newExtension : extensions) {
			final File fileWithNewExtension = FileSystem.addExtension(fileWithoutExtension, newExtension);
			if (relativeFile.isAbsolute()) {
				appendSafeTitleAnchorExistenceTest(receiver, fileWithNewExtension, anchor);
			} else {
				for (final String resource : getDestinationRoots()) {
					final File fileInResource = canon(FileSystem.makeAbsolute(fileWithNewExtension, new File(resource)));
					appendSafeTitleAnchorExistenceTest(receiver, fileInResource, anchor);
				}
				for (final String resource : getTempResourceRoots()) {
					final File fileInResource = canon(FileSystem.makeAbsolute(fileWithNewExtension, new File(resource)));
					appendSafeTitleAnchorExistenceTest(receiver, fileInResource, anchor);
				}
			}
		}
		receiver.newLine();

		final StringBuilder errorFilename = new StringBuilder();
		errorFilename.append(Strings.convertToJavaString(fileWithoutExtension.toString()));
		errorFilename.append(".*"); //$NON-NLS-1$

		receiver.append(List.class).append("<String> alternativeLists = new "); //$NON-NLS-1$
		receiver.append(ArrayList.class).append("<>(alternatives);").newLine(); //$NON-NLS-1$
		receiver.append(Collections.class).append(".sort(alternativeLists, (a, b) -> {"); //$NON-NLS-1$
		receiver.increaseIndentation().newLine();
		receiver.append("int la = ").append(StringUtils.class).append(".getLevenshteinDistance(a, \""); //$NON-NLS-1$ //$NON-NLS-2$
		receiver.append(Strings.convertToJavaString(anchor)).append("\");").newLine(); //$NON-NLS-1$
		receiver.append("int lb = ").append(StringUtils.class).append(".getLevenshteinDistance(b, \""); //$NON-NLS-1$ //$NON-NLS-2$
		receiver.append(Strings.convertToJavaString(anchor)).append("\");").newLine(); //$NON-NLS-1$
		receiver.append("int cmp = la - lb;").newLine(); //$NON-NLS-1$
		receiver.append("if (cmp != 0) return cmp;").newLine(); //$NON-NLS-1$
		receiver.append("return a.compareTo(b);").decreaseIndentation().newLine(); //$NON-NLS-1$
		receiver.append("});").newLine(); //$NON-NLS-1$

		receiver.append("throw new ").append(AssertionError.class).append("(\""); //$NON-NLS-1$ //$NON-NLS-2$
		receiver.append(Strings.convertToJavaString(MessageFormat.format(Messages.DynamicValidationContext_1,
				anchor, errorFilename)));
		receiver.append("\" + alternativeLists.toString());"); //$NON-NLS-1$
	}

	private static void appendSafeTitleAnchorExistenceTest(ITreeAppendable receiver, File fileInResource, String anchor) {
		receiver.newLine();
		receiver.append("{").increaseIndentation().newLine(); //$NON-NLS-1$
		receiver.append("// ").append(FileSystem.extension(fileInResource)).append(": "); //$NON-NLS-1$ //$NON-NLS-2$
		receiver.append(fileInResource.getName()).newLine();
		receiver.append(File.class).append(" theFile = new ").append(File.class).append("(\""); //$NON-NLS-1$ //$NON-NLS-2$
		receiver.append(Strings.convertToJavaString(fileInResource.toString())).append("\");"); //$NON-NLS-1$
		receiver.newLine().append("if (theFile.exists()) {"); //$NON-NLS-1$
		receiver.increaseIndentation().newLine();
		receiver.append("String content = ").append(Strings.class); //$NON-NLS-1$
		receiver.append(".concat(\"\\n\", ").append(Files.class); //$NON-NLS-1$
		receiver.append(".readLines(theFile, "); //$NON-NLS-1$
		receiver.append(Charset.class).append(".defaultCharset()));").newLine(); //$NON-NLS-1$
		receiver.append(Matcher.class).append(" matcher = sectionPattern.matcher(content);").newLine(); //$NON-NLS-1$
		receiver.append("while (matcher.find()) {").increaseIndentation().newLine(); //$NON-NLS-1$
		receiver.append("String title = matcher.group(1);").newLine(); //$NON-NLS-1$
		receiver.append("String key1 = computeHeaderIdWithSectionNumber(title);").newLine(); //$NON-NLS-1$
		receiver.append("String key2 = computeHeaderIdWithoutSectionNumber(title);").newLine(); //$NON-NLS-1$
		receiver.append("if (\"").append(Strings.convertToJavaString(anchor)).append("\".equals(key1) || \""); //$NON-NLS-1$ //$NON-NLS-2$
		receiver.append(Strings.convertToJavaString(anchor)).append("\".equals(key2)) {"); //$NON-NLS-1$
		receiver.increaseIndentation().newLine().append("return;").decreaseIndentation().newLine(); //$NON-NLS-1$
		receiver.append("}").decreaseIndentation().newLine(); //$NON-NLS-1$
		receiver.append("}").newLine().append(Assertions.class).append(".fail(\""); //$NON-NLS-1$ //$NON-NLS-2$
		receiver.append(Strings.convertToJavaString(MessageFormat.format(Messages.DynamicValidationContext_0,
				anchor, fileInResource.getName())));
		receiver.append("\");").newLine(); //$NON-NLS-1$
		receiver.append("return;"); //$NON-NLS-1$
		receiver.decreaseIndentation().newLine();
		receiver.append("}").decreaseIndentation().newLine().append("}"); //$NON-NLS-1$ //$NON-NLS-2$
	}

}

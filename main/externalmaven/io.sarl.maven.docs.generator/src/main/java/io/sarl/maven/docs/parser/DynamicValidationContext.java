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

package io.sarl.maven.docs.parser;

import java.io.File;
import java.util.Collections;
import java.util.List;

import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.junit.Assert;

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

	private List<String> tmpResources;

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

	/** Append to the given receiver the code for testing the existency of a file.
	 *
	 * @param receiver the receiver.
	 * @param relativeFile the filename to test.
	 * @param errorLabel the label to be output when the file was not found.
	 */
	public void appendFileExistencyTest(ITreeAppendable receiver, File relativeFile, String errorLabel) {
		if (relativeFile.isAbsolute()) {
			receiver.newLine();
			receiver.append(File.class).append(" file = new ").append(File.class); //$NON-NLS-1$
			receiver.append("(\"").append(Strings.convertToJavaString(relativeFile.toString())).append("\");"); //$NON-NLS-1$ //$NON-NLS-2$
			receiver.newLine();
			receiver.append(Assert.class).append(".assertTrue(\"" + Strings.convertToJavaString(errorLabel) //$NON-NLS-1$
					+ ": \" + file, file.exists());"); //$NON-NLS-1$
		} else {
			for (final String resource : getResourceRoots()) {
				final File fileInResource = FileSystem.makeAbsolute(relativeFile, new File(resource));
				appendSafeFileExistencyTest(receiver, fileInResource);
			}
			for (final String resource : getTempResourceRoots()) {
				final File fileInResource = FileSystem.makeAbsolute(relativeFile, new File(resource));
				appendSafeFileExistencyTest(receiver, fileInResource);
			}
			receiver.newLine();
			receiver.append(Assert.class).append(".fail(\"" + Strings.convertToJavaString(errorLabel) //$NON-NLS-1$
				+ ": " + Strings.convertToJavaString(relativeFile.toString()) + "\");"); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/** Append to the given receiver the code for testing the existency of a file.
	 *
	 * @param receiver the receiver.
	 * @param relativeFile the filename to test.
	 * @param errorLabel the label to be output when the file was not found.
	 * @param extensions the file extensions to be considered as equivalent.
	 */
	public void appendFileExistencyTest(ITreeAppendable receiver, File relativeFile, String errorLabel, Iterable<String> extensions) {
		if (!hasExtension(relativeFile, extensions)) {
			appendFileExistencyTest(receiver, relativeFile, errorLabel);
			return;
		}

		final File fileWithoutExtension = FileSystem.removeExtension(relativeFile);

		for (final String newExtension : extensions) {
			final File fileWithNewExtension = FileSystem.addExtension(fileWithoutExtension, newExtension);
			if (relativeFile.isAbsolute()) {
				appendSafeFileExistencyTest(receiver, fileWithNewExtension);
			} else {
				for (final String resource : getResourceRoots()) {
					final File fileInResource = FileSystem.makeAbsolute(fileWithNewExtension, new File(resource));
					appendSafeFileExistencyTest(receiver, fileInResource);
				}
				for (final String resource : getTempResourceRoots()) {
					final File fileInResource = FileSystem.makeAbsolute(fileWithNewExtension, new File(resource));
					appendSafeFileExistencyTest(receiver, fileInResource);
				}
			}
		}
		receiver.newLine();
		receiver.append(Assert.class).append(".fail(\""); //$NON-NLS-1$
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

	private static void appendSafeFileExistencyTest(ITreeAppendable receiver, File fileInResource) {
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

}

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

package io.sarl.maven.docs;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.Map;

import com.google.common.base.Strings;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.arakhne.afc.vmutil.FileSystem;

import io.sarl.maven.docs.markdown.MarkdownParser;
import io.sarl.maven.docs.parser.AbstractMarkerLanguageParser;

/** Maven MOJO that is generating the documentation for the SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Mojo(name = "generate", defaultPhase = LifecyclePhase.COMPILE, requiresDependencyResolution = ResolutionScope.COMPILE)
public class GenerateMojo extends AbstractDocumentationMojo {

	/**
	 * Location of the generated documentation.
	 */
	@Parameter(defaultValue = "${basedir}/target/classes", required = true)
	protected String outputDirectory;

	/**
	 * Indicates if the Markdown references (addresses in links) should be transformed to
	 * their equivalent HTML addresses. For example "./repo/file.md" is transformed
	 * to "./repo/file.html".
	 *
	 * @see #transformPureHtmlLinks
	 */
	@Parameter(defaultValue = "true", required = false)
	protected boolean md2html;

	/** Indicates if the pure html references (addresses in the "a" tags) should be
	 * transformed to their equivalent HTML addresses. For example "./repo/file.md" is transformed
	 * to "./repo/file.html".
	 *
	 * @see #md2html
	 */
	@Parameter(defaultValue = "true", required = false)
	protected boolean transformPureHtmlLinks;

	/**
	 * Indicates if the sections should be automatically numbered.
	 */
	@Parameter(defaultValue = "true", required = false)
	protected boolean autoSectionNumbering;

	/**
	 * The name of the style to be applied to the outline.
	 * If {@code null} or empty, no style will be applied to the outline.
	 */
	@Parameter(required = false)
	protected String outlineStyleId;

	/**
	 * Indicates the depth level of the section titles that should appear in the outline.
	 *
	 * <p>The depth is a range of the depth levels that should be output.
	 * The format of the range may be: <ul>
	 * <li>{@code n} for only the depth {@code n};</li>
	 * <li>{@code n-m} for the depths from {@code n} to {@code m};</li>
	 * <li>{@code n-} for the depths greater than or equal to {@code n};</li>
	 * <li>{@code -n} for the depths lower than or equal to {@code n};</li>
	 * </ul>
	 */
	@Parameter(defaultValue = "2-3", required = false)
	protected String outlineDepth;

	/**
	 * Indicates if the outline must be automatically generated.
	 *
	 * @since 0.12
	 */
	@Parameter(defaultValue = "true", required = false)
	protected boolean outline;

	/**
	 * Specify an external marker that must be written in place of the automatically generated outline.
	 * This marker is used by the Markdown generator to create automatically the outline, by itself.
	 *
	 * @since 0.12
	 */
	@Parameter(defaultValue = "", required = false)
	protected String outlineExternalMarker;

	/**
	 * Specify if the external Markdown generator is Kramdown. Several internal behaviors are integrated
	 * into the documentation generator in order to fix the bugs of Kramdown (mostly related to the unexpected
	 * auto-formating of the section header reference marks).
	 *
	 * @since 0.12
	 */
	@Parameter(defaultValue = "false", required = false)
	protected boolean kramdown;

	/**
	 * Indicates if the YAML header should be added.
	 */
	@Parameter(defaultValue = "false", required = false)
	protected boolean addYamlHeader;

	/**
	 * Indicates if an hyperlink to the operation documentation should be
	 * added to each generated operation name.
	 */
	@Parameter(defaultValue = "true", required = false)
	protected boolean addLinkToOperationName;

	@Override
	protected String getSkippingMessage() {
		return null;
	}

	@Override
	protected AbstractMarkerLanguageParser createLanguageParser(File inputFile) throws MojoExecutionException, IOException {
		final AbstractMarkerLanguageParser parser = super.createLanguageParser(inputFile);
		if (parser instanceof MarkdownParser) {
			final MarkdownParser mdParser = (MarkdownParser) parser;
			mdParser.setAutoSectionNumbering(this.autoSectionNumbering);
			mdParser.setAddLinkToOperationName(this.addLinkToOperationName);
			mdParser.setOutlineDepthRange(AbstractMarkerLanguageParser.parseRange(this.outlineDepth, 1));
			mdParser.setMarkdownToHtmlReferenceTransformation(this.md2html);
			mdParser.setPureHtmlReferenceTransformation(this.transformPureHtmlLinks);
			mdParser.setOutlineStyleId(this.outlineStyleId);
			mdParser.setOutlineExternalMarker(this.outlineExternalMarker);
			mdParser.setOutlineGeneration(this.outline);
			mdParser.setKramdownFix(this.kramdown);
		}
		return parser;
	}

	@Override
	public String internalExecute(Map<File, File> files) {
		getLog().info(Messages.GenerateMojo_0);
		return internalExecute(files, FileSystem.convertStringToFile(this.outputDirectory));
	}

	@Override
	protected void internalExecute(File sourceFolder, File inputFile, File relativeInputFile, File outputFolder,
			AbstractMarkerLanguageParser parser) throws IOException {
		getLog().debug(MessageFormat.format(Messages.GenerateMojo_1, inputFile.getName()));
		final String newContent = parser.transform(inputFile);

		getLog().debug(MessageFormat.format(Messages.GenerateMojo_2, inputFile.getName()));
		final File relativeFile = FileSystem.makeRelative(inputFile, sourceFolder);
		final File outputFile = FileSystem.makeAbsolute(relativeFile, outputFolder);

		outputFile.getParentFile().mkdirs();
		try (FileWriter writer = new FileWriter(outputFile)) {
			if (this.addYamlHeader) {
				writer.write("---\ntitle: \""); //$NON-NLS-1$
				writer.write(Strings.nullToEmpty(parser.extractPageTitle(newContent)));
				writer.write("\"\nlayout: default\n---\n\n"); //$NON-NLS-1$
			}
			writer.write(newContent);
			writer.flush();
		} finally {
			getLog().debug(MessageFormat.format(Messages.GenerateMojo_3, inputFile.getName()));
		}
	}

}

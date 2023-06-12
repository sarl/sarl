/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.doclet2.html.frames;

import static io.sarl.docs.doclet2.html.framework.HtmlTags.*;

import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.Collection;

import javax.tools.Diagnostic.Kind;

import jdk.javadoc.doclet.Reporter;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.DocumentType;
import org.jsoup.nodes.Element;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.AbstractDocumentationGenerator;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.Navigation.NavigationKind;

/** Generate the main HTML index file.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class HtmlIndexGeneratorImpl extends AbstractDocumentationGenerator implements HtmlIndexGenerator {

	/** Name of the main frame for package content.
	 */
	public static final String MAIN_PACKAGE_FRAME_NAME = "packageFrame";
	
	/** Name of the main frame for type content.
	 */
	public static final String MAIN_TYPE_FRAME_NAME = "typeFrame";

	private static final String SCRIPT_0 = "    tmpTargetPage = \"\" + window.location.search;\n"
			+ "    if (tmpTargetPage != \"\" && tmpTargetPage != \"undefined\")\n"
			+ "        tmpTargetPage = tmpTargetPage.substring(1);\n"
			+ "    if (tmpTargetPage.indexOf(\":\") != -1 || (tmpTargetPage != \"\" && !validURL(tmpTargetPage)))\n"
			+ "        tmpTargetPage = \"undefined\";\n"
			+ "    targetPage = tmpTargetPage;\n"
			+ "    function validURL(url) {\n"
			+ "        try {\n"
			+ "            url = decodeURIComponent(url);\n"
			+ "        }\n"
			+ "        catch (error) {\n"
			+ "            return false;\n"
			+ "        }\n"
			+ "        var pos = url.indexOf(\".html\");\n"
			+ "        if (pos == -1 || pos != url.length - 5)\n"
			+ "            return false;\n"
			+ "        var allowNumber = false;\n"
			+ "        var allowSep = false;\n"
			+ "        var seenDot = false;\n"
			+ "        for (var i = 0; i < url.length - 5; i++) {\n"
			+ "            var ch = url.charAt(i);\n"
			+ "            if ('a' <= ch && ch <= 'z' ||\n"
			+ "                    'A' <= ch && ch <= 'Z' ||\n"
			+ "                    ch == '$' ||\n"
			+ "                    ch == '_' ||\n"
			+ "                    ch.charCodeAt(0) > 127) {\n"
			+ "                allowNumber = true;\n"
			+ "                allowSep = true;\n"
			+ "            } else if ('0' <= ch && ch <= '9'\n"
			+ "                    || ch == '-') {\n"
			+ "                if (!allowNumber)\n"
			+ "                     return false;\n"
			+ "            } else if (ch == '/' || ch == '.') {\n"
			+ "                if (!allowSep)\n"
			+ "                    return false;\n"
			+ "                allowNumber = false;\n"
			+ "                allowSep = false;\n"
			+ "                if (ch == '.')\n"
			+ "                     seenDot = true;\n"
			+ "                if (ch == '/' && seenDot)\n"
			+ "                     return false;\n"
			+ "            } else {\n"
			+ "                return false;\n"
			+ "            }\n"
			+ "        }\n"
			+ "        return true;\n"
			+ "    }\n"
			+ "    function loadFrames() {\n"
			+ "        if (targetPage != \"\" && targetPage != \"undefined\")\n"
			+ "             top.classFrame.location = top.targetPage;\n"
			+ "    }";
	
	public HtmlIndexGeneratorImpl() {
		//
	}

	@Override
	protected String getDocumentTitleFor(String elementName) {
		return getDocumentationTitle();
	}

	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(NavigationKind.OVERVIEW);
	}

	@Override
	public void generate(Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		initGenerator(cssStylesheets, jsScripts, reporter, environment, cliOptions);
		getReporter().print(Kind.NOTE, Messages.HtmlIndexGeneratorImpl_0);
		computePaths(getPathBuilder().htmlIndexFile(), true);
		//
		final Path outputPath = getCliOptions().getOutputDirectory().resolve(getRelativePath());
		//
		final DocumentType docType = getHtmlFactory().createDocumentType(
				"html",
				"-//W3C//DTD XHTML 1.0 Frameset//EN",
				"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd");
		final Document document = getHtmlFactory().createDocument(docType, this);
		final String title = getDocumentTitleFor(null);
		setLastTitle(title);
		final Element htmlTag = getHtmlAccessor().getRootElement(document);
		//
		generateContentHead(htmlTag);
		generateContentBody(htmlTag);
		//
		if (!getCliOptions().isFakeOutput()) {
			getReporter().print(Kind.NOTE, MessageFormat.format(Messages.HtmlIndexGeneratorImpl_1, outputPath.toString()));
			writeDocument(outputPath, document);
		}
	}

	/** Generate the content head of the main index file.
	 *
	 * @param content the content.
	 */
	protected void generateContentHead(Element content) {
		final Element head = getHtmlFactory().createHeadTag(content);
		getHtmlFactory().createTitleTag(head, getDocumentationTitle());
		final Element script = getHtmlFactory().createScriptTag(head, "text/javascript");
		script.append(SCRIPT_0);
	}

	/** Generate the content body of the main index file.
	 *
	 * @param content the content.
	 */
	protected void generateContentBody(Element content) {
		final Element bodyTag = getHtmlFactory().createBodyTag(content);
		bodyTag.remove();
		//
		final Element docFrame = getHtmlFactory().createFramesetTag(content);
		docFrame.attr(COLS_ATTR, "20%,80%");
		docFrame.attr(TITLE_ATTR, Messages.HtmlIndexGeneratorImpl_2);
		docFrame.attr(ONLOAD_ATTR, "top.loadFrames()");
		//
		final Element leftFrames = getHtmlFactory().createFramesetTag(docFrame);
		leftFrames.attr(ROWS_ATTR, "30%,70%");
		leftFrames.attr(TITLE_ATTR, Messages.HtmlIndexGeneratorImpl_3);
		leftFrames.attr(ONLOAD_ATTR, "top.loadFrames()");
		//
		final Element leftOverviewFrame = getHtmlFactory().createFrameTag(leftFrames);
		Path path = getPathToRoot().resolve(getPathBuilder().overviewFrame());
		leftOverviewFrame.attr(SRC_ATTR, getHtmlFactory().path2UrlPath(path));
		leftOverviewFrame.attr(NAME_ATTR, "packageListFrame");
		leftOverviewFrame.attr(TITLE_ATTR, Messages.HtmlIndexGeneratorImpl_7);
		//
		final Element allClassFrame = getHtmlFactory().createFrameTag(leftFrames);
		path = getPathToRoot().resolve(getPathBuilder().allTypesFrame());
		allClassFrame.attr(SRC_ATTR, getHtmlFactory().path2UrlPath(path));
		allClassFrame.attr(NAME_ATTR, MAIN_PACKAGE_FRAME_NAME);
		allClassFrame.attr(TITLE_ATTR, Messages.HtmlIndexGeneratorImpl_8);
		//
		final Element typeFrame = getHtmlFactory().createFrameTag(docFrame);
		path = getPathToRoot().resolve(getPathBuilder().overviewSummary());
		typeFrame.attr(SRC_ATTR, getHtmlFactory().path2UrlPath(path));
		typeFrame.attr(NAME_ATTR, MAIN_TYPE_FRAME_NAME);
		typeFrame.attr(TITLE_ATTR, Messages.HtmlIndexGeneratorImpl_9);
		typeFrame.attr(SCROLLING_ATTR, BOOLEAN_YES_ATTR_VALUE);
		//
		final Element noframes = getHtmlFactory().createNoFramesTag(docFrame);
		final Element noscript = getHtmlFactory().createNoScriptTag(noframes);
		final Element noscriptDiv = getHtmlFactory().createDivTag(noscript, null);
		noscriptDiv.appendText(Messages.HtmlIndexGeneratorImpl_4);
		final Element title = getHtmlFactory().createDivTag(noframes, null);
		title.appendText(Messages.HtmlIndexGeneratorImpl_5);
		final Element alert = getHtmlFactory().createDivTag(noframes, null);
		alert.append(Messages.HtmlIndexGeneratorImpl_6);
	}

}

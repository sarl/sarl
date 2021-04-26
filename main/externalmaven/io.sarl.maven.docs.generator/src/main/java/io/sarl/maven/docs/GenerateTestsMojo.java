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
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.Proxy.Type;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.net.URLConnection;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.inject.Injector;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.generator.trace.ILocationData;
import org.eclipse.xtext.generator.trace.LocationData;
import org.eclipse.xtext.generator.trace.TraceRegionSerializer;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.output.FakeTreeAppendable;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.opentest4j.AssertionFailedError;
import org.opentest4j.TestAbortedException;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.util.Utils;
import io.sarl.maven.docs.markdown.MarkdownParser;
import io.sarl.maven.docs.parser.AbstractMarkerLanguageParser;
import io.sarl.maven.docs.parser.DynamicValidationComponent;
import io.sarl.maven.docs.parser.DynamicValidationContext;
import io.sarl.maven.docs.parser.ValidationComponent;
import io.sarl.maven.docs.testing.DocumentationSetup;
import io.sarl.maven.docs.testing.ScriptExecutor;

/** Maven MOJO that is generating the documentation tests for the SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Mojo(name = "generatetests", defaultPhase = LifecyclePhase.GENERATE_TEST_SOURCES,
	requiresDependencyResolution = ResolutionScope.TEST)
public class GenerateTestsMojo extends AbstractDocumentationMojo {

	private static final String BASE_PACKAGE = "io.sarl.maven.docs"; //$NON-NLS-1$

	/**
	 * Indicates if the references to the local files should be validated.
	 */
	@Parameter(defaultValue = "true", required = false)
	protected boolean localLinkValidation;

	/**
	 * Indicates if the references to the remote Internet pages should be validated.
	 */
	@Parameter(defaultValue = "true", required = false)
	protected boolean remoteLinkValidation;

	/**
	 * Indicates if the timeout to use for connecting to remote hosts.
	 * This timeout value is expressed in milliseconds.
	 */
	@Parameter(defaultValue = "5000", required = false)
	protected int remoteLinkTimeOut;

	/**
	 * Indicates if a timeout for connecting to remote hosts should be ignored.
	 * If the timeout is ignore, the corresponding test will be marked as "ignored".
	 *
	 * @see Assume
	 */
	@Parameter(defaultValue = "true", required = false)
	protected boolean ignoreRemoteLinkTimeOut;

	/**
	 * Indicates if the references to the local files should be validated.
	 */
	@Parameter(defaultValue = "true", required = false)
	protected boolean localImageValidation;

	private TraceRegionSerializer traceSerializer;

	/** Replies the trace serializer.
	 *
	 * @return the trace serializer.
	 */
	protected TraceRegionSerializer getTraceSerializer() {
		if (this.traceSerializer == null) {
			this.traceSerializer = this.injector.getInstance(TraceRegionSerializer.class);
		}
		return this.traceSerializer;
	}

	@Override
	protected String getSkippingMessage() {
		final String[] variableNames = {
			"maven.test.skip", //$NON-NLS-1$
			"skipTests", //$NON-NLS-1$
			"io.sarl.docs.tests.skip", //$NON-NLS-1$
		};
		String value = null;
		int i = 0;
		while (Strings.isEmpty(value) && i < variableNames.length) {
			value = System.getProperty(variableNames[i]);
			++i;
		}
		i = 0;
		while (Strings.isEmpty(value) && i < variableNames.length) {
			value = System.getenv(variableNames[i]);
			++i;
		}
		if (Boolean.valueOf(value)) {
			return "Tests are skipped."; //$NON-NLS-1$
		}
		return null;
	}

	@Override
	protected AbstractMarkerLanguageParser createLanguageParser(File inputFile) throws MojoExecutionException, IOException {
		final AbstractMarkerLanguageParser parser = super.createLanguageParser(inputFile);
		if (parser instanceof MarkdownParser) {
			final MarkdownParser mdParser = (MarkdownParser) parser;
			mdParser.setLocalImageReferenceValidation(this.localImageValidation);
			mdParser.setLocalFileReferenceValidation(this.localLinkValidation);
			mdParser.setRemoteReferenceValidation(
					!this.session.isOffline() && this.remoteLinkValidation);
		}
		return parser;
	}

	@Override
	protected String internalExecute(Map<File, File> files) {
		getLog().info(Messages.GenerateTestsMojo_11);
		final File output = FileSystem.convertStringToFile(this.testSourceDirectory);
		final String msg = internalExecute(files, output);
		if (!Strings.isEmpty(msg)) {
			return msg;
		}

		try {
			generateAbstractTest(output);
		} catch (IOException exception) {
			final String message = Throwables.getRootCause(exception).getLocalizedMessage();
			getLog().error(message);
			getLog().debug(exception);
			return message;
		}

		return null;
	}

	@Override
	protected String internalExecute(Map<File, File> files, File outputFolder) {
		try {
			FileSystem.delete(outputFolder);
		} catch (IOException exception) {
			final String message = formatErrorMessage(outputFolder, exception);
			getLog().error(message);
			getLog().debug(exception);
			return message;
		}
		return super.internalExecute(files, outputFolder);
	}

	@SuppressWarnings({"checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity", "all"})
	@Override
	protected void internalExecute(File sourceFolder, File inputFile, File relativeInputFile, File outputFolder,
			AbstractMarkerLanguageParser parser) throws IOException {
		getLog().debug(MessageFormat.format(Messages.GenerateTestsMojo_0, inputFile.getName()));
		final List<ValidationComponent> successCompilationComponents = new ArrayList<>();
		final List<ValidationComponent> failureCompilationComponents = new ArrayList<>();
		final List<ValidationComponent> factualComponents = new ArrayList<>();
		for (final ValidationComponent component : parser.getStandardValidationComponents(inputFile)) {
			if (component.isCompilable()) {
				if (component.isExecutable()) {
					factualComponents.add(component);
				} else {
					successCompilationComponents.add(component);
				}
			} else {
				failureCompilationComponents.add(component);
			}
		}

		final DynamicValidationContext validationContext = new DynamicValidationContext();
		validationContext.setSourceRoots(this.session.getCurrentProject().getCompileSourceRoots());
		validationContext.setResourceRoots(Lists.transform(this.session.getCurrentProject().getResources(),
				it -> it.getDirectory()));
		validationContext.setDestinationRoots(
				Collections.singletonList(this.session.getCurrentProject().getBuild().getOutputDirectory()));
		final List<DynamicValidationComponent> specificComponents = parser.getMarkerSpecificValidationComponents(
				inputFile, sourceFolder, validationContext);
		if (successCompilationComponents.isEmpty() && failureCompilationComponents.isEmpty()
				&& factualComponents.isEmpty() && specificComponents.isEmpty()) {
			return;
		}

		// Do not change the "Test" postfix because it is used by Surefire for detecting test classes.
		final String basicTestName = toTestName(inputFile);
		final String generalTestName = basicTestName + "Test"; //$NON-NLS-1$

		final ImportManager importManager = new ImportManager();
		final TraceableTreeAppendable it = new TraceableTreeAppendable(importManager);

		final String displayName = toClassDisplayName(relativeInputFile, basicTestName, generalTestName);
		
		it.append("@").append(DisplayName.class).append("(\"").append(displayName).append("\")").newLine();
		it.append("@").append(Tag.class).append("(\"documentation\")").newLine();
		it.append("@").append(Tag.class).append("(\"doc\")").newLine();
		it.append("public class ").append(generalTestName).append(" extends ")
			.append(BASE_PACKAGE).append(".AbstractBaseTest").append(" {").increaseIndentation().newLine();
		
		generateTestsForSuccessCode(it, importManager, relativeInputFile, successCompilationComponents);
		generateTestsForFailureCode(it, importManager, relativeInputFile, failureCompilationComponents);
		generateTestsForFacts(it, importManager, relativeInputFile, factualComponents);
		generateDynamicTests(it, importManager, relativeInputFile, specificComponents);

		it.decreaseIndentation().newLine().append("}").newLine();

		final File packagePath = relativeInputFile.getParentFile();
		final String packageName = toPackageName("docs", packagePath); //$NON-NLS-1$
		write(outputFolder, packageName, generalTestName, importManager, it);

		//generateTraceFile(outputFolder, packageName, generalTestName, it.getTraceRegions());
	}

	private void generateTestsForSuccessCode(ITreeAppendable parent, ImportManager importManager,
			File inputFile, List<ValidationComponent> successCompilationComponents) {
		int i = 0;
		for (final ValidationComponent component : successCompilationComponents) {
			getLog().debug(MessageFormat.format(Messages.GenerateTestsMojo_1,
					inputFile.getName(), component.getLinenoInSourceFile(), component.getCode()));
			final String actionName = toActionName("success", component, i); //$NON-NLS-1$
			final String displayName = toTestDisplayName(Messages.GenerateTestsMojo_7, i, component);
			final ILocationData location = new LocationData(
					// Offset
					component.getOffsetInSourceFile(),
					// Length
					component.getLengthInSourceFile(),
					// Line number
					component.getLinenoInSourceFile(),
					// End line number
					component.getEndLinenoInSourceFile(),
					// Source URI
					null);
			final ITreeAppendable it = parent.trace(location);
			//
			it.append("@").append(Test.class).newLine();
			it.append("@").append(DisplayName.class).append("(\"").append(displayName).append("\")").newLine();
			it.append("@").append(Tag.class).append("(\"success\")").newLine();
			it.append("@").append(Tag.class).append("(\"success_").append(Integer.toString(i)).append("\")").newLine();
			it.append("public void ").append(actionName).append("() throws ")
				.append(Exception.class).append(" {").increaseIndentation().newLine();
			it.append(List.class).append("<String> issues = getScriptExecutor().compile(")
				.append(str(component.getLinenoInSourceFile())).append(", \"")
				.append(str(component.getCode())).append("\");").newLine();
			it.append("assertNoIssue(").append(str(component.getLinenoInSourceFile()))
				.append(", issues);").decreaseIndentation().newLine();
			it.append("}").newLine();
			//
			++i;
		}
	}

	private void generateTestsForFailureCode(ITreeAppendable parent, ImportManager importManager,
			File inputFile, List<ValidationComponent> failureCompilationComponents) {
		int i = 0;
		for (final ValidationComponent component : failureCompilationComponents) {
			getLog().debug(MessageFormat.format(Messages.GenerateTestsMojo_2,
					inputFile.getName(), component.getLinenoInSourceFile(), component.getCode()));
			final String actionName = toActionName("failure", component, i); //$NON-NLS-1$
			final String displayName = toTestDisplayName(Messages.GenerateTestsMojo_8, i, component);
			final ILocationData location = new LocationData(
					// Offset
					component.getOffsetInSourceFile(),
					// Length
					component.getLengthInSourceFile(),
					// Line number
					component.getLinenoInSourceFile(),
					// End line number
					component.getEndLinenoInSourceFile(),
					// Source URI
					null);
			final ITreeAppendable it = parent.trace(location);
			//
			it.append("@").append(Test.class).newLine();
			it.append("@").append(DisplayName.class).append("(\"").append(displayName).append("\")").newLine();
			it.append("@").append(Tag.class).append("(\"failure\")").newLine();
			it.append("@").append(Tag.class).append("(\"failure_").append(Integer.toString(i)).append("\")").newLine();
			it.append("public void ").append(actionName).append("() throws ")
				.append(Exception.class).append(" {").increaseIndentation().newLine();
			it.append(List.class).append("<String> issues = getScriptExecutor().compile(")
				.append(str(component.getLinenoInSourceFile())).append(", \"")
				.append(str(component.getCode())).append("\");").newLine();
			it.append("assertIssues(").append(str(component.getLinenoInSourceFile()))
				.append(", issues);").decreaseIndentation().newLine();
			it.append("}").newLine();
			//
			++i;
		}
	}

	private void generateTestsForFacts(ITreeAppendable parent, ImportManager importManager,
			File inputFile, List<ValidationComponent> factualComponents) {
		int i = 0;
		for (final ValidationComponent component : factualComponents) {
			getLog().debug(MessageFormat.format(Messages.GenerateTestsMojo_3,
					inputFile.getName(), component.getLinenoInSourceFile(), component.getCode()));
			final String actionName = toActionName("fact", component, i); //$NON-NLS-1$
			final String displayName = toTestDisplayName(Messages.GenerateTestsMojo_9, i, component);
			final ILocationData location = new LocationData(
					// Offset
					component.getOffsetInSourceFile(),
					// Length
					component.getLengthInSourceFile(),
					// Line number
					component.getLinenoInSourceFile(),
					// End line number
					component.getEndLinenoInSourceFile(),
					// Source URI
					null);
			final ITreeAppendable it = parent.trace(location);
			//
			it.append("@").append(Test.class).newLine();
			it.append("@").append(DisplayName.class).append("(\"").append(displayName).append("\")").newLine();
			it.append("@").append(Tag.class).append("(\"fact\")").newLine();
			it.append("@").append(Tag.class).append("(\"fact_").append(Integer.toString(i)).append("\")").newLine();
			it.append("public void ").append(actionName).append("() throws ")
				.append(Exception.class).append(" {").increaseIndentation().newLine();
			it.append("final String expected = ").append(Utils.class).append(".dump(")
				.append(Boolean.class).append(".TRUE, false) + \"\\nOR\\nObject {\\n}\\n\";").newLine();
			it.append("Object result;").newLine();
			it.append("try {").increaseIndentation().newLine();
			it.append("result = getScriptExecutor().execute(").append(str(component.getLinenoInSourceFile()))
				.append(", \"").append(str(component.getCode())).append("\");").decreaseIndentation().newLine();
			it.append("} catch (").append(Throwable.class).append(" exception) {").increaseIndentation().newLine();
			it.append("throw new ").append(AssertionFailedError.class)
				.append("(exception.getLocalizedMessage() + \" [line: ")
				.append(str(component.getLinenoInSourceFile())).append("]\", expected, ")
				.append(Throwables.class).append(".getStackTraceAsString(exception));").decreaseIndentation().newLine();
			it.append("}").newLine();
			it.append("if (result instanceof ").append(Boolean.class).append(") {").increaseIndentation().newLine();
			it.append("boolean boolResult = ((").append(Boolean.class).append(") result).booleanValue();").newLine();
			it.append("if (!boolResult) {").increaseIndentation().newLine();
			it.append("throw new ").append(AssertionFailedError.class)
				.append("(\"Invalid expression result [line: ").append(str(component.getLinenoInSourceFile()))
				.append("]\", expected, ").append(Utils.class).append(".dump(result, false));").decreaseIndentation().newLine();
			it.append("}").decreaseIndentation().newLine();
			it.append("} else if (result == null || result instanceof ").append(Exception.class)
				.append(") {").increaseIndentation().newLine();
			it.append("throw new ").append(AssertionFailedError.class)
				.append("(\"Invalid expression result [line: ").append(str(component.getLinenoInSourceFile()))
				.append("]\", expected, ").append(Utils.class).append(".dump(result, false));").decreaseIndentation().newLine();
			it.append("}").decreaseIndentation().newLine();
			it.append("}").newLine();
			//
			++i;
		}
	}

	private void generateDynamicTests(ITreeAppendable parent, ImportManager importManager,
			File inputFile, List<DynamicValidationComponent> specificComponents) {
		int i = 0;
		for (final DynamicValidationComponent component : specificComponents) {
			getLog().debug(MessageFormat.format(Messages.GenerateTestsMojo_4,
					inputFile.getName(), component.functionName() + i));
			final String actionName = toActionName("dyn_" + component.functionName(), component, i);
			final String displayName = toTestDisplayName(Messages.GenerateTestsMojo_10, i, component);
			final ILocationData location = new LocationData(
					// Offset
					component.getOffsetInSourceFile(),
					// Length
					component.getLengthInSourceFile(),
					// Line number
					component.getLinenoInSourceFile(),
					// End line number
					component.getEndLinenoInSourceFile(),
					// Source URI
					null);
			final ITreeAppendable it = parent.trace(location);
			//
			it.append("@").append(Test.class).newLine();
			it.append("@").append(DisplayName.class).append("(\"").append(displayName).append("\")").newLine();
			it.append("@").append(Tag.class).append("(\"other\")").newLine();
			it.append("@").append(Tag.class).append("(\"other_").append(Integer.toString(i)).append("\")").newLine();
			it.append("public void ").append(actionName).append("() throws ")
				.append(Exception.class).append(" {").increaseIndentation().newLine();
			component.generateValidationCode(it);
			it.decreaseIndentation().newLine();
			it.append("}").newLine();
			//
			++i;
		}
	}

	/*private void generateTraceFile(File root, String packageName, String typeName, Collection<AbstractTraceRegion> regions) throws IOException {
		if (!regions.isEmpty()) {
			File folderFile = getOutputFolder(root, packageName);
			File outputFile = getOutputTraceFilename(folderFile, typeName);
			getLog().debug(MessageFormat.format(Messages.GenerateTestsMojo_6, outputFile.getName()));
			folderFile.mkdirs();
			try (final FileOutputStream os = new FileOutputStream(outputFile)) {
				for (final AbstractTraceRegion region : regions) {
					getTraceSerializer().writeTraceRegionTo(region, os);
				}
			}
		}
	}*/

	@SuppressWarnings({"checkstyle:methodlength", "checkstyle:npathcomplexity", "deprecation"})
	private void generateAbstractTest(File outputFolder) throws IOException {
		getLog().debug(Messages.GenerateTestsMojo_5); //$NON-NLS-1$
		final ImportManager importManager = new ImportManager();
		final ITreeAppendable it = new FakeTreeAppendable(importManager);

		it.append("@").append(SuppressWarnings.class).append("(\"all\")").newLine();
		it.append("public class AbstractBaseTest {").increaseIndentation().newLine();
		it.append("protected static String STR_SUCCESS = \"success\";").newLine();
		it.append("protected static String STR_FAILURE = \"failure\";").newLine();
		it.append("protected static String STR_FACT = \"fact\";").newLine();
		it.append("private static ").append(Injector.class).append(" injector = ")
		.append(DocumentationSetup.class).append(".doSetup();").newLine();
		it.append("private ").append(ScriptExecutor.class).append(" scriptExecutor;").newLine();

		it.append("protected ").append(ScriptExecutor.class).append(" getScriptExecutor() {").increaseIndentation().newLine();
		it.append("if (this.scriptExecutor == null) {").increaseIndentation().newLine();
		it.append("this.scriptExecutor = this.injector.getInstance(").append(ScriptExecutor.class).append(".class);").newLine();

		final StringBuilder cp = new StringBuilder();
		for (final File cpElement : getClassPath()) {
			if (cp.length() > 0) {
				cp.append(File.pathSeparator);
			}
			cp.append(cpElement.getAbsolutePath());
		}
		it.append("scriptExecutor.setClassPath(\"").append(str(cp)).append("\");").newLine();

		final StringBuilder mp = new StringBuilder();
		for (final File mpElement : getModulePath()) {
			if (mp.length() > 0) {
				mp.append(File.pathSeparator);
			}
			mp.append(mpElement.getAbsolutePath());
		}
		it.append("scriptExecutor.setModulePath(\"").append(str(mp)).append("\");").newLine();

		final String bootPath = getBootClassPath();
		if (!Strings.isEmpty(bootPath)) {
			it.append("scriptExecutor.setBootClassPath(\"") //$NON-NLS-1$
				.append(str(bootPath)).append("\");") //$NON-NLS-1$
				.newLine();
		}
		JavaVersion version = null;
		if (!Strings.isEmpty(this.source)) {
			try {
				version = JavaVersion.fromQualifier(this.source);
			} catch (Throwable exception) {
				version = null;
			}
		}
		if (version == null) {
			try {
				version = JavaVersion.fromQualifier(SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
			} catch (Throwable exception) {
				version = null;
			}
		}
		if (version == null) {
			throw new IllegalStateException("unknown Java version"); //$NON-NLS-1$
		}
		it.append("scriptExecutor.setJavaSourceVersion(\"").append(version.getQualifier()).append("\");").newLine();

		it.append("scriptExecutor.setTempFolder(new ").append(File.class).append("(\"")
		.append(str(this.tempDirectory.getAbsolutePath())).append("\"));")
		.decreaseIndentation().newLine();

		it.append("}").newLine();
		it.append("return this.scriptExecutor;").decreaseIndentation().newLine();
		it.append("}").newLine();

		it.append("public void assertNoIssue(int lineno, ").append(List.class).append("<String> issues) {")
		.increaseIndentation().newLine();
		it.append("if (issues != null && !issues.isEmpty()) {").increaseIndentation().newLine();
		it.append(StringBuilder.class).append(" msg = new ").append(StringBuilder.class).append("();").newLine();
		it.append("for (String message : issues) {").increaseIndentation().newLine();
		it.append("msg.append(message).append(\"\\n\");").decreaseIndentation().newLine();
		it.append("}").newLine();
		it.append("throw new ").append(AssertionFailedError.class)
		.append("(\"Expecting no issue but find one [line:\" + lineno + \"]\\n\" + msg, \"\", msg.toString());")
		.decreaseIndentation().newLine();
		it.append("}").decreaseIndentation().newLine();
		it.append("}").newLine();

		it.append("public void assertIssues(int lineno, ").append(List.class).append("<String> issues) {")
		.increaseIndentation().newLine();
		it.append("if (issues == null || issues.isEmpty()) {").increaseIndentation().newLine();
		it.append(Assertions.class).append(".fail(\"Expecting issues but did not find one [line:\" + lineno + \"]\");")
		.decreaseIndentation().newLine();
		it.append("}").decreaseIndentation().newLine();
		it.append("}").newLine();

		it.append("public String computeHeaderIdWithSectionNumber(String header) {").increaseIndentation().newLine();
		it.append("String id = header.replaceAll(\"[^a-zA-Z0-9]+\", \"-\");").newLine();
		it.append("id = id.toLowerCase();").newLine();
		it.append("id = id.replaceFirst(\"^[^a-zA-Z0-9]+\", \"\");").newLine();
		it.append("id = id.replaceFirst(\"[^a-zA-Z0-9]+$\", \"\");").newLine();
		it.append("if (").append(Strings.class).append(".isEmpty(id)) {").increaseIndentation().newLine();
		it.append("return \"section\";").decreaseIndentation().newLine();
		it.append("}").newLine();
		it.append("return id;").decreaseIndentation().newLine();
		it.append("}").newLine();

		it.append("public String computeHeaderIdWithoutSectionNumber(String header) {").increaseIndentation().newLine();
		it.append("String id = computeHeaderIdWithSectionNumber(header);").newLine();
		it.append("id = id.replaceFirst(\"^[0-9.\\\\-]+\", \"\");").newLine();
		it.append("return id;").decreaseIndentation().newLine();
		it.append("}").newLine();

		it.append("public static String getHttpCodeExplanation(int code) {").increaseIndentation().newLine();
		it.append("switch (code) {").increaseIndentation().newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_BAD_METHOD: return \"Method Not Allowed\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_CREATED: return \"Created\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_ACCEPTED: return \"Accepted\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_AUTHORITATIVE: return \"Non-Authoritative Information\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NO_CONTENT: return \"No Content\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_RESET: return \"Reset Content\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PARTIAL: return \"Partial Content\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_MULT_CHOICE: return \"Multiple Choices\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_MOVED_PERM: return \"Moved Permanently\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_MOVED_TEMP: return \"Temporary Redirect\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_SEE_OTHER: return \"See Other\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_MODIFIED: return \"Not Modified\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_USE_PROXY: return \"Use Proxy\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_BAD_REQUEST: return \"Bad Request\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_UNAUTHORIZED: return \"Unauthorized\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PAYMENT_REQUIRED: return \"Payment Required\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_FORBIDDEN: return \"Forbidden\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_FOUND: return \"Not Found\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_ACCEPTABLE: return \"Not Acceptable\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PROXY_AUTH: return \"Proxy Authentication Required\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_CLIENT_TIMEOUT: return \"Request Time-Out\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_CONFLICT: return \"Conflict\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_GONE: return \"Gone\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_LENGTH_REQUIRED: return \"Length Required\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PRECON_FAILED: return \"Precondition Failed\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_ENTITY_TOO_LARGE: return \"Request Entity Too Large\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_REQ_TOO_LONG: return \"Request-URI Too Large\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_UNSUPPORTED_TYPE: return \"Unsupported Media Type\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_INTERNAL_ERROR: return \"Internal Server Error\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_IMPLEMENTED: return \"Not Implemented\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_BAD_GATEWAY: return \"Bad Gateway\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_UNAVAILABLE: return \"Service Unavailable\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_GATEWAY_TIMEOUT: return \"Gateway Timeout\";").newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_VERSION: return \"HTTP Version Not Supported\";").newLine();
		it.append("default: return null;").decreaseIndentation().newLine();
		it.append("}").decreaseIndentation().newLine();
		it.append("}").newLine();

		it.append("public static boolean isAcceptableHttpCode(int code) {").increaseIndentation().newLine();
		it.append("return code == ").append(HttpURLConnection.class)
		.append(".HTTP_OK || code == ").append(HttpURLConnection.class).append(".HTTP_MOVED_TEMP;")
		.decreaseIndentation().newLine();
		it.append("}").newLine();

		it.append("public static void assertURLAccessibility(int lineno, ").append(URL.class)
		.append(" url) throws ").append(Exception.class).append(" {").increaseIndentation().newLine();
		it.append("int code;").newLine();
		it.append(HttpURLConnection.class).append(".setFollowRedirects(false);").newLine();
		it.append(URLConnection.class).append(" connection = url.openConnection();").newLine();
		it.append(Assumptions.class).append(".assumeTrue(connection instanceof ")
		.append(HttpURLConnection.class).append(", \"Not an UTL with http[s] protocol\");").newLine();
		it.append(HttpURLConnection.class).append(" httpConnection = (").append(HttpURLConnection.class)
		.append(") connection;").newLine();
		it.append("try {").increaseIndentation().newLine();
		it.append("httpConnection.setInstanceFollowRedirects(false);").newLine();
		it.append("httpConnection.setConnectTimeout(").append(str(this.remoteLinkTimeOut)).append(");").newLine();
		it.append("httpConnection.setReadTimeout(").append(str(this.remoteLinkTimeOut)).append(");").newLine();
		it.append("httpConnection.setAllowUserInteraction(false);").newLine();
		it.append("httpConnection.setRequestMethod(\"HEAD\");").newLine();
		it.append("httpConnection.connect();").newLine();
		it.append("code = httpConnection.getResponseCode();").decreaseIndentation().newLine();
		it.append("} catch (").append(IOException.class).append(" exception) {").increaseIndentation().newLine();
		it.append(Throwable.class).append(" rootCause = ").append(Throwables.class).append(".getRootCause(exception);").newLine();
		if (this.ignoreRemoteLinkTimeOut) {
			it.append("if (rootCause instanceof ").append(SocketTimeoutException.class).append(") {").increaseIndentation().newLine();
			it.append("throw new ").append(TestAbortedException.class)
			.append("(\"Connection time-out at line \" + lineno + \" when connecting to: \" + url.toExternalForm());")
			.decreaseIndentation().newLine();
			it.append("}").newLine();
		}
		it.append("throw new ").append(RuntimeException.class)
		.append("(\"Error at line \" + lineno + \" when connecting to: \" + url.toExternalForm(), rootCause);")
		.decreaseIndentation().newLine();
		it.append("} finally {").increaseIndentation().newLine();
		it.append("httpConnection.disconnect();").decreaseIndentation().newLine();
		it.append("}").newLine();
		it.append("if (isAcceptableHttpCode(code)) {").increaseIndentation().newLine();
		it.append("return;").decreaseIndentation().newLine();
		it.append("}").newLine();
		it.append("String explanation = getHttpCodeExplanation(code);").newLine();
		it.append("String codeMsg = !").append(Strings.class).append(".isEmpty(explanation) ? code + \"\\\"\" + explanation + \"\\\"\" : Integer.toString(code);")
		.newLine();
		it.append(Assertions.class).append(".fail(\"Invalid response code \" + codeMsg + \" at line \" + lineno + \" when connecting to: \" + url.toExternalForm());")
		.decreaseIndentation().newLine();
		it.append("}").newLine();

		if (!this.session.isOffline() && !this.session.getRequest().getProxies().isEmpty()) {

			it.append("private static boolean proxyNameMatches(String pattern, String name) {").increaseIndentation().newLine();
			it.append(Pattern.class).append(" pat = ").append(Pattern.class)
			.append(".compile(pattern, ").append(Pattern.class).append(".CASE_INSENSITIVE);").newLine();
			it.append(Matcher.class).append(" mat = pat.matcher(name);").newLine();
			it.append("return mat.matches();").decreaseIndentation().newLine();
			it.append("}").newLine();

			it.append("static {").increaseIndentation().newLine();
			it.append("final ").append(ProxySelector.class).append(" defaultSelector = ")
			.append(ProxySelector.class).append(".getDefault();").newLine();
			it.append(ProxySelector.class).append(" newSelector = new ")
			.append(ProxySelector.class).append("() {").increaseIndentation().newLine();
			it.append("public ").append(List.class).append("<").append(Proxy.class)
			.append("> select(").append(java.net.URI.class).append(" uri) {").increaseIndentation().newLine();
			it.append(List.class).append("<").append(Proxy.class).append("> proxies = new ")
			.append(ArrayList.class).append("(defaultSelector.select(uri));").newLine();

			for (final org.apache.maven.settings.Proxy proxy : this.session.getRequest().getProxies()) {
				it.append("if (\"").append(str(proxy.getProtocol())).append("\".equals(uri.getScheme())) {").increaseIndentation().newLine();

				final String nonProxyHosts = proxy.getNonProxyHosts();
				boolean hasProxy = false;
				if (!Strings.isEmpty(nonProxyHosts)) {
					if (nonProxyHosts != null) {
						hasProxy = true;
						it.append("if ("); //$NON-NLS-1$
						final StringTokenizer tokenizer = new StringTokenizer(nonProxyHosts, "|"); //$NON-NLS-1$
						boolean first = true;
						while (tokenizer.hasMoreTokens()) {
							String pattern = tokenizer.nextToken();
							pattern = pattern.replace(".", "\\.").replace("*", ".*"); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$//$NON-NLS-4$
							if (first) {
								first = false;
							} else {
								it.newLine().append(" && "); //$NON-NLS-1$
							}
							it.append("!proxyNameMatches(\"^").append(Strings.convertToJavaString(pattern)); //$NON-NLS-1$
							it.append("$\", uri.getHost())"); //$NON-NLS-1$
						}
						it.append(") {"); //$NON-NLS-1$
						it.increaseIndentation().newLine();
					}
				}

				it.append("proxies.add(new ").append(Proxy.class).append("(")
				.append(Type.class).append(".HTTP, new ").append(InetSocketAddress.class)
				.append("(\"").append(str(proxy.getHost())).append("\", ")
				.append(str(proxy.getPort())).append(")));");

				if (hasProxy) {
					it.decreaseIndentation().newLine();
					it.append("}"); //$NON-NLS-1$
				}

				it.decreaseIndentation().newLine();
				it.append("}"); //$NON-NLS-1$
			}

			it.newLine().append("return ").append(Collections.class).append(".unmodifiableList(proxies);")
			.decreaseIndentation().newLine();
			it.append("}").newLine();
			it.append("public void connectFailed(").append(java.net.URI.class).append(" uri, ")
			.append(SocketAddress.class).append(" sa, ").append(IOException.class).append(" ioe) {")
			.increaseIndentation().newLine();
			it.append("throw new ").append(RuntimeException.class).append("(ioe);").decreaseIndentation().newLine();
			it.append("}").decreaseIndentation().newLine();
			it.append("};").newLine();
			it.append(ProxySelector.class).append(".setDefault(newSelector);").decreaseIndentation().newLine();
			it.append("}");
		}

		it.decreaseIndentation().newLine().append("}").newLine();

		write(outputFolder,
				BASE_PACKAGE, "AbstractBaseTest", //$NON-NLS-1$
				importManager, it);
	}

	/** Create the name for the output filder.
	 *
	 * @param root the root folder.
	 * @param packageName the name of the package.
	 * @return the output folder.
	 */
	private static File getOutputFolder(File root, String packageName) {
		final File relativeFolder = toPackageFolder(packageName);
		return FileSystem.join(root, relativeFolder);
	}

	/** Create the name for the Java file.
	 *
	 * @param folderName the folder name.
	 * @param typeName the type name.
	 * @return the Java filename.
	 */
	protected static File getOutputJavaFilename(File folderName, String typeName) {
		return new File(folderName, typeName + ".java"); //$NON-NLS-1$
	}

	/** Create the name for the trace file.
	 *
	 * @param folderName the folder name.
	 * @param typeName the type name.
	 * @return the trace filename.
	 */
	protected static File getOutputTraceFilename(File folderName, String typeName) {
		return new File(folderName, typeName + ".java._trace"); //$NON-NLS-1$
	}

	private static void write(File root, String packageName, String typeName, ImportManager importManager,
			ITreeAppendable it) throws IOException {
		File outputFile = getOutputFolder(root, packageName);
		outputFile.mkdirs();
		outputFile = getOutputJavaFilename(outputFile, typeName);
		try (FileWriter writer = new FileWriter(outputFile)) {
			writer.write("/* This file was automatically generated. Do not change its content. */\n\n"); //$NON-NLS-1$
			writer.write("package "); //$NON-NLS-1$
			writer.write(packageName);
			writer.write(";\n"); //$NON-NLS-1$
			for (final String importedType : importManager.getImports()) {
				writer.write("import "); //$NON-NLS-1$
				writer.write(importedType);
				writer.write(";\n"); //$NON-NLS-1$
			}
			writer.write(it.getContent());
			writer.flush();
		}
	}

	private static String toTestName(File inputFile) {
		return Strings.toFirstUpper(FileSystem.shortBasename(inputFile).replaceAll("[^a-zA-Z0-9]+", "")); //$NON-NLS-1$
	}

	private static String toActionName(String name, ValidationComponent component, int index) {
		final StringBuilder fullName = new StringBuilder();
		fullName.append(name).append("_");
		fullName.append(component.getLinenoInSourceFile()).append("_to_");
		fullName.append(component.getEndLinenoInSourceFile()).append("_");
		fullName.append(index);
		return fullName.toString();
	}

	private static String toTestDisplayName(String name, int index, ValidationComponent component) {
		final String nm;
		if (Strings.isEmpty(name)) {
			if (component instanceof DynamicValidationComponent) {
				nm = ((DynamicValidationComponent) component).functionName();
			} else {
				nm = component.getSourceFile().getName();
			}
		} else {
			nm = name;
		}
		return str(MessageFormat.format(nm, index, component.getLinenoInSourceFile()));
	}

	private static String toClassDisplayName(File inputFile, String basicTestName, String generalTestName) {
		return str(MessageFormat.format(Messages.GenerateTestsMojo_12, inputFile.getName(), basicTestName, generalTestName,
				inputFile.getParentFile().getPath()));
	}

	private static String str(Object obj) {
		if (obj == null) {
			return ""; //$NON-NLS-1$
		}
		return Strings.convertToJavaString(obj.toString());
	}

}

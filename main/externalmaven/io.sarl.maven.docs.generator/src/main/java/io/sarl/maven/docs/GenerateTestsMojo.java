/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
import java.net.Proxy.Type;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.SocketTimeoutException;
import java.net.URI;
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
import org.apache.maven.settings.Proxy;
import org.arakhne.afc.util.ListUtil;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.compiler.output.FakeTreeAppendable;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.ComparisonFailure;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;

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
		getLog().info("Generating the testing resources"); //$NON-NLS-1$
		final File output = FileSystem.convertStringToFile(this.testSourceDirectory);
		final String msg = internalExecute(files, output);
		if (!Strings.isEmpty(msg)) {
			return msg;
		}
		try {
			generateRunner(output);
			generateAbstractTest(output);
			return null;
		} catch (IOException exception) {
			final String message = Throwables.getRootCause(exception).getLocalizedMessage();
			getLog().error(message);
			getLog().debug(exception);
			return message;
		}

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

		// Do not change the "Test" postfix because it is used by Surefire for detecting tests.
		final String generalTestName = toTestName(inputFile) + "Test"; //$NON-NLS-1$

		final ImportManager importManager = new ImportManager();
		final ITreeAppendable it = new FakeTreeAppendable(importManager);

		it.append("@").append(SuppressWarnings.class).append("(\"all\")"); //$NON-NLS-1$//$NON-NLS-2$
		it.newLine();
		it.append("@").append(RunWith.class).append("("); //$NON-NLS-1$ //$NON-NLS-2$
		it.append(BASE_PACKAGE).append(".DocumentationTestRunner.class)"); //$NON-NLS-1$
		it.newLine();
		it.append("public class ").append(generalTestName).append(" extends ");  //$NON-NLS-1$//$NON-NLS-2$
		it.append(BASE_PACKAGE).append(".AbstractBaseTest {"); //$NON-NLS-1$
		it.increaseIndentation();

		int i = 0;
		for (final ValidationComponent component : successCompilationComponents) {
			getLog().debug(MessageFormat.format(Messages.GenerateTestsMojo_1,
					inputFile.getName(), component.getLineno(), component.getCode()));
			final String actionName = "success_" + component.getLineno() + "_" + i; //$NON-NLS-1$ //$NON-NLS-2$
			it.newLine();
			it.append("@").append(Test.class); //$NON-NLS-1$
			it.newLine();
			it.append("public void ").append(actionName).append("() throws Exception {"); //$NON-NLS-1$ //$NON-NLS-2$
			it.increaseIndentation().newLine();
			it.append(List.class).append("<String> issues = getScriptExecutor().compile("); //$NON-NLS-1$
			it.append(Integer.toString(component.getLineno())).append(", \""); //$NON-NLS-1$
			it.append(Strings.convertToJavaString(component.getCode()));
			it.append("\");"); //$NON-NLS-1$
			it.newLine();
			it.append("assertNoIssue(" + component.getLineno() + ", issues);"); //$NON-NLS-1$ //$NON-NLS-2$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			++i;
		}

		i = 0;
		for (final ValidationComponent component : failureCompilationComponents) {
			getLog().debug(MessageFormat.format(Messages.GenerateTestsMojo_2,
					inputFile.getName(), component.getLineno(), component.getCode()));
			final String actionName = "failure_" + component.getLineno() + "_" + i; //$NON-NLS-1$ //$NON-NLS-2$
			it.newLine();
			it.append("@").append(Test.class); //$NON-NLS-1$
			it.newLine();
			it.append("public void ").append(actionName).append("() throws Exception {"); //$NON-NLS-1$ //$NON-NLS-2$
			it.increaseIndentation().newLine();
			it.append(List.class).append("<String> issues = getScriptExecutor().compile("); //$NON-NLS-1$
			it.append(Integer.toString(component.getLineno())).append(", \""); //$NON-NLS-1$
			it.append(Strings.convertToJavaString(component.getCode()));
			it.append("\");"); //$NON-NLS-1$
			it.newLine();
			it.append("assertIssues(" + component.getLineno() + ", issues);"); //$NON-NLS-1$ //$NON-NLS-2$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			++i;
		}

		i = 0;
		for (final ValidationComponent component : factualComponents) {
			getLog().debug(MessageFormat.format(Messages.GenerateTestsMojo_3,
					inputFile.getName(), component.getLineno(), component.getCode()));
			final String actionName = "fact_" + component.getLineno() + "_" + i; //$NON-NLS-1$ //$NON-NLS-2$
			it.newLine();
			it.append("@").append(Test.class); //$NON-NLS-1$
			it.newLine();
			it.append("public void ").append(actionName).append("() throws Exception {"); //$NON-NLS-1$ //$NON-NLS-2$
			it.increaseIndentation().newLine();
			it.append("final String expected = ").append(Utils.class).append(".dump(Boolean.TRUE, false) + "); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("\"\\nOR\\nObject {\\n}\\n\";"); //$NON-NLS-1$
			it.newLine();
			it.append("Object result;"); //$NON-NLS-1$
			it.newLine();
			it.append("try {"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append("result = getScriptExecutor().execute(").append(Integer.toString(component.getLineno())); //$NON-NLS-1$
			it.append(", \"").append(Strings.convertToJavaString(component.getCode())); //$NON-NLS-1$
			it.append("\");"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("} catch (Throwable exception) {"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append("throw new ").append(ComparisonFailure.class); //$NON-NLS-1$
			it.append("(exception.getLocalizedMessage() + \" [line: ").append(Integer.toString(component.getLineno())); //$NON-NLS-1$
			it.append("]\", expected, ").append(Throwables.class); //$NON-NLS-1$
			it.append(".getStackTraceAsString(exception));"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			it.newLine();
			it.append("if (result instanceof Boolean) {"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append("boolean boolResult = ((Boolean) result).booleanValue();"); //$NON-NLS-1$
			it.newLine();
			it.append("if (!boolResult) {"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append("throw new ").append(ComparisonFailure.class); //$NON-NLS-1$
			it.append("(\"Invalid expression result [line: ").append(Integer.toString(component.getLineno())); //$NON-NLS-1$
			it.append("]\", expected, ").append(Utils.class); //$NON-NLS-1$
			it.append(".dump(result, false));"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("} else if (result == null || result instanceof Exception) {"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append("throw new ").append(ComparisonFailure.class); //$NON-NLS-1$
			it.append("(\"Invalid expression result [line: ").append(Integer.toString(component.getLineno())); //$NON-NLS-1$
			it.append("]\", expected, ").append(Utils.class); //$NON-NLS-1$
			it.append(".dump(result, false));"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			++i;
		}

		i = 0;
		for (final DynamicValidationComponent component : specificComponents) {
			getLog().debug(MessageFormat.format(Messages.GenerateTestsMojo_4,
					inputFile.getName(), component.functionName() + i));
			final String actionName = component.functionName() + i;
			it.newLine();
			it.append("@").append(Test.class); //$NON-NLS-1$
			it.newLine();
			it.append("public void ").append(actionName).append("() throws Exception {"); //$NON-NLS-1$ //$NON-NLS-2$
			it.increaseIndentation().newLine();
			component.generateValidationCode(it);
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			++i;
		}

		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine();

		final File packagePath = relativeInputFile.getParentFile();
		final String packageName = toPackageName("docs", packagePath); //$NON-NLS-1$
		write(outputFolder, packageName, generalTestName, importManager, it);
	}

	@SuppressWarnings({"checkstyle:methodlength", "checkstyle:npathcomplexity"})
	private void generateAbstractTest(File outputFolder) throws IOException {
		getLog().debug("Generating abstract test"); //$NON-NLS-1$
		final ImportManager importManager = new ImportManager();
		final ITreeAppendable it = new FakeTreeAppendable(importManager);
		it.append("@").append(SuppressWarnings.class).append("(\"all\")"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("public class AbstractBaseTest {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("protected static String STR_SUCCESS = \"success\";"); //$NON-NLS-1$
		it.newLine();
		it.append("protected static String STR_FAILURE = \"failure\";"); //$NON-NLS-1$
		it.newLine();
		it.append("protected static String STR_FACT = \"fact\";"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("private static ").append(Injector.class).append(" injector = "); //$NON-NLS-1$ //$NON-NLS-2$
		it.append(DocumentationSetup.class).append(".doSetup();"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("private ").append(ScriptExecutor.class).append(" scriptExecutor;"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine().newLine();
		it.append("protected ").append(ScriptExecutor.class).append(" getScriptExecutor() {"); //$NON-NLS-1$ //$NON-NLS-2$
		it.increaseIndentation().newLine();
		it.append("if (this.scriptExecutor == null) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("this.scriptExecutor = this.injector.getInstance(").append(ScriptExecutor.class).append(".class);"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		final StringBuilder cp = new StringBuilder();
		for (final File cpElement : getClassPath()) {
			if (cp.length() > 0) {
				cp.append(":"); //$NON-NLS-1$
			}
			cp.append(cpElement.getAbsolutePath());
		}
		it.append("scriptExecutor.setClassPath(\""); //$NON-NLS-1$
		it.append(Strings.convertToJavaString(cp.toString()));
		it.append("\");"); //$NON-NLS-1$
		it.newLine();
		final String bootPath = getBootClassPath();
		if (!Strings.isEmpty(bootPath)) {
			it.append("scriptExecutor.setBootClassPath(\""); //$NON-NLS-1$
			it.append(Strings.convertToJavaString(bootPath));
			it.append("\");"); //$NON-NLS-1$
			it.newLine();
		}
		JavaVersion version = null;
		if (!Strings.isEmpty(this.source)) {
			version = JavaVersion.fromQualifier(this.source);
		}
		if (version == null) {
			version = JavaVersion.JAVA8;
		}
		it.append("scriptExecutor.setJavaSourceVersion(\""); //$NON-NLS-1$
		it.append(Strings.convertToJavaString(version.getQualifier()));
		it.append("\");"); //$NON-NLS-1$
		it.newLine();
		it.append("scriptExecutor.setTempFolder(").append(FileSystem.class); //$NON-NLS-1$
		it.append(".convertStringToFile(\""); //$NON-NLS-1$
		it.append(Strings.convertToJavaString(this.tempDirectory.getAbsolutePath()));
		it.append("\"));"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("return this.scriptExecutor;"); //$NON-NLS-1$
		it.newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("public void assertNoIssue(int lineno, ").append(List.class).append("<String> issues) {"); //$NON-NLS-1$ //$NON-NLS-2$
		it.increaseIndentation().newLine();
		it.append("if (issues != null && !issues.isEmpty()) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append(StringBuilder.class).append(" msg = new ").append(StringBuilder.class).append("();"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("for (String message : issues) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("msg.append(message).append(\"\\n\");"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine();
		it.append("throw new ").append(ComparisonFailure.class).append(//$NON-NLS-1$
				"(\"Expecting no issue but find one [line:\" + lineno + \"]\", \"\", msg.toString());"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("public void assertIssues(int lineno, ").append(List.class).append("<String> issues) {"); //$NON-NLS-1$ //$NON-NLS-2$
		it.increaseIndentation().newLine();
		it.append("if (issues == null || issues.isEmpty()) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append(Assert.class).append(".fail(\"Expecting issues but did not find one [line:\" + lineno + \"]\");"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("public String computeHeaderIdWithSectionNumber(String header) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("String id = header.replaceAll(\"[^a-zA-Z0-9]+\", \"-\");").newLine(); //$NON-NLS-1$
		it.append("id = id.toLowerCase();").newLine(); //$NON-NLS-1$
		it.append("id = id.replaceFirst(\"^[^a-zA-Z0-9]+\", \"\");").newLine(); //$NON-NLS-1$
		it.append("id = id.replaceFirst(\"[^a-zA-Z0-9]+$\", \"\");").newLine(); //$NON-NLS-1$
		it.append("if (Strings.isEmpty(id)) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("return \"section\";"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}").newLine(); //$NON-NLS-1$
		it.append("return id;"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("public String computeHeaderIdWithoutSectionNumber(String header) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("String id = computeHeaderIdWithSectionNumber(header);").newLine(); //$NON-NLS-1$
		it.append("id = id.replaceFirst(\"^[0-9.\\\\-]+\", \"\");").newLine(); //$NON-NLS-1$
		it.append("return id;"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("public static String getHttpCodeExplanation(int code) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("switch (code) {"); //$NON-NLS-1$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_BAD_METHOD: return \"Method Not Allowed\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_CREATED: return \"Created\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_ACCEPTED: return \"Accepted\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_AUTHORITATIVE: return "); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("\"Non-Authoritative Information\";"); //$NON-NLS-1$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NO_CONTENT: return \"No Content\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_RESET: return \"Reset Content\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PARTIAL: return \"Partial Content\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_MULT_CHOICE: return \"Multiple Choices\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_MOVED_PERM: return \"Moved Permanently\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_MOVED_TEMP: return \"Temporary Redirect\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_SEE_OTHER: return \"See Other\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_MODIFIED: return \"Not Modified\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_USE_PROXY: return \"Use Proxy\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_BAD_REQUEST: return \"Bad Request\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_UNAUTHORIZED: return \"Unauthorized\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PAYMENT_REQUIRED: return \"Payment Required\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_FORBIDDEN: return \"Forbidden\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_FOUND: return \"Not Found\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_ACCEPTABLE: return \"Not Acceptable\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PROXY_AUTH: return "); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("\"Proxy Authentication Required\";"); //$NON-NLS-1$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_CLIENT_TIMEOUT: return \"Request Time-Out\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_CONFLICT: return \"Conflict\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_GONE: return \"Gone\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_LENGTH_REQUIRED: return \"Length Required\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PRECON_FAILED: return \"Precondition Failed\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_ENTITY_TOO_LARGE: return "); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("\"Request Entity Too Large\";"); //$NON-NLS-1$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_REQ_TOO_LONG: return "); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("\"Request-URI Too Large\";"); //$NON-NLS-1$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_UNSUPPORTED_TYPE: return "); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("\"Unsupported Media Type\";"); //$NON-NLS-1$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_INTERNAL_ERROR: return "); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("\"Internal Server Error\";"); //$NON-NLS-1$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_IMPLEMENTED: return \"Not Implemented\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_BAD_GATEWAY: return \"Bad Gateway\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_UNAVAILABLE: return \"Service Unavailable\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_GATEWAY_TIMEOUT: return \"Gateway Timeout\";"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_VERSION: return "); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("\"HTTP Version Not Supported\";"); //$NON-NLS-1$
		it.newLine();
		it.append("default: return null;"); //$NON-NLS-1$
		it.newLine();
		it.append("}"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("public static boolean isAcceptableHttpCode(int code) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("return code == ").append(HttpURLConnection.class).append(".HTTP_OK"); //$NON-NLS-1$ //$NON-NLS-2$
		it.increaseIndentation().newLine();
		it.append("|| code == ").append(HttpURLConnection.class).append(".HTTP_MOVED_TEMP"); //$NON-NLS-1$ //$NON-NLS-2$
		it.decreaseIndentation().newLine();
		it.append(";"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("public static void assertURLAccessibility(int lineno, ").append(URL.class); //$NON-NLS-1$
		it.append(" url) throws ").append(Exception.class).append(" {");  //$NON-NLS-1$//$NON-NLS-2$
		it.increaseIndentation().newLine();
		it.append("int code;"); //$NON-NLS-1$
		it.newLine();
		it.append(HttpURLConnection.class).append(".setFollowRedirects(false);"); //$NON-NLS-1$
		it.newLine();
		it.append(URLConnection.class).append(" connection = url.openConnection();"); //$NON-NLS-1$
		it.newLine();
		it.append(Assume.class).append(".assumeTrue(\"Not an UTL with http[s] protocol\", connection instanceof "); //$NON-NLS-1$
		it.append(HttpURLConnection.class).append(");"); //$NON-NLS-1$
		it.newLine();
		it.append(HttpURLConnection.class).append(" httpConnection = ("); //$NON-NLS-1$
		it.append(HttpURLConnection.class).append(") connection;"); //$NON-NLS-1$
		it.newLine();
		it.append("try {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("httpConnection.setInstanceFollowRedirects(false);"); //$NON-NLS-1$
		it.newLine();
		it.append("httpConnection.setConnectTimeout(").append(Integer.toString(this.remoteLinkTimeOut)); //$NON-NLS-1$
		it.append(");"); //$NON-NLS-1$
		it.newLine();
		it.append("httpConnection.setReadTimeout(").append(Integer.toString(this.remoteLinkTimeOut)); //$NON-NLS-1$
		it.append(");"); //$NON-NLS-1$
		it.newLine();
		it.append("httpConnection.setAllowUserInteraction(false);"); //$NON-NLS-1$
		it.newLine();
		it.append("httpConnection.setRequestMethod(\"HEAD\");"); //$NON-NLS-1$
		it.newLine();
		it.append("httpConnection.connect();"); //$NON-NLS-1$
		it.newLine();
		it.append("code = httpConnection.getResponseCode();"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("} catch (").append(IOException.class).append(" exception) {"); //$NON-NLS-1$ //$NON-NLS-2$
		it.increaseIndentation().newLine();
		it.append("Throwable rootCause = ").append(Throwables.class).append(".getRootCause(exception);"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		if (this.ignoreRemoteLinkTimeOut) {
			it.append("if (rootCause instanceof ").append(SocketTimeoutException.class).append(") {"); //$NON-NLS-1$ //$NON-NLS-2$
			it.increaseIndentation().newLine();
			it.append(Assume.class).append(".assumeNoException(\""); //$NON-NLS-1$
			it.append("Connection time-out at line \" + lineno + \" when connecting to: \" + url.toExternalForm()"); //$NON-NLS-1$
			it.append(", rootCause);"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			it.newLine();
		}
		it.append("throw new ").append(RuntimeException.class); //$NON-NLS-1$
		it.append("(\"Error at line \" + lineno + \" when connecting to: \" + url.toExternalForm(), rootCause);"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("} finally {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("httpConnection.disconnect();"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine();
		it.append("if (isAcceptableHttpCode(code)) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("return;"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine();
		it.append("String explanation = getHttpCodeExplanation(code);"); //$NON-NLS-1$
		it.newLine();
		it.append("String codeMsg = !").append(Strings.class); //$NON-NLS-1$
		it.append(".isEmpty(explanation) ? code + \"/\\\"\" + explanation + \"\\\"\" : Integer.toString(code);"); //$NON-NLS-1$
		it.newLine();
		it.append(Assert.class).append(".fail(\"Invalid response code \" + codeMsg + \" at line \" + lineno "); //$NON-NLS-1$
		it.append("+ \" when connecting to: \" + url.toExternalForm());"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$

		if (!this.session.isOffline() && !this.session.getRequest().getProxies().isEmpty()) {
			it.newLine().newLine();
			it.append("private static boolean proxyNameMatches(String pattern, String name) {"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append(Pattern.class).append(" pat = "); //$NON-NLS-1$
            it.append(Pattern.class).append(".compile(pattern, "); //$NON-NLS-1$
            it.append(Pattern.class).append(".CASE_INSENSITIVE);"); //$NON-NLS-1$
			it.newLine();
            it.append(Matcher.class).append(" mat = pat.matcher(name);"); //$NON-NLS-1$
			it.newLine();
			it.append("return mat.matches();"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			it.newLine().newLine();
			it.append("static {"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append("final ").append(ProxySelector.class).append(" defaultSelector = "); //$NON-NLS-1$ //$NON-NLS-2$
			it.append(ProxySelector.class).append(".getDefault();"); //$NON-NLS-1$
			it.newLine();
			it.append(ProxySelector.class).append(" newSelector = new ").append(ProxySelector.class); //$NON-NLS-1$
			it.append("() {"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append("public ").append(List.class); //$NON-NLS-1$
			it.append("<").append(java.net.Proxy.class).append("> select(final "); //$NON-NLS-1$ //$NON-NLS-2$
			it.append(URI.class).append(" uri) {"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append(List.class).append("<").append(java.net.Proxy.class).append("> proxies = new "); //$NON-NLS-1$ //$NON-NLS-2$
			it.append(ArrayList.class).append("(defaultSelector.select(uri));"); //$NON-NLS-1$
			it.newLine();
			for (final Proxy proxy : this.session.getRequest().getProxies()) {
				it.append("if (\"").append(Strings.convertToJavaString(proxy.getProtocol())); //$NON-NLS-1$
				it.append("\".equals(uri.getScheme())) {"); //$NON-NLS-1$
				it.increaseIndentation().newLine();
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

				it.append("proxies.add(new ").append(java.net.Proxy.class); //$NON-NLS-1$
				it.append("(").append(Type.class).append(".HTTP, new "); //$NON-NLS-1$ //$NON-NLS-2$
				it.append(InetSocketAddress.class).append("(\""); //$NON-NLS-1$
				it.append(Strings.convertToJavaString(proxy.getHost()));
				it.append("\", ").append(Integer.toString(proxy.getPort())); //$NON-NLS-1$
				it.append(")));"); //$NON-NLS-1$
				if (hasProxy) {
					it.decreaseIndentation().newLine();
					it.append("}"); //$NON-NLS-1$
				}
				it.decreaseIndentation().newLine();
				it.append("}"); //$NON-NLS-1$
				it.newLine();
			}
			it.append("return ").append(Collections.class).append(".unmodifiableList(proxies);"); //$NON-NLS-1$ //$NON-NLS-2$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			it.newLine();
			it.append("public void connectFailed(").append(URI.class); //$NON-NLS-1$
			it.append(" uri, ").append(SocketAddress.class); //$NON-NLS-1$
			it.append(" sa, ").append(IOException.class); //$NON-NLS-1$
			it.append(" ioe) {"); //$NON-NLS-1$
			it.increaseIndentation().newLine();
			it.append("throw new ").append(RuntimeException.class); //$NON-NLS-1$
			it.append("(ioe);"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("};"); //$NON-NLS-1$
			it.newLine();
			it.append(ProxySelector.class).append(".setDefault(newSelector);"); //$NON-NLS-1$
			it.decreaseIndentation().newLine();
			it.append("}"); //$NON-NLS-1$
		}

		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine();

		write(outputFolder,
				BASE_PACKAGE, "AbstractBaseTest", //$NON-NLS-1$
				importManager, it);
	}

	private void generateRunner(File outputFolder) throws IOException {
		getLog().debug("Generating test runner"); //$NON-NLS-1$
		final ImportManager importManager = new ImportManager();
		final ITreeAppendable it = new FakeTreeAppendable(importManager);
		it.append("@").append(SuppressWarnings.class).append("(\"all\")"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("public class DocumentationTestRunner extends ").append(BlockJUnit4ClassRunner.class).append(" {"); //$NON-NLS-1$//$NON-NLS-2$
		it.increaseIndentation().newLine();
		it.append("protected ").append(Pattern.class).append(" pattern = "); //$NON-NLS-1$ //$NON-NLS-2$
		it.append(Pattern.class).append(".compile(\"^((?:success)|(?:failure)|(?:fact))\\\\_([0-9]+)\\\\_\");"); //$NON-NLS-1$
		it.newLine();
		it.append("protected String testName(").append(FrameworkMethod.class).append(" method) {"); //$NON-NLS-1$ //$NON-NLS-2$
		it.increaseIndentation().newLine();
		it.append("String methodName = method.getMethod().getName();"); //$NON-NLS-1$
		it.newLine();
		it.append(Matcher.class).append(" matcher = pattern.matcher(methodName);"); //$NON-NLS-1$
		it.newLine();
		it.append("if (matcher.find()) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("String typeName = matcher.group(1);"); //$NON-NLS-1$
		it.newLine();
		it.append("int lineno = Integer.valueOf(matcher.group(2));"); //$NON-NLS-1$
		it.newLine();
		it.append("if (\"success\".equals(typeName)) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("return \"Success block #\" + lineno;"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.append("if (\"failure\".equals(typeName)) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("return \"Failing block #\" + lineno;"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.append("if (\"fact\".equals(typeName)) {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("return \"Factual expression #\" + lineno;"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.append("return methodName.replaceAll(\"[_]+\", \" \");"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("protected ").append(List.class).append("<"); //$NON-NLS-1$ //$NON-NLS-2$
		it.append(FrameworkMethod.class).append("> getChildren() {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append(List.class).append("<").append(FrameworkMethod.class); //$NON-NLS-1$
		it.append("> sortedList = new ").append(ArrayList.class).append("<>();"); //$NON-NLS-1$ //$NON-NLS-2$
		it.newLine();
		it.append("for (").append(FrameworkMethod.class).append(" method : computeTestMethods()) {"); //$NON-NLS-1$ //$NON-NLS-2$
		it.increaseIndentation().newLine();
		it.append(ListUtil.class).append(".add(sortedList, (c1, c2) -> {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append(Matcher.class).append(" matcher1 = pattern.matcher(c1.getName());"); //$NON-NLS-1$
		it.newLine();
		it.append("if (!matcher1.find()) return c1.getName().compareTo(c2.getName());"); //$NON-NLS-1$
		it.newLine();
		it.append(Matcher.class).append(" matcher2 = pattern.matcher(c2.getName());"); //$NON-NLS-1$
		it.newLine();
		it.append("if (!matcher2.find()) return c1.getName().compareTo(c2.getName());"); //$NON-NLS-1$
		it.newLine();
		it.append("int cmp = Integer.compare(Integer.valueOf(matcher1.group(2)), Integer.valueOf(matcher1.group(2)));"); //$NON-NLS-1$
		it.newLine();
		it.append("if (cmp != 0) return cmp;"); //$NON-NLS-1$
		it.newLine();
		it.append("return c1.getName().compareTo(c2.getName());"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}, method, true, false);"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine();
		it.append("return sortedList;"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine().newLine();
		it.append("public DocumentationTestRunner(Class<?> clazz) throws "); //$NON-NLS-1$
		it.append(InitializationError.class).append(" {"); //$NON-NLS-1$
		it.increaseIndentation().newLine();
		it.append("super(clazz);"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.decreaseIndentation().newLine();
		it.append("}"); //$NON-NLS-1$
		it.newLine();

		write(outputFolder,
				BASE_PACKAGE, "DocumentationTestRunner", //$NON-NLS-1$
				importManager, it);
	}

	private static void write(File root, String packageName, String typeName, ImportManager importManager,
			ITreeAppendable it) throws IOException {
		final File relativeFolder = toPackageFolder(packageName);
		File outputFile = FileSystem.join(root, relativeFolder);
		outputFile.mkdirs();
		outputFile = new File(outputFile, typeName + ".java"); //$NON-NLS-1$
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
		return Strings.toFirstUpper(FileSystem.shortBasename(inputFile).replaceAll("[^a-zA-Z0-9]+", "")); //$NON-NLS-1$ //$NON-NLS-2$
	}

}

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

package io.sarl.docs.generator;

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
import org.junit.Assume;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.opentest4j.AssertionFailedError;
import org.opentest4j.TestAbortedException;

import io.sarl.docs.generator.markdown.MarkdownParser;
import io.sarl.docs.generator.parser.AbstractMarkerLanguageParser;
import io.sarl.docs.generator.parser.DynamicValidationComponent;
import io.sarl.docs.generator.parser.DynamicValidationContext;
import io.sarl.docs.generator.parser.ValidationComponent;
import io.sarl.docs.validator.DocumentationSetup;
import io.sarl.docs.validator.ScriptExecutor;
import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.util.Utils;

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
		
		it.append("@").append(DisplayName.class).append("(\"").append(displayName).append("\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		it.append("@").append(Tag.class).append("(\"documentation\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("@").append(Tag.class).append("(\"doc\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("public class ").append(generalTestName).append(" extends ") //$NON-NLS-1$ //$NON-NLS-2$
			.append(BASE_PACKAGE).append(".AbstractBaseTest").append(" {").increaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		
		generateTestsForSuccessCode(it, importManager, relativeInputFile, successCompilationComponents);
		generateTestsForFailureCode(it, importManager, relativeInputFile, failureCompilationComponents);
		generateTestsForFacts(it, importManager, relativeInputFile, factualComponents);
		generateDynamicTests(it, importManager, relativeInputFile, specificComponents);

		it.decreaseIndentation().newLine().append("}").newLine(); //$NON-NLS-1$

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
			it.append("@").append(Test.class).newLine(); //$NON-NLS-1$
			it.append("@").append(DisplayName.class).append("(\"").append(displayName).append("\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			it.append("@").append(Tag.class).append("(\"success\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("@").append(Tag.class).append("(\"success_").append(Integer.toString(i)).append("\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			it.append("public void ").append(actionName).append("() throws ") //$NON-NLS-1$ //$NON-NLS-2$
				.append(Exception.class).append(" {").increaseIndentation().newLine(); //$NON-NLS-1$
			it.append(List.class).append("<String> issues = getScriptExecutor().compile(") //$NON-NLS-1$
				.append(str(component.getLinenoInSourceFile())).append(", \"") //$NON-NLS-1$
				.append(str(component.getCode())).append("\");").newLine(); //$NON-NLS-1$
			it.append("assertNoIssue(").append(str(component.getLinenoInSourceFile())) //$NON-NLS-1$
				.append(", issues);").decreaseIndentation().newLine(); //$NON-NLS-1$
			it.append("}").newLine(); //$NON-NLS-1$
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
			it.append("@").append(Test.class).newLine(); //$NON-NLS-1$
			it.append("@").append(DisplayName.class).append("(\"").append(displayName).append("\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			it.append("@").append(Tag.class).append("(\"failure\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("@").append(Tag.class).append("(\"failure_").append(Integer.toString(i)).append("\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			it.append("public void ").append(actionName).append("() throws ") //$NON-NLS-1$ //$NON-NLS-2$
				.append(Exception.class).append(" {").increaseIndentation().newLine(); //$NON-NLS-1$
			it.append(List.class).append("<String> issues = getScriptExecutor().compile(") //$NON-NLS-1$
				.append(str(component.getLinenoInSourceFile())).append(", \"") //$NON-NLS-1$
				.append(str(component.getCode())).append("\");").newLine(); //$NON-NLS-1$
			it.append("assertIssues(").append(str(component.getLinenoInSourceFile())) //$NON-NLS-1$
				.append(", issues);").decreaseIndentation().newLine(); //$NON-NLS-1$
			it.append("}").newLine(); //$NON-NLS-1$
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
			it.append("@").append(Test.class).newLine(); //$NON-NLS-1$
			it.append("@").append(DisplayName.class).append("(\"").append(displayName).append("\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			it.append("@").append(Tag.class).append("(\"fact\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("@").append(Tag.class).append("(\"fact_").append(Integer.toString(i)).append("\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			it.append("public void ").append(actionName).append("() throws ") //$NON-NLS-1$ //$NON-NLS-2$
				.append(Exception.class).append(" {").increaseIndentation().newLine(); //$NON-NLS-1$
			it.append("final String expected = ").append(Utils.class).append(".dump(") //$NON-NLS-1$ //$NON-NLS-2$
				.append(Boolean.class).append(".TRUE, false) + \"\\nOR\\nObject {\\n}\\n\";").newLine(); //$NON-NLS-1$
			it.append("Object result;").newLine(); //$NON-NLS-1$
			it.append("try {").increaseIndentation().newLine(); //$NON-NLS-1$
			it.append("result = getScriptExecutor().execute(").append(str(component.getLinenoInSourceFile())) //$NON-NLS-1$
				.append(", \"").append(str(component.getCode())).append("\");").decreaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("} catch (").append(Throwable.class).append(" exception) {").increaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("throw new ").append(AssertionFailedError.class) //$NON-NLS-1$
				.append("(exception.getLocalizedMessage() + \" [line: ") //$NON-NLS-1$
				.append(str(component.getLinenoInSourceFile())).append("]\", expected, ") //$NON-NLS-1$
				.append(Throwables.class).append(".getStackTraceAsString(exception));").decreaseIndentation().newLine(); //$NON-NLS-1$
			it.append("}").newLine(); //$NON-NLS-1$
			it.append("if (result instanceof ").append(Boolean.class).append(") {").increaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("boolean boolResult = ((").append(Boolean.class).append(") result).booleanValue();").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("if (!boolResult) {").increaseIndentation().newLine(); //$NON-NLS-1$
			it.append("throw new ").append(AssertionFailedError.class) //$NON-NLS-1$
				.append("(\"Invalid expression result [line: ").append(str(component.getLinenoInSourceFile())) //$NON-NLS-1$
				.append("]\", expected, ").append(Utils.class).append(".dump(result, false));").decreaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("}").decreaseIndentation().newLine(); //$NON-NLS-1$
			it.append("} else if (result == null || result instanceof ").append(Exception.class) //$NON-NLS-1$
				.append(") {").increaseIndentation().newLine(); //$NON-NLS-1$
			it.append("throw new ").append(AssertionFailedError.class) //$NON-NLS-1$
				.append("(\"Invalid expression result [line: ").append(str(component.getLinenoInSourceFile())) //$NON-NLS-1$
				.append("]\", expected, ").append(Utils.class).append(".dump(result, false));").decreaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("}").decreaseIndentation().newLine(); //$NON-NLS-1$
			it.append("}").newLine(); //$NON-NLS-1$
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
			final String actionName = toActionName("dyn_" + component.functionName(), component, i); //$NON-NLS-1$
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
			it.append("@").append(Test.class).newLine(); //$NON-NLS-1$
			it.append("@").append(DisplayName.class).append("(\"").append(displayName).append("\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			it.append("@").append(Tag.class).append("(\"other\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("@").append(Tag.class).append("(\"other_").append(Integer.toString(i)).append("\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			it.append("public void ").append(actionName).append("() throws ") //$NON-NLS-1$ //$NON-NLS-2$
				.append(Exception.class).append(" {").increaseIndentation().newLine(); //$NON-NLS-1$
			component.generateValidationCode(it);
			it.decreaseIndentation().newLine();
			it.append("}").newLine(); //$NON-NLS-1$
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

	@SuppressWarnings("removal")
	private void generateAbstractTest(File outputFolder) throws IOException {
		getLog().debug(Messages.GenerateTestsMojo_5);
		final ImportManager importManager = new ImportManager();
		final ITreeAppendable it = new FakeTreeAppendable(importManager);

		it.append("@").append(SuppressWarnings.class).append("(\"all\")").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("public class AbstractBaseTest {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("protected static String STR_SUCCESS = \"success\";").newLine(); //$NON-NLS-1$
		it.append("protected static String STR_FAILURE = \"failure\";").newLine(); //$NON-NLS-1$
		it.append("protected static String STR_FACT = \"fact\";").newLine(); //$NON-NLS-1$
		it.append("private static ").append(Injector.class).append(" injector = ") //$NON-NLS-1$ //$NON-NLS-2$
			.append(DocumentationSetup.class).append(".doSetup();").newLine(); //$NON-NLS-1$
		it.append("private ").append(ScriptExecutor.class).append(" scriptExecutor;").newLine(); //$NON-NLS-1$ //$NON-NLS-2$

		it.append("protected ").append(ScriptExecutor.class).append(" getScriptExecutor() {").increaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("if (this.scriptExecutor == null) {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("this.scriptExecutor = this.injector.getInstance(").append(ScriptExecutor.class).append(".class);").newLine(); //$NON-NLS-1$ //$NON-NLS-2$

		final StringBuilder cp = new StringBuilder();
		for (final File cpElement : getClassPath()) {
			if (cp.length() > 0) {
				cp.append(File.pathSeparator);
			}
			cp.append(cpElement.getAbsolutePath());
		}
		it.append("scriptExecutor.setClassPath(\"").append(str(cp)).append("\");").newLine(); //$NON-NLS-1$ //$NON-NLS-2$

		final StringBuilder mp = new StringBuilder();
		for (final File mpElement : getModulePath()) {
			if (mp.length() > 0) {
				mp.append(File.pathSeparator);
			}
			mp.append(mpElement.getAbsolutePath());
		}
		it.append("scriptExecutor.setModulePath(\"").append(str(mp)).append("\");").newLine(); //$NON-NLS-1$ //$NON-NLS-2$

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
		it.append("scriptExecutor.setJavaSourceVersion(\"").append(version.getQualifier()).append("\");").newLine(); //$NON-NLS-1$ //$NON-NLS-2$

		it.append("scriptExecutor.setTempFolder(new ").append(File.class).append("(\"") //$NON-NLS-1$ //$NON-NLS-2$
		.append(str(this.tempDirectory.getAbsolutePath())).append("\"));") //$NON-NLS-1$
		.decreaseIndentation().newLine();

		it.append("}").newLine(); //$NON-NLS-1$
		it.append("return this.scriptExecutor;").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").newLine(); //$NON-NLS-1$

		it.append("public void assertNoIssue(int lineno, ").append(List.class).append("<String> issues) {") //$NON-NLS-1$ //$NON-NLS-2$
		.increaseIndentation().newLine();
		it.append("if (issues != null && !issues.isEmpty()) {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append(StringBuilder.class).append(" msg = new ").append(StringBuilder.class).append("();").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("for (String message : issues) {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("msg.append(message).append(\"\\n\");").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").newLine(); //$NON-NLS-1$
		it.append("throw new ").append(AssertionFailedError.class) //$NON-NLS-1$
		.append("(\"Expecting no issue but find one [line:\" + lineno + \"]\\n\" + msg, \"\", msg.toString());") //$NON-NLS-1$
		.decreaseIndentation().newLine();
		it.append("}").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").newLine(); //$NON-NLS-1$

		it.append("public void assertIssues(int lineno, ").append(List.class).append("<String> issues) {") //$NON-NLS-1$ //$NON-NLS-2$
		.increaseIndentation().newLine();
		it.append("if (issues == null || issues.isEmpty()) {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append(Assertions.class).append(".fail(\"Expecting issues but did not find one [line:\" + lineno + \"]\");") //$NON-NLS-1$
		.decreaseIndentation().newLine();
		it.append("}").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").newLine(); //$NON-NLS-1$

		it.append("public String computeHeaderIdWithSectionNumber(String header) {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("String id = header.replaceAll(\"[^a-zA-Z0-9]+\", \"-\");").newLine(); //$NON-NLS-1$
		it.append("id = id.toLowerCase();").newLine(); //$NON-NLS-1$
		it.append("id = id.replaceFirst(\"^[^a-zA-Z0-9]+\", \"\");").newLine(); //$NON-NLS-1$
		it.append("id = id.replaceFirst(\"[^a-zA-Z0-9]+$\", \"\");").newLine(); //$NON-NLS-1$
		it.append("if (").append(Strings.class).append(".isEmpty(id)) {").increaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("return \"section\";").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").newLine(); //$NON-NLS-1$
		it.append("return id;").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").newLine(); //$NON-NLS-1$

		it.append("public String computeHeaderIdWithoutSectionNumber(String header) {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("String id = computeHeaderIdWithSectionNumber(header);").newLine(); //$NON-NLS-1$
		it.append("id = id.replaceFirst(\"^[0-9.\\\\-]+\", \"\");").newLine(); //$NON-NLS-1$
		it.append("return id;").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").newLine(); //$NON-NLS-1$

		it.append("public static String getHttpCodeExplanation(int code) {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("switch (code) {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_BAD_METHOD: return \"Method Not Allowed\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_CREATED: return \"Created\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_ACCEPTED: return \"Accepted\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_AUTHORITATIVE: return \"Non-Authoritative Information\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NO_CONTENT: return \"No Content\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_RESET: return \"Reset Content\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PARTIAL: return \"Partial Content\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_MULT_CHOICE: return \"Multiple Choices\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_MOVED_PERM: return \"Moved Permanently\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_MOVED_TEMP: return \"Temporary Redirect\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_SEE_OTHER: return \"See Other\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_MODIFIED: return \"Not Modified\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_USE_PROXY: return \"Use Proxy\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_BAD_REQUEST: return \"Bad Request\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_UNAUTHORIZED: return \"Unauthorized\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PAYMENT_REQUIRED: return \"Payment Required\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_FORBIDDEN: return \"Forbidden\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_FOUND: return \"Not Found\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_ACCEPTABLE: return \"Not Acceptable\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PROXY_AUTH: return \"Proxy Authentication Required\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_CLIENT_TIMEOUT: return \"Request Time-Out\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_CONFLICT: return \"Conflict\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_GONE: return \"Gone\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_LENGTH_REQUIRED: return \"Length Required\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_PRECON_FAILED: return \"Precondition Failed\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_ENTITY_TOO_LARGE: return \"Request Entity Too Large\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_REQ_TOO_LONG: return \"Request-URI Too Large\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_UNSUPPORTED_TYPE: return \"Unsupported Media Type\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_INTERNAL_ERROR: return \"Internal Server Error\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_NOT_IMPLEMENTED: return \"Not Implemented\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_BAD_GATEWAY: return \"Bad Gateway\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_UNAVAILABLE: return \"Service Unavailable\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_GATEWAY_TIMEOUT: return \"Gateway Timeout\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("case ").append(HttpURLConnection.class).append(".HTTP_VERSION: return \"HTTP Version Not Supported\";").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("default: return null;").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").newLine(); //$NON-NLS-1$

		it.append("public static boolean isAcceptableHttpCode(int code) {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("return code == ").append(HttpURLConnection.class) //$NON-NLS-1$
		.append(".HTTP_OK || code == ").append(HttpURLConnection.class).append(".HTTP_MOVED_TEMP;") //$NON-NLS-1$ //$NON-NLS-2$
		.decreaseIndentation().newLine();
		it.append("}").newLine(); //$NON-NLS-1$

		it.append("public static void assertURLAccessibility(int lineno, ").append(URL.class) //$NON-NLS-1$
		.append(" url) throws ").append(Exception.class).append(" {").increaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("int code;").newLine(); //$NON-NLS-1$
		it.append(HttpURLConnection.class).append(".setFollowRedirects(false);").newLine(); //$NON-NLS-1$
		it.append(URLConnection.class).append(" connection = url.openConnection();").newLine(); //$NON-NLS-1$
		it.append(Assumptions.class).append(".assumeTrue(connection instanceof ") //$NON-NLS-1$
		.append(HttpURLConnection.class).append(", \"Not an UTL with http[s] protocol\");").newLine(); //$NON-NLS-1$
		it.append(HttpURLConnection.class).append(" httpConnection = (").append(HttpURLConnection.class) //$NON-NLS-1$
		.append(") connection;").newLine(); //$NON-NLS-1$
		it.append("try {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("httpConnection.setInstanceFollowRedirects(false);").newLine(); //$NON-NLS-1$
		it.append("httpConnection.setConnectTimeout(").append(str(this.remoteLinkTimeOut)).append(");").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("httpConnection.setReadTimeout(").append(str(this.remoteLinkTimeOut)).append(");").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append("httpConnection.setAllowUserInteraction(false);").newLine(); //$NON-NLS-1$
		it.append("httpConnection.setRequestMethod(\"HEAD\");").newLine(); //$NON-NLS-1$
		it.append("httpConnection.connect();").newLine(); //$NON-NLS-1$
		it.append("code = httpConnection.getResponseCode();").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("} catch (").append(IOException.class).append(" exception) {").increaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		it.append(Throwable.class).append(" rootCause = ").append(Throwables.class).append(".getRootCause(exception);").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
		if (this.ignoreRemoteLinkTimeOut) {
			it.append("if (rootCause instanceof ").append(SocketTimeoutException.class).append(") {").increaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("throw new ").append(TestAbortedException.class) //$NON-NLS-1$
			.append("(\"Connection time-out at line \" + lineno + \" when connecting to: \" + url.toExternalForm());") //$NON-NLS-1$
			.decreaseIndentation().newLine();
			it.append("}").newLine(); //$NON-NLS-1$
		}
		it.append("throw new ").append(RuntimeException.class) //$NON-NLS-1$
		.append("(\"Error at line \" + lineno + \" when connecting to: \" + url.toExternalForm(), rootCause);") //$NON-NLS-1$
		.decreaseIndentation().newLine();
		it.append("} finally {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("httpConnection.disconnect();").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").newLine(); //$NON-NLS-1$
		it.append("if (isAcceptableHttpCode(code)) {").increaseIndentation().newLine(); //$NON-NLS-1$
		it.append("return;").decreaseIndentation().newLine(); //$NON-NLS-1$
		it.append("}").newLine(); //$NON-NLS-1$
		it.append("String explanation = getHttpCodeExplanation(code);").newLine(); //$NON-NLS-1$
		it.append("String codeMsg = !").append(Strings.class).append(".isEmpty(explanation) ? code + \"\\\"\" + explanation + \"\\\"\" : Integer.toString(code);") //$NON-NLS-1$ //$NON-NLS-2$
		.newLine();
		it.append(Assertions.class).append(".fail(\"Invalid response code \" + codeMsg + \" at line \" + lineno + \" when connecting to: \" + url.toExternalForm());") //$NON-NLS-1$
		.decreaseIndentation().newLine();
		it.append("}").newLine(); //$NON-NLS-1$

		if (!this.session.isOffline() && !this.session.getRequest().getProxies().isEmpty()) {

			it.append("private static boolean proxyNameMatches(String pattern, String name) {").increaseIndentation().newLine(); //$NON-NLS-1$
			it.append(Pattern.class).append(" pat = ").append(Pattern.class) //$NON-NLS-1$
			.append(".compile(pattern, ").append(Pattern.class).append(".CASE_INSENSITIVE);").newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append(Matcher.class).append(" mat = pat.matcher(name);").newLine(); //$NON-NLS-1$
			it.append("return mat.matches();").decreaseIndentation().newLine(); //$NON-NLS-1$
			it.append("}").newLine(); //$NON-NLS-1$

			it.append("static {").increaseIndentation().newLine(); //$NON-NLS-1$
			it.append("final ").append(ProxySelector.class).append(" defaultSelector = ") //$NON-NLS-1$ //$NON-NLS-2$
			.append(ProxySelector.class).append(".getDefault();").newLine(); //$NON-NLS-1$
			it.append(ProxySelector.class).append(" newSelector = new ") //$NON-NLS-1$
			.append(ProxySelector.class).append("() {").increaseIndentation().newLine(); //$NON-NLS-1$
			it.append("public ").append(List.class).append("<").append(Proxy.class) //$NON-NLS-1$ //$NON-NLS-2$
			.append("> select(").append(java.net.URI.class).append(" uri) {").increaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append(List.class).append("<").append(Proxy.class).append("> proxies = new ") //$NON-NLS-1$ //$NON-NLS-2$
			.append(ArrayList.class).append("(defaultSelector.select(uri));").newLine(); //$NON-NLS-1$

			for (final org.apache.maven.settings.Proxy proxy : this.session.getRequest().getProxies()) {
				it.append("if (\"").append(str(proxy.getProtocol())).append("\".equals(uri.getScheme())) {").increaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$

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

				it.append("proxies.add(new ").append(Proxy.class).append("(") //$NON-NLS-1$ //$NON-NLS-2$
				.append(Type.class).append(".HTTP, new ").append(InetSocketAddress.class) //$NON-NLS-1$
				.append("(\"").append(str(proxy.getHost())).append("\", ") //$NON-NLS-1$ //$NON-NLS-2$
				.append(str(proxy.getPort())).append(")));"); //$NON-NLS-1$

				if (hasProxy) {
					it.decreaseIndentation().newLine();
					it.append("}"); //$NON-NLS-1$
				}

				it.decreaseIndentation().newLine();
				it.append("}"); //$NON-NLS-1$
			}

			it.newLine().append("return ").append(Collections.class).append(".unmodifiableList(proxies);") //$NON-NLS-1$ //$NON-NLS-2$
			.decreaseIndentation().newLine();
			it.append("}").newLine(); //$NON-NLS-1$
			it.append("public void connectFailed(").append(java.net.URI.class).append(" uri, ") //$NON-NLS-1$ //$NON-NLS-2$
			.append(SocketAddress.class).append(" sa, ").append(IOException.class).append(" ioe) {") //$NON-NLS-1$ //$NON-NLS-2$
			.increaseIndentation().newLine();
			it.append("throw new ").append(RuntimeException.class).append("(ioe);").decreaseIndentation().newLine(); //$NON-NLS-1$ //$NON-NLS-2$
			it.append("}").decreaseIndentation().newLine(); //$NON-NLS-1$
			it.append("};").newLine(); //$NON-NLS-1$
			it.append(ProxySelector.class).append(".setDefault(newSelector);").decreaseIndentation().newLine(); //$NON-NLS-1$
			it.append("}"); //$NON-NLS-1$
		}

		it.decreaseIndentation().newLine().append("}").newLine(); //$NON-NLS-1$

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
		return Strings.toFirstUpper(FileSystem.shortBasename(inputFile).replaceAll("[^a-zA-Z0-9]+", "")); //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static String toActionName(String name, ValidationComponent component, int index) {
		final StringBuilder fullName = new StringBuilder();
		fullName.append(name).append("_").append(index).append("_"); //$NON-NLS-1$ //$NON-NLS-2$
		fullName.append(component.getLinenoInSourceFile()).append("_to_"); //$NON-NLS-1$
		fullName.append(component.getEndLinenoInSourceFile());
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
		final String filename = component.getSourceFile() != null ? component.getSourceFile().getName() : "?"; //$NON-NLS-1$
		return str(MessageFormat.format(nm, index, filename, component.getLinenoInSourceFile()));
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

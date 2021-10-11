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


package io.sarl.maven.docs.testing;

import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.UnaryOperator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.inject.Inject;
import javax.inject.Provider;

import com.google.common.io.Files;
import com.google.inject.Injector;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.interpreter.IEvaluationResult;
import org.eclipse.xtext.xbase.interpreter.IExpressionInterpreter;
import org.eclipse.xtext.xbase.validation.IssueCodes;

import io.sarl.lang.compiler.batch.CleaningPolicy;
import io.sarl.lang.compiler.batch.ICompilatedResourceReceiver;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.compiler.batch.SarlBatchCompilerUtils;
import io.sarl.lang.interpreter.SarlExpressionInterpreter;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlScript;

/** Sarl script executor.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SarlScriptExecutor implements ScriptExecutor {

	private File tmpFolder = null;

	private String classPath = Strings.emptyIfNull(null);

	private String modulePath = Strings.emptyIfNull(null);

	private UnaryOperator<ClassLoader> classLoaderBuilder;

	private String sourceVersion = Strings.emptyIfNull(null);

	private Provider<SarlBatchCompiler> compilerProvider;

	private Provider<IExpressionInterpreter> interpreterProvider;

	/** Change the injector.
	 *
	 * @param injector the new injector.
	 */
	@Inject
	public void setInjector(Injector injector) {
		this.compilerProvider = injector.getProvider(SarlBatchCompiler.class);
		this.interpreterProvider = injector.getProvider(IExpressionInterpreter.class);
	}

	@Override
	public void setTempFolder(File dirname) {
		this.tmpFolder = dirname;
	}

	@Override
	public void setClassPath(String classpath) {
		this.classPath = Strings.emptyIfNull(classpath);
	}

	@Override
	public void setModulePath(String modulePath) {
		this.modulePath = Strings.emptyIfNull(modulePath);
	}

	@Override
	public void setClassLoaderBuilder(UnaryOperator<ClassLoader> builder) {
		this.classLoaderBuilder = builder;
	}

	@Override
	public void setJavaSourceVersion(String version) {
		this.sourceVersion = Strings.emptyIfNull(version);
	}

	@Override
	public boolean isModuleSupported() {
		return SarlBatchCompilerUtils.isModuleSupported(this.sourceVersion);
	}

	private File createRootFolder() throws IOException {
		return FileSystem.createTempDirectory("sarldocs", null, this.tmpFolder); //$NON-NLS-1$
	}

	private static File createSourceFolder(File root) {
		return new File(root, "src"); //$NON-NLS-1$
	}

	private static File createGenFolder(File root) {
		return new File(root, "src-gen"); //$NON-NLS-1$
	}

	private static File createBinFolder(File root) {
		return new File(root, "bin"); //$NON-NLS-1$
	}

	private static File createFile(File sourceFolder, String content) throws IOException {
		File file = new File(sourceFolder, "test.sarl"); //$NON-NLS-1$
		file.getParentFile().mkdirs();
		Files.write(content.getBytes(), file);
		return file;
	}

	@Override
	public CompiledFile compile(int lineno, String code, List<String> issues, ICompilatedResourceReceiver receiver) throws Exception {
		File rootFolder = createRootFolder();
		File sourceFolder = createSourceFolder(rootFolder);
		File genFolder = createGenFolder(rootFolder);
		File binFolder = createBinFolder(rootFolder);
		final SarlBatchCompiler compiler = this.compilerProvider.get();
		compiler.setBasePath(rootFolder.getAbsolutePath());
		compiler.addSourcePath(sourceFolder);
		compiler.setClassOutputPath(binFolder);
		compiler.setOutputPath(genFolder);
		compiler.setGenerateGeneratedAnnotation(false);
		compiler.setGenerateInlineAnnotation(false);
		compiler.setGenerateSyntheticSuppressWarnings(true);
		compiler.setCleaningPolicy(CleaningPolicy.NO_CLEANING);
		compiler.setClassPath(this.classPath);
		if (isModuleSupported()) {
			compiler.setModulePath(this.modulePath);
		}
		compiler.setJavaSourceVersion(this.sourceVersion);
		compiler.setAllWarningSeverities(Severity.IGNORE);
		compiler.setWarningSeverity(IssueCodes.DEPRECATED_MEMBER_REFERENCE, Severity.ERROR);
		compiler.setJavaCompilerVerbose(false);
		final Logger nopLogger = Logger.getAnonymousLogger();
		nopLogger.setLevel(Level.OFF);
		compiler.setLogger(nopLogger);
		if (issues != null) {
			compiler.addIssueMessageListener((issue, uri, message) -> {
				if (issue.isSyntaxError() || issue.getSeverity() == Severity.ERROR) {
					final Integer line = issue.getLineNumber();
					final int issueLine = (line == null ? 0 : line.intValue()) + lineno;
					issues.add(MessageFormat.format(Messages.SarlScriptExecutor_1, message, issueLine));
				}
			});
		}
		if (receiver != null) {
			compiler.addCompiledResourceReceiver(receiver);
		}
		File file = createFile(sourceFolder, code);
		compiler.compile();
		return new CompiledFile(rootFolder, file);
	}

	@Override
	public Object execute(int lineno, String code) throws Exception {
		final List<String> issues = new ArrayList<>();
		final Collection<Resource> resources = new ArrayList<>();

		final CompiledFile outFile = compile(lineno,
				"package x.x.x;\n" //$NON-NLS-1$
				+ "class ____Fake_Class____ {\nstatic var __fake_attr__ : Object = {\n" //$NON-NLS-1$
				+ code + ";\n};\n}", //$NON-NLS-1$
				issues, (it) -> {
					resources.add(it);
				});
		try {
			assertNoIssue(lineno, issues);
			if (resources.isEmpty()) {
				throw new NoXtextResourceException(lineno);
			}
			for (Resource resource : resources) {
				SarlScript script = (SarlScript) resource.getContents().get(0);
				SarlClass clazz = (SarlClass) script.getXtendTypes().get(0);
				SarlField field = (SarlField) clazz.getMembers().get(0);
				XExpression xexpression = field.getInitialValue();
				final IExpressionInterpreter interpreter = this.interpreterProvider.get();
				if (interpreter instanceof SarlExpressionInterpreter && this.classLoaderBuilder != null) {
					final SarlExpressionInterpreter exprEvaluator = (SarlExpressionInterpreter) interpreter;
					final ClassLoader expandClassLoader = exprEvaluator.expandClassLoader(this.classLoaderBuilder);
					System.getProperties().put(ScriptExecutor.PROP_CLASS_LOADER, expandClassLoader);
				}
				IEvaluationResult result = interpreter.evaluate(xexpression);
				if (result.getException() == null) {
					return result.getResult();
				}
				throw new RuntimeException(result.getException());
			}
		} finally {
			if (outFile != null && outFile.getRootFolder() != null) {
				FileSystem.delete(outFile.getRootFolder());
			}
		}
		return null;
	}

	private static void assertNoIssue(int lineno, List<String> issues) {
		if (issues != null && !issues.isEmpty()) {
			StringBuilder msg = new StringBuilder();
			for (String message : issues) {
				msg.append(message).append("\n"); //$NON-NLS-1$
			}
			fail(MessageFormat.format(Messages.SarlScriptExecutor_0, lineno, msg.toString()));
		}
	}

}

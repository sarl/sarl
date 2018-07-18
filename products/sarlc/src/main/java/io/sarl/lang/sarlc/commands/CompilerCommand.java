/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.lang.sarlc.commands;

import java.io.File;
import java.nio.file.Path;
import java.util.Collections;
import java.util.LinkedList;

import com.google.common.collect.Iterables;
import com.google.inject.Provider;
import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.meta.application.CommandMetadata;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.SARLConfig;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.maven.bqextension.Constants;

/**
 * Command for compiling with SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class CompilerCommand extends CommandWithMetadata {

	private final Provider<SarlBatchCompiler> compiler;

	private final Provider<SarlcConfig> configuration;

	/** Constructor.
	 *
	 * @param compiler the SARL batch compiler.
	 * @param configuration the configuration of the tool.
	 */
	public CompilerCommand(Provider<SarlBatchCompiler> compiler, Provider<SarlcConfig> configuration) {
		super(CommandMetadata
				.builder(CompilerCommand.class)
				.description(Messages.CompilerCommand_0));
		this.compiler = compiler;
		this.configuration = configuration;
	}

	@Override
	@SuppressWarnings("checkstyle:npathcomplexity")
	public CommandOutcome run(Cli cli) {
		if (cli.standaloneArguments().isEmpty()) {
			return CommandOutcome.failed(Constants.ERROR_CODE, Messages.CompilerCommand_1);
		}

		final SarlcConfig config = this.configuration.get();

		File outputPath = config.getOutputPath();
		File workingPath = config.getWorkingPath();
		File classOutputPath = config.getClassOutputPath();

		if (outputPath == null || workingPath == null || classOutputPath == null) {
			final Iterable<File> cliFiles = Iterables.transform(
					cli.standaloneArguments(),
					it -> toFile(it));
			File root = determineCommonRoot(Iterables.concat(
					cliFiles,
					Collections.singleton(outputPath),
					Collections.singleton(workingPath),
					Collections.singleton(classOutputPath)));
			if (root != null) {
				root = normalize(root);
				if (outputPath == null) {
					outputPath = toFile(root, SARLConfig.FOLDER_SOURCE_GENERATED);
				}
				if (workingPath == null) {
					workingPath = toFile(root, SARLConfig.FOLDER_TMP);
				}
				if (classOutputPath == null) {
					classOutputPath = toFile(root, SARLConfig.FOLDER_BIN);
				}
			}
		}

		if (outputPath == null) {
			outputPath = toFile(cwd(), SARLConfig.FOLDER_SOURCE_GENERATED);
		}
		if (workingPath == null) {
			workingPath = toFile(cwd(), SARLConfig.FOLDER_TMP);
		}
		if (classOutputPath == null) {
			classOutputPath = toFile(cwd(), SARLConfig.FOLDER_BIN);
		}

		final SarlBatchCompiler comp = this.compiler.get();

		comp.setOutputPath(outputPath);
		comp.setClassOutputPath(classOutputPath);
		comp.setTempDirectory(workingPath);

		for (final String cliArg : cli.standaloneArguments()) {
			comp.addSourcePath(cliArg);
		}

		if (!comp.compile()) {
			return CommandOutcome.failed(Constants.ERROR_CODE, ""); //$NON-NLS-1$
		}
		return CommandOutcome.succeeded();
	}

	private static File normalize(File filename) {
		final Path path1 = toFile(SARLConfig.FOLDER_SOURCE_SARL).toPath();
		final Path path2 = toFile(SARLConfig.FOLDER_SOURCE_JAVA).toPath();
		final Path path3 = toFile(SARLConfig.FOLDER_TEST_SOURCE_SARL).toPath();
		final Path path = filename.toPath();
		Path toRemove = null;
		if (path.endsWith(path1)) {
			toRemove = path1;
		} else if (path.endsWith(path2)) {
			toRemove = path2;
		} else if (path.endsWith(path3)) {
			toRemove = path3;
		}
		if (toRemove != null) {
			final int nb = toRemove.getNameCount();
			File res = filename;
			for (int i = 0; i < nb; ++i) {
				res = res.getParentFile();
			}
			return res;
		}
		return filename;
	}

	private static File cwd() {
		return new File("").getAbsoluteFile(); //$NON-NLS-1$
	}

	private static File toFile(String filename) {
		File result = null;
		for (final String element : filename.split("\\/")) { //$NON-NLS-1$
			if (result == null) {
				result = new File(element);
			} else {
				result = new File(result, element);
			}
		}
		return result;
	}

	private static File toFile(File root, String filename) {
		File result = root;
		for (final String element : filename.split("\\/")) { //$NON-NLS-1$
			result = new File(result, element);
		}
		return result;
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
	private static File determineCommonRoot(Iterable<File> files) {
		LinkedList<String> longuestPrefix = null;

		for (final File file : files) {
			if (file == null) {
				continue;
			}
			final LinkedList<String> components = splitFile(file);
			if (longuestPrefix == null) {
				longuestPrefix = components;
			} else {
				int i = 0;
				while (i < longuestPrefix.size() && i < components.size()
						&& Strings.equal(longuestPrefix.get(i), components.get(i))) {
					++i;
				}
				while (i < longuestPrefix.size()) {
					longuestPrefix.removeLast();
				}
				if (longuestPrefix.isEmpty()) {
					return null;
				}
			}
		}

		if (longuestPrefix == null) {
			return null;
		}

		File prefix = null;
		for (final String component : longuestPrefix) {
			if (prefix == null) {
				prefix = new File(component);
			} else {
				prefix = new File(prefix, component);
			}
		}

		return prefix;
	}

	private static LinkedList<String> splitFile(File file) {
		final LinkedList<String> elements = new LinkedList<>();
		File current = file;
		do {
			elements.addFirst(current.getName());
			current = current.getParentFile();
		} while (current != null);
		return elements;
	}

}

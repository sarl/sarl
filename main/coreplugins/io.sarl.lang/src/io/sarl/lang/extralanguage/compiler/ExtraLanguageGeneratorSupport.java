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

package io.sarl.lang.extralanguage.compiler;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.inject.Named;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.GeneratorContext;
import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.generator.IGenerator;
import org.eclipse.xtext.generator.IGenerator2;
import org.eclipse.xtext.generator.IGeneratorContext;

/** The generator from SARL to the default target language and an extra target language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ExtraLanguageGeneratorSupport implements IGenerator, IGenerator2 {

	/** Name of the injected element for the main generator.
	 */
	public static final String MAIN_GENERATOR_NAME = "io.sarl.lang.generator.extra.MAIN"; //$NON-NLS-1$

	private IGenerator2 mainGenerator;

	private IExtraLanguageGeneratorProvider extraGeneratorProvider;

	private Logger logger;

	/** Change the logger.
	 *
	 * @param logger the logger.
	 */
	@Inject
	public void setLogger(Logger logger) {
		this.logger = logger;
	}

	/** Replies the logger.
	 *
	 * @return the logger.
	 */
	public Logger getLogger() {
		return this.logger;
	}

	/** Change the provider of the extra generators.
	 *
	 * @param provider the provider.
	 */
	@Inject
	public void setExtraGeneratorProvider(IExtraLanguageGeneratorProvider provider) {
		this.extraGeneratorProvider = provider;
	}

	/** Replies the provider of the extra generators.
	 *
	 * @return the provider.
	 */
	public IExtraLanguageGeneratorProvider getExtraGeneratorProvider() {
		return this.extraGeneratorProvider;
	}

	/** Change the main generator that should be used for generating files from SARL resource.
	 *
	 * @param generator the generator.
	 */
	@Inject
	public void setMainGenerator(@Named(MAIN_GENERATOR_NAME) IGenerator2 generator) {
		this.mainGenerator = generator;
	}

	/** Change the main generator that should be usedd dfor generating files from SARL resource.
	 *
	 * @return the generator.
	 */
	public IGenerator2 getMainGenerator() {
		return this.mainGenerator;
	}

	/** Generate the file(s) from the input resource.
	 *
	 * @param input the input resource.
	 * @param fsa the file system access.
	 * @param context the generator context.
	 */
	public final void generate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		try {
			beforeGenerate(input, fsa, context);
			doGenerate(input, fsa, context);
		} finally {
			afterGenerate(input, fsa, context);
		}
	}

	@Override
	public final void doGenerate(Resource input, IFileSystemAccess fsa) {
		final IFileSystemAccess2 casted = (IFileSystemAccess2) fsa;
		final GeneratorContext context = new GeneratorContext();
		try {
			beforeGenerate(input, casted, context);
			doGenerate(input, casted, context);
		} finally {
			afterGenerate(input, casted, context);
		}
	}

	@Override
	public void doGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final IGenerator2 mainGenerator = getMainGenerator();
		if (mainGenerator != null) {
			mainGenerator.doGenerate(input, fsa, context);
		}
		final Iterable<IRootGenerator> generators = getExtraGeneratorProvider().getGenerators(context, input);
		if (generators != null) {
			for (final IGenerator2 generator : generators) {
				try {
					generator.doGenerate(input, fsa, context);
				} catch (Throwable exception) {
					getLogger().log(Level.SEVERE, exception.getLocalizedMessage(), exception);
				}
			}
		}
	}

	@Override
	public void beforeGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final IGenerator2 mainGenerator = getMainGenerator();
		if (mainGenerator != null) {
			mainGenerator.beforeGenerate(input, fsa, context);
		}
		final Iterable<IRootGenerator> generators = getExtraGeneratorProvider().getGenerators(context, input);
		if (generators != null) {
			for (final IGenerator2 generator : generators) {
				try {
					generator.beforeGenerate(input, fsa, context);
				} catch (Throwable exception) {
					getLogger().log(Level.SEVERE, exception.getLocalizedMessage(), exception);
				}
			}
		}
	}

	@Override
	public void afterGenerate(Resource input, IFileSystemAccess2 fsa, IGeneratorContext context) {
		final IGenerator2 mainGenerator = getMainGenerator();
		if (mainGenerator != null) {
			mainGenerator.afterGenerate(input, fsa, context);
		}
		final Iterable<IRootGenerator> generators = getExtraGeneratorProvider().getGenerators(context, input);
		if (generators != null) {
			for (final IGenerator2 generator : generators) {
				try {
					generator.afterGenerate(input, fsa, context);
				} catch (Throwable exception) {
					getLogger().log(Level.SEVERE, exception.getLocalizedMessage(), exception);
				}
			}
		}
	}

}

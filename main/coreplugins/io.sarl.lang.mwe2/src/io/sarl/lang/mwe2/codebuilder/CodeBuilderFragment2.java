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

package io.sarl.lang.mwe2.codebuilder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.logging.Logger;

import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.xtext.GrammarUtil;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractStubGeneratingFragment;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.IXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.Issues;
import org.eclipse.xtext.xtext.generator.model.GuiceModuleAccess.BindingFactory;
import org.eclipse.xtext.xtext.generator.model.project.IBundleProjectConfig;
import org.eclipse.xtext.xtext.generator.model.project.IXtextProjectConfig;

import io.sarl.lang.mwe2.codebuilder.extractor.CodeElementExtractor;
import io.sarl.lang.mwe2.codebuilder.fragments.AbstractAppenderBuilderFragment;
import io.sarl.lang.mwe2.codebuilder.fragments.AbstractBuilderBuilderFragment;
import io.sarl.lang.mwe2.codebuilder.fragments.AbstractSubCodeBuilderFragment;
import io.sarl.lang.mwe2.codebuilder.fragments.BuilderFactoryFragment;
import io.sarl.lang.mwe2.codebuilder.fragments.DocumentationBuilderFragment;
import io.sarl.lang.mwe2.codebuilder.fragments.ScriptBuilderFragment;

/**
 * A {@link AbstractXtextGeneratorFragment} that enables to create a code builder for the generated language.
 *
 * <p>The generated builder could be used for helping to create Eobjects from scratch
 * (in ui wizard for example).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class CodeBuilderFragment2 extends AbstractStubGeneratingFragment {

	private static final Logger LOG = Logger.getLogger(CodeBuilderFragment2.class.getName());

	private Collection<AbstractSubCodeBuilderFragment> subFragments;

	@Inject
	private CodeElementExtractor grammarExtractor;

	/** Replies the language name.
	 *
	 * @return the language name.
	 */
	@Pure
	public String getLanguageName() {
		return Strings.toFirstUpper(GrammarUtil.getSimpleName(getGrammar()).toLowerCase());
	}

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		this.grammarExtractor.initialize(getGrammar());
		this.subFragments = initializeSubGenerators(injector);
		for (final IXtextGeneratorFragment subFragment : this.subFragments) {
			subFragment.initialize(injector);
		}
	}

	/** Initialize the sub generators.
	 *
	 * @param injector the injector.
	 * @return the list of the generators.
	 */
	@SuppressWarnings("static-method")
	protected Collection<AbstractSubCodeBuilderFragment> initializeSubGenerators(Injector injector) {
		final Collection<AbstractSubCodeBuilderFragment> fragments = new ArrayList<>();
		fragments.add(injector.getInstance(BuilderFactoryFragment.class));
		fragments.add(injector.getInstance(DocumentationBuilderFragment.class));
		fragments.add(injector.getInstance(AbstractBuilderBuilderFragment.class));
		fragments.add(injector.getInstance(AbstractAppenderBuilderFragment.class));
		fragments.add(injector.getInstance(ScriptBuilderFragment.class));
		return fragments;
	}

	@Override
	public void checkConfiguration(Issues issues) {
		super.checkConfiguration(issues);
		if (this.subFragments == null) {
			issues.addError("Sub generators are not created"); //$NON-NLS-1$
		} else {
			for (final IXtextGeneratorFragment subFragment : this.subFragments) {
				subFragment.checkConfiguration(issues);
			}
		}
	}

	@Override
	@SuppressWarnings("checkstyle:npathcomplexity")
	public void generate() {
		LOG.info("Generating the code builder for " + getLanguageName()); //$NON-NLS-1$
		final IXtextProjectConfig generalProjectConfig = getProjectConfig();
		final IBundleProjectConfig ideProjectConfig = generalProjectConfig.getGenericIde();
		if (!ideProjectConfig.isEnabled()) {
			LOG.finest("Generic IDE project is disabled. Skipping the generation of the code builder."); //$NON-NLS-1$
			return;
		}

		BuilderFactoryFragment fragment = null;
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			if (subFragment instanceof BuilderFactoryFragment) {
				fragment = (BuilderFactoryFragment) subFragment;
			} else {
				subFragment.generate();
			}
		}
		if (fragment != null) {
			fragment.generate();
		}

		if (isGenerateStub()) {
			if (isGenerateXtendStub()) {
				for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
					subFragment.generateXtendStubs();
				}
			} else {
				for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
					subFragment.generateJavaStubs();
				}
			}
		}
		createRuntimeBindings().contributeTo(getLanguage().getRuntimeGenModule());
		createEclipseBindings().contributeTo(getLanguage().getEclipsePluginGenModule());
		createIdeaBindings().contributeTo(getLanguage().getIdeGenModule());
		createWebBindings().contributeTo(getLanguage().getWebGenModule());

		final Set<String> exportedPackages = getProjectConfig().getRuntime().getManifest().getExportedPackages();
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.getExportedPackages(exportedPackages);
		}
	}

	/** Create the runtime bindings for the builders.
	 *
	 * @return the bindings.
	 */
	protected BindingFactory createRuntimeBindings() {
		final BindingFactory factory = new BindingFactory(getClass().getName());
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateRuntimeBindings(factory);
		}
		return factory;
	}

	/** Create the Eclipse bindings for the builders.
	 *
	 * @return the bindings.
	 */
	protected BindingFactory createEclipseBindings() {
		final BindingFactory factory = new BindingFactory(getClass().getName());
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateEclipseBindings(factory);
		}
		return factory;
	}

	/** Create the IDEA bindings for the builders.
	 *
	 * @return the bindings.
	 */
	protected BindingFactory createIdeaBindings() {
		final BindingFactory factory = new BindingFactory(getClass().getName());
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateIdeaBindings(factory);
		}
		return factory;
	}

	/** Create the Web-interface bindings for the builders.
	 *
	 * @return the bindings.
	 */
	protected BindingFactory createWebBindings() {
		final BindingFactory factory = new BindingFactory(getClass().getName());
		for (final AbstractSubCodeBuilderFragment subFragment : this.subFragments) {
			subFragment.generateWebBindings(factory);
		}
		return factory;
	}

}

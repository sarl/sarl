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

package io.sarl.lang;

import javax.inject.Inject;

import com.google.inject.Binder;
import com.google.inject.Injector;
import com.google.inject.Provider;
import com.google.inject.name.Names;
import org.eclipse.xtext.generator.IGenerator2;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.scoping.impl.AbstractDeclarativeScopeProvider;
import org.eclipse.xtext.service.SingletonBinding;
import org.eclipse.xtext.validation.CompositeEValidator;
import org.eclipse.xtext.validation.IssueSeveritiesProvider;
import org.eclipse.xtext.xbase.typesystem.internal.DefaultReentrantTypeResolver;

import io.sarl.lang.bugfixes.pending.bug621.Bug621SARLValidator;
import io.sarl.lang.bugfixes.refused.bug623.Bug623SARLReentrantTypeResolver;
import io.sarl.lang.bugfixes.unpublished.bug356.Bug356ImportedNamespaceScopeProvider;
import io.sarl.lang.bugfixes.unpublished.bug356.Bug356QualifiedNameConverter;
import io.sarl.lang.compiler.SARLJvmGenerator;
import io.sarl.lang.compiler.extra.ExtraLanguageGeneratorSupport;
import io.sarl.lang.validation.ConfigurableIssueSeveritiesProvider;
import io.sarl.lang.validation.IConfigurableIssueSeveritiesProvider;
import io.sarl.lang.validation.SARLValidator;

/**
 * Use this class to register components to be used at runtime / without the
 * Equinox extension registry.
 *
 * <p>DOT NOT ADD BINDINGS IN THIS CLASS. PREFER TO UPDATE THE MWE2 SCRIPT.
 *
 * @author $Author: sgalland$
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLRuntimeModule extends io.sarl.lang.AbstractSARLRuntimeModule {

	/** Provider of {@link ConfigurableIssueSeveritiesProvider}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.5
	 */
	private static class ConfigurableIssueSeveritiesProviderProvider implements Provider<ConfigurableIssueSeveritiesProvider> {

		private ConfigurableIssueSeveritiesProvider severityProvider;

		@Inject
		private Injector injector;

		ConfigurableIssueSeveritiesProviderProvider() {
			//
		}

		@Override
		public ConfigurableIssueSeveritiesProvider get() {
			if (this.severityProvider == null) {
				this.severityProvider = new ConfigurableIssueSeveritiesProvider();
				this.injector.injectMembers(this.severityProvider);
			}
			return this.severityProvider;
		}

	}

	@Override
	public void configure(Binder binder) {
		super.configure(binder);
		binder.bind(boolean.class).annotatedWith(Names.named(CompositeEValidator.USE_EOBJECT_VALIDATOR))
				.toInstance(false);
		// Configure a system singleton for issue severities provider
		final ConfigurableIssueSeveritiesProviderProvider provider = new ConfigurableIssueSeveritiesProviderProvider();
		binder.bind(ConfigurableIssueSeveritiesProvider.class).toProvider(provider);
		binder.bind(IssueSeveritiesProvider.class).toProvider(provider);
		binder.bind(IConfigurableIssueSeveritiesProvider.class).toProvider(provider);
		// Configure the extra generator/validator provider.
		binder.bind(IGenerator2.class).annotatedWith(Names.named(ExtraLanguageGeneratorSupport.MAIN_GENERATOR_NAME))
				.to(SARLJvmGenerator.class);
	}

	@Override
	public void configureIScopeProviderDelegate(Binder binder) {
		binder.bind(IScopeProvider.class).annotatedWith(Names.named(AbstractDeclarativeScopeProvider.NAMED_DELEGATE))
				.to(Bug356ImportedNamespaceScopeProvider.class);
	}

	@Override
	public Class<? extends IQualifiedNameConverter> bindIQualifiedNameConverter() {
		return Bug356QualifiedNameConverter.class;
	}

	@Override
	public Class<? extends DefaultReentrantTypeResolver> bindDefaultReentrantTypeResolver() {
		return Bug623SARLReentrantTypeResolver.class;
	}

	@Override
	@SingletonBinding(eager = true)
	public Class<? extends SARLValidator> bindSARLValidator() {
		return Bug621SARLValidator.class;
	}

}

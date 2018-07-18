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

package io.sarl.maven.bqextension.modules;

import static io.bootique.BQCoreModule.extend;

import com.google.common.util.concurrent.Service;
import com.google.inject.Binder;
import com.google.inject.Module;
import com.google.inject.binder.AnnotatedBindingBuilder;
import com.google.inject.multibindings.Multibinder;
import io.bootique.ConfigModule;
import io.bootique.command.Command;
import io.bootique.config.ConfigurationFactory;
import io.bootique.meta.application.OptionMetadata;

import io.sarl.maven.bqextension.configs.Config;
import io.sarl.maven.bqextension.configs.Configs;

/** Abstract module that has access to its bootique configuration of "sarl.*".
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public abstract class AbstractConfigModule extends ConfigModule {

	private Binder binder;

	@Override
	protected String defaultConfigPrefix() {
		return Configs.SARL_ROOT_PROPERTY_PREFIX + CONFIG_PREFIX_BUILDER.toName(getClass());
	}

	/** Replies the configuration accessor.
	 *
	 * @param <T> the type of the configuration.
	 * @param configFactory accessor to the bootique factory.
	 * @param type the type of the configuration.
	 * @return the configuration accessor.
	 */
	protected final <T> T get(ConfigurationFactory configFactory, Class<T> type) {
		assert configFactory != null;
		assert type != null;
		return configFactory.config(type, this.configPrefix);
	}

	@Override
	public final void configure(Binder builder) {
		assert this.binder == null;
		assert builder != null;
		this.binder = builder;
		try {
			configure();
			// Automatic registration of the config types.
			for (final BQConfigTypes type : getClass().getAnnotationsByType(BQConfigTypes.class)) {
				for (final Class<? extends Config> configType : type.value()) {
					registerConfig(configType);
				}
			}
		} finally {
			this.binder = null;
		}
	}

	/** Configure a binder via the exposed methods.
	 */
	protected abstract void configure();

	/** Get the underlying {@code Binder}.
	 *
	 * @return the binder.
	 */
	protected final Binder binder() {
		assert this.binder != null : "The binder can only be used inside configure()"; //$NON-NLS-1$
		return this.binder;
	}

	/**  Adds a dependency from this module to {@code type}. When the injector is
	 * created, Guice will report an error if {@code type} cannot be injected.
	 * Note that this requirement may be satisfied by implicit binding, such as
	 * a public no-arguments constructor.
	 *
	 * @param type the required binded type.
	 */
	protected final void requireBinding(Class<?> type) {
		binder().getProvider(type);
	}

	/** Adds a dependency from this module to {@code type}. When the injector is
	 * created, Guice will report an error if {@code type} cannot be injected.
	 * Note that this requirement may be satisfied by implicit binding, such as
	 * a public no-arguments constructor.
	 * The given binder for the set of services is updated with the given {@code type}.
	 *
	 * @param type the type of service to bind.
	 * @param serviceSetBinder the set of services to update.
	 */
	protected final void requireServiceBinding(Class<? extends Service> type, Multibinder<Service> serviceSetBinder) {
		requireBinding(type);
		serviceSetBinder.addBinding().to(type);
	}

	/**
	 * Create an module's instance of the given type and install it.
	 *
	 * @param type the type of the module to install.
	 */
	protected final void install(Class<? extends Module> type) {
		assert type != null;
		try {
			install(type.newInstance());
		} catch (InstantiationException | IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}

	/** Install the given module.
	 *
	 * @param module the module to install.
	 */
	protected final void install(Module module) {
		binder().install(module);
	}

	/** Binds the given type.
	 *
	 * @param <T> the type to binder.
	 * @param clazz the type to binder.
	 * @return the binding builder.
	 */
	protected final <T> AnnotatedBindingBuilder<T> bind(Class<T> clazz) {
		return binder().bind(clazz);
	}

	/** Associate the Bootique configuration variable to an environment variable.
	 *
	 * @param bootiqueVariable the name of the bootique configuration variable.
	 */
	protected void associateEnvironmentVariable(String bootiqueVariable) {
		extend(binder()).declareVar(bootiqueVariable, Configs.toEnvironmentVariableName(bootiqueVariable));
	}

	/** Associate the Bootique configuration variable to an command line option.
	 *
	 * @param opt the description of the option.
	 */
	protected void associateOption(OptionMetadata.Builder opt) {
		extend(binder()).addOption(opt.build());
	}

	/** Associate the Bootique configuration variable to an command line option.
	 *
	 * @param bootiqueVariable the name of the bootique configuration variable.
	 * @param cliOption name of the command line option.
	 * @param defaultValue the default value.
	 */
	protected void associateOption(String bootiqueVariable, String cliOption, String defaultValue) {
		extend(binder()).addOption(bootiqueVariable, defaultValue, cliOption);
	}

	/** Add a command to the bootique.
	 *
	 * @param command the command to add.
	 */
	protected void associateCommand(Class<? extends Command> command) {
		extend(binder()).addCommand(command);
	}

	/** Register a configuration into the set of configs.
	 *
	 * @param config the type of the configuration to register.
	 */
	protected void registerConfig(Class<? extends Config> config) {
		assert config != null;
		final Multibinder<Config> exporterBinder = Multibinder.newSetBinder(binder(), Config.class);
		exporterBinder.addBinding().to(config);
	}

}

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

import java.lang.reflect.Type;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

import com.google.common.base.Strings;
import com.google.inject.Module;
import io.bootique.BQModule;
import io.bootique.BQModuleProvider;
import io.bootique.Bootique;

import io.sarl.maven.bqextension.configs.Config;
import io.sarl.maven.bqextension.configs.Configs;

/** A provider of a BQ module that enables to extract documentation from the module definition itself.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public final class DocumentedModuleProvider implements BQModuleProvider {

	private final Class<? extends Module> type;

	private String description;

	private Map<String, Type> configs;

	private Collection<Class<? extends Module>> overrides;

	private DocumentedModuleProvider(Class<? extends Module> type) {
		this.type = type;
	}

	private DocumentedModuleProvider(Class<? extends Module> type, String description) {
		this.type = type;
		this.description = Strings.nullToEmpty(description);
	}

	/** Add the given modules to the provided Bootique instance.
	 *
	 * @param bootique the bootique instance.
	 * @param modules the modules to add.
	 * @return the bootique instance.
	 */
	@SafeVarargs
	public static Bootique modules(Bootique bootique, Class<? extends Module>... modules) {
		for (final Class<? extends Module> module : modules) {
			bootique.module(new DocumentedModuleProvider(module));
		}
		return bootique;
	}

	/** Add the given module to the provided Bootique instance.
	 *
	 * @param bootique the bootique instance.
	 * @param module the module to add.
	 * @param description the module's description.
	 * @return the bootique instance.
	 */
	public static Bootique module(Bootique bootique, Class<? extends Module> module, String description) {
		bootique.module(new DocumentedModuleProvider(module, description));
		return bootique;
	}

	@Override
	public Module module() {
		try {
			return this.type.getDeclaredConstructor().newInstance();
		} catch (Exception exception) {
			throw new RuntimeException(MessageFormat.format(Messages.DocumentedModuleProvider_0,
					this.type.getName()), exception);
		}
	}

	@Override
	public BQModule.Builder moduleBuilder() {
		return BQModule
				.builder(module())
				.overrides(overrides())
				.providerName(name())
				.description(description())
				.configs(configs());
	}

	@Override
	public Map<String, Type> configs() {
		if (this.configs == null) {
			this.configs = new TreeMap<>();
			for (final BQConfigTypes anno : this.type.getAnnotationsByType(BQConfigTypes.class)) {
				for (final Class<? extends Config> configType : anno.value()) {
					this.configs.put(Configs.getSectionName(configType), configType);
				}
			}
		}
		return this.configs;
	}

	@Override
	public Collection<Class<? extends Module>> overrides() {
		if (this.overrides == null) {
			this.overrides = new ArrayList<>();
			for (final BQModuleOverrides anno : this.type.getAnnotationsByType(BQModuleOverrides.class)) {
				this.overrides.addAll(Arrays.asList(anno.value()));
			}
		}
		return this.overrides;
	}

	/** Replies the description of the module.
	 *
	 * @return the description.
	 */
	public String description() {
		if (this.description == null) {
			final io.sarl.maven.bqextension.modules.BQModule anno = this.type.getAnnotation(
					io.sarl.maven.bqextension.modules.BQModule.class);
			String str = null;
			if (anno != null) {
				str = anno.value();
			}
			this.description = Strings.nullToEmpty(str);
		}
		return this.description.isEmpty() ? null : this.description;
	}
}

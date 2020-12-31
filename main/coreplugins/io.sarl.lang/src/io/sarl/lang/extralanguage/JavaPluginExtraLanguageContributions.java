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

package io.sarl.lang.extralanguage;

import java.util.ArrayList;
import java.util.Collection;
import java.util.ServiceLoader;
import java.util.function.Predicate;

import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Singleton;

/** Tool for obtaining all the contributions as an extra-language generators.
 *
 * <p>The contributions are obtained from the definitions of Java plugins.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@Singleton
public class JavaPluginExtraLanguageContributions implements IExtraLanguageContributions {

	@Inject
	private Injector injector;

	private ServiceLoader<IExtraLanguageContribution> serviceLoader;

	private Collection<IExtraLanguageContribution> contributions;

	private Predicate<IExtraLanguageContribution> checker;

	@Override
	public Collection<IExtraLanguageContribution> getContributions() {
		if (this.serviceLoader == null) {
			this.serviceLoader = ServiceLoader.load(IExtraLanguageContribution.class, getClass().getClassLoader());
		}
		if (this.contributions == null) {
			this.contributions = new ArrayList<>();
			for (final IExtraLanguageContribution contrib : this.serviceLoader) {
				if (this.checker == null || this.checker.test(contrib)) {
					this.injector.injectMembers(contrib);
					this.contributions.add(contrib);
				}
			}
		}
		return this.contributions;
	}

	@Override
	public void setContributionChecker(Predicate<IExtraLanguageContribution> checker) {
		this.checker = checker;
	}

}

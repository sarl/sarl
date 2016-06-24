/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.docs.utils;

import com.google.inject.Injector;
import org.jnario.lib.AbstractSpecCreator;

import io.sarl.lang.tests.SARLInjectorProvider;

/** Creator of specification model dedicated to SARL.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLSpecCreator extends AbstractSpecCreator {

	private final static SARLInjectorProvider injectorProvider = new SARLInjectorProvider();
	
	static {
		injectorProvider.setupRegistry();
	}
	
	private Injector injector;
	
	@Override
	protected <T> T create(Class<T> klass) {
		return this.injector.getInstance(klass);
	}

	@Override
	public void beforeSpecRun() {
		this.injector = injectorProvider.getInjector();
	}

	@Override
	public void afterSpecRun() {
		this.injector = null;
	}

}

/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.maven.compiler.abstractmojos;

import java.lang.ref.WeakReference;

import com.google.inject.Binder;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Provides;
import com.google.inject.Singleton;

import io.sarl.lang.compiler.batch.IJavaBatchCompiler;

/** Child injection module for the SARL maven plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class MavenPrivateModule implements Module {

	private final WeakReference<AbstractSarlBatchCompilerMojo> owner;
	
	/**
	 * Constructor.
	 *
	 * @param owner the owner of this injection module.
	 */
	public MavenPrivateModule(AbstractSarlBatchCompilerMojo owner) {
		this.owner = new WeakReference<>(owner);
	}

	@Override
	public void configure(Binder binder) {
		//
	}

	/** Provides the Java compiler.
	 *
	 * @param injector the current injector.
	 * @return the compiler.
	 */
	@Provides
	@Singleton
	public IJavaBatchCompiler providesJavaBatchCompiler(Injector injector) {
		final var own = this.owner.get();
		final var cmp = own.getJavaCompiler();
		final var compiler = cmp.newCompilerInstance(own.getProject(),
				own.mavenHelper,
				own.isTestContext(),
				injector);
		return compiler;
	}

}
